// Package pkgmgr implements the small subset of package-manifest handling
// (nox.toml) and project scaffolding described in the Nox language spec:
// `nox init`, `nox get`, and reading dependency declarations for `nox
// build`. It intentionally implements just enough of a TOML-like format for
// this one file shape — it is not a general TOML parser.
package pkgmgr

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
)

func gitCloneCmd(url, dest string) *exec.Cmd {
	return exec.Command("git", "clone", "--depth", "1", url, dest)
}

type Manifest struct {
	Name         string
	Version      string
	Dependencies map[string]string // name -> source (e.g. "github.com/user/repo")
}

func DefaultManifest(name string) *Manifest {
	return &Manifest{Name: name, Version: "0.1.0", Dependencies: map[string]string{}}
}

// Load reads and parses a nox.toml file.
func Load(path string) (*Manifest, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	m := &Manifest{Dependencies: map[string]string{}}
	section := ""
	for _, rawLine := range strings.Split(string(data), "\n") {
		line := strings.TrimSpace(rawLine)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]") {
			section = strings.TrimSpace(line[1 : len(line)-1])
			continue
		}
		eq := strings.Index(line, "=")
		if eq < 0 {
			continue
		}
		key := strings.TrimSpace(line[:eq])
		val := strings.TrimSpace(line[eq+1:])
		val = strings.Trim(val, `"`)
		switch section {
		case "package":
			switch key {
			case "name":
				m.Name = val
			case "version":
				m.Version = val
			}
		case "dependencies":
			m.Dependencies[key] = val
		}
	}
	if m.Name == "" {
		return nil, fmt.Errorf("nox.toml: missing [package] name")
	}
	return m, nil
}

// Save writes the manifest back out in the canonical layout.
func (m *Manifest) Save(path string) error {
	var sb strings.Builder
	sb.WriteString("[package]\n")
	fmt.Fprintf(&sb, "name = %q\n", m.Name)
	fmt.Fprintf(&sb, "version = %q\n", m.Version)
	sb.WriteString("\n[dependencies]\n")
	var names []string
	for k := range m.Dependencies {
		names = append(names, k)
	}
	sort.Strings(names)
	for _, k := range names {
		fmt.Fprintf(&sb, "%s = %q\n", k, m.Dependencies[k])
	}
	return os.WriteFile(path, []byte(sb.String()), 0644)
}

// Init scaffolds a new package directory: <dir>/nox.toml and
// <dir>/src/main.nox.
func Init(dir, name string) error {
	if err := os.MkdirAll(filepath.Join(dir, "src"), 0755); err != nil {
		return err
	}
	m := DefaultManifest(name)
	if err := m.Save(filepath.Join(dir, "nox.toml")); err != nil {
		return err
	}
	mainPath := filepath.Join(dir, "src", "main.nox")
	if _, err := os.Stat(mainPath); os.IsNotExist(err) {
		stub := "package main\n\nimport(\n    \"io\"\n)\n\nfunc main() {\n    io::println(\"Hello, World!\")\n}\n"
		if err := os.WriteFile(mainPath, []byte(stub), 0644); err != nil {
			return err
		}
	}
	return nil
}

// Get fetches a dependency (e.g. "github.com/user/repo") via `git clone`
// into <root>/.nox/pkg/<source>/, and records it in the manifest's
// [dependencies] table (saved separately by the caller). It requires `git`
// to be available on PATH; this is a best-effort implementation of `nox
// get` — private registries, version pinning, and non-git sources are not
// supported.
func Get(root, source string) (string, error) {
	dest := filepath.Join(root, ".nox", "pkg", filepath.FromSlash(source))
	if _, err := os.Stat(dest); err == nil {
		return dest, nil // already fetched
	}
	if err := os.MkdirAll(filepath.Dir(dest), 0755); err != nil {
		return "", err
	}
	url := source
	if !strings.Contains(url, "://") {
		url = "https://" + url
	}
	if !strings.HasSuffix(url, ".git") {
		url += ".git"
	}
	cmd := gitCloneCmd(url, dest)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("git clone %s: %v\n%s", url, err, string(out))
	}
	return dest, nil
}

// path and directory, or ("", "", false) if none is found.
func FindManifest(dir string) (manifestPath, root string, ok bool) {
	d, err := filepath.Abs(dir)
	if err != nil {
		return "", "", false
	}
	for {
		p := filepath.Join(d, "nox.toml")
		if _, err := os.Stat(p); err == nil {
			return p, d, true
		}
		parent := filepath.Dir(d)
		if parent == d {
			return "", "", false
		}
		d = parent
	}
}
