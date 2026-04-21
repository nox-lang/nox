package toolchain

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// extractFS copies every file in src (an embed.FS rooted, per Go's
// go:embed semantics, at the directories named in the //go:embed
// directive — e.g. "include/..." and "lib/...") into dst, preserving
// structure.
func extractFS(src fs.FS, dst string) error {
	return fs.WalkDir(src, ".", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		target := filepath.Join(dst, filepath.FromSlash(path))
		if d.IsDir() {
			return os.MkdirAll(target, 0755)
		}
		data, err := fs.ReadFile(src, path)
		if err != nil {
			return err
		}
		if err := os.MkdirAll(filepath.Dir(target), 0755); err != nil {
			return err
		}
		return os.WriteFile(target, data, 0644)
	})
}

// extractFSSub is extractFS but strips a leading path prefix (e.g. "tcc/")
// from every embedded path before writing it under dst, so the extracted
// tree's own root lands directly at dst instead of at dst/<prefix>.
func extractFSSub(src fs.FS, prefix, dst string) error {
	return fs.WalkDir(src, ".", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel := strings.TrimPrefix(path, prefix)
		rel = strings.TrimPrefix(rel, "/")
		if rel == "" {
			if d.IsDir() {
				return os.MkdirAll(dst, 0755)
			}
			return nil
		}
		target := filepath.Join(dst, filepath.FromSlash(rel))
		if d.IsDir() {
			return os.MkdirAll(target, 0755)
		}
		data, err := fs.ReadFile(src, path)
		if err != nil {
			return err
		}
		if err := os.MkdirAll(filepath.Dir(target), 0755); err != nil {
			return err
		}
		// tcc's build needs to run scripts (configure) and write outputs
		// alongside sources; keep them writable+executable after extraction.
		return os.WriteFile(target, data, 0755)
	})
}
