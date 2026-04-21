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
