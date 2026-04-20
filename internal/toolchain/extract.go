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
