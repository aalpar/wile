package wile

import (
	"embed"
	"io/fs"
)

//go:embed stdlib/lib
var stdlibRaw embed.FS

// StdLibFS provides the standard Scheme libraries shipped with wile
// (e.g., (wile algebra), (wile match), etc.) as an embedded filesystem.
// The filesystem is rooted at stdlib/, so library paths resolve as "lib/...".
// Consumers add it to the engine with WithSourceFS(StdLibFS).
var StdLibFS = mustSub(stdlibRaw, "stdlib")

func mustSub(fsys fs.FS, dir string) fs.FS {
	sub, err := fs.Sub(fsys, dir)
	if err != nil {
		panic(err)
	}
	return sub
}
