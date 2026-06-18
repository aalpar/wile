// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Package stdlib provides the embedded R7RS standard library files.
//
// These libraries are compiled into the binary and available via
// [FS] for use with [github.com/aalpar/wile.WithSourceFS]. Embedders
// who want zero-configuration library support can use this directly:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithSourceFS(stdlib.FS),
//	    wile.WithSourceOS(),
//	    wile.WithLibraryPaths(),
//	)
//
// Paths in [FS] are relative to the library root (e.g., "scheme/base.sld",
// "chibi/test.sld"), not prefixed with "lib/".
package stdlib

import (
	"embed"
	"io/fs"

	"github.com/aalpar/wile/pkg/werr"
)

//go:embed lib
var rawFS embed.FS

// FS is the embedded standard library filesystem with the "lib/" prefix
// stripped. Library paths match the search path convention directly:
// "scheme/base.sld", "srfi/1.sld", etc.
var FS fs.FS

// LibFS is the embedded standard library filesystem with the "lib/" prefix
// retained. Library paths resolve as "lib/scheme/base.sld", etc. This is the
// shape consumed by [github.com/aalpar/wile/pkg/wile.StdLibFS], which re-exports
// it so the public embedder API needs no second embed of the same tree.
var LibFS fs.FS = rawFS

func init() {
	sub, err := fs.Sub(rawFS, "lib")
	if err != nil {
		panic(werr.WrapForeignErrorf(err, "stdlib: failed to create sub-FS"))
	}
	FS = sub
}
