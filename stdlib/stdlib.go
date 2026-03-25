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
package stdlib

import "embed"

//go:embed lib
var FS embed.FS
