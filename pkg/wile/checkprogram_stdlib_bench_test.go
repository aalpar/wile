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

package wile_test

import (
	"context"
	"io/fs"
	"path"
	"sort"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// stdlibLibrarySource is one embedded .sld and its content. The .sld files are
// the only self-contained compilation units in pkg/stdlib/lib: a bare .scm there
// is a library BODY, pulled in by an (include ...) declaration, and checking one
// on its own fails on unbound identifiers its .sld would have imported.
type stdlibLibrarySource struct {
	name string
	code string
}

// loadStdlibLibrarySources reads every embedded .sld. It is shared by the
// benchmark and by any census probe that wants the same corpus.
func loadStdlibLibrarySources(tb testing.TB) []stdlibLibrarySource {
	tb.Helper()
	var q []stdlibLibrarySource
	walkErr := fs.WalkDir(stdlib.FS, ".", func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(p, ".sld") {
			return nil
		}
		content, readErr := fs.ReadFile(stdlib.FS, p)
		if readErr != nil {
			return readErr
		}
		q = append(q, stdlibLibrarySource{name: path.Clean(p), code: string(content)})
		return nil
	})
	if walkErr != nil {
		tb.Fatalf("walk stdlib: %v", walkErr)
	}
	return q
}

// BenchmarkCheckProgramStdlib times parse + expand + validate + compile over the
// whole embedded stdlib, with no execution — the in-process form of
// `wile --check` across pkg/stdlib/lib. CheckProgram on a .sld compiles the
// library's included bodies too, so the corpus is the real one, not the .sld
// declaration headers alone.
//
// One engine per iteration: a library registers on first check, so re-checking
// it in the same engine is not the same work. Subtract
// BenchmarkEngineStartup/kitchen-sink to isolate the checking cost.
func BenchmarkCheckProgramStdlib(b *testing.B) {
	ctx := context.Background()
	srcs := loadStdlibLibrarySources(b)
	// Every directory holding a .sld is a library path: an (include "x.scm")
	// declaration is resolved against the library paths, not against the
	// checked file's own directory, so "." alone misses chibi/diff.scm et al.
	dirs := map[string]bool{".": true}
	for _, s := range srcs {
		dirs[path.Dir(s.name)] = true
	}
	paths := make([]string, 0, len(dirs))
	for d := range dirs {
		paths = append(paths, d)
	}
	sort.Strings(paths)
	opts := []wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(paths...),
	}

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		eng, err := wile.NewEngine(ctx, opts...)
		if err != nil {
			b.Fatalf("NewEngine: %v", err)
		}
		for _, s := range srcs {
			checkErr := eng.CheckProgram(ctx, s.code, s.name)
			if checkErr != nil {
				b.Fatalf("check %s: %v", s.name, checkErr)
			}
		}
		closeErr := eng.Close()
		if closeErr != nil {
			b.Fatalf("close: %v", closeErr)
		}
	}
}
