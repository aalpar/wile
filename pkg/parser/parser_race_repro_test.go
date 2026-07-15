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

package parser

import (
	"context"
	"os"
	"strings"
	"sync"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
)

// TestParserSharedInstanceRace reproduces the reviews/2026-07-13 §4 CONCURRENCY
// finding at pkg/extensions/io/prim_read_write.go:227. The io read primitives
// cache one *parser.Parser per port and call ReadSyntax on it OUTSIDE the io
// state lock (the lock guards only get-or-create of the cache entry). Two
// SRFI-18 threads reading one port therefore drive the SAME parser instance
// concurrently. *Parser carries no synchronization: ReadSyntax mutates p.cur,
// p.err, p.toks, and the lazily-allocated p.datumLabels map with no guard, so
// the shared-instance access is a data race and the concurrent datumLabels map
// write is Go's UNRECOVERABLE "fatal error: concurrent map writes".
//
// This is a deliberate race, so it is gated OUT of `make ci` / `make test-race`.
// Run it explicitly to confirm the finding:
//
//	WILE_RACE_REPRO=1 go test -race -run TestParserSharedInstanceRace ./pkg/parser/
//
// Expected under -race: a WARNING: DATA RACE on *parser.Parser fields, and
// intermittently a "fatal error: concurrent map writes" on p.datumLabels.
// Once the finding is fixed (the io cache must hold the lock across ReadSyntax,
// or hand each caller its own parser), promote this to an ungated guard.
func TestParserSharedInstanceRace(t *testing.T) {
	if os.Getenv("WILE_RACE_REPRO") == "" {
		t.Skip("deliberate race repro; set WILE_RACE_REPRO=1 to run under -race")
	}

	env := environment.NewNamespace().Runtime()
	// A datum-label form forces the lazy p.datumLabels map allocation on every
	// ReadSyntax call, widening the concurrent-map-write window.
	src := strings.Repeat("#0=(a b c) #0# ", 4096)
	p := NewParser(env, true, strings.NewReader(src))

	var wg sync.WaitGroup
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range 4096 {
				_, err := p.ReadSyntax(context.Background())
				if err != nil {
					return
				}
			}
		}()
	}
	wg.Wait()
}
