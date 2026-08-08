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

package io

import (
	"bytes"
	"context"
	"strings"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/values"
)

// TestConcurrentMapAccess_T1 verifies concurrent access to a State's tokenizers
// and parsers caches does not race or panic (T1 from the architectural review).
// It is an internal test because the caches and their mutex are unexported per
// engine — run with -race. Under the former package globals this drove the
// shared maps; now it drives one engine's per-State caches, the same locking
// discipline the primitives use.
func TestConcurrentMapAccess_T1(t *testing.T) {
	c := qt.New(t)
	st := NewState(nil)

	numPorts := 10
	ports := make([]*values.PortObject, numPorts)
	for i := range numPorts {
		ports[i] = values.NewStringInputPortWithBuffer(bytes.NewBufferString("(+ 1 2) (+ 3 4)"))
	}

	numGoroutines := 20
	var wg sync.WaitGroup

	// Concurrent parser creation and access (PrimRead pattern).
	t.Run("concurrent parser access", func(t *testing.T) {
		for i := range numGoroutines {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]
				rr, _ := port.AsRuneReader()
				st.mu.Lock()
				entry, ok := st.parsers[port]
				if !ok || entry == nil {
					entry = &parserEntry{parser: parser.NewParser(nil, true, rr)}
					st.parsers[port] = entry
				}
				st.mu.Unlock()
				c.Assert(entry, qt.Not(qt.IsNil))
			}(i)
		}
		wg.Wait()
	})

	// Concurrent delete operations (evictPortCache pattern).
	t.Run("concurrent delete", func(t *testing.T) {
		for _, port := range ports {
			rr, _ := port.AsRuneReader()
			st.mu.Lock()
			st.parsers[port] = &parserEntry{parser: parser.NewParser(nil, true, rr)}
			st.mu.Unlock()
		}
		for i := range numGoroutines {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]
				evictPortCache(st, port)
			}(i)
		}
		wg.Wait()
	})

	// Concurrent mixed operations (read, write, delete).
	t.Run("concurrent mixed operations", func(t *testing.T) {
		for i := 0; i < numGoroutines*3; i++ {
			wg.Add(1)
			opType := i % 3
			go func(portIdx, op int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]
				switch op {
				case 0:
					st.mu.Lock()
					_ = st.parsers[port]
					st.mu.Unlock()
				case 1:
					rr, _ := port.AsRuneReader()
					st.mu.Lock()
					st.parsers[port] = &parserEntry{parser: parser.NewParser(nil, true, rr)}
					st.mu.Unlock()
				case 2:
					evictPortCache(st, port)
				}
			}(i, opType)
		}
		wg.Wait()
	})

	// NewState fully constructs the caches (no lazy init), so both are non-nil.
	c.Assert(st.tokenizers, qt.Not(qt.IsNil))
	c.Assert(st.parsers, qt.Not(qt.IsNil))
}

// TestConcurrentReadSyntaxSamePort_T1_1 guards the reviews/2026-07-13 §4
// CONCURRENCY finding at prim_read_write.go. The io read primitives cache one
// *parser.Parser per port; before the fix they called ReadSyntax on it OUTSIDE
// the state lock (which guarded only cache get-or-create), so two threads
// reading ONE port drove the SAME parser concurrently — a data race on
// p.cur/p.err/p.toks and an UNRECOVERABLE "fatal error: concurrent map writes"
// on the lazily-allocated p.datumLabels. Unlike TestConcurrentMapAccess_T1
// above (which exercises only map get-or-create/evict), this drives concurrent
// ReadSyntax on a single shared cache entry — the actual finding. readSyntaxCached
// now holds a per-port lock across the whole ReadSyntax call, serialising reads
// on one port. Run under -race; must stay green.
func TestConcurrentReadSyntaxSamePort_T1_1(t *testing.T) {
	st := NewState(nil)
	// Datum labels force the lazy p.datumLabels map allocation on every read,
	// widening the concurrent-map-write window the finding hinges on.
	src := strings.Repeat("#0=(a b c) #0# ", 8192)
	port := values.NewStringInputPortWithBuffer(bytes.NewBufferString(src))
	rr, _ := port.AsRuneReader()
	mk := func() *parser.Parser {
		return parser.NewParser(nil, true, rr)
	}

	var wg sync.WaitGroup
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for {
				_, err := st.readSyntaxCached(context.Background(), port, mk)
				if err != nil {
					return
				}
			}
		}()
	}
	wg.Wait()
}
