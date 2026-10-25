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
	"github.com/aalpar/wile/pkg/werr"
)

// TestConcurrentMapAccess_T1 verifies concurrent access to a State's parsers
// cache does not race or panic (T1 from the architectural review).
// It is an internal test because the cache and its mutex are unexported per
// engine — run with -race. Under the former package globals this drove the
// shared map; now it drives one engine's per-State cache through the code the
// primitives call: readSyntaxLocked (and through it readSyntaxCached's
// get-or-create) and evictPortCache. Each port has one reader, so what contends
// is the cache map, not one port's parser (TestConcurrentReadSyntaxSamePort_T1_1
// covers that). Assertions run on the test goroutine: a FailNow from a spawned
// goroutine does not stop the test.
func TestConcurrentMapAccess_T1(t *testing.T) {
	const numPorts = 20
	newPorts := func() []*values.PortObject {
		q := make([]*values.PortObject, numPorts)
		for i := range numPorts {
			q[i] = values.NewStringInputPortWithBuffer(bytes.NewBufferString("(+ 1 2) (+ 3 4)"))
		}
		return q
	}
	// read is the PrimRead pattern: the port's read lock, the cache's own
	// get-or-create, then a read.
	read := func(st *State, port *values.PortObject) error {
		rr, _ := port.AsRuneReader()
		mk := func() *parser.Parser {
			return parser.NewParser(nil, true, rr)
		}
		syn, err := readSyntaxLocked(context.Background(), st, port, mk)
		if err != nil {
			return err
		}
		if syn == nil {
			return errNoDatumRead
		}
		return nil
	}

	// Concurrent parser creation and access, one reader per port.
	t.Run("concurrent parser access", func(t *testing.T) {
		st := NewState(nil)
		ports := newPorts()
		errs := make([]error, numPorts)
		var wg sync.WaitGroup
		for i, port := range ports {
			wg.Go(func() {
				errs[i] = read(st, port)
			})
		}
		wg.Wait()
		for i, err := range errs {
			qt.Assert(t, err, qt.IsNil, qt.Commentf("port %d", i))
		}
		st.mu.Lock()
		cached := len(st.parsers)
		st.mu.Unlock()
		qt.Assert(t, cached, qt.Equals, numPorts)
	})

	// Concurrent delete operations (evictPortCache pattern).
	t.Run("concurrent delete", func(t *testing.T) {
		st := NewState(nil)
		ports := newPorts()
		for _, port := range ports {
			qt.Assert(t, read(st, port), qt.IsNil)
		}
		var wg sync.WaitGroup
		for i := range 2 * numPorts {
			wg.Go(func() {
				evictPortCache(st, ports[i%numPorts])
			})
		}
		wg.Wait()
		qt.Assert(t, st.parsers, qt.HasLen, 0)
	})

	// Concurrent mixed operations: per port, one reader, one evicter and one
	// map lookup.
	t.Run("concurrent mixed operations", func(t *testing.T) {
		st := NewState(nil)
		ports := newPorts()
		errs := make([]error, numPorts)
		var wg sync.WaitGroup
		for i, port := range ports {
			wg.Go(func() {
				errs[i] = read(st, port)
			})
			wg.Go(func() {
				evictPortCache(st, port)
			})
			wg.Go(func() {
				st.mu.Lock()
				_ = st.parsers[port]
				st.mu.Unlock()
			})
		}
		wg.Wait()
		for i, err := range errs {
			qt.Assert(t, err, qt.IsNil, qt.Commentf("port %d", i))
		}
	})
}

// errNoDatumRead is TestConcurrentMapAccess_T1's report that a read returned
// neither a datum nor an error.
var errNoDatumRead = werr.NewStaticError("readSyntaxLocked returned no datum and no error")

// TestConcurrentReadSyntaxSamePort_T1_1 guards the reviews/2026-07-13 §4
// CONCURRENCY finding at prim_read_write.go. The io read primitives cache one
// *parser.Parser per port; before the fix they called ReadSyntax on it OUTSIDE
// the state lock (which guarded only cache get-or-create), so two threads
// reading ONE port drove the SAME parser concurrently — a data race on
// p.cur/p.err/p.toks and an UNRECOVERABLE "fatal error: concurrent map writes"
// on the lazily-allocated p.datumLabels. Unlike TestConcurrentMapAccess_T1
// above (which exercises only map get-or-create/evict), this drives concurrent
// ReadSyntax on a single shared cache entry — the actual finding. readSyntaxLocked
// holds the port's read lock across the lookup, the ReadSyntax call and the
// eviction, serialising reads on one port. Run under -race; must stay green.
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
		wg.Go(func() {
			for {
				_, err := readSyntaxLocked(context.Background(), st, port, mk)
				if err != nil {
					return
				}
			}
		})
	}
	wg.Wait()
}
