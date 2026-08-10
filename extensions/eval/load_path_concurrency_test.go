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

package eval_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	exteval "github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/pkg/wile"
)

// Wave 3 item 15, gate 46b. Two concurrent load chains, each loading a file in
// its OWN directory that does a directory-relative (load "inner.scm") of the
// same basename. Each must reach its own inner.scm.
//
// This is the defect the per-namespace load stack caused. That stack was hung
// off the Namespace and shared by every thread in the engine, so the two chains
// interleaved their pushes and whichever pushed last decided the current
// directory for BOTH — one chain compiled the other chain's file. It is a source
// SUBSTITUTION, not a data race, so -race cannot see it; only an assertion on
// what got loaded can.
//
// Each directory's inner.scm defines a name only that directory's main.scm
// refers to, so a substitution surfaces as an unbound variable rather than as a
// silently wrong number. Repeated, because a scheduling-dependent failure that
// runs once proves nothing.
//
// The engines are separate per chain here — see the sibling test for the
// same-engine shape, which is the one the namespace stack actually broke.
func TestLoadPathStack_ConcurrentChainsResolveIndependently(t *testing.T) {
	const rounds = 20

	dirA := t.TempDir()
	dirB := t.TempDir()

	write := func(dir, name, body string) {
		t.Helper()
		qt.Assert(t, os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644), qt.IsNil)
	}
	// Distinct binding names: if a chain reaches the other directory's
	// inner.scm, its own name is never defined and the load fails loudly.
	write(dirA, "inner.scm", `(define va 300)`)
	write(dirA, "main.scm", `(load "inner.scm") va`)
	write(dirB, "inner.scm", `(define vb 300)`)
	write(dirB, "main.scm", `(load "inner.scm") vb`)

	for round := range rounds {
		t.Run(fmt.Sprintf("round-%d", round), func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()

			type outcome struct {
				val wile.Value
				err error
			}
			results := make([]outcome, 2)
			dirs := []string{dirA, dirB}

			var wg sync.WaitGroup
			var start sync.WaitGroup
			start.Add(1)
			for i, dir := range dirs {
				wg.Add(1)
				go func() {
					defer wg.Done()
					eng, err := wile.NewEngine(ctx, wile.WithExtension(exteval.Extension))
					if err != nil {
						results[i] = outcome{err: err}
						return
					}
					main := filepath.Join(dir, "main.scm")
					// Release both goroutines as close together as possible, so the
					// two chains overlap rather than running back to back.
					start.Wait()
					val, evalErr := eng.EvalMultiple(ctx, fmt.Sprintf(`(load %q)`, main))
					results[i] = outcome{val: val, err: evalErr}
				}()
			}
			start.Done()
			wg.Wait()

			for i := range results {
				c.Assert(results[i].err, qt.IsNil,
					qt.Commentf("chain %d resolved the wrong inner.scm", i))
				c.Assert(results[i].val.SchemeString(), qt.Equals, "300")
			}
		})
	}
}

// The same shape inside ONE engine, driven by SRFI-18 threads. This is the exact
// configuration the per-namespace stack broke: one Namespace, one stack, two
// concurrent chains. It needs the threads extension, so it is skipped when the
// engine cannot provide it rather than silently asserting nothing.
func TestLoadPathStack_ConcurrentThreadsInOneEngineResolveIndependently(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	dirA := t.TempDir()
	dirB := t.TempDir()
	write := func(dir, name, body string) {
		t.Helper()
		qt.Assert(t, os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644), qt.IsNil)
	}
	write(dirA, "inner.scm", `(define va 300)`)
	write(dirA, "main.scm", `(load "inner.scm") va`)
	write(dirB, "inner.scm", `(define vb 300)`)
	write(dirB, "main.scm", `(load "inner.scm") vb`)

	probeEng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	probe, err := probeEng.EvalMultiple(ctx, `(if (procedure? make-thread) 'yes 'no)`)
	if err != nil || probe.SchemeString() != "yes" {
		t.Skip("SRFI-18 threads unavailable in this profile")
	}

	program := fmt.Sprintf(`
(let ((ta (make-thread (lambda () (load %q))))
      (tb (make-thread (lambda () (load %q)))))
  (thread-start! ta)
  (thread-start! tb)
  (vector (thread-join! ta) (thread-join! tb)))`,
		filepath.Join(dirA, "main.scm"), filepath.Join(dirB, "main.scm"))

	// A fresh engine per round: the loaded files define top-level names, and
	// under the immutable top-level default a second round would be refused for
	// redefining them — a collision on the BINDING, which has nothing to do with
	// the path resolution under test.
	for round := range 20 {
		eng, engErr := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		c.Assert(engErr, qt.IsNil)
		result, evalErr := eng.EvalMultiple(ctx, program)
		c.Assert(evalErr, qt.IsNil, qt.Commentf("round %d: a thread resolved the wrong inner.scm", round))
		c.Assert(result.SchemeString(), qt.Equals, "#(300 300)", qt.Commentf("round %d", round))
	}
}
