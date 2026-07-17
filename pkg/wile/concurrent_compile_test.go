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

package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
)

// TestCompilePathIsSafeUnderConcurrentThreads guards the parts of the ownership
// line that HOLD: two SRFI-18 threads compiling forms that bind DISJOINT names do
// not race. Run under -race; without it, this only asserts the evals complete.
//
// SCOPE, and read this before trusting the name. The line itself is currently
// VIOLATED for the shared case: two threads defining the SAME top-level name race
// on Binding.meta (CONFIRMED 2026-07-17, see TestCompileDefineMetaRace). This test
// is green because every thread here mints distinct names, which is exactly the
// variable that repro changes. A green result here is evidence about gensym, scope
// minting and global ALLOCATION — not about the compile path as a whole.
//
// That blindness is the point of writing it down. This test was authored first, it
// passed under -race on the first run, and it would have been reported as "compile
// is already safe" had a static audit not gone looking for the shared-*Binding
// write it cannot see.
//
// What it does guard: three independent mechanisms, none enforced by a type, any of
// which a future change could quietly drop:
//
//   - gensym is stateless: values.NewTemporaryVariableName draws from crypto/rand,
//     so there is no counter to race on (values/utils.go). A counter-based gensym
//     would reintroduce one.
//   - scope identity is atomic: values.NewScope mints from an atomic.Uint64
//     (values/scope.go).
//   - global ALLOCATION is locked: GlobalEnvironmentFrame.CreateGlobalBinding takes
//     a full Lock for its check-then-write (global_environment_frame.go). Steady-state
//     global READS are lock-free by design and are not what this exercises.
//
// TEETH, and why this comment is load-bearing. A green -race test proves nothing
// until you know it can fail, and this one is a NEGATIVE result. Its teeth were
// verified 2026-07-17 by a throwaway positive control in this same harness: eight
// threads doing (vector-set! v (modulo k 8) i) on one shared vector reported
// WARNING: DATA RACE on the first run. That establishes both that the detector sees
// through this setup and that these threads genuinely overlap. It also shows the
// control was chosen carefully — the obvious candidate, a shared hashtable, is
// lock-free since T1.4 and does NOT race, so it would have produced a silent
// false-clean. If this test is ever rewritten, re-verify the teeth the same way; a
// concurrency test that cannot fail is worse than none, because it reads as
// evidence.
//
// The shared vector racing is not a defect: T1.4 decided value types carry no
// synchronization and sharing one across threads is the user's responsibility. The
// contract this guards is narrower — the ENGINE's own compile machinery.
func TestCompilePathIsSafeUnderConcurrentThreads(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name string
		// code spawns 8 threads that each compile 40 distinct forms via eval.
		code string
		why  string
	}{
		{
			name: "distinct expressions",
			why:  "the bare compile path: parse, expand, compile, no global writes",
			code: `
(define e (environment '(scheme base)))
(define (spawn i)
  (make-thread
    (lambda ()
      (let loop ((k 0))
        (if (< k 40)
            (begin
              (eval (list 'let (list (list 'x k) (list 'y (+ k 1)))
                          (list '+ 'x 'y))
                    e)
              (loop (+ k 1)))
            'done)))))
(define ts (map spawn '(0 1 2 3 4 5 6 7)))
(for-each thread-start! ts)
(for-each thread-join! ts)
'ok`,
		},
		{
			name: "minting distinct globals",
			why:  "drives CreateGlobalBinding's check-then-write from 8 threads at once",
			code: `
(define e (environment '(scheme base)))
(define (spawn i)
  (make-thread
    (lambda ()
      (let loop ((k 0))
        (if (< k 40)
            (begin
              (eval (list 'define
                          (string->symbol
                            (string-append "g" (number->string i) "_" (number->string k)))
                          k)
                    e)
              (loop (+ k 1)))
            'done)))))
(define ts (map spawn '(0 1 2 3 4 5 6 7)))
(for-each thread-start! ts)
(for-each thread-join! ts)
'ok`,
		},
		{
			name: "macro expansion",
			why:  "expansion mints scope sets and hygienic temporaries concurrently",
			code: `
(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ a) a)
    ((_ a b ...) (let ((t a)) (if t t (my-or b ...))))))
(define e (environment '(scheme base)))
(define (spawn i)
  (make-thread
    (lambda ()
      (let loop ((k 0))
        (if (< k 40)
            (begin
              (eval (list 'let (list (list 'z k)) (list 'if (list '> 'z 0) 'z 0)) e)
              (loop (+ k 1)))
            'done)))))
(define ts (map spawn '(0 1 2 3 4 5 6 7)))
(for-each thread-start! ts)
(for-each thread-join! ts)
'ok`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx, WithProfile(KitchenSink),
				WithSourceFS(stdlib.FS), WithLibraryPaths("."))
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				qt.Assert(t, eng.Close(), qt.IsNil)
			}()

			_, err = eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("%s", tc.why))
		})
	}
}
