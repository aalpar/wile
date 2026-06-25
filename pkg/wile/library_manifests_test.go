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

	"github.com/aalpar/wile/pkg/stdlib"

	qt "github.com/frankban/quicktest"
)

// manifestEngine builds an engine that can import the sealed stdlib libraries
// (Phase 8 manifest-completeness tests). Mirrors compositionEngine.
func manifestEngine(t *testing.T) *Engine {
	t.Helper()
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestSchemeBaseExportsLetSyntax pins Phase 8 Task 8A (R7RS §4.3.1 / §7.3): both
// let-syntax and letrec-syntax are part of (scheme base) and MUST be importable by
// name. Before the fix, (only (scheme base) let-syntax …) failed with
// "identifier not exported by (scheme base)".
func TestSchemeBaseExportsLetSyntax(t *testing.T) {
	testCases := []struct {
		name    string
		program string
		want    string
	}{
		{
			name: "let-syntax importable and usable",
			program: `(import (only (scheme base) let-syntax))
                      (let-syntax ((m (syntax-rules () ((_ x) (+ x 1))))) (m 41))`,
			want: "42",
		},
		{
			name: "letrec-syntax importable and usable",
			program: `(import (only (scheme base) letrec-syntax))
                      (letrec-syntax ((m (syntax-rules () ((_ x) (+ x 1))))) (m 41))`,
			want: "42",
		},
		{
			name: "both importable together",
			program: `(import (only (scheme base) let-syntax letrec-syntax))
                      (+ (let-syntax ((a (syntax-rules () ((_) 1)))) (a))
                         (letrec-syntax ((b (syntax-rules () ((_) 2)))) (b)))`,
			want: "3",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := manifestEngine(t)
			result, err := eng.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSchemeR5RSManifestCompleteness pins Phase 8 Task 8D: (scheme r5rs) must export
// the full set of R5RS-required bindings. Before the fix the manifest omitted the
// derived syntax (cond/case/let/…), the transcendental math procedures, char
// predicates, file ops, eval/force, and the R5RS exactness aliases.
//
// Each row imports the tested names via (only (scheme r5rs) …). The (only …) modifier
// validates every requested identifier against r5rs's export manifest at import time —
// a name not in the manifest raises "identifier not exported by (scheme r5rs)". That is
// the discriminating step: under KitchenSink the core syntax/primitives are also bound
// ambiently, so a bare (import (scheme r5rs)) would let unexported names resolve anyway.
// Using (only …) forces resolution through the manifest, then exercising the binding
// proves it is both exported and functional.
func TestSchemeR5RSManifestCompleteness(t *testing.T) {
	testCases := []struct {
		name    string
		program string
		want    string
	}{
		// Derived syntax.
		{"cond", `(import (only (scheme r5rs) cond else)) (cond (#t 7) (else 0))`, "7"},
		{"case", `(import (only (scheme r5rs) case else)) (case 2 ((1) 'a) ((2) 'b) (else 'c))`, "b"},
		{"and", `(import (only (scheme r5rs) and)) (and 1 2 3)`, "3"},
		{"or", `(import (only (scheme r5rs) or)) (or #f 5)`, "5"},
		{"let", `(import (only (scheme r5rs) let)) (let ((x 4)) x)`, "4"},
		{"let-star", `(import (only (scheme r5rs) let*)) (let* ((x 1) (y (+ x 1))) y)`, "2"},
		{"letrec", `(import (only (scheme r5rs) letrec lambda if)) (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 4))`, "24"},
		{"named-let", `(import (only (scheme r5rs) let if)) (let loop ((i 0) (s 0)) (if (= i 5) s (loop (+ i 1) (+ s i))))`, "10"},
		{"do", `(import (only (scheme r5rs) do)) (do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s))`, "10"},
		{"delay-force", `(import (only (scheme r5rs) delay force)) (force (delay (+ 1 2)))`, "3"},
		{"quasiquote", "(import (only (scheme r5rs) quasiquote unquote)) `(1 ,(+ 1 1) 3)", "(1 2 3)"},
		{"define-syntax", `(import (only (scheme r5rs) define-syntax syntax-rules)) (define-syntax inc (syntax-rules () ((_ x) (+ x 1)))) (inc 5)`, "6"},
		{"let-syntax", `(import (only (scheme r5rs) let-syntax syntax-rules)) (let-syntax ((m (syntax-rules () ((_) 9)))) (m))`, "9"},
		{"letrec-syntax", `(import (only (scheme r5rs) letrec-syntax syntax-rules)) (letrec-syntax ((m (syntax-rules () ((_) 9)))) (m))`, "9"},
		// Exactness aliases (R5RS spelling).
		{"exact->inexact", `(import (only (scheme r5rs) exact->inexact)) (exact->inexact 1/2)`, "0.5"},
		{"inexact->exact", `(import (only (scheme r5rs) inexact->exact)) (inexact->exact 0.5)`, "1/2"},
		// Transcendental math.
		{"sqrt", `(import (only (scheme r5rs) sqrt)) (sqrt 16)`, "4"},
		{"exp-log", `(import (only (scheme r5rs) exp log round)) (round (log (exp 1)))`, "1.0"},
		{"atan", `(import (only (scheme r5rs) atan)) (atan 0)`, "0.0"},
		// Char predicates / case.
		{"char-upcase", `(import (only (scheme r5rs) char-upcase)) (char-upcase #\a)`, "#\\A"},
		{"char-alphabetic?", `(import (only (scheme r5rs) char-alphabetic?)) (char-alphabetic? #\z)`, "#t"},
		{"char-ci=?", `(import (only (scheme r5rs) char-ci=?)) (char-ci=? #\A #\a)`, "#t"},
		// Strings.
		{"make-string", `(import (only (scheme r5rs) make-string)) (make-string 3 #\x)`, `"xxx"`},
		{"string-ci=?", `(import (only (scheme r5rs) string-ci=?)) (string-ci=? "ABC" "abc")`, "#t"},
		{"string-copy", `(import (only (scheme r5rs) string-copy)) (string-copy "hello")`, `"hello"`},
		{"string-set!", `(import (only (scheme r5rs) string-set! make-string string-ref)) (let ((s (make-string 2 #\a))) (string-set! s 0 #\z) (string-ref s 0))`, "#\\z"},
		// Mutation.
		{"set-car!", `(import (only (scheme r5rs) set-car! list car)) (let ((p (list 1 2))) (set-car! p 9) (car p))`, "9"},
		{"vector-fill!", `(import (only (scheme r5rs) vector-fill! make-vector vector-ref)) (let ((v (make-vector 3 0))) (vector-fill! v 7) (vector-ref v 1))`, "7"},
		// eval / environments.
		{"eval", `(import (only (scheme r5rs) eval scheme-report-environment)) (eval '(+ 2 3) (scheme-report-environment 5))`, "5"},
		{"interaction-environment", `(import (only (scheme r5rs) eval interaction-environment)) (eval '(* 3 4) (interaction-environment))`, "12"},
		{"null-environment", `(import (only (scheme r5rs) null-environment)) (procedure? null-environment)`, "#t"},
		// Ports / input. The string-port constructor (open-input-string) is an
		// R7RS-only helper, so we pull it from (scheme base); the binding under test
		// (input-port? / read-char / eof-object?) is the R5RS name resolved through
		// (only (scheme r5rs) …) — exercised against a base-provided string port.
		{"input-port?", `(import (only (scheme r5rs) input-port? current-input-port)) (input-port? (current-input-port))`, "#t"},
		{"read-char-from-string", `(import (only (scheme r5rs) read-char) (only (scheme base) open-input-string)) (read-char (open-input-string "Q"))`, "#\\Q"},
		{"eof-object?", `(import (only (scheme r5rs) eof-object? read-char) (only (scheme base) open-input-string)) (eof-object? (read-char (open-input-string "")))`, "#t"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng := manifestEngine(t)
			result, err := eng.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSchemeR5RSBareImportNoConflict pins that (scheme r5rs)'s expanded import list
// (base + inexact + complex + char + cxr + read + write + lazy + eval + repl + load +
// file) carries no cross-library import conflict — a bare (import (scheme r5rs)) must
// load and a re-exported name must resolve. Guards against the strict R7RS §5.6
// conflict check (PR #793) firing on a diamond among r5rs's own imports.
func TestSchemeR5RSBareImportNoConflict(t *testing.T) {
	c := qt.New(t)
	eng := manifestEngine(t)
	result, err := eng.EvalMultiple(context.Background(),
		`(import (scheme r5rs)) (exact->inexact (sqrt 4))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "2.0")
}
