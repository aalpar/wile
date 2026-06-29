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

package core_test

// Differential golden corpus for the continuation subsystem (item 1 of
// plans/2026-06-28-continuation-testing-enhancement.md).
//
// Each program is paired with its pinned, R7RS-correct output. This turns "it
// returned the right thing once" into a standing regression net: any future VM change
// that perturbs a pinned continuation output fails here. The corpus deliberately spans
// the boundaries the reification+flip touched (call/cc escape + generators, dynamic-wind
// ordering, guard, call-with-values arity, call-with-exit, prompts/composable,
// parameterize) so a regression in any one of them surfaces as a concrete diff.
//
// TestGenerateContinuationCorpus (WILE_WRITE_CORPUS=1) emits these same programs to
// test/scheme/generated/continuation-corpus.scm so test/compare-schemes.sh can diff
// Wile against Chez/Chibi/Racket on the identical corpus — a true cross-implementation
// differential oracle when those binaries are present.

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// continuationGolden is the corpus. want is result.SchemeString() — the value's Scheme
// representation (strings render with quotes, like write).
var continuationGolden = []struct {
	name string
	code string
	want string
}{
	// --- call/cc: escape, identity, multi-shot ---
	{"callcc-escape-discards-pending", `(call/cc (lambda (k) (+ 1 (k 10))))`, "10"},
	{"callcc-value-flows", `(+ 1 (call/cc (lambda (k) (k 5))))`, "6"},
	{"callcc-is-procedure", `(call/cc procedure?)`, "#t"},
	{"callcc-multishot-counter", `(let ((k #f) (n 0)) (call/cc (lambda (c) (set! k c))) (set! n (+ n 1)) (if (< n 3) (k #f) n))`, "3"},
	{"callcc-escape-past-cwv", `(let ((k #f) (entered #f)) (call/cc (lambda (c) (set! k c))) (if entered 'escaped (begin (set! entered #t) (call-with-values (lambda () (k 1)) (lambda args 'consumer-ran)))))`, "escaped"},
	{"callcc-generator-counts-to-3", `(let ((k #f) (count 0) (trace '())) (call-with-values (lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0)))) (lambda (x) (set! trace (cons x trace)))) (set! count (+ count 1)) (if (< count 3) (k count) (reverse trace)))`, "(1 2 3)"},
	{"callcc-foreach-frames-capturable", `(let ((result '())) (for-each (lambda (x) (call/cc (lambda (k) (set! result (cons x result))))) '(1 2 3)) (reverse result))`, "(1 2 3)"},

	// --- dynamic-wind: ordering on normal / exception / escape / re-entry ---
	{"dynamic-wind-normal-order", `(let ((o '())) (dynamic-wind (lambda () (set! o (cons 'b o))) (lambda () (set! o (cons 'd o))) (lambda () (set! o (cons 'a o)))) (reverse o))`, "(b d a)"},
	{"dynamic-wind-nested-order", `(let ((r '())) (define (note x) (set! r (cons x r))) (dynamic-wind (lambda () (note 'b1)) (lambda () (dynamic-wind (lambda () (note 'b2)) (lambda () (note 'body)) (lambda () (note 'a2)))) (lambda () (note 'a1))) (reverse r))`, "(b1 b2 body a2 a1)"},
	{"dynamic-wind-after-fires-once-on-raise", `(let ((fired 0)) (guard (e (#t (list 'caught fired))) (dynamic-wind (lambda () #f) (lambda () (raise 'x)) (lambda () (set! fired (+ fired 1))))))`, "(caught 1)"},
	{"dynamic-wind-reentry-via-callcc", `(let ((trace '()) (k #f)) (dynamic-wind (lambda () (set! trace (cons 'in trace))) (lambda () (call/cc (lambda (c) (set! k c)))) (lambda () (set! trace (cons 'out trace)))) (if (< (length trace) 4) (k #f) (reverse trace)))`, "(in out in out)"},
	{"dynamic-wind-before-reruns-on-prompt-reentry", `(let ((k #f) (before 0)) (call-with-continuation-prompt (lambda () (dynamic-wind (lambda () (set! before (+ before 1))) (lambda () (call/cc (lambda (c) (set! k c) 'first))) (lambda () #f))) (default-continuation-prompt-tag) #f) (call-with-continuation-prompt (lambda () (k 'second)) (default-continuation-prompt-tag) (lambda (v) v)) before)`, "2"},

	// --- guard / exceptions ---
	{"guard-clause-binds-condition", `(guard (e ((number? e) e) (else 'other)) (raise 5))`, "5"},
	{"guard-clause-selection", `(guard (e ((eq? e 'a) 1) ((eq? e 'b) 2) (else 3)) (raise 'b))`, "2"},
	{"guard-nested-outer-catches", `(guard (outer ((symbol? outer) (list 'o outer))) (guard (inner ((number? inner) (list 'i inner))) (raise 'sym)))`, "(o sym)"},
	{"guard-reraise-to-weh", `(with-exception-handler (lambda (e) (list 'outer e)) (lambda () (guard (x ((string? x) 'str)) (raise 99))))`, "(outer 99)"},
	{"raise-continuable-returns-handler-value", `(with-exception-handler (lambda (e) (list 'h e)) (lambda () (raise-continuable 7)))`, "(h 7)"},
	{"error-object-message", `(guard (e (#t (error-object-message e))) (error "boom" 1 2))`, `"boom"`},

	// --- call-with-values / values arity ---
	{"cwv-three-values", `(call-with-values (lambda () (values 1 2 3)) list)`, "(1 2 3)"},
	{"cwv-zero-values", `(call-with-values (lambda () (values)) (lambda () 'none))`, "none"},
	{"cwv-tail-loop-O1-frames", `(let loop ((n 1000000)) (call-with-values (lambda () (values n)) (lambda (m) (if (= m 0) 'done (loop (- m 1))))))`, "done"},
	{"let-values-binds", `(let-values (((a b) (values 1 2)) ((c) (values 3))) (+ a b c))`, "6"},

	// --- call-with-exit ---
	{"call-with-exit-escape", `(call-with-exit (lambda (e) (e 42) 99))`, "42"},
	{"call-with-exit-normal-multivalue", `(call-with-values (lambda () (call-with-exit (lambda (k) (values 1 2 3)))) list)`, "(1 2 3)"},

	// --- prompts / composable / abort ---
	{"prompt-normal-return", `(call-with-continuation-prompt (lambda () 5) (make-continuation-prompt-tag) #f)`, "5"},
	{"prompt-abort-to-handler", `(call-with-continuation-prompt (lambda () (+ 1 (abort-current-continuation (default-continuation-prompt-tag) 41))) (default-continuation-prompt-tag) (lambda (v) (* v 2)))`, "82"},
	{"prompt-abort-handler-squares", `(+ 100 (call-with-continuation-prompt (lambda () (abort-current-continuation (default-continuation-prompt-tag) 5)) (default-continuation-prompt-tag) (lambda (v) (* v v))))`, "125"},
	{"composable-continuation-composes", `(call-with-continuation-prompt (lambda () (* 2 (call-with-composable-continuation (lambda (k) (+ 1 (k 5))) (default-continuation-prompt-tag)))) (default-continuation-prompt-tag) (lambda (v) v))`, "11"},

	// --- parameterize + control ---
	{"parameterize-scoped", `(let ((p (make-parameter 1))) (list (p) (parameterize ((p 2)) (p)) (p)))`, "(1 2 1)"},
	{"parameterize-value-under-callcc", `(let ((p (make-parameter 10)) (k #f)) (parameterize ((p 20)) (call/cc (lambda (c) (set! k c))) (p)))`, "20"},
}

func TestContinuationGoldenCorpus(t *testing.T) {
	for _, tc := range continuationGolden {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestGenerateContinuationCorpus emits the corpus to a .scm file so
// test/compare-schemes.sh can diff Wile against other Scheme implementations on the
// same programs. Gated on WILE_WRITE_CORPUS=1 so the normal test run does not write to
// the source tree; run it explicitly to refresh the cross-scheme corpus.
func TestGenerateContinuationCorpus(t *testing.T) {
	if os.Getenv("WILE_WRITE_CORPUS") == "" {
		t.Skip("set WILE_WRITE_CORPUS=1 to (re)emit test/scheme/generated/continuation-corpus.scm")
	}
	var b strings.Builder
	b.WriteString(";; AUTO-GENERATED by TestGenerateContinuationCorpus (WILE_WRITE_CORPUS=1).\n")
	b.WriteString(";; Continuation differential corpus — see plans/2026-06-28-continuation-testing-enhancement.md.\n")
	b.WriteString(";; Run a reference Scheme over these and diff against Wile via test/compare-schemes.sh.\n")
	b.WriteString("(import (scheme base) (scheme write))\n")
	for _, tc := range continuationGolden {
		b.WriteString("\n;; want: " + tc.want + "\n(write ")
		b.WriteString(tc.code)
		b.WriteString(") (newline)\n")
	}
	dir := filepath.FromSlash("../../../test/scheme/generated")
	err := os.MkdirAll(dir, 0o755)
	if err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	path := filepath.Join(dir, "continuation-corpus.scm")
	err = os.WriteFile(path, []byte(b.String()), 0o644)
	if err != nil {
		t.Fatalf("write: %v", err)
	}
	t.Logf("wrote %d-program corpus to %s", len(continuationGolden), path)
}
