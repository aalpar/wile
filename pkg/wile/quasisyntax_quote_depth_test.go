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
	"testing"
)

// quasisyntax is a passthrough form, so the opaque-subtree scan enters its body
// at code depth. Before the scan learned quasisyntax's markers, a quote form
// inside the template hit the depth-0 quote barrier and the `#,` beneath it was
// never marked: the set!s ran, but let-bound f was inlined with its stale body.
// Each row evaluates to (n f-result); n proves the unsyntax fired, f-result
// proves the inliner saw it. The set! is hidden in a begin so the ONLY evidence
// the analysis has is the position walk.
const quasisyntaxStaleBody = `(begin (set! n 1) (set! f (lambda () 99)))`

func TestQuasisyntaxUnsyntaxUnderQuote(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		// The measured defect: (1 7) on master.
		{"unsyntax under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		// Controls isolating the mechanism to the depth-0 quote barrier: neither
		// the bare unsyntax nor a syntax wrapper met it. Both were (1 99) on master.
		{"control: bare unsyntax",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a #,` + quasisyntaxStaleBody + `)) (list n (f)))`,
			"(1 99)"},
		{"control: unsyntax under a syntax form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (syntax (#,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		{"control: quasiquote counterpart",
			"(let ((f (lambda () 7)) (n 0)) `(a (quote (,"+quasisyntaxStaleBody+"))) (list n (f)))",
			"(1 99)"},
		// Quote nested deeper, in a vector element, and through unsyntax-splicing.
		{"unsyntax under two quote forms",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (b (quote (#,` + quasisyntaxStaleBody + `)))))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax in a vector under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote #(#,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax-splicing under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,@(begin (set! n 1) (set! f (lambda () 99)) '()))))) (list n (f)))`,
			"(1 99)"},
		// A quasiquote reached from the unsyntax's code position re-enters the
		// quasiquote dialect: the `,` beneath it is live, its quote is no barrier.
		{"quasiquote inside an unsyntax",
			"(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,`(b (quote (,"+quasisyntaxStaleBody+"))))))) (list n (f)))",
			"(1 99)"},
		// The dotted tail: `(a . #,e) reads as (a unsyntax e) in the spine.
		{"dotted unsyntax under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (b . #,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
	})
}

// A marker of the OTHER dialect inside a template is data, in both directions,
// and a depth-2 unsyntax stays data. These rows must not move: they are what the
// marker set is for, and what nestingAlwaysRuntime does NOT change (it picks the
// expansion path, not the evaluated positions).
func TestQuasisyntaxMarkersAreDialectLocal(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		// Depth 2: the set! never runs, so n stays 0 and f keeps its body.
		{"nested quasisyntax keeps an inner unsyntax as data",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (quasisyntax (a #,` + quasisyntaxStaleBody + `))) (list n (f)))`,
			"(0 7)"},
		{"nested quasisyntax under a quote keeps an inner unsyntax as data",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (quasisyntax (a (quote (#,` + quasisyntaxStaleBody + `))))) (list n (f)))`,
			"(0 7)"},
		// A comma in a quasisyntax template is not an escape; an unsyntax in a
		// quasiquote template is not one either.
		{"unquote inside a quasisyntax template is data",
			"(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (unquote "+quasisyntaxStaleBody+"))) (list n (f)))",
			"(0 7)"},
		{"unsyntax inside a quasiquote template is data",
			"(let ((f (lambda () 7)) (n 0)) `(a (unsyntax "+quasisyntaxStaleBody+")) (list n (f)))",
			"(0 7)"},
		// The flag's discriminating behaviour, measured on master: the nested
		// form takes the runtime path and (syntax z) yields z's VALUE.
		{"nestingAlwaysRuntime materializes a bound local",
			`(let ((z 5)) (quasisyntax (quasisyntax (a (unsyntax z)))))`,
			"#'(#'quasisyntax #'(#'a #'(#'unsyntax #'5)))"},
	})
}

// opaqueSubtreeMentions (or_lower.go) shares the scan, so the same blindness hid
// the only reference to a let's binder: the lowering dropped a frame that was
// live. Measured on master, rows 1 and 2 answered an unbound-"t" compile error
// and the OUTER t (9); the unquote form of the same shapes already answered #f.
func TestQuasisyntaxUnsyntaxUnderQuoteKeepsLetBinder(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		{"if-shaped let, unsyntax under a quote form",
			`(let ((t #f)) (if t t (quasisyntax (a (quote (#,t))))))`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
		{"if-shaped let over a global of the same name",
			`(define t 9) (let ((t #f)) (if t t (quasisyntax (a (quote (#,t))))))`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
		{"control: bare unsyntax",
			`(define t 9) (let ((t #f)) (if t t (quasisyntax (a #,t))))`,
			"#'(#'a #'#f)"},
	})
}
