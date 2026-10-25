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
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// runQuasisyntaxRows evaluates each row under the default inline threshold and
// under WithInlineThreshold(0). The stale-body miscompile needs the inliner on,
// the frame-dropping ones need it off or on depending on the shape, so a row that
// only one configuration can see would pass unnoticed in the other.
func runQuasisyntaxRows(t *testing.T, rows []innerPositionRow) {
	t.Helper()
	configs := []struct {
		name string
		opts []wile.EngineOption
	}{
		{"default", nil},
		{"inline-0", []wile.EngineOption{wile.WithInlineThreshold(0)}},
	}
	for _, cfg := range configs {
		t.Run(cfg.name, func(t *testing.T) {
			for _, row := range rows {
				t.Run(row.name, func(t *testing.T) {
					opts := []wile.EngineOption{
						wile.WithProfile(wile.KitchenSink),
						wile.WithSourceFS(stdlib.FS),
						wile.WithLibraryPaths("."),
					}
					opts = append(opts, cfg.opts...)
					eng, err := wile.NewEngine(context.Background(), opts...)
					qt.Assert(t, err, qt.IsNil)
					t.Cleanup(func() {
						_ = eng.Close()
					})
					v, err := eng.EvalMultiple(context.Background(), row.src)
					qt.Assert(t, err, qt.IsNil)
					qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
				})
			}
		})
	}
}

// quasisyntax is a passthrough form, so the opaque-subtree scan enters its body
// at code depth. Before the scan learned quasisyntax's markers, a quote form
// inside the template hit the depth-0 quote barrier and the `#,` beneath it was
// never marked: the set!s ran, but let-bound f was inlined with its stale body.
// Each row evaluates to (n f-result); n proves the unsyntax fired, f-result
// proves the inliner saw it. The set! is hidden in a begin so the ONLY evidence
// the analysis has is the position walk.
const quasisyntaxStaleBody = `(begin (set! n 1) (set! f (lambda () 99)))`

func TestQuasisyntaxUnsyntaxUnderQuote(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
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
			"(let ((f (lambda () 7)) (n 0)) `(a (quote (," + quasisyntaxStaleBody + "))) (list n (f)))",
			"(1 99)"},
		// Quote nested deeper, in a vector element, and through unsyntax-splicing.
		{"unsyntax under two quote forms",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (b (quote (#,` + quasisyntaxStaleBody + `)))))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax in a vector under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote #(#,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax in a vector directly in the template",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a #(#,` + quasisyntaxStaleBody + `))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax-splicing under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,@(begin (set! n 1) (set! f (lambda () 99)) '()))))) (list n (f)))`,
			"(1 99)"},
		// A quasiquote reached from the unsyntax's code position re-enters the
		// quasiquote dialect: the `,` beneath it is live, its quote is no barrier.
		{"quasiquote inside an unsyntax",
			"(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,`(b (quote (," + quasisyntaxStaleBody + "))))))) (list n (f)))",
			"(1 99)"},
		// The dotted tail: `(a . #,e) reads as (a unsyntax e) in the spine.
		{"dotted unsyntax under a quote form",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (b . #,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		// Depth 2 stepped back out by two unsyntax forms reaches code, where the
		// quote is a barrier for what it contains but not for the unsyntax around it.
		{"depth-2 unsyntax then quote then unsyntax reaches code",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (quasisyntax (a #,(quote (#,` + quasisyntaxStaleBody + `))))) (list n (f)))`,
			"(1 99)"},
		// The boxing scan (ForEachOpaqueLiveSymbol, ref_index.go) is a separate
		// consumer of the same walk: the assigned binder is captured, so a scan
		// that misses the set! leaves g reading a stale copy of n.
		{"boxing: captured binder assigned under a quote form",
			`(define v (vector 0))
			 (define g
			   (let ((n 0))
			     (let ((f (lambda () n)))
			       (quasisyntax (a (quote (#,(begin (vector-set! v 0 1) (set! n 99))))))
			       f)))
			 (list (g) (vector-ref v 0))`,
			"(99 1)"},
		{"boxing control: captured binder assigned by a bare unsyntax",
			`(define v (vector 0))
			 (define g
			   (let ((n 0))
			     (let ((f (lambda () n)))
			       (quasisyntax (a #,(begin (vector-set! v 0 1) (set! n 99))))
			       f)))
			 (list (g) (vector-ref v 0))`,
			"(99 1)"},
		{"top-level define assigned under a quote form in the same unit",
			`(begin (define g 0) (let ((f (lambda () 7))) (quasisyntax (a (quote (#,(begin (set! g 1) (set! f (lambda () 99))))))) (list g (f))))`,
			"(1 99)"},
	})
}

// A marker of the OTHER dialect inside a template is data, in both directions.
// Every row puts a quote form between the marker and a stale-body set!, so a
// scan that read the foreign marker as an escape or a nesting head, or kept the
// quote barrier, answers a wrong VALUE.
func TestQuasisyntaxMarkersAreDialectLocal(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
		{"unquote is data in a quasisyntax template",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (unquote (quote (#,` + quasisyntaxStaleBody + `))))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax is data in a quasiquote template",
			"(let ((f (lambda () 7)) (n 0)) `(a (unsyntax (quote (," + quasisyntaxStaleBody + ")))) (list n (f)))",
			"(1 99)"},
		{"unquote-splicing is data in a quasisyntax template",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (unquote-splicing (quote (#,` + quasisyntaxStaleBody + `))))) (list n (f)))`,
			"(1 99)"},
		{"unsyntax-splicing is data in a quasiquote template",
			"(let ((f (lambda () 7)) (n 0)) `(a (unsyntax-splicing (quote (," + quasisyntaxStaleBody + ")))) (list n (f)))",
			"(1 99)"},
		{"quasiquote head is data in a quasisyntax template",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quasiquote (#,` + quasisyntaxStaleBody + `)))) (list n (f)))`,
			"(1 99)"},
		{"quasisyntax head is data in a quasiquote template",
			"(let ((f (lambda () 7)) (n 0)) `(a (quasisyntax (," + quasisyntaxStaleBody + "))) (list n (f)))",
			"(1 99)"},
	})
}

// Compiler-behaviour pins, not scan pins: the set! never runs at depth 2, and
// nestingAlwaysRuntime takes the nested form to the runtime path. They hold
// whatever the scan does, because a data position runs nothing; they fix the
// values the scan has to agree with.
func TestQuasisyntaxNestedDepthBehaviour(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
		{"nested quasisyntax keeps an inner unsyntax as data",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (quasisyntax (a #,` + quasisyntaxStaleBody + `))) (list n (f)))`,
			"(0 7)"},
		{"nested quasisyntax under a quote keeps an inner unsyntax as data",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (quasisyntax (a (quote (#,` + quasisyntaxStaleBody + `))))) (list n (f)))`,
			"(0 7)"},
		{"unquote inside a quasisyntax template is data",
			"(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (unquote " + quasisyntaxStaleBody + "))) (list n (f)))",
			"(0 7)"},
		{"unsyntax inside a quasiquote template is data",
			"(let ((f (lambda () 7)) (n 0)) `(a (unsyntax " + quasisyntaxStaleBody + ")) (list n (f)))",
			"(0 7)"},
		{"a quote form at code depth under an unsyntax is a barrier",
			`(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (b #,(quote (#,` + quasisyntaxStaleBody + `)))))) (list n (f)))`,
			"(0 7)"},
		{"nestingAlwaysRuntime materializes a bound local",
			`(let ((z 5)) (quasisyntax (quasisyntax (a (unsyntax z)))))`,
			"#'(#'quasisyntax #'(#'a #'(#'unsyntax #'5)))"},
	})
}

// A symbol in DATA position of a quasisyntax template is a READ: expandQuasi emits
// (syntax sym) for it, and (syntax sym) on a bound local yields the local's value.
// The or-shaped lowering drops a let's frame when nothing mentions the binder, so
// the mention scan has to count template data, not only the evaluated positions.
// The stamp scan must not (TestImmutableTopLevel_OpaqueSubtreeOverMark).
func TestQuasisyntaxTemplateDataIsRead(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
		{"data symbol and a bare unsyntax",
			`(let ((t #f)) (if t t (quasisyntax (a t #,1))))`,
			"#'(#'a #'#f #'1)"},
		{"data symbol over an outer binder of the same name",
			`(let ((t 'outer)) (let ((t #f)) (if t t (quasisyntax (b t #,0)))))`,
			"#'(#'b #'#f #'0)"},
		{"nested unsyntax at depth one",
			`(let ((z #f)) (if z z (quasisyntax (quasisyntax (a (unsyntax z))))))`,
			"#'(#'quasisyntax #'(#'a #'(#'unsyntax #'#f)))"},
		{"binder is a procedure parameter",
			`(define (h x) (let ((t x)) (if t t (quasisyntax (b t #,0))))) (h #f)`,
			"#'(#'b #'#f #'0)"},
		{"self-tail loop",
			`(define (lp k) (let ((t #f)) (if t t (if (= k 0) (quasisyntax (a t #,0)) (lp (- k 1)))))) (lp 3)`,
			"#'(#'a #'#f #'0)"},
		{"data symbol inside a quote form",
			`(let ((t #f)) (if t t (quasisyntax (a (quote (t)) #,1))))`,
			"#'(#'a #'(#'quote #'(#'#f)) #'1)"},
		{"data symbol inside a vector",
			`(let ((t #f)) (if t t (quasisyntax (a #(a t #,1)))))`,
			"#'(#'a #'(#'a #'#f #'1))"},
		{"or: the macro temporary, not the user binder",
			`(let ((t #f)) (or t (quasisyntax (a t #,1))))`,
			"#'(#'a #'#f #'1)"},
	})
}

// A head that resolves to a binding denoting no form is an ordinary head at code
// depth: the compiler dispatches on what a head denotes, so a local named after
// a marker is a call and its operands are evaluated. Inside templates the
// spelling fallback stays, as quasiKeywords.headName has it.
func TestMarkerNamedLocalIsAnOrdinaryCall(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
		{"local quasisyntax in an unquote position",
			"(let ((quasisyntax list) (f (lambda () 7))) `(,(quasisyntax (begin (set! f (lambda () 99)) 1))) (f))",
			"99"},
		{"local quasisyntax, if-shaped let",
			"(let ((quasisyntax list)) (let ((t #f)) (if t t `(,(quasisyntax t)))))",
			"((#f))"},
		{"local quote in an unsyntax position",
			`(let ((quote list) (f (lambda () 7))) (quasisyntax (a #,(quote (begin (set! f (lambda () 99)) 1)))) (f))`,
			"99"},
		{"local quasiquote in an unsyntax position",
			`(let ((quasiquote list) (f (lambda () 7))) (quasisyntax (a #,(quasiquote (begin (set! f (lambda () 99)) 1)))) (f))`,
			"99"},
	})
}

// A binder the raw subtree introduces itself shadows a marker-named head inside
// its own body. The scan resolved heads in the node's frame, which holds none of
// those binders, so the inner quote read as the barrier and the set! under it went
// unmarked: f was inlined stale (7), or, where the closure captured it, never
// boxed, and the set! faulted at runtime. Each row answers (n f-result).
func TestMarkerNamedBinderInsideOpaqueSubtree(t *testing.T) {
	t.Parallel()

	const b = `(begin (set! n 1) (set! f (lambda () 99)) 1)`
	row := func(name, form string) innerPositionRow {
		return innerPositionRow{name, `(let ((f (lambda () 7)) (n 0)) ` + form + ` (list n (f)))`, "(1 99)"}
	}
	runQuasisyntaxRows(t, []innerPositionRow{
		// The measured defect: store-free: free slot 0 holds *machine.MachineClosure, not a box.
		row("lambda parameter under an unsyntax", `(quasisyntax (a #,((lambda (quote) (quote `+b+`)) list)))`),
		// (0 7)-shaped on master: answered 7 by default and 99 under inline-0.
		row("let name under an unsyntax", `(quasisyntax (a #,(let ((quote list)) (quote `+b+`))))`),
		row("quasiquote: lambda parameter under an unquote", "`(a ,((lambda (quote) (quote "+b+")) list))"),
		row("quasiquote: rest parameter", "(guard (e (#t #f)) `(a ,((lambda quote (quote "+b+")))))"),
		row("quasiquote: case-lambda clause parameter", "`(a ,((case-lambda ((quote) (quote "+b+"))) list))"),
		row("quasiquote: let* init sees the earlier name", "`(a ,(let* ((quote list) (y (quote "+b+"))) y))"),
		row("quasiquote: letrec init sees every name", "`(a ,(letrec ((quote list) (y (lambda () (quote "+b+")))) (y)))"),
		row("quasiquote: letrec* init sees every name", "`(a ,(letrec* ((quote list) (y (quote "+b+"))) y))"),
		row("quasiquote: named let name", "`(a ,(let lp ((quote list)) (quote "+b+")))"),
		row("quasiquote: named let tag", "`(a ,(let quote ((k 1)) (if (= k 1) (quote (begin (set! n 1) (set! f (lambda () 99)) 2)) k)))"),
		row("internal define under an unsyntax", `(quasisyntax (a #,((lambda () (define quote list) (quote `+b+`)))))`),
		row("quasiquote: internal define", "`(a ,((lambda () (define quote list) (quote "+b+"))))"),
		row("quasiquote: internal define spliced through a begin", "`(a ,(let () (begin (define quote list)) (quote "+b+")))"),
		// Derived forms reach the walk expanded (compilation's expandQuasiOperands),
		// so their binders are the core forms' binders.
		row("let-values under an unsyntax", `(quasisyntax (a #,(let-values (((quote) (values list))) (quote `+b+`))))`),
		row("quasiquote: do loop variable", "`(a ,(do ((quote list) (i 0 (+ i 1))) ((= i 1) 5) (quote "+b+")))"),
		{"user macro binder under an unsyntax",
			`(define-syntax bind1 (syntax-rules () ((_ v e body) ((lambda (v) body) e))))
			 (let ((f (lambda () 7)) (n 0)) (quasisyntax (a #,(bind1 quote list (quote ` + b + `)))) (list n (f)))`,
			"(1 99)"},
	})
}

// `(a . `x) reads as (a quasiquote x), and the compiler takes x one level deeper
// (dottedTailCell). The scan read it element-wise, so the unquote under it reached
// depth 0 and its quote form became a barrier over the inner unquote the compiler
// fires. The quasisyntax row follows Wile's compiler and Chez; Racket does not
// read a dotted quasisyntax tail as nesting and answers (0 7).
func TestDottedQuasiquoteTailIsOneLevelDeeper(t *testing.T) {
	t.Parallel()

	const b = `(begin (set! n 1) (set! f (lambda () 99)) 1)`
	row := func(name, form string) innerPositionRow {
		return innerPositionRow{name, `(let ((f (lambda () 7)) (n 0)) ` + form + ` (list n (f)))`, "(1 99)"}
	}
	runQuasisyntaxRows(t, []innerPositionRow{
		row("list under a quote", "`(a . `(b ,'(,"+b+")))"),
		row("vector under a quote", "`(a . `(b ,'#(,"+b+")))"),
		// Read element-wise, the vector sat at depth 0, a self-evaluating literal.
		row("vector under the unquote", "`(a . `(b ,#(,"+b+")))"),
		row("quasisyntax dialect", `(quasisyntax (a . (quasisyntax (b #,(quote (#,`+b+`))))))`),
	})
}

// opaqueSubtreeMentions (or_lower.go) shares the scan, so the same blindness hid
// the only reference to a let's binder: the lowering dropped a frame that was
// live. Measured on master, rows 1 and 2 answered an unbound-"t" compile error
// and the OUTER t (9); the unquote form of the same shapes already answered #f.
func TestQuasisyntaxUnsyntaxUnderQuoteKeepsLetBinder(t *testing.T) {
	t.Parallel()

	runQuasisyntaxRows(t, []innerPositionRow{
		{"if-shaped let, unsyntax under a quote form",
			`(let ((t #f)) (if t t (quasisyntax (a (quote (#,t))))))`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
		{"if-shaped let over a global of the same name",
			`(define t 9) (let ((t #f)) (if t t (quasisyntax (a (quote (#,t))))))`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
		{"control: bare unsyntax",
			`(define t 9) (let ((t #f)) (if t t (quasisyntax (a #,t))))`,
			"#'(#'a #'#f)"},
		{"self-tail loop, unsyntax under a quote form",
			`(define (loop k) (let ((t #f)) (if t t (if (= k 0) (quasisyntax (a (quote (#,t)))) (loop (- k 1)))))) (loop 3)`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
	})
}

// A head spelled quasiquote or quasisyntax that resolves to a binding denoting no
// form is not necessarily a call to the compiler: a Predeclared slot (a LATER
// define in the same body) and a top-level variable reached from inside a let
// body still compile as the form (validate.headDenotesSpecialForm). The scan reads
// such a head both ways and reports the union, so each row below puts the unquote
// beneath a quote barrier or a binder the call reading alone would hide.
func TestDemotedOpenerIsReadBothWays(t *testing.T) {
	t.Parallel()

	b := `(set! f (lambda () 99))`
	runQuasisyntaxRows(t, []innerPositionRow{
		{"top-level quasiquote variable, template opener under an unsyntax",
			`(define quasiquote list) (let ((f (lambda () 7))) (quasisyntax (a #,(quasiquote (b (quote (unquote ` + b + `)))))) (f))`,
			"99"},
		{"predeclared quasiquote, binder as template data",
			`(begin (define t 9) (define r (let ((t #f)) (if t t (quasisyntax (a #,(quasiquote (b (quote (unquote t))))))))) (define quasiquote list) r)`,
			"#'(#'a #'(#'b #'(#'quote #'#f)))"},
		{"predeclared quasiquote, boxing",
			`(begin (define g (let ((n 0)) (let ((f (lambda () n))) (quasisyntax (a #,(quasiquote (b (quote (unquote (set! n 99))))))) f))) (define quasiquote list) (g))`,
			"99"},
		{"top-level quasisyntax variable, unsyntax under a quote form",
			`(define quasisyntax list) (let ((f (lambda () 7))) (quasisyntax (a (quote (#,` + b + `)))) (f))`,
			"99"},
		{"predeclared quasisyntax, unsyntax under a quote form",
			`(begin (define r (let ((f (lambda () 7))) (quasisyntax (a (quote (#,` + b + `)))) (f))) (define quasisyntax list) r)`,
			"99"},
		{"predeclared quasisyntax, unsyntax in a vector",
			`(begin (define r (let ((f (lambda () 7))) (quasisyntax (a #(#,` + b + `))) (f))) (define quasisyntax list) r)`,
			"99"},
		{"predeclared quasisyntax, binder under a quote form",
			`(begin (define r (let ((t #f)) (if t t (quasisyntax (a (quote (#,t))))))) (define quasisyntax list) r)`,
			"#'(#'a #'(#'quote #'(#'#f)))"},
	})
}

// The stamp walk reads the same head both ways, so a top-level variable named
// quasisyntax in an EARLIER unit still lets a later unit's hidden set! be marked.
func TestDemotedOpenerKeepsTopLevelSetLegal(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	for _, inline := range []bool{false, true} {
		opts := []wile.EngineOption{wile.WithProfile(wile.Small), wile.WithImmutableTopLevel()}
		if inline {
			opts = append(opts, wile.WithInlineThreshold(0))
		}
		eng, err := wile.NewEngine(ctx, opts...)
		qt.Assert(t, err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, `(define quasisyntax list)`)
		qt.Assert(t, err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, `(begin (define x 1) (let () (quasisyntax (a (quote (#,(set! x 9)))))))`)
		qt.Assert(t, err, qt.IsNil)
		v, err := eng.EvalMultiple(ctx, `(begin (set! x 2) x)`)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("the hidden set! is real: x must not keep a Stable stamp"))
		qt.Assert(t, v.SchemeString(), qt.Equals, "2")
		qt.Assert(t, eng.Close(), qt.IsNil)
	}
}
