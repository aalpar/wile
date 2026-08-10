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
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// Scheme-level gates for review wave 5 cluster D — the pattern matcher's three
// independent matching gaps (2.3.3, 2.3.4, §3 discovery 1) plus the two
// template/pattern hygiene defects (2.2.13, 2.2.14).
//
// Every case below was observed to produce the value named in its comment at
// 003b3353 / fix/w1-syntax-case-relocation. The unit-level pins in
// pkg/internal/match cover the bytecode shapes; these cover the behaviour a
// Scheme program can see, which is what the review's §8 found missing.

// evalScheme evaluates one form and returns its printed representation.
func evalScheme(t *testing.T, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	v, err := engine.Eval(ctx, engine.MustParse(ctx, src))
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// mustEvalScheme evaluates one form and fails the test if it errors.
func mustEvalScheme(t *testing.T, src string) string {
	t.Helper()
	got, err := evalScheme(t, src)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	return got
}

// 2.3.3 — an ellipsis over a variable-free subpattern must repeat, not degrade
// to a literal compare against the ellipsis symbol. R7RS §4.3.2's "F matches P"
// clause list is purely structural; no clause mentions variable occurrence.
//
// All five forms raised "syntax-rules: no matching clause" at the base.
func TestEllipsisOverVariableFreeSubpattern(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "three repetitions of a literal",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ 1 ...) 'matched-ones)))
                    (m 1 1 1))`,
			want: "matched-ones",
		},
		{
			name: "zero repetitions of a literal",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ 1 ...) 'matched-ones)))
                    (m))`,
			want: "matched-ones",
		},
		{
			name: "one repetition inside a sublist",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ (1 ...)) 'matched-sub)))
                    (m (1)))`,
			want: "matched-sub",
		},
		{
			name: "zero repetitions inside a sublist",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ (1 ...)) 'matched-sub)))
                    (m ()))`,
			want: "matched-sub",
		},
		{
			name: "one repetition inside a vector",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ #(1 ...)) 'matched-vec)))
                    (m #(1)))`,
			want: "matched-vec",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// The repetition is real, not a blanket accept: a literal subpattern still has
// to match the input it repeats over. At the base the first clause matched
// nothing, so this returned (other other other).
func TestEllipsisOverVariableFreeSubpatternStillDiscriminates(t *testing.T) {
	src := `(begin
              (define-syntax m
                (syntax-rules ()
                  ((_ 1 ...) 'ones)
                  ((_ x ...) 'other)))
              (list (m 1 1) (m 2 2) (m 1 2)))`
	got := mustEvalScheme(t, src)
	want := "(ones other other)"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}

// 2.3.4 — `(a ... . r)` against an improper input list. Every case here gave
// "no matching clause" at the base, in BOTH syntax-rules and syntax-case.
//
// The values matter, not just the match: the ellipsis loop has to halt with the
// position on the LAST pair, so `(1 2 . 3)` yields a=(1 2) r=3 (Racket's
// ((1 2) 3)). An exit test at the loop head would yield a=(1) r=3 and satisfy a
// bare "it matches" assertion.
func TestEllipsisFollowedByDottedTail(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "syntax-rules, two elements then a dotted tail",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ a ... . r) (list (list a ...) r))))
                    (m 1 2 . 3))`,
			want: "((1 2) 3)",
		},
		{
			name: "syntax-rules, one element then a dotted tail",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ a ... . r) (list (list a ...) r))))
                    (m 1 . 2))`,
			want: "((1) 2)",
		},
		{
			// Zero repetitions: this one dies at the PRE-LOOP advance past the
			// macro keyword, before SkipIfEmpty is ever reached.
			name: "syntax-rules, zero elements then a dotted tail",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ a ... . r) (list (list a ...) r))))
                    (m . 3))`,
			want: "(() 3)",
		},
		{
			name: "syntax-case, two elements then a dotted tail",
			src: `(begin
                    (define-syntax m
                      (lambda (stx)
                        (syntax-case stx ()
                          ((_ a ... . r) #'(list (list a ...) r)))))
                    (m 1 2 . 3))`,
			want: "((1 2) 3)",
		},
		{
			name: "fixed element before the ellipsis and the dotted tail",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ z a ... . r) (list z (list a ...) r))))
                    (m 0 1 2 . 3))`,
			want: "(0 (1 2) 3)",
		},
		{
			// CONTROL: a proper-list input still binds r to (). This passed at
			// the base and must keep passing.
			name: "control: proper input still binds the tail to the empty list",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ a ... . r) (list (list a ...) r))))
                    (m 1 2))`,
			want: "((1 2) ())",
		},
		{
			// CONTROL: a fixed element before the tail, no ellipsis. Passed at
			// the base.
			name: "control: fixed element before a dotted tail",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ z . r) (list z 'r))))
                    (m 1 2 . 3))`,
			want: "(1 (2 . 3))",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// §3 discovery 1 — a trailing `_` after a sublist element. R7RS §4.3.2
// explicitly permits multiple underscores, and Racket accepts every form here.
//
// The gap has BOTH polarities and one root cause: `_` emitted no bytecode at
// all, so ByteCodeDone's one-instruction lookahead could not distinguish "the
// pattern ends here" from "the next pattern element is a wildcard".
func TestTrailingWildcardAfterSublist(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			// "no matching clause" at the base.
			name: "list sublist then a trailing wildcard",
			src: `(begin
                    (define-syntax k (syntax-rules () ((k (y) _) 'ok)))
                    (k (1) 9))`,
			want: "ok",
		},
		{
			name: "ellipsis sublist then a trailing wildcard",
			src: `(begin
                    (define-syntax k (syntax-rules () ((k (y ...) _) 'ok)))
                    (k (1) 9))`,
			want: "ok",
		},
		{
			name: "wildcard ellipsis sublist then a trailing wildcard",
			src: `(begin
                    (define-syntax k (syntax-rules () ((k (_ ...) _) 'ok)))
                    (k (1) 9))`,
			want: "ok",
		},
		{
			// It is sublist DESCENT generally, not lists: the vector form fails
			// identically at the base.
			name: "vector sublist then a trailing wildcard",
			src: `(begin
                    (define-syntax k (syntax-rules () ((k #(y) _) 'ok)))
                    (k #(1) 9))`,
			want: "ok",
		},
		{
			// The FALSE ACCEPT polarity: two underscores need three input
			// elements. At the base this returned ok on a two-element input.
			name: "two trailing wildcards reject a two-element input",
			src: `(begin
                    (define-syntax k
                      (syntax-rules ()
                        ((k (y) _ _) 'three)
                        ((k (y) _) 'two)))
                    (list (k (1) 9) (k (1) 9 8)))`,
			want: "(two three)",
		},
		{
			// The pattern variable still binds the right element — a wildcard
			// that consumed nothing would shift everything left.
			name: "wildcard before a pattern variable inside a sublist",
			src: `(begin
                    (define-syntax k (syntax-rules () ((k (_ y)) y)))
                    (k (1 2)))`,
			want: "2",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// 2.2.13 — a `#&` box in a template was emitted verbatim, and a box in a
// pattern never matched. pkg/syntax/syntax_box.go states the contract the
// expander broke: "a box in a macro template propagates scopes to what it
// holds". The vector analogue is the control — it already worked, which is what
// made the expander's self-evaluating default arm the gap.
//
// No test asserts "the box's identifier resolves to some unrelated binding":
// there is no compiler case for *SyntaxBox at all, so a box is self-evaluating
// data and the failure is always a wrong DATUM, never a capture.
func TestBoxInMacroTemplateAndPattern(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			// #&x at the base.
			name: "template box substitutes its content",
			src: `(begin
                    (define-syntax b (syntax-rules () ((_ x) #&x)))
                    (b 5))`,
			want: "#&5",
		},
		{
			// #&(x ...) at the base — the whole boxed subtree was verbatim,
			// ellipses included.
			name: "template box repeats an ellipsis inside it",
			src: `(begin
                    (define-syntax b (syntax-rules () ((_ x ...) #&(x ...))))
                    (b 5 6))`,
			want: "#&(5 6)",
		},
		{
			// CONTROL: the vector analogue already worked.
			name: "control: template vector substitutes",
			src: `(begin
                    (define-syntax v (syntax-rules () ((_ x) #(x x))))
                    (v 5))`,
			want: "#(5 5)",
		},
		{
			// "no matching clause" at the base.
			name: "pattern box binds its content",
			src: `(begin
                    (define-syntax p (syntax-rules () ((_ #&y) y)))
                    (p #&7))`,
			want: "7",
		},
		{
			name: "pattern box discriminates against a non-box",
			src: `(begin
                    (define-syntax p
                      (syntax-rules ()
                        ((_ #&y) (list 'boxed y))
                        ((_ y) (list 'bare y))))
                    (list (p #&7) (p 7)))`,
			want: "((boxed 7) (bare 7))",
		},
		{
			name: "pattern box holding a sublist",
			src: `(begin
                    (define-syntax p (syntax-rules () ((_ #&(a b)) (list b a))))
                    (p #&(1 2)))`,
			want: "(2 1)",
		},
		{
			name: "round trip: box pattern into box template",
			src: `(begin
                    (define-syntax p (syntax-rules () ((_ #&y) #&y)))
                    (p #&7))`,
			want: "#&7",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// 2.2.14 — a syntax-rules macro defined INSIDE a syntax-case clause body, whose
// template freely references the enclosing clause's pattern variable, resolved
// that reference at the USE site.
//
// The reference reaches applyHygieneToSymbol's empty-LocalScopes local-binding
// arm because a syntax-case pattern variable is bound scope-less on the
// pattern-variable frame. That arm returned srcCtx — which UseSiteCtx has
// already replaced with the invoking code's context — so `a` in the inner
// template picked up the lambda's `a` (99) instead of the pattern variable
// (42). Racket gives 42, and the sibling local-binding arm gives 42 for the
// identical shape.
//
// This test HAS to be Scheme-level: the arm's only other driver is a mock
// (pkg/internal/match/literal_match_test.go TestApplyHygieneToSymbol/"free
// identifier with hasLocalBinding true"), and with its nil UseSiteCtx
// templateCtx == srcCtx, so it passes either way. It is a control, not a gate.
func TestFreeTemplateIdInsideSyntaxCaseClauseResolvesAtDefinitionSite(t *testing.T) {
	src := `(begin
              (define-syntax outer
                (lambda (stx)
                  (syntax-case stx ()
                    ((_ a)
                     (let-syntax ((inner (syntax-rules () ((_) a))))
                       ((lambda (a) (inner)) #'99))))))
              (outer 42))`
	got := mustEvalScheme(t, src)
	want := "42"
	if got != want {
		t.Errorf("got %s, want %s (99 means the free template id resolved at the use site)", got, want)
	}
}

// 2.2.12 — a syntax-case pattern literal ignored use-site bindings.
// SyntaxCaseClause carried no LiteralSyntax at all and the match passed no
// binding checker, so match.go's hygiene block was gated off with BOTH
// conjuncts nil on this path.
//
// The correct behaviour is a 2x2 over {syntax-rules, syntax-case} x {let-bound
// shadow, global shadow}. The plumbing in this change closed the syntax-case x
// let-bound cell.
//
// The two global-shadow cells were once labelled as recording a WRONG answer that
// wave 1 §6 would flip. That label was mistaken and the (ELSE 1) wants must NOT be
// flipped. These rows are one compilation unit: the macro and the `define else`
// live in the same (begin ...), so the definition-site pin and the use-site
// resolution are the SAME binding and R7RS §4.3.2's first arm matches. Racket 9.2
// answers (ELSE 1) for the same program. The definition-site pin preserves it;
// wave 1 §6's own gate is TestPatternLiteralIdentifiedByDefinitionSiteBinding
// below, where the two forms are separate top-level units.
func TestPatternLiteralRespectsAUseSiteShadow(t *testing.T) {
	const rulesMacro = `(define-syntax m
                          (syntax-rules (else)
                            ((_ else x) (list 'ELSE x))
                            ((_ y x) (list 'OTHER y x))))`
	const caseMacro = `(define-syntax m
                         (lambda (stx)
                           (syntax-case stx (else)
                             ((_ else x) #'(list 'ELSE x))
                             ((_ y x) #'(list 'OTHER y x)))))`

	cases := []struct {
		name string
		src  string
		want string
		// closedBy names the change that makes want correct.
		closedBy string
	}{
		{
			// CONTROL: already correct at the base.
			name:     "syntax-rules, let-bound shadow",
			src:      "(begin " + rulesMacro + " (let ((else 5)) (m else 1)))",
			want:     "(OTHER 5 1)",
			closedBy: "already-correct",
		},
		{
			// The defect. (ELSE 1) at the base.
			name:     "syntax-case, let-bound shadow",
			src:      "(begin " + caseMacro + " (let ((else 5)) (m else 1)))",
			want:     "(OTHER 5 1)",
			closedBy: "wave5-9B",
		},
		{
			name:     "syntax-rules, global shadow",
			src:      "(begin " + rulesMacro + " (define else 5) (m else 1))",
			want:     "(ELSE 1)",
			closedBy: "already-correct: same unit, one binding",
		},
		{
			name:     "syntax-case, global shadow",
			src:      "(begin " + caseMacro + " (define else 5) (m else 1))",
			want:     "(ELSE 1)",
			closedBy: "already-correct: same unit, one binding",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s (cell closed by %s)", got, tc.want, tc.closedBy)
			}
		})
	}
}

// evalMultiForm evaluates src as SEPARATE top-level forms on a fresh engine and
// returns the last value's printed representation.
//
// It exists because mustEvalScheme above cannot express the wave 1 §6 defect:
// that helper evaluates ONE (begin ...) form, and a define in a begin body binds
// its name in that body, not at top level. `(begin (define else #f) (cond (else
// 'TOOK-ELSE)))` therefore returns #<void> at HEAD, but for an unrelated reason —
// the body-local `else` is a lexical shadow the literal check already refuses, so
// the clause degrades to an ordinary (test . body) clause whose test is #f. Only a
// genuine top-level define exercises the definition-site question.
func evalMultiForm(t *testing.T, src string) string {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = engine.Close()
	})
	v, err := engine.EvalMultiple(ctx, src)
	if err != nil {
		t.Fatalf("eval %q: %v", src, err)
	}
	return v.Internal().SchemeString()
}

// Wave 1 §6 — a syntax-rules pattern literal is identified by its DEFINITION-site
// binding, not by whatever the use site happens to have bound the name to.
//
// R7RS §4.3.2: an input subform matches a literal identifier only if both
// occurrences have the same lexical binding, or the two are the same identifier
// and both are unbound. Before this fix both sides were resolved through the one
// USE-site environment, which makes the comparison vacuous for a zero-scoped
// global: `(define else #f)` rebinds `else` at top level, both sides resolve to
// that same new binding, and the literal matched — so `cond` took its else branch
// on a name the program had taken over. Racket 9.2 returns #<void> for row 1 and a
// procedure for row 2.
func TestPatternLiteralIdentifiedByDefinitionSiteBinding(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			// TOOK-ELSE at the base.
			name: "top-level define of else does not match cond's else literal",
			src:  "(define else #f)\n(cond (else 'TOOK-ELSE))",
			want: "#<void>",
		},
		{
			// #f at the base: `=>` still matched the literal, so the clause was
			// read as (test => receiver) and applied the lambda to 7. With `=>`
			// taken over by the program the clause is an ordinary (test result
			// ...) clause, whose value is the last result — the lambda itself.
			// procedure? wraps it because a closure's printed form is unstable.
			name: "top-level define of => makes the cond clause an ordinary one",
			src:  "(define => #f)\n(procedure? (cond (7 => (lambda (v) 'X)) (#t 'FELL)))",
			want: "#t",
		},
		{
			// CONTROL: a lexical shadow was already refused, and must stay refused.
			name: "let-bound else is still not the literal",
			src:  "(let ((else #f)) (cond (else 'TOOK-ELSE)))",
			want: "#<void>",
		},
		{
			// CONTROL: a user macro whose literal is unbound at ITS definition site
			// keeps matching the same-named use-site identifier, per R7RS's
			// "same identifier, both unbound" arm. A pin that fired on an unbound
			// literal would answer (OTHER lit 1) here.
			name: "literal unbound at definition time still matches by name",
			src: "(define-syntax m (syntax-rules (lit) ((_ lit x) (list 'LIT x)) ((_ y x) (list 'OTHER y x))))\n" +
				"(define lit 5)\n" +
				"(m lit 1)",
			want: "(LIT 1)",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := evalMultiForm(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// w16LibEngine builds an engine whose library search path holds exactly one
// library file, mirroring newHostileEngine (import_postseal_test.go).
func w16LibEngine(t *testing.T, librarySrc string) *wile.Engine {
	t.Helper()
	libFS := fstest.MapFS{
		"w16lib.scm": &fstest.MapFile{Data: []byte(librarySrc)},
	}
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(libFS),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// The cross-library half of wave 1 §6, and the reason the definition-site pin
// cannot be replaced by "is the use-site name bound?". A library defines `lit`
// and a macro with `lit` as a pattern literal, and exports only the macro. The
// importing program cannot see the library's `lit` at all, so its own `lit` — or
// its complete absence of one — is a different binding and must not match. Every
// row answered MATCHED-LITERAL at the base.
func TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding(t *testing.T) {
	const libRules = `(define-library (w16lib)
  (export mg)
  (import (scheme base))
  (begin
    (define lit 42)
    (define-syntax mg
      (syntax-rules (lit)
        ((_ lit) 'MATCHED-LITERAL)
        ((_ x) 'OTHER)))))
`
	const libRulesExportingLit = `(define-library (w16lib)
  (export mg lit)
  (import (scheme base))
  (begin
    (define lit 42)
    (define-syntax mg
      (syntax-rules (lit)
        ((_ lit) 'MATCHED-LITERAL)
        ((_ x) 'OTHER)))))
`
	const libCase = `(define-library (w16lib)
  (export mg)
  (import (scheme base))
  (begin
    (define lit 42)
    (define-syntax mg
      (lambda (stx)
        (syntax-case stx (lit)
          ((_ lit) #''MATCHED-LITERAL)
          ((_ x) #''OTHER))))))
`

	cases := []struct {
		name string
		lib  string
		src  string
		want string
	}{
		{
			name: "importing only the macro leaves the literal's binding out of reach",
			lib:  libRules,
			src:  "(import (w16lib))\n(mg lit)",
			want: "OTHER",
		},
		{
			name: "the program's own lit is a different binding",
			lib:  libRules,
			src:  "(import (w16lib))\n(define lit 7)\n(mg lit)",
			want: "OTHER",
		},
		{
			// The import mints a FRESH *Binding for a re-exported name, so the
			// use-site and definition-site pointers can never be equal here. This
			// row is what makes literalNotShadowed's IsImported rider load-bearing:
			// without it the answer degrades to OTHER.
			name: "importing the literal too restores the match",
			lib:  libRulesExportingLit,
			src:  "(import (w16lib))\n(mg lit)",
			want: "MATCHED-LITERAL",
		},
		{
			name: "syntax-case takes the same path",
			lib:  libCase,
			src:  "(import (w16lib))\n(mg lit)",
			want: "OTHER",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng := w16LibEngine(t, tc.lib)
			got := evalString(t, eng, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// The dotted-tail traversal must not turn a proper-list pattern into one that
// accepts improper input: `(_ a ...)` requires a list, so the unconsumed tail
// is a mismatch and the second clause takes it.
func TestProperListPatternStillRejectsDottedInput(t *testing.T) {
	src := `(begin
              (define-syntax m
                (syntax-rules ()
                  ((_ a ...) 'proper)
                  ((_ a ... . r) 'improper)))
              (list (m 1 2) (m 1 . 2) (m 1 2 . 3)))`
	got := mustEvalScheme(t, src)
	want := "(proper improper improper)"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}
