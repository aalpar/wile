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
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestP1_BootstrapMacrosVisibleFromNestedTransformer (F10): fails on master with
// "no such binding and".
func TestP1_BootstrapMacrosVisibleFromNestedTransformer(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax m
  (lambda (stx)
    (datum->syntax #f
      (let-syntax ((k (lambda (s) (datum->syntax #f (and 1 2)))))
        (k)))))
(m)`)
	c.Assert(got, qt.Equals, "2")
}

// TestP1_Q8NestedTransformerUsesSyntaxCase is design Q8's own pin: a let-syntax
// inside a transformer body whose transformer uses syntax-case, under the Scheme
// layer, one rung above TestP1_BootstrapMacrosVisibleFromNestedTransformer: k is
// a transformer written inside another transformer's body, so the let-syntax RHS
// in ITS body evaluates at phase 3 (design §3.0), and the syntax-case there binds
// the pattern variable v the §3.0 pin asks for. Green through the Go compiler until Task 8
// defines syntax-case in Scheme (F1: an excluded row does not stop the
// name-dispatched compiler), so it discriminates arm 2b only from Task 8 on.
func TestP1_Q8NestedTransformerUsesSyntaxCase(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax m
  (lambda (stx)
    (datum->syntax #f
      (let-syntax ((k (lambda (s)
                        (datum->syntax #f
                          (let-syntax ((j (lambda (w) (syntax-case w () ((_ v) #'(+ 3 v))))))
                            (j 4))))))
        (k)))))
(m)`, wile.WithSchemeSyntaxForms())
	c.Assert(got, qt.Equals, "7")
}

const syntaxCaseProbe = `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x) #'x))))
(m 5)`

// TestP1_SwitchSelectsTheLayer: the Go syntax-case compiler runs under
// WithGoSyntaxForms and is unreachable under WithSchemeSyntaxForms (design §7
// P1's unreachability pin, as a process-global counter delta).
func TestP1_SwitchSelectsTheLayer(t *testing.T) {
	c := qt.New(t)
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe, wile.WithGoSyntaxForms()), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles() > before, qt.IsTrue)

	before = compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe, wile.WithSchemeSyntaxForms()), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
}

// TestP1_EnvironmentVariableSetsTheDefault (F9): WILE_SYNTAX_FORMS is read per
// NewEngine, so the whole suite runs both ways from the environment.
func TestP1_EnvironmentVariableSetsTheDefault(t *testing.T) {
	c := qt.New(t)
	t.Setenv("WILE_SYNTAX_FORMS", "scheme")
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)

	t.Setenv("WILE_SYNTAX_FORMS", "go")
	before = compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles() > before, qt.IsTrue)
}

// scLibFS: a library whose macro uses syntax-case. Library environments bootstrap
// from the namespace's registry, so the switch must reach them (F9).
var scLibFS = fstest.MapFS{
	"sclib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (sclib)
  (import (scheme base))
  (export m)
  (begin (define-syntax m (lambda (stx) (syntax-case stx () ((_ x) #'(list x x)))))))`),
	},
}

func TestP1_SwitchReachesLibraryEnvironments(t *testing.T) {
	c := qt.New(t)
	before := compilation.GoSyntaxFormCompiles()
	got := evalSyntaxForms(t, `(import (sclib)) (m 5)`,
		wile.WithSchemeSyntaxForms(), wile.WithSourceFS(stdlib.FS), wile.WithSourceFS(scLibFS), wile.WithLibraryPaths())
	c.Assert(got, qt.Equals, "(5 5)")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
}

// TestP1_SchemeLayerClosesDefects: design §5.3 (lone-identifier pattern), §5.4
// (clause-body locals arrive boxed, in a syntax-case body and a with-syntax body
// alike), §2.3 (a clause-body `let` binder over a pattern variable name) and two
// Go-matcher gaps (`x ... ...`, a with-syntax body template over an
// outer pattern variable) die with the Go syntax-case. Every row fails under
// WithGoSyntaxForms — that is the point — and moves into
// test/scheme/syntax-case-test.scm in P3.
func TestP1_SchemeLayerClosesDefects(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{
			name: "§5.3 lone identifier pattern binds the whole form",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx () (x #'(quote x))))) (m 1 2)`,
			want: "(m 1 2)",
		},
		{
			name: "§5.4 a local helper called from a clause body is not boxed",
			src: `(define-syntax m
  (lambda (stx)
    (let ((n 7))
      (syntax-case stx ()
        ((_) (if (box? n) #'"boxed" #'"plain"))))))
(m)`,
			want: `"plain"`,
		},
		{
			name: "§5.4 the transformer's own parameter is usable as a datum->syntax template",
			src: `(define-syntax m
  (lambda (stx)
    (syntax-case stx ()
      ((_ v) (datum->syntax stx (list 'quote (syntax->datum #'v)))))))
(m (a b))`,
			want: "(a b)",
		},
		{
			name: "§5.4 a with-syntax body reads its enclosing local unboxed",
			src: `(define-syntax m
  (lambda (stx)
    (let ((n 7))
      (syntax-case stx ()
        ((_) (with-syntax ((x #'a)) (if (box? n) #'"boxed" #'"plain")))))))
(m)`,
			want: `"plain"`,
		},
		{
			name: "§2.3 a clause-body let binder over a pattern variable name",
			src: `(define-syntax m
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) (let ((x 1)) #'(quote x))))))
(m foo)`,
			want: "x",
		},
		{
			// The Go layer answers (quasisyntax (unsyntax 5)): it evaluates an
			// operand at quasisyntax depth 2, where R6RS §12.8 lowers the depth
			// to 1 and leaves the form alone. petite answers (quasiquote
			// (unquote x)) for the quasiquote twin and racket answers
			// (quasisyntax (unsyntax x)) for this one (both measured
			// 2026-09-06). Returns here from integration/quasisyntax_test.go in
			// P3.
			name: "nested quasisyntax at depth 2 does not evaluate",
			src:  "(let ((x 5)) (syntax->datum (quasisyntax (quasisyntax #,x))))",
			want: "(quasisyntax (unsyntax x))",
		},
		{
			name: "matcher gap: x ... ... splices two ellipsis levels",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx () ((_ (x ...) ...) #'(list x ... ...))))) (m (1 2) (3))`,
			want: "(1 2 3)",
		},
		{
			name: "matcher gap: a with-syntax body template reads an outer pattern variable",
			src: `(define-syntax m
  (lambda (stx)
    (syntax-case stx ()
      ((_ v ...)
       (with-syntax (((t ...) (generate-temporaries #'(v ...))))
         #'(let ((t v) ...) (list t ...)))))))
(m 1 2)`,
			want: "(1 2)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src, wile.WithSchemeSyntaxForms()), qt.Equals, tc.want)
		})
	}
}

// TestP1_SchemeLayerDiagnostics pins the Scheme layer's error TEXT. These rows
// live here rather than in test/scheme/syntax-case-test.scm because the two
// layers differ in where the error is raised, not just in what it says: a Go
// syntax-case diagnostic is an expander-level Go error and never enters the VM,
// so (guard … (eval …)) catches it, while %syntax-violation returns from a
// primitive inside a macro transformer, the VM turns that into a raise, the
// transformer's macro sub-context carries no handler chain, and the exception
// escapes past any guard around the eval. (error "boom") inside a transformer is
// equally uncatchable on the Go layer, so this is Wile's existing behaviour
// rather than anything the layer introduces — but it means a Scheme suite that
// must pass under both layers cannot assert on it. The Go error is read
// directly here instead.
//
// The source location on these is pinned by TestP05_SyntaxViolationCarriesSource.
func TestP1_SchemeLayerDiagnostics(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{
			name: "no clause matches",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x) #'x)))) (m)`,
			want: `syntax-case: no clause matches the form`,
		},
		{
			name: "duplicate pattern variable",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x x) #'x)))) (m 1 2)`,
			want: `syntax-case: duplicate pattern variable`,
		},
		{
			name: "pattern variable used with too few ellipses",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x ...) #'x)))) (m 1)`,
			want: `syntax: pattern variable used with too few ellipses`,
		},
		{
			name: "unsyntax outside quasisyntax",
			src:  `(define-syntax m (lambda (stx) #'(unsyntax 1))) (m)`,
			want: `unsyntax: not in quasisyntax context`,
		},
		{
			name: "literal is not an identifier",
			src:  `(define-syntax m (lambda (stx) (syntax-case stx (1) ((_ x) #'x)))) (m 2)`,
			want: `syntax-case: literal is not an identifier`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, tc.src, wile.WithSchemeSyntaxForms())
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, tc.want)
		})
	}
}

// TestP2_SyntaxRulesAndERAreScheme: the Go compilers for both transformer
// producers are unreachable under the Scheme layer. The answers agree on both
// layers (identifier? on a form leaf is #t since the Go ER boundary hands the
// proc the spine), so the counter is what tells the layers apart.
func TestP2_SyntaxRulesAndERAreScheme(t *testing.T) {
	c := qt.New(t)
	// The for-syntax import is the subject as much as the accessor is: it used to
	// compile ONE Go syntax form even under WithSchemeSyntaxForms, which is the
	// counter delta below. TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules pins
	// that directly; this row keeps it honest on the two-layer path.
	src := `(import (for-syntax (scheme base)))  ; cadr
(define-syntax sr (syntax-rules () ((_ x) (list x x))))
(define-syntax er (er-macro-transformer (lambda (f r c) (list (r 'quote) (identifier? (cadr f))))))
(list (sr 1) (er zz))`
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, src, withStdlibResolver(wile.WithSchemeSyntaxForms())...), qt.Equals, "((1 1) #t)")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
	c.Assert(evalSyntaxForms(t, src, withStdlibResolver(wile.WithGoSyntaxForms())...), qt.Equals, "((1 1) #t)")
	c.Assert(compilation.GoSyntaxFormCompiles() > before, qt.IsTrue)
}

// syntaxLayers is both shipped implementations of the syntax forms, for a
// contract both must honor: two layers disagreeing on one program is the
// internal-invariant form of a defect (I031).
var syntaxLayers = []struct {
	name string
	opt  wile.EngineOption
}{
	{"go", wile.WithGoSyntaxForms()},
	{"scheme", wile.WithSchemeSyntaxForms()},
}

// TestP2_ERPassThroughKeepsScopes closes design §5.1: an identifier that merely
// passes through an ER macro keeps its scopes, so the syntax-rules twin and the
// ER twin agree. A boundary that strips them answers 2 (the user's tmp is
// captured by the macro's binder).
func TestP2_ERPassThroughKeepsScopes(t *testing.T) {
	src := `(import (for-syntax (scheme base)))  ; cadr
(define-syntax er-id (er-macro-transformer (lambda (f r c) (cadr f))))
(define-syntax via-er (syntax-rules () ((_ e) (let ((tmp 1)) (er-id (+ tmp e))))))
(define-syntax via-sr (syntax-rules () ((_ e) (let ((tmp 1)) (+ tmp e)))))
(list (let ((tmp 10)) (via-er tmp)) (let ((tmp 10)) (via-sr tmp)))`
	for _, l := range syntaxLayers {
		t.Run(l.name, func(t *testing.T) {
			qt.Assert(t, evalSyntaxForms(t, src, withStdlibResolver(l.opt)...), qt.Equals, "(11 11)")
		})
	}
}

// TestP2_ERUseSitePrunedBinder: a binder that reaches an ER define through a
// syntax-rules macro wears that macro's use-site scope, and the ER boundary
// prunes it as every other transformer boundary does. Unpruned, zz is unbound.
func TestP2_ERUseSitePrunedBinder(t *testing.T) {
	src := `(import (for-syntax (scheme base)))  ; cadr
(define-syntax def5 (er-macro-transformer (lambda (f r c) (list (r 'define) (cadr f) 5))))
(define-syntax via (syntax-rules () ((_ n) (def5 n))))
(define (body) (via zz) zz)
(via top)
(list (body) top)`
	for _, l := range syntaxLayers {
		t.Run(l.name, func(t *testing.T) {
			qt.Assert(t, evalSyntaxForms(t, src, withStdlibResolver(l.opt)...), qt.Equals, "(5 5)")
		})
	}
}

// TestP2_ERBoundaryScopes pins what the ER boundary does and does not take from
// the use-site triad. A raw symbol the proc introduces stays use-site: it binds
// and is bound by the user's names (no flip), and it shares a pass-through
// binder's scopes (no use-site stamp on the input); through a syntax-rules
// wrapper it carries the wrapper's scope, so it no longer meets the user's x.
// Binders the output defines are pruned in every definition shape.
func TestP2_ERBoundaryScopes(t *testing.T) {
	const bindref = `(define-syntax bindref (er-macro-transformer (lambda (f r c) (list (r 'let) (list (list (cadr f) 7)) 'x))))
(define x 'global)
`
	const via = "(define-syntax via (syntax-rules () ((_ n) (m n))))\n"
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{"raw ref meets a pass-through binder", bindref + `(bindref x)`, "7"},
		{"raw ref behind a syntax-rules wrapper", bindref +
			"(define-syntax via (syntax-rules () ((_ n) (bindref n))))\n(via x)", "global"},
		{"raw define binds the user's name",
			`(define-syntax def-it (er-macro-transformer (lambda (f r c) (list (r 'define) 'it 5))))
(def-it) it`, "5"},
		{"raw let binds the user's name",
			`(define-syntax alet (er-macro-transformer (lambda (f r c) (list (r 'let) (list (list 'it 7)) (cadr f)))))
(alet it)`, "7"},
		{"prune inside begin",
			`(define-syntax m (er-macro-transformer (lambda (f r c) (list (r 'begin) (list (r 'define) (cadr f) 1)))))
` + via + `(via zz) zz`, "1"},
		{"prune define-syntax",
			`(define-syntax m (er-macro-transformer (lambda (f r c) (list (r 'define-syntax) (cadr f) (list (r 'syntax-rules) '() (list '(_) 3))))))
` + via + `(via mm) (mm)`, "3"},
		{"prune procedure define",
			`(define-syntax m (er-macro-transformer (lambda (f r c) (list (r 'define) (list (cadr f)) 4))))
` + via + `(via ff) (ff)`, "4"},
		{"prune through two wrappers",
			`(define-syntax m (er-macro-transformer (lambda (f r c) (list (r 'define) (cadr f) 5))))
` + via + "(define-syntax via2 (syntax-rules () ((_ n) (via n))))\n(via2 qq) qq", "5"},
	}
	for _, tc := range tcs {
		src := "(import (for-syntax (scheme base)))  ; cadr\n" + tc.src
		for _, l := range syntaxLayers {
			t.Run(tc.name+"/"+l.name, func(t *testing.T) {
				qt.Assert(t, evalSyntaxForms(t, src, withStdlibResolver(l.opt)...), qt.Equals, tc.want)
			})
		}
	}
}

// TestP2_ERContract (Q6): the proc receives the spine with identifier leaves
// intact — identifier? is the test, compare the equality; symbol? is #f; pair?
// on a form element stays #t, which is what a whole-tree pass-through of syntax
// pairs would silently break.
func TestP2_ERContract(t *testing.T) {
	src := `(import (for-syntax (scheme base))  ; cadr
        (for-syntax (scheme cxr)))  ; caddr, cadddr — cxr.sld, not base.sld
(define-syntax er-probe
  (er-macro-transformer
    (lambda (f r c)
      (list (r 'list)
            (identifier? (cadr f)) (symbol? (cadr f))
            (c (cadr f) (r 'magic)) (pair? (caddr f)) (cadddr f)))))
(er-probe magic (a b) 3)`
	for _, l := range syntaxLayers {
		t.Run(l.name, func(t *testing.T) {
			qt.Assert(t, evalSyntaxForms(t, src, withStdlibResolver(l.opt)...), qt.Equals, "(#t #f #t #t 3)")
		})
	}
}

// TestP2_ERSpineContainers: a box and a cyclic datum in the form are plain
// values to the proc, not syntax. A box unwrapped one level would arrive as its
// content; a cyclic datum walked as a spine would never terminate.
func TestP2_ERSpineContainers(t *testing.T) {
	tcs := []struct {
		name string
		use  string
		want string
	}{
		{"box", `(probe #&(a b))`, "(#t #f #t)"},
		{"cyclic datum", `(probe #0=(a . #0#))`, "(#f #t #t)"},
	}
	for _, tc := range tcs {
		src := `(import (for-syntax (scheme base)))  ; cadr
(define-syntax probe
  (er-macro-transformer
    (lambda (f r c)
      (let ((x (cadr f)))
        (list (r 'quote)
              (list (box? x) (pair? x) (pair? (if (box? x) (unbox x) (cdr x)))))))))
` + tc.use
		for _, l := range syntaxLayers {
			t.Run(tc.name+"/"+l.name, func(t *testing.T) {
				qt.Assert(t, evalSyntaxForms(t, src, withStdlibResolver(l.opt)...), qt.Equals, tc.want)
			})
		}
	}
}

// TestP2_ERContractCrossLibrary is the same contract read across a library
// boundary — the only path on which the form leaves the engine's own namespace.
// er-contract is exported from (wile er-macro-test).
func TestP2_ERContractCrossLibrary(t *testing.T) {
	for _, l := range syntaxLayers {
		t.Run(l.name, func(t *testing.T) {
			got := evalSyntaxForms(t, `(import (wile er-macro-test)) (er-contract x)`,
				l.opt, wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths())
			qt.Assert(t, got, qt.Equals, "(#t #f)")
		})
	}
}

// TestP2_ERRenameIsFreshPerInvocation is a guard: two uses of an ER macro that
// renames tmp nest without collision, and a rename of a top-level
// definition-site binding resolves there. It passes on master; the flip must
// keep it passing, so it runs on whichever layer the suite selects.
func TestP2_ERRenameIsFreshPerInvocation(t *testing.T) {
	c := qt.New(t)
	// The for-syntax import is part of the subject: it used to move (r 'helper)
	// off the definition site, answering (user use). A rename now resolves at the
	// OUTPUT phase (TestERRenameDenotesTheOutputPhase), so the import supplies
	// cadr to the transformer body and moves nothing.
	got := evalSyntaxForms(t, `(import (for-syntax (scheme base)) (for-syntax (scheme cxr)))  ; cadr / caddr
(define (helper) 'def)
(define-syntax er-or
  (er-macro-transformer
    (lambda (f r c)
      (list (r 'let) (list (list (r 'tmp) (cadr f)))
            (list (r 'if) (r 'tmp) (r 'tmp) (caddr f))))))
(define-syntax er-h (er-macro-transformer (lambda (f r c) (list (r 'helper)))))
(let ((helper (lambda () 'use)) (tmp 'user))
  (list (er-or #f (er-or #f tmp)) (er-h)))`, withStdlibResolver()...)
	c.Assert(got, qt.Equals, "(user def)")
}

// TestP2_ERRenameResolvesAtDefinitionSite is the pin for design §3.5's rename
// contract on a definition-site LOCAL: the renamed identifier carries #'k's
// scopes, so the macro's own binder wins over a use-site binder of the same
// name. On the Go layer the rename is empty+intro scoped and this is a compile
// error (`no such binding "helper" with compatible scopes`).
func TestP2_ERRenameResolvesAtDefinitionSite(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(let ((helper (lambda () 'def)))
  (define-syntax er-h (er-macro-transformer (lambda (f r c) (list (r 'helper)))))
  (let ((helper (lambda () 'use))) (er-h)))`, wile.WithSchemeSyntaxForms())
	c.Assert(got, qt.Equals, "def")
}

// TestP2_ERCompareRequiresIdentifiers pins the compare contract (design §3.5):
// the shim passes `free-identifier=?`, which takes two identifiers. The Go layer
// widened it to accept a raw symbol (er_macro_compare.go's *values.Symbol arm);
// the Scheme layer raises instead. Every in-tree ER macro compares a form leaf
// against (rename 'x) — (cadr form) in literal-check and er_macro_compare.scm,
// (car clause) in er_macro_cond.scm — and under the spine contract those leaves
// are identifiers, so nothing depends on the widening.
func TestP2_ERCompareRequiresIdentifiers(t *testing.T) {
	c := qt.New(t)
	err := evalSyntaxFormsErr(t, `(import (for-syntax (scheme base)))  ; cadr
(define-syntax er-cmp
  (er-macro-transformer (lambda (f r c) (list (r 'quote) (c 'magic (cadr f))))))
(er-cmp magic)`, withStdlibResolver(wile.WithSchemeSyntaxForms())...)
	c.Assert(err, qt.ErrorMatches, `(?s).*free-identifier=\?: argument 1 is not an identifier.*`)
}

// TestP2_NestedExpansionsOfOneMacroDoNotShareScopes is the regression pin for
// the scope-set aliasing defect the Scheme layer exposed
// (values.AddScopeToSet). A template identifier is ONE shared *SyntaxSymbol
// here, where the Go producer minted a fresh copy per expansion, so two nested
// expansions flip two different introduction scopes into the same spare slot of
// one backing array. The outer binder kept its own intro scope while the outer
// body's references read the inner one, and the form failed to compile with
// `no such binding "zz" with compatible scopes`.
//
// (or (or #f #f) #t) is the shape that found it: every bootstrap `or` beyond
// the first operand is one of these, and (wile algebra interval) failed to load.
func TestP2_NestedExpansionsOfOneMacroDoNotShareScopes(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{"or nested in its own first operand", `(or (or #f #f) #t)`, "#t"},
		{
			name: "a user macro nested in its own let init",
			src: `(define-syntax m1 (syntax-rules () ((_ e) (let ((zz e)) (if zz zz 9)))))
(m1 (m1 #f))`,
			want: "9",
		},
		{
			name: "the same shape through syntax-case",
			src: `(define-syntax m1 (lambda (s) (syntax-case s () ((_ e) #'(let ((zz e)) (if zz zz 9))))))
(m1 (m1 #f))`,
			want: "9",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src, wile.WithSchemeSyntaxForms()), qt.Equals, tc.want)
		})
	}
}

// TestP2_SyntaxRulesSurface covers the R7RS §4.3.2 shapes beyond the R6RS
// derivation (design §3.4). Every row holds on both layers, so it runs on
// whichever the suite selects; it is the P2 shape gate, not a pin.
func TestP2_SyntaxRulesSurface(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{"custom ellipsis, and ... as an ordinary identifier under it",
			`(define-syntax m-ell (syntax-rules ::: () ((_ x :::) (list x ::: '...)))) (m-ell 1 2 3)`,
			"(1 2 3 ...)"},
		{"... in the literals list is a literal",
			`(define-syntax m-lit-ell (syntax-rules (...) ((_ ...) 1) ((_ x) 2))) (list (m-lit-ell ...) (m-lit-ell 5))`,
			"(1 2)"},
		{"_ in patterns", `(define-syntax m-under (syntax-rules () ((_ _ x _) x))) (m-under 1 2 3)`, "2"},
		{"(... ...) escapes an ellipsis in a template",
			`(define-syntax m-esc (syntax-rules () ((_ x) '(x (... ...))))) (m-esc a)`, "(a ...)"},
		{"the keyword position is ignored",
			`(define-syntax m-kw (syntax-rules () ((anything-here) 'ok))) (m-kw)`, "ok"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, "(let () "+tc.src+")"), qt.Equals, tc.want)
		})
	}
}
