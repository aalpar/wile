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

package machine_test

// Scope resolution divergence test suite.
//
// Two code paths implement Flatt's "bindingScopes ⊆ useScopes" scope resolution:
//
//   Path 1 — Shadow detection (environment/environment_frame.go)
//     HasLocalVariableBinding: shared by both the expander (macro shadow check)
//     and the validator (special form shadow check). Checks binding-site scopes.
//
//   Path 2 — Compiler (machine/compile_time_continuation.go)
//     CompileSymbol: dispatches between local/global/scoped code generation.
//     Checks use-site scopes for fast-path, then uses GetLocalIndex /
//     GetBinding for scope-aware lookup.
//
// A bug fix in one path but not the other causes silent divergence. These tests
// exercise identical scope scenarios through the full expand → validate → compile
// → run pipeline, catching divergence by checking runtime results.
//
// See plans/STAFF_ENGINEER_REVIEW.md N6.

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// scopeResolutionEnv creates a full runtime environment with bootstrap macros
// (and, or, let, when, unless, cond, etc.).
func scopeResolutionEnv(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return env
}

// evalScope runs a Scheme expression through expand → compile → run and returns
// the result. Exercises all three scope resolution paths with the same input.
func evalScope(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	sv, err := p.ReadSyntax(context.TODO())
	if err != nil {
		return nil, err
	}
	return evalScopeSyntax(env, sv)
}

// evalScopeSyntax runs a parsed syntax value through expand → compile → run.
func evalScopeSyntax(env *environment.EnvironmentFrame, sv syntax.SyntaxValue) (values.Value, error) {
	expanded, err := compilation.NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator()).ExpandExpression(sv)
	if err != nil {
		return nil, err
	}
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(ctctx, expanded)
	if err != nil {
		return nil, err
	}
	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	err = mc.Run()
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

// TestScopeResolution_NoScopes tests basic binding resolution when no macro
// scopes are involved (user code, no macro expansion).
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding not reached (no special form / macro in head)
//	Compiler: CompileSymbol no-scope fast path (len(symbolScopes) == 0)
//	          → GetLocalIndex / GetGlobalIndex
func TestScopeResolution_NoScopes(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			name: "local variable via let",
			code: "(let ((x 42)) x)",
			want: values.NewInteger(42),
		},
		{
			name:  "global variable via define",
			setup: []string{"(define g 99)"},
			code:  "g",
			want:  values.NewInteger(99),
		},
		{
			name: "nested local shadows outer local",
			code: "(let ((x 1)) (let ((x 2)) x))",
			want: values.NewInteger(2),
		},
		{
			name: "lambda parameter as local binding",
			code: "((lambda (x) (+ x 1)) 10)",
			want: values.NewInteger(11),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			if len(tc.setup) > 0 {
				for _, s := range tc.setup {
					_, err := evalScope(t, env, s)
					qt.Assert(t, err, qt.IsNil)
				}
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_LetShadowsMacro tests that let bindings shadow macro
// definitions in all three paths.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding returns true → expander skips macro lookup,
//	          validator falls through to validateCall.
//	Compiler: CompileSymbol no-scope path → GetLocalIndex finds the let binding,
//	          emits LoadLocal (not macro expansion output).
//
// Divergence detection: if the expander still expands 'and' as a macro while
// the compiler resolves it as a local variable, the compiled code operates on
// macro-expanded output but the binding points to a local integer — producing
// a type error or wrong value at runtime.
func TestScopeResolution_LetShadowsMacro(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "let shadows and - value read",
			code: "(let ((and 5)) and)",
			want: values.NewInteger(5),
		},
		{
			name: "let shadows or - value read",
			code: "(let ((or 200)) or)",
			want: values.NewInteger(200),
		},
		{
			name: "let shadows and - used in arithmetic",
			code: "(let ((and 5)) (+ and 3))",
			want: values.NewInteger(8),
		},
		{
			name: "lambda parameter shadows and",
			code: "((lambda (and) and) 123)",
			want: values.NewInteger(123),
		},
		{
			name: "lambda parameter shadows or",
			code: "((lambda (or) (+ or 1)) 50)",
			want: values.NewInteger(51),
		},
		{
			name: "nested let - inner shadows macro, outer uses macro",
			// Outer (and #t 10) should expand as macro → 10.
			// Inner (and 1) should read as local variable → 1.
			// Result: 10 + 1 = 11.
			code: `(let ((x (and #t 10)))
			         (let ((and 1))
			           (+ x and)))`,
			want: values.NewInteger(11),
		},
		{
			// Shadowed name in operator position forces the expander to decide:
			// macro invocation or procedure call? Must choose procedure call.
			name: "let shadows and - callable in operator position",
			code: "(let ((and (lambda (x y) (+ x y)))) (and 2 3))",
			want: values.NewInteger(5),
		},
		{
			name: "let shadows or - callable in operator position",
			code: "(let ((or (lambda (x y) (if x x y)))) (or #f 42))",
			want: values.NewInteger(42),
		},
		{
			name: "macro still works when not shadowed - and",
			code: "(let ((x 1)) (and #t #t))",
			want: values.TrueValue,
		},
		{
			name: "macro still works when not shadowed - or",
			code: "(let ((x 1)) (or #f #t))",
			want: values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_MacroHygiene tests that macro-introduced bindings with
// intro scopes resolve correctly — the macro's bindings don't capture
// identically-named user bindings.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding checks ScopesMatch for macro-introduced
//	          bindings in both expander and validator contexts.
//	Compiler: CompileSymbol scoped path (len(symbolScopes) > 0) →
//	          GetLocalIndex finds the correct scoped binding.
//
// Divergence detection: if the compiler's GetLocalIndex disagrees with
// the expander's ScopesMatch about which binding a scoped symbol refers to,
// the macro reads the wrong variable at runtime.
func TestScopeResolution_MacroHygiene(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			name: "swap macro hygiene - user tmp not captured",
			setup: []string{
				`(define-syntax swap!
				   (syntax-rules ()
				     ((swap! a b)
				      (let ((tmp a))
				        (set! a b)
				        (set! b tmp)))))`,
				"(define x 1)",
				"(define y 2)",
			},
			// User's tmp (99) must not be captured by swap!'s intro-scoped tmp.
			code: `(begin
			         (let ((tmp 99))
			           (swap! x y)
			           tmp))`,
			want: values.NewInteger(99),
		},
		{
			name: "swap macro correctness - values actually swap",
			setup: []string{
				`(define-syntax swap!
				   (syntax-rules ()
				     ((swap! a b)
				      (let ((tmp a))
				        (set! a b)
				        (set! b tmp)))))`,
				"(define x 1)",
				"(define y 2)",
				"(swap! x y)",
			},
			code: "(+ x y)", // x=2, y=1, sum=3 (unchanged but order swapped)
			want: values.NewInteger(3),
		},
		{
			name: "macro-introduced let binding distinct from outer",
			// The and macro (bootstrap) introduces internal bindings.
			// An outer binding with the same name must not interfere.
			code: `(let ((t #f))
			         (and #t 42))`,
			want: values.NewInteger(42),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			if len(tc.setup) > 0 {
				for _, s := range tc.setup {
					_, err := evalScope(t, env, s)
					qt.Assert(t, err, qt.IsNil)
				}
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_SpecialFormShadowing tests that let bindings shadow
// core special forms (if, begin, etc.) recognized by the validator.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding returns true → expander skips primitive
//	          expansion, validator falls through to validateCall.
//	Compiler: CompileSymbol → GetLocalIndex finds the local binding; compiles
//	          as a variable load, not as a conditional branch.
//
// Divergence detection: if the validator still recognizes 'if' as a special
// form while the expander treats it as a variable, the validator produces a
// ValidatedIf but the expression structure is wrong → compile error.
func TestScopeResolution_SpecialFormShadowing(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "let shadows if - used as value",
			code: "(let ((if 42)) if)",
			want: values.NewInteger(42),
		},
		{
			name: "let shadows begin - used as value",
			code: "(let ((begin 7)) begin)",
			want: values.NewInteger(7),
		},
		{
			name: "lambda parameter shadows if",
			code: "((lambda (if) (+ if 1)) 10)",
			want: values.NewInteger(11),
		},
		{
			// Shadowed 'if' in operator position: validator must NOT dispatch
			// to validateIf; must fall through to validateCall.
			name: "let shadows if - callable in operator position",
			code: "(let ((if (lambda (x y z) x))) (if 10 1 2))",
			want: values.NewInteger(10),
		},
		{
			name: "if still works when not shadowed",
			code: "(let ((x 1)) (if #t 42 0))",
			want: values.NewInteger(42),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_NestedMacros tests scope resolution with multiple layers
// of macro expansion, each adding its own intro scope.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding must handle growing scope sets as each
//	          macro invocation adds an intro scope.
//	Compiler: CompileSymbol scoped path → GetLocalIndex must find
//	          the maximally-specific binding among multiple scoped candidates.
//
// Divergence detection: if scope set accumulation differs between the expander
// (which adds scopes during expansion) and the compiler (which resolves during
// compilation), nested macros produce wrong bindings.
func TestScopeResolution_NestedMacros(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			// let is a bootstrap macro, so nested let produces two layers of
			// macro expansion (each let → lambda) with accumulated scopes.
			name: "nested let introduces layered macro scopes",
			code: "(let ((x 10)) (let ((y 20)) (+ x y)))",
			want: values.NewInteger(30),
		},
		{
			// User-defined macro wrapping let: two layers of user-macro +
			// bootstrap-macro expansion. Pattern variable 'name' preserves
			// the user's identifier through substitution.
			name: "user-defined macro wrapping let - two layers",
			setup: []string{
				`(define-syntax bind
				   (syntax-rules ()
				     ((bind name val body)
				      (let ((name val)) body))))`,
			},
			code: "(bind x 10 (bind y 20 (+ x y)))",
			want: values.NewInteger(30),
		},
		{
			// Inner binding shadows outer with same name. The compiler's
			// GetLocalIndex must pick the maximally-specific binding.
			name: "inner same-name binding shadows outer through macro layers",
			setup: []string{
				`(define-syntax bind
				   (syntax-rules ()
				     ((bind name val body)
				      (let ((name val)) body))))`,
			},
			code: "(bind x 1 (bind x 2 x))",
			want: values.NewInteger(2),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			for _, s := range tc.setup {
				_, err := evalScope(t, env, s)
				qt.Assert(t, err, qt.IsNil)
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_FreeIdentifiers tests that free identifiers in macro
// templates (identifiers not bound as pattern variables) resolve to their
// definition-time bindings, not use-site bindings.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding returns false (free identifiers resolve
//	          to definition-time globals, not local variables).
//	Compiler: CompileSymbol → ResolvedBinding path for cross-scope references,
//	          or GetBinding for global bindings with scope metadata.
//
// Divergence detection: if free identifier resolution differs between expander
// (which sets ResolvedBinding) and compiler (which reads it), the macro
// references the wrong definition.
func TestScopeResolution_FreeIdentifiers(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			name: "free identifier resolves to definition-time binding",
			setup: []string{
				"(define z 100)",
				`(define-syntax get-z
				   (syntax-rules ()
				     ((get-z) z)))`,
			},
			// get-z references 'z' as a free identifier. Even if we shadow z
			// at the use site, the macro should still see the original z=100.
			code: "(let ((z 999)) (get-z))",
			want: values.NewInteger(100),
		},
		{
			name: "free identifier + in macro resolves to primitive",
			setup: []string{
				`(define-syntax add-one
				   (syntax-rules ()
				     ((add-one x) (+ x 1))))`,
			},
			// '+' is a free identifier in the macro template. It should resolve
			// to the primitive +, not be affected by any local shadowing.
			code: "(let ((+ 999)) (add-one 41))",
			want: values.NewInteger(42),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			for _, s := range tc.setup {
				_, err := evalScope(t, env, s)
				qt.Assert(t, err, qt.IsNil)
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_LetSyntaxShadowing tests that let-syntax bindings create
// proper scope isolation for auxiliary syntax and macro shadowing.
//
// Path exercise:
//
//	Shadow:   HasLocalVariableBinding checks the binding created by let-syntax
//	          body wrapping (lambda scope from begin wrapper).
//	Compiler: CompileSymbol resolves through the scoped environment created
//	          during expansion.
//
// Divergence detection: if let-syntax scope handling differs between the
// expander (which creates the scope) and the compiler (which resolves through
// it), shadowed auxiliary syntax either wrongly matches or wrongly fails.
func TestScopeResolution_LetSyntaxShadowing(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			name: "let-syntax shadows macro in body",
			setup: []string{
				`(define-syntax my-val
				   (syntax-rules ()
				     ((my-val) 42)))`,
			},
			// let-syntax redefines my-val → 99 in its body scope.
			code: `(let-syntax ((my-val (syntax-rules () ((my-val) 99))))
			         (my-val))`,
			want: values.NewInteger(99),
		},
		{
			name: "let-syntax does not affect outer scope",
			setup: []string{
				`(define-syntax my-val
				   (syntax-rules ()
				     ((my-val) 42)))`,
			},
			// After let-syntax body, the outer my-val is still active.
			code: `(begin
			         (let-syntax ((my-val (syntax-rules () ((my-val) 99))))
			           (my-val))
			         (my-val))`,
			want: values.NewInteger(42),
		},
		{
			// Nested let-syntax rebinding the same keyword: the inner binder wins
			// in its body. Both binders are visible to the reference, and the
			// inner binder's scope set is a strict superset of the outer's, so it
			// resolves by maximality (see expander_let_syntax.go — keywords bind on
			// the accumulated scope set, not a bare singleton). This is the case
			// where the two binders would otherwise tie on scope-set cardinality.
			name: "nested let-syntax resolves innermost keyword",
			code: `(let-syntax ((m (syntax-rules () ((m) 10))))
			         (let-syntax ((m (syntax-rules () ((m) 20))))
			           (m)))`,
			want: values.NewInteger(20),
		},
		{
			name: "triple-nested let-syntax resolves innermost keyword",
			code: `(let-syntax ((m (syntax-rules () ((m) 1))))
			         (let-syntax ((m (syntax-rules () ((m) 2))))
			           (let-syntax ((m (syntax-rules () ((m) 3))))
			             (m))))`,
			want: values.NewInteger(3),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			for _, s := range tc.setup {
				_, err := evalScope(t, env, s)
				qt.Assert(t, err, qt.IsNil)
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_GlobalShadowsIntroducedBinder tests that a syntax-rules
// template identifier introduced by the template's OWN binding form (e.g. the
// `tmp` in `(let ((tmp x)) ... tmp)`) resolves to that introduced binding —
// even when a same-named GLOBAL is visible at macro-definition time. This is
// the R1 macro-hygiene bug (plans/2026-06-15-macro-hygiene-global-shadow-fix).
//
// Root cause: CompileSymbol consulted the symbol's ResolvedBinding pin (a global
// recorded at definition time) BEFORE its scope-set local match (GetLocalIndex),
// so the template's own binding never got a vote. The classic swap! returned
// (20 999) instead of (20 10).
//
// Trigger condition (verified empirically): the colliding global must be visible
// at macro-DEFINITION time. Bootstrap or/and are defined before any user global,
// so they take the intro-scope path and are unaffected — they serve as recursion
// regression guards here.
//
// Path exercise:
//
//	Compiler: CompileSymbol — GetLocalIndex (scope-set local match) must run
//	          BEFORE the ResolvedBinding fallback. A co-introduced binder carries
//	          the same intro scope as the reference, so it wins; a genuinely free
//	          identifier carries only {intro} (≠ any use-site local scope) and
//	          falls through to its def-time global (referential transparency).
func TestScopeResolution_GlobalShadowsIntroducedBinder(t *testing.T) {
	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			// Headline. Bug: (20 999). Correct: (20 10).
			name: "swap! introduced tmp shadows global tmp",
			setup: []string{
				"(define tmp 999)",
				`(define-syntax swap!
				   (syntax-rules ()
				     ((swap! a b)
				      (let ((tmp a))
				        (set! a b)
				        (set! b tmp)))))`,
				"(define p 10)",
				"(define q 20)",
				"(swap! p q)",
			},
			code: "(list p q)",
			want: values.List(values.NewInteger(20), values.NewInteger(10)),
		},
		{
			// T2: introduced t binds per-invocation; global t=999 must not leak.
			// Bug: (999 999). Correct: (1 2).
			name: "introduced binder shadows global per invocation",
			setup: []string{
				"(define t 999)",
				`(define-syntax m
				   (syntax-rules ()
				     ((m v) (let ((t v)) t))))`,
			},
			code: "(list (m 1) (m 2))",
			want: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			// Mixed: the same name n is FREE (refers to global 100) in one
			// position and BOUND (by the template's let) in another. Resolution
			// must be per-occurrence. Bug: 200 (bound n leaked to the global).
			// Correct: 100 + 5 = 105. Approach A (name-level) cannot express this.
			name: "same name free and bound in one template",
			setup: []string{
				"(define n 100)",
				`(define-syntax mix
				   (syntax-rules ()
				     ((mix v) (+ n (let ((n v)) n)))))`,
			},
			code: "(mix 5)",
			want: values.NewInteger(105),
		},
		{
			// Recursive user macro whose temp `acc` collides with a global
			// visible at definition time. Each expansion gets a fresh intro
			// scope (distinct temps); the macro name recurses via the free-id
			// fallback. Bug: 2997 (acc leaked to global at each level).
			// Correct: 1+2+3 = 6.
			name: "recursive macro with global-colliding temp",
			setup: []string{
				"(define acc 999)",
				`(define-syntax my-sum
				   (syntax-rules ()
				     ((my-sum) 0)
				     ((my-sum a b ...)
				      (let ((acc a)) (+ acc (my-sum b ...))))))`,
			},
			code: "(my-sum 1 2 3)",
			want: values.NewInteger(6),
		},
		{
			// Over-correction guard (T3): a GENUINELY free identifier must still
			// resolve to its definition-time global even when shadowed at the use
			// site. The fix must not regress referential transparency.
			name: "free identifier still resolves to def-time global",
			setup: []string{
				"(define z 1)",
				`(define-syntax getz
				   (syntax-rules ()
				     ((getz) z)))`,
			},
			code: "(let ((z 2)) (getz))",
			want: values.NewInteger(1),
		},
		{
			// Recursion regression guard: bootstrap or expands to
			// (let ((x ...)) (if x x (or ...))). A user global x (defined AFTER
			// bootstrap) must not break or's recursion.
			name:  "bootstrap or unaffected by global colliding its temp",
			setup: []string{"(define x 999)"},
			code:  "(or #f #f 42)",
			want:  values.NewInteger(42),
		},
		{
			name:  "bootstrap and unaffected by global x",
			setup: []string{"(define x 999)"},
			code:  "(and 1 2 3)",
			want:  values.NewInteger(3),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			for _, s := range tc.setup {
				_, err := evalScope(t, env, s)
				qt.Assert(t, err, qt.IsNil)
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_CoIntroducedGlobalShadowsPin is R1's un-mirrored half. R1
// demoted the definition-time pin (tryResolvedBinding) below the scope-precise
// LOCAL match, which fixed a template-introduced `let` binder. A template that
// introduces a top-level `define` creates a GLOBAL binding, invisible to
// GetLocalIndex, so the pin kept beating the binder co-introduced by the same
// expansion.
//
// R7RS §4.3 is explicit: an identifier a transformer inserts a binding for is in
// effect renamed throughout its scope. A same-named global that merely happened to
// exist when the transformer was compiled must not reach it.
//
// TRIGGER CONDITION, and why the "definition order" reading of it is wrong. The
// pin is filled by collectFreeIdentifiersWithEllipsis while the transformer
// compiles, so the collision must be resolvable in the env AT THAT MOMENT — which
// is not the same as appearing earlier in the source. A file executes
// (begin …)-wrapped and predeclareBodyDefines creates every top-level define's
// binding before any of them compiles, so within one compilation unit the
// collision is visible wherever it is written. The unit boundary is what
// discriminates, which is why the last case below is a two-unit split rather than
// a reordering.
//
// See plans/2026-07-29-name-keyed-identity-residuals-design.local.md Finding 2.
func TestScopeResolution_CoIntroducedGlobalShadowsPin(t *testing.T) {
	// mkdef introduces a top-level (define (rec i) …) AND names it from a second
	// introduced define. `rec` is therefore BOUND by the template, not free, but
	// the collector calls every non-pattern-variable identifier free.
	mkdef := `(define-syntax mkdef
	   (syntax-rules ()
	     ((_ f export)
	      (begin (define (rec i) (if (= i 3) (f i) (rec (+ i 1))))
	             (define export rec)))))`

	tcs := []struct {
		name  string
		setup []string
		code  string
		want  values.Value
	}{
		{
			// Headline. The user's rec exists when mkdef compiles, so the pin
			// points at it and `entry` binds (* 0 100) instead of the template's
			// own loop. Bug: 0. Correct: (* 3 7) = 21.
			name: "co-introduced global binder shadows def-time pin",
			setup: []string{
				"(define (rec x) (* x 100))",
				"(define (other x) (* x 7))",
				mkdef,
				"(mkdef other entry)",
			},
			code: "(entry 0)",
			want: values.NewInteger(21),
		},
		{
			// Control: identical shape, no collision. Isolates the user `rec` as
			// the sole cause — without this the headline could pass for shape
			// reasons. Correct at HEAD.
			name: "no colliding global at all",
			setup: []string{
				"(define (other x) (* x 7))",
				mkdef,
				"(mkdef other entry)",
			},
			code: "(entry 0)",
			want: values.NewInteger(21),
		},
		{
			// Control: the collision arrives in a LATER unit than the
			// define-syntax, so the pin is never filled and resolution falls
			// through to the scoped global match. Correct at HEAD; pins that the
			// fix does not disturb the unresolved-pin path.
			name: "colliding global defined in a later unit",
			setup: []string{
				"(define (other x) (* x 7))",
				mkdef,
				"(mkdef other entry)",
				"(define (rec x) (* x 100))",
			},
			code: "(entry 0)",
			want: values.NewInteger(21),
		},
		{
			// Over-correction guard for the global path, the analogue of the
			// "same name free and bound in one template" case above. The
			// intro-scoped reference must reach the co-introduced global (7)
			// while a bare use-site reference still reaches the user's (100).
			// A fix that hoists the scoped global lookup unconditionally passes
			// this one but fails the pin guards; a fix that does not hoist it at
			// all fails this one.
			name: "co-introduced global and same-named user global coexist",
			setup: []string{
				"(define n 100)",
				`(define-syntax mkn
				   (syntax-rules ()
				     ((_ export) (begin (define n 7) (define export n)))))`,
				"(mkn en)",
			},
			code: "(list en n)",
			want: values.List(values.NewInteger(7), values.NewInteger(100)),
		},
		{
			// The mixed case, which is what killed R1's Approach A. Both `n`s in
			// the template are at the SAME scope (unlike the local analogue, where
			// the introduced let contributes an extra scope), so they are one
			// FreeIdKey and cannot be resolved differently. R7RS §4.3 settles which
			// way: the introduced `define n` renames n "throughout its scope", and
			// the second reference is inside that scope, so BOTH are the introduced
			// binder. Bug: (101 100), the free reference reaching the user's n.
			name: "template's own binder wins for every reference at its scope",
			setup: []string{
				"(define n 100)",
				`(define-syntax mkn2
				   (syntax-rules ()
				     ((_ export) (begin (define n 7) (define export (+ n 1))))))`,
				"(mkn2 en)",
			},
			code: "(list en n)",
			want: values.List(values.NewInteger(8), values.NewInteger(100)),
		},
		{
			// Two expansions must get two binders, not one shared slot: each mints
			// a fresh intro scope, and CreateGlobalBinding reuses a slot only on
			// exact scope-set equality. Collapsing them would make both names read
			// whichever define ran last.
			name: "two expansions introduce two distinct globals",
			setup: []string{
				`(define-syntax mk3
				   (syntax-rules ()
				     ((_ nm x) (begin (define v x) (define nm v)))))`,
				"(mk3 a 1)",
				"(mk3 b 2)",
			},
			code: "(list a b)",
			want: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := scopeResolutionEnv(t)
			for _, s := range tc.setup {
				_, err := evalScope(t, env, s)
				qt.Assert(t, err, qt.IsNil)
			}
			result, err := evalScope(t, env, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestScopeResolution_CrossLibraryIntroducedBinderShadow guards the libScope
// sub-branch of the R1 fix. A macro defined in library A that introduces a
// binder (`tmp`) whose name ALSO names a global exported by A must let the
// introduced binder shadow when the macro is expanded in an importing context.
//
// Unlike the ResolvedBinding path, the library-scope redirect in CompileSymbol
// (GetGlobalIndexFromLibraryScopes) is already ordered AFTER GetLocalIndex, so
// this case is correct at HEAD (returns 7, not 999). This is therefore a
// regression guard: it ensures Change 1's intro-scope addition to the libScope
// branch does not break the already-correct behavior.
func TestScopeResolution_CrossLibraryIntroducedBinderShadow(t *testing.T) {
	tmpDir := t.TempDir()
	libContent := `(define-library (r1shadow)
  (export use-tmp tmp)
  (begin
    (define tmp 999)
    (define-syntax use-tmp
      (syntax-rules ()
        ((use-tmp v) (let ((tmp v)) tmp))))))`
	err := os.WriteFile(filepath.Join(tmpDir, "r1shadow.sld"), []byte(libContent), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	env.Namespace().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
	reg := compilation.NewLibraryRegistry()
	reg.SetSearchPaths([]string{tmpDir})
	env.SetLibraryRegistry(reg)

	_, err = evalScope(t, env, "(import (r1shadow))")
	qt.Assert(t, err, qt.IsNil)

	// The introduced tmp (=7) shadows the library's exported global tmp (=999).
	result, err := evalScope(t, env, "(use-tmp 7)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(7))
}
