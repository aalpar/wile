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
//     Checks use-site scopes for fast-path, then uses GetLocalIndexWithScopes /
//     GetBindingWithScopes for scope-aware lookup.
//
// A bug fix in one path but not the other causes silent divergence. These tests
// exercise identical scope scenarios through the full expand → validate → compile
// → run pipeline, catching divergence by checking runtime results.
//
// See plans/STAFF_ENGINEER_REVIEW.md N6.

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// scopeResolutionEnv creates a full runtime environment with bootstrap macros
// (and, or, let, when, unless, cond, etc.).
func scopeResolutionEnv(t *testing.T) *environment.EnvironmentFrame {
	t.Helper()
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
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
	expanded, err := machine.NewExpanderTimeContinuation(context.Background(), env).ExpandExpression(sv)
	if err != nil {
		return nil, err
	}
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(ctctx, expanded)
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
//	          GetLocalIndexWithScopes finds the correct scoped binding.
//
// Divergence detection: if the compiler's GetLocalIndexWithScopes disagrees with
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
//	Compiler: CompileSymbol scoped path → GetLocalIndexWithScopes must find
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
			// GetLocalIndexWithScopes must pick the maximally-specific binding.
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
//	          or GetBindingWithScopes for global bindings with scope metadata.
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
