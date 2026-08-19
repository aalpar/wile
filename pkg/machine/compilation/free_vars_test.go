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

package compilation

import (
	"context"
	"go/ast"
	"go/token"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// freeVarTestGlobals are bound as ordinary globals so the fixtures can call
// arithmetic and list primitives. They must be GLOBAL, not local: "a global is
// not a free variable" is one of the properties under test, and a global that
// failed to resolve at all would satisfy it vacuously.
var freeVarTestGlobals = []string{"+", "-", "*", "=", "car", "cdr", "cons", "list", "null?"}

// newFreeVarEnv builds the compile environment the free-variable fixtures run
// in: the minimal namespace plus the globals above.
func newFreeVarEnv() *environment.EnvironmentFrame {
	env := newNamespace(environment.NewNamespace().Runtime())
	for _, name := range freeVarTestGlobals {
		env.MaybeCreateOwnGlobalBinding(values.NewSymbol(name), environment.BindingTypeVariable, nil)
	}
	return env
}

// compileToTemplate runs code through the real expander and compiler and returns
// the program template.
//
// The fixtures go through the production path on purpose. Pass 1 is wired into
// compileClosureBody, so driving the collector directly would leave the wiring —
// which frame is passed as `enclosing`, and whether it is taken before or after
// childEnv is built — untested, and getting that wrong is silent.
func compileToTemplate(t *testing.T, code string) *machine.NativeTemplate {
	t.Helper()
	env := newFreeVarEnv()
	prog := parseSchemeExpr(t, env, code)
	ctx := context.Background()
	eval := machine.NewVMMacroEvaluator()
	expanded, err := NewExpanderTimeContinuation(ctx, env, eval).ExpandTopLevelExpression(prog)
	qt.Assert(t, err, qt.IsNil)
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := NewCompileTimeCallContext(ctx, false)
	err = NewCompileTimeContinuation(tpl, env, eval).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)
	return tpl
}

// templatesAtMaxDepth returns every template at the maximum nesting depth in
// tpl's literal pool, in literal-pool order — which for case-lambda clauses is
// clause order, since they are compiled and appended in sequence.
func templatesAtMaxDepth(t *testing.T, tpl *machine.NativeTemplate) []*machine.NativeTemplate {
	t.Helper()
	var best []*machine.NativeTemplate
	bestDepth := 0
	var walk func(cur *machine.NativeTemplate, depth int)
	walk = func(cur *machine.NativeTemplate, depth int) {
		if depth > bestDepth {
			best, bestDepth = nil, depth
		}
		if depth == bestDepth && depth > 0 {
			best = append(best, cur)
		}
		for _, lit := range cur.Literals() {
			sub, ok := lit.(*machine.NativeTemplate)
			if ok {
				walk(sub, depth+1)
			}
		}
	}
	walk(tpl, 0)
	if len(best) == 0 {
		t.Fatalf("fixture compiled to no sub-template at all: there is no lambda to inspect")
	}
	return best
}

// deepestTemplate returns the unique template at maximum nesting depth, and
// fails the test when the maximum is not unique — a fixture with two
// equally-deep lambdas does not have "an innermost lambda", and silently picking
// one would make the assertion depend on literal-pool order.
func deepestTemplate(t *testing.T, tpl *machine.NativeTemplate) *machine.NativeTemplate {
	t.Helper()
	best := templatesAtMaxDepth(t, tpl)
	if len(best) != 1 {
		t.Fatalf("fixture has %d equally deep templates — no unique innermost lambda", len(best))
	}
	return best[0]
}

// freeNameKeys projects a template's recorded free-variable layout to spellings,
// in slot order.
func freeNameKeys(tpl *machine.NativeTemplate) []string {
	names := tpl.FreeNames()
	if len(names) == 0 {
		return nil
	}
	q := make([]string, len(names))
	for i, n := range names {
		q[i] = n.Key
	}
	return q
}

// innermostLambdaFreeVars returns the free-variable spellings of the innermost
// lambda in code, in free-vector slot order.
//
// Spelling is used for readability. The implementation keys on the resolved
// (slot, depth), and the two macro fixtures below are what prove it: each has
// two `tmp` binders from different expansions, which a spelling-keyed
// implementation collapses.
func innermostLambdaFreeVars(t *testing.T, code string) []string {
	t.Helper()
	return freeNameKeys(deepestTemplate(t, compileToTemplate(t, code)))
}

// TestCollectFreeVars pins Pass 1's membership rule: a reference is free iff no
// binder introduced INSIDE the lambda covers it and it resolves to a local
// OUTSIDE it. No boundary count is consulted, which is what makes the
// immediately-applied-lambda and named-let shapes need no special case.
func TestCollectFreeVars(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want []string
	}{
		{
			name: "no free variables",
			code: `(lambda (a) (lambda (b) b))`,
			want: nil,
		},
		{
			name: "one free variable from the enclosing lambda",
			code: `(lambda (a) (lambda (b) (+ a b)))`,
			want: []string{"a"},
		},
		{
			name: "transitive through two boundaries",
			code: `(lambda (a) (lambda (b) (lambda (c) (+ a c))))`,
			want: []string{"a"},
		},
		{
			name: "globals are not free variables",
			code: `(lambda (a) (lambda (b) (cons car b)))`,
			want: nil,
		},
		{
			name: "shadowed by the inner lambda's own parameter",
			code: `(lambda (a) (lambda (a) a))`,
			want: nil,
		},
		{
			name: "shadowed by a let inside the inner lambda",
			code: `(lambda (a) (lambda (b) (let ((a 1)) a)))`,
			want: nil,
		},
		{
			// The over-approximating direction of walkLet: the init is walked
			// with the let's own binders NOT pushed, so it resolves outward.
			// Masking it would DROP a genuinely free `a`.
			name: "let init is outside the let's own binders",
			code: `(lambda (a) (lambda (b) (let ((a a)) a)))`,
			want: []string{"a"},
		},
		{
			// Wile does NOT lower ((lambda (x) e) v) to a let, so this really is
			// a nested lambda. WalkBindingRefs reports it at depth 0, so a
			// depth-based predicate omitted `a` here.
			name: "immediately-applied lambda still captures",
			code: `(lambda (a) ((lambda (x) (+ x a)) 1))`,
			want: []string{"a"},
		},
		{
			// bodyCreatesEscapingClosure exempts this shape, and that exemption
			// is wrong for free-variable membership.
			//
			// `loop` is in the answer, and that is not an artifact: the loop
			// lambda genuinely closes over its own letrec binding, which is
			// design §5.3.1's T2 tier. Source order puts `a` first.
			name: "named let loop lambda captures the enclosing parameter",
			code: `(lambda (a) (let loop ((i 0)) (if (= i 3) a (loop (+ i 1)))))`,
			want: []string{"a", "loop"},
		},
		{
			name: "set! target counts as a reference",
			code: `(lambda (a) (lambda (b) (set! a b)))`,
			want: []string{"a"},
		},
		{
			name: "internal define shadows an enclosing binding",
			code: `(lambda (a) (lambda (b) (define a 1) (+ a b)))`,
			want: nil,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := innermostLambdaFreeVars(t, tc.code)
			c.Assert(got, qt.DeepEquals, tc.want)
		})
	}
}

// TestCollectFreeVarsPerCaseLambdaClause pins that EVERY clause of a
// case-lambda gets its own layout, computed with that clause's own parameters
// pushed.
//
// It needs its own test because a case-lambda's clauses are siblings at the same
// nesting depth, so "the innermost lambda" is not defined for it — which is
// precisely the structure a per-clause walk has to get right.
func TestCollectFreeVarsPerCaseLambdaClause(t *testing.T) {
	c := qt.New(t)
	// Clause 1 captures `a` and shadows nothing. Clause 2 shadows `a` with its
	// own parameter, so it must capture only `b`.
	code := `(lambda (a b) (case-lambda ((x) (+ x a)) ((a y) (+ a y b))))`
	tpls := templatesAtMaxDepth(t, compileToTemplate(t, code))
	got := make([][]string, len(tpls))
	for i, tpl := range tpls {
		got[i] = freeNameKeys(tpl)
	}
	c.Assert(got, qt.DeepEquals, [][]string{{"a"}, {"b"}},
		qt.Commentf("clause layouts are computed per clause, in clause order"))
}

// TestCollectFreeVarsDistinguishesSameSpelledBinders is the binding-identity
// ratchet. Both fixtures put two `tmp` binders from different expansions in play
// at once; a Sym.Key-keyed implementation collapses them and still passes every
// membership fixture above, because merging two variables that happen to hold
// compatible values still evaluates correctly.
//
// The two cases hit the two independent places identity is decided:
// `seen` (which slot a reference lands in) and `boundInside` (whether a binder
// covers a reference at all).
func TestCollectFreeVarsDistinguishesSameSpelledBinders(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want []string
	}{
		{
			// The `seen` map. After expansion the innermost lambda is free in
			// BOTH tmps — the macro's, bound by the template's own let, and the
			// user's, substituted in through the pattern variable. Keying on the
			// spelling merges them into one slot.
			name: "two same-spelled binders both free need two slots",
			code: `(begin
			         (define-syntax two
			           (syntax-rules ()
			             ((_ body) (let ((tmp 1)) (lambda () (+ tmp body))))))
			         (lambda (x) (let ((tmp 10)) (two tmp))))`,
			want: []string{"tmp", "tmp"},
		},
		{
			// boundInside. The macro introduces a lambda whose PARAMETER is
			// spelled `tmp`; the body substituted into it references the user's
			// `tmp`. A spelling-only "bound inside" test reports the reference as
			// shadowed and drops it, yielding an empty free set.
			name: "a same-spelled macro binder does not shadow the user reference",
			code: `(begin
			         (define-syntax shadow
			           (syntax-rules ()
			             ((_ body) (lambda (tmp) body))))
			         (lambda (x) (let ((tmp 5)) (shadow tmp))))`,
			want: []string{"tmp"},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := innermostLambdaFreeVars(t, tc.code)
			c.Assert(got, qt.DeepEquals, tc.want)
		})
	}
}

// binderCarryingForms are the ValidatedExpr implementations whose fields include
// a *syntax.SyntaxSymbol binder, i.e. the forms whose walk MUST push binders.
// freeVarCollector.walk has to type-switch on every one of them.
//
// ValidatedSetBang is deliberately absent: its Name field is a REFERENCE to an
// existing binding, not a binder, and walk handles it for that reason rather
// than this one.
var binderCarryingForms = map[string]bool{
	"ValidatedLambda":           true,
	"ValidatedCaseLambda":       true,
	"ValidatedCaseLambdaClause": true,
	"ValidatedLet":              true,
	"ValidatedDefine":           true,
}

// TestCollectFreeVarsCoversEveryBinder fails when pkg/internal/validate grows a
// binding form that walk() does not know about.
//
// A missed binder is an UNDER-approximation of "bound inside", which produces a
// WRONG free set — a variable that should have been shadowed is captured
// instead — and no value assertion in the suite would notice, because the
// captured value is usually the same one.
//
// The check is structural, in the shape compile_symbol_reachability_test.go
// already uses: parse validated_forms.go with go/ast, find every struct type
// carrying a binder, and assert free_vars.go type-asserts on each. The
// binderCarryingForms map above is the expected answer, and the test fails in
// BOTH directions — an unexpected new binder form, or a form that stopped
// carrying one — so the map cannot quietly drift out of date either.
func TestCollectFreeVarsCoversEveryBinder(t *testing.T) {
	c := qt.New(t)
	fset := token.NewFileSet()
	files := parsePackageDir(t, fset, "../../internal/validate")

	structs := validatedFormStructs(files)
	// Vacuity guard: a parse that discovered nothing would pass everything below.
	c.Assert(len(structs) > 0, qt.IsTrue,
		qt.Commentf("no validated form was discovered — the go/ast scan is "+
			"broken, not the collector"))

	// Round 1: forms that carry a binder directly.
	found := map[string]bool{}
	for name, st := range structs {
		if structHasBinderField(st) {
			found[name] = true
		}
	}
	// Round 2: forms that carry a round-1 form. ValidatedCaseLambda is the case
	// — it holds []*ValidatedCaseLambdaClause and no binder of its own, yet
	// walking it is exactly how the clause binders get pushed. One round
	// suffices for today's tree; a deeper nesting would need a fixpoint, and the
	// count assertion below is what would say so.
	for name, st := range structs {
		if found[name] {
			continue
		}
		for inner := range found {
			if structMentions(st, inner) {
				found[name] = true
				break
			}
		}
	}

	// Two forms are discovered structurally and excluded by name, with the reason
	// stated, rather than by weakening the field test:
	//   - ValidatedSetBang's Name is a REFERENCE to an existing binding. walk
	//     handles it, but as a use, not as a binder.
	//   - ValidatedSymbol IS a reference.
	delete(found, "ValidatedSetBang")
	delete(found, "ValidatedSymbol")

	c.Assert(found, qt.DeepEquals, binderCarryingForms,
		qt.Commentf("the set of binder-carrying validated forms changed. Every "+
			"one of them must be an arm of freeVarCollector.walk (directly, or "+
			"through walkProc for a ValidatedBodyAndParams), or the free set is "+
			"wrong in the silent direction. Update walk FIRST, then this map."))

	walkArms := typeSwitchTargets(t, "walk")
	for name := range binderCarryingForms {
		if name == "ValidatedCaseLambdaClause" {
			// Reached through ValidatedCaseLambda.Clauses(), not by its own type
			// assertion — a clause is never a direct child of anything else.
			continue
		}
		c.Assert(walkArms[name], qt.IsTrue,
			qt.Commentf("freeVarCollector.walk has no arm for validate.%s", name))
	}
}

// validatedFormStructs returns the struct types in files that are validated
// FORMS, keyed by name. Membership is "embeds validatedBase" — the marker every
// ValidatedExpr implementation carries, and the one that keeps helper structs
// (ValidatedParams, ValidatedLetBinding, rawLetBinding, validatedProcBase) out
// of the answer without listing them.
func validatedFormStructs(files []*ast.File) map[string]*ast.StructType {
	q := map[string]*ast.StructType{}
	for _, f := range files {
		ast.Inspect(f, func(n ast.Node) bool {
			ts, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}
			st, ok := ts.Type.(*ast.StructType)
			if !ok {
				return true
			}
			if structEmbeds(st, "validatedBase") {
				q[ts.Name.Name] = st
			}
			return true
		})
	}
	return q
}

// structEmbeds reports whether st has an anonymous field named name.
func structEmbeds(st *ast.StructType, name string) bool {
	for _, f := range st.Fields.List {
		if len(f.Names) > 0 {
			continue
		}
		id, ok := f.Type.(*ast.Ident)
		if ok && id.Name == name {
			return true
		}
	}
	return false
}

// structHasBinderField reports whether st declares a field that carries a
// binder: a *syntax.SyntaxSymbol directly, or one of the three helper structs
// that hold them.
func structHasBinderField(st *ast.StructType) bool {
	for _, name := range []string{"SyntaxSymbol", "ValidatedParams", "ValidatedLetBinding", "validatedProcBase"} {
		if structMentions(st, name) {
			return true
		}
	}
	return false
}

// structMentions reports whether any field type of st names the given type,
// looking through pointers, slices, and package qualifiers.
func structMentions(st *ast.StructType, name string) bool {
	for _, f := range st.Fields.List {
		if exprNamesType(f.Type, name) {
			return true
		}
	}
	return false
}

// exprNamesType reports whether a type expression mentions name, looking through
// pointers, slices, and package qualifiers.
func exprNamesType(e ast.Expr, name string) bool {
	found := false
	ast.Inspect(e, func(n ast.Node) bool {
		id, ok := n.(*ast.Ident)
		if ok && id.Name == name {
			found = true
		}
		return !found
	})
	return found
}

// typeSwitchTargets returns the set of validate.X type names that fn asserts on
// with a comma-ok type assertion.
func typeSwitchTargets(t *testing.T, fn string) map[string]bool {
	t.Helper()
	files := parsePackageDir(t, token.NewFileSet(), ".")
	q := map[string]bool{}
	for _, f := range files {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok || fd.Name.Name != fn || fd.Body == nil {
				continue
			}
			ast.Inspect(fd.Body, func(n ast.Node) bool {
				ta, ok := n.(*ast.TypeAssertExpr)
				if !ok || ta.Type == nil {
					return true
				}
				star, ok := ta.Type.(*ast.StarExpr)
				if !ok {
					return true
				}
				sel, ok := star.X.(*ast.SelectorExpr)
				if ok {
					q[sel.Sel.Name] = true
				}
				return true
			})
		}
	}
	return q
}
