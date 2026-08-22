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

package validate

import (
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
)

// letBinds builds a ValidatedLet of the given kind over name/init pairs.
func letBinds(kind LetKind, names []string, inits []ValidatedExpr, body ...ValidatedExpr) *ValidatedLet {
	bindings := make([]ValidatedLetBinding, 0, len(names))
	for i, n := range names {
		bindings = append(bindings, ValidatedLetBinding{
			Name: syntax.NewSyntaxSymbol(n, nil),
			Init: inits[i],
		})
	}
	return &ValidatedLet{
		formName: kind.String(),
		Kind:     kind,
		Bindings: bindings,
		body:     body,
	}
}

// namedLet builds the shape validateNamedLet produces: a letrec whose single
// binding is the loop lambda and whose body is the priming call. The recursive
// calls therefore sit in the binding's INIT, not in the let body.
func namedLet(tag string, ps *ValidatedParams, loopBody []ValidatedExpr, prime ...ValidatedExpr) *ValidatedLet {
	lambda := &ValidatedLambda{
		formName: "lambda",
		params:   ps, body: loopBody,
	}
	v := letBinds(LetKindLetrec, []string{tag}, []ValidatedExpr{lambda},
		call(symRef(tag)))
	if len(prime) > 0 {
		v.body = prime
	}
	v.Tag = syntax.NewSyntaxSymbol(tag, nil)
	return v
}

// TestALocalProvesLocallyBoundOperators covers the A-local rule: a call whose
// operator is lexically bound is proven from its binder instead of refused.
//
// Before A-local every one of the positive cases below returned false, because
// `bound.has(name)` was itself the refusal. Since a named let validates to a
// letrec whose single binding is the loop lambda, that refusal covered every
// loop-shaped procedure in the language.
//
// `map` is the un-provable callee throughout: it is imported but NOT stamped
// CaptureSafe (captureSafeTestPrims), so it models a callee that may invoke an
// arbitrary Scheme procedure. It is deliberately not a capture OPERATOR — a
// call/cc anywhere in the body is caught by bodyCannotCaptureCaller's whole-tree
// scan before the callee walk runs, which would make these cases pass for the
// wrong reason.
func TestALocalProvesLocallyBoundOperators(t *testing.T) {
	env := envWithImported(t, "<=", "-", "car", "map", "f", "xs")

	// (define (g n) (let loop ((i n)) (if (<= i 0) i (loop (- i 1)))))
	loopOK := fnWith("g", params("n"),
		namedLet("loop", params("i"),
			[]ValidatedExpr{ifx(call(symRef("<="), symRef("i"), lit()),
				symRef("i"),
				call(symRef("loop"), call(symRef("-"), symRef("i"), lit())))}))
	if !ProcedureBodyIsCaptureSafe(loopOK, selfIn(t, env, "g"), env) {
		t.Error("a named let over capture-safe primitives must be provable")
	}

	// The same loop calling map: the local's body decides, so the whole procedure
	// is refused.
	loopBad := fnWith("g", params("n"),
		namedLet("loop", params("i"),
			[]ValidatedExpr{call(symRef("map"), symRef("f"), symRef("xs"))}))
	if ProcedureBodyIsCaptureSafe(loopBad, selfIn(t, env, "g"), env) {
		t.Error("a named let whose body calls a procedure-invoking callee must be refused")
	}

	// Internal define, function form: (define (g n) (define (h i) (car i)) (h n))
	innerDefine := fnWith("g", params("n"),
		defineFn("h", call(symRef("car"), symRef("i"))),
		call(symRef("h"), symRef("n")))
	if !ProcedureBodyIsCaptureSafe(innerDefine, selfIn(t, env, "g"), env) {
		t.Error("a function-form internal define must be provable from its own body")
	}

	// A local bound to something the walk cannot see through stays refused.
	computedInit := fnWith("g", params("n"),
		letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{call(symRef("car"), symRef("n"))},
			call(symRef("h"))))
	if ProcedureBodyIsCaptureSafe(computedInit, selfIn(t, env, "g"), env) {
		t.Error("a local bound to a computed init is not provable and must be refused")
	}
}

// TestALocalRefusesMutatedLocal pins the second disqualifier: a name set! within
// its own binding form no longer denotes its initializer, so the initializer is
// not evidence about it.
func TestALocalRefusesMutatedLocal(t *testing.T) {
	env := envWithImported(t, "car", "map", "f", "xs")
	safeInit := func() ValidatedExpr {
		return lamOf([]string{"i"}, call(symRef("car"), symRef("i")))
	}

	// Control: the same local, never set!, is provable.
	clean := fnWith("g", params("n"),
		letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{safeInit()},
			call(symRef("h"), symRef("n"))))
	if !ProcedureBodyIsCaptureSafe(clean, selfIn(t, env, "g"), env) {
		t.Fatal("control: an un-mutated local with a capture-safe body must be provable")
	}

	// (let ((h (lambda (i) (car i)))) (set! h map) (h n))
	mutated := fnWith("g", params("n"),
		letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{safeInit()},
			setBang("h", symRef("map")),
			call(symRef("h"), symRef("n"))))
	if ProcedureBodyIsCaptureSafe(mutated, selfIn(t, env, "g"), env) {
		t.Error("a local set! after binding must be refused — its init no longer describes it")
	}
}

// TestALocalLetInitPositionDoesNotSeeOwnBinding pins the init/body scope split.
// A let's own name inside its own init denotes the OUTER binding, so the
// initializer is not evidence about it — only the letrec family may read its own
// bindings from an init position. Collapsing the two scopes accepts a call to
// whatever the enclosing scope bound the name to, which for a parameter is
// whatever the caller passed.
func TestALocalLetInitPositionDoesNotSeeOwnBinding(t *testing.T) {
	env := envWithImported(t, "car")
	safeInit := func() ValidatedExpr {
		return lamOf(nil, call(symRef("car"), lit()))
	}

	// (define (g a) (let ((a <safe>) (b (lambda () (a)))) (b)))
	// The (a) inside b's init is the PARAMETER a, not the sibling binding.
	plain := fnWith("g", params("a"),
		letBinds(LetKindLet, []string{"a", "b"}, []ValidatedExpr{
			safeInit(),
			lamOf(nil, call(symRef("a"))),
		}, call(symRef("b"))))
	if ProcedureBodyIsCaptureSafe(plain, selfIn(t, env, "g"), env) {
		t.Error("a plain let's own name in an init position is the OUTER binding and must refuse")
	}

	// The same shape as a letrec, where the init genuinely does see the binding.
	rec := fnWith("g", params("a"),
		letBinds(LetKindLetrec, []string{"a", "b"}, []ValidatedExpr{
			safeInit(),
			lamOf(nil, call(symRef("a"))),
		}, call(symRef("b"))))
	if !ProcedureBodyIsCaptureSafe(rec, selfIn(t, env, "g"), env) {
		t.Error("a letrec init does see its own bindings and must be provable")
	}
}

// TestALocalCoInductionDischargesCycles covers the recursive shapes: a local
// that calls itself, a local pair that call each other, and a local that calls
// back out to the define it lives in. None of them needs a proof stack — the
// enclosing walk visits each body exactly once — but all three were refused
// before A-local, and a future recursive formulation must keep them.
func TestALocalCoInductionDischargesCycles(t *testing.T) {
	env := envWithImported(t, "<=", "-", "car", "map", "f", "xs")

	// Local mutual recursion, both clean:
	//   (letrec ((a (lambda (n) (b n)))
	//            (b (lambda (n) (if (<= n 0) n (a (- n 1))))))
	//     (a n))
	mutual := fnWith("g", params("n"),
		letBinds(LetKindLetrec, []string{"a", "b"}, []ValidatedExpr{
			lamOf([]string{"n"}, call(symRef("b"), symRef("n"))),
			lamOf([]string{"n"}, ifx(call(symRef("<="), symRef("n"), lit()),
				symRef("n"),
				call(symRef("a"), call(symRef("-"), symRef("n"), lit())))),
		}, call(symRef("a"), symRef("n"))))
	if !ProcedureBodyIsCaptureSafe(mutual, selfIn(t, env, "g"), env) {
		t.Error("clean local mutual recursion must be provable — the cycle is discharged")
	}

	// The same pair with one member calling map: refusal must propagate OUT of
	// the cycle rather than being swallowed by the assumption.
	mutualBad := fnWith("g", params("n"),
		letBinds(LetKindLetrec, []string{"a", "b"}, []ValidatedExpr{
			lamOf([]string{"n"}, call(symRef("b"), symRef("n"))),
			lamOf([]string{"n"}, call(symRef("map"), symRef("f"), symRef("xs"))),
		}, call(symRef("a"), symRef("n"))))
	if ProcedureBodyIsCaptureSafe(mutualBad, selfIn(t, env, "g"), env) {
		t.Error("local mutual recursion where one member is unprovable must be refused")
	}

	// The enclosing define, called from inside a local. selfName must still be in
	// force inside the local's body: mid-compile `g` is not yet stamped, so a
	// binding lookup would read IsCaptureSafe()==false and refuse.
	//   (define (g n) (let loop ((i n)) (if (<= i 0) (g i) (loop (- i 1)))))
	callsEnclosing := fnWith("g", params("n"),
		namedLet("loop", params("i"),
			[]ValidatedExpr{ifx(call(symRef("<="), symRef("i"), lit()),
				call(symRef("g"), symRef("i")),
				call(symRef("loop"), call(symRef("-"), symRef("i"), lit())))}))
	if !ProcedureBodyIsCaptureSafe(callsEnclosing, selfIn(t, env, "g"), env) {
		t.Error("a local calling back into the define being proven must be discharged, not refused")
	}
}

// TestALocalNestedSameNameLocalIsStillInspected covers two locals in nested
// scopes sharing a name. Each body must be decided on its own merits; anything
// that treats the name as already-answered — a name-keyed proof stack, a
// name-keyed memo — would wave the inner one through, a false positive in the
// direction the kill criterion forbids.
func TestALocalNestedSameNameLocalIsStillInspected(t *testing.T) {
	env := envWithImported(t, "car", "map", "f", "xs")

	// (define (g n)
	//   (let ((h (lambda () (let ((h (lambda () (map f xs)))) (h)))))
	//     (h)))
	// The INNER h is unprovable, and the outer h shares its name.
	shadowed := fnWith("g", params("n"),
		letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{
			lamOf(nil,
				letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{
					lamOf(nil, call(symRef("map"), symRef("f"), symRef("xs"))),
				}, call(symRef("h")))),
		}, call(symRef("h"))))
	if ProcedureBodyIsCaptureSafe(shadowed, selfIn(t, env, "g"), env) {
		t.Error("an inner same-named local must be decided on its own merits, not as the outer one")
	}
}

// TestALocalNearerBinderDoesNotMask covers a local that calls a name which a
// NEARER binder also binds, harmlessly. The unprovable body is the one in scope
// where the local was written, and it must still decide the verdict: nothing may
// substitute the nearer binding for it.
func TestALocalNearerBinderDoesNotMask(t *testing.T) {
	env := envWithImported(t, "car", "map", "f", "xs")

	// (define (g)
	//   (letrec ((h    (lambda () (map f xs)))       ; the h `loop' really calls
	//            (loop (lambda () (h))))
	//     (let ((h (lambda () (car xs))))            ; a nearer, harmless h
	//       (loop))))
	nearerIsSafe := fnWith("g", params(),
		letBinds(LetKindLetrec, []string{"h", "loop"}, []ValidatedExpr{
			lamOf(nil, call(symRef("map"), symRef("f"), symRef("xs"))),
			lamOf(nil, call(symRef("h"))),
		}, letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{
			lamOf(nil, call(symRef("car"), symRef("xs"))),
		}, call(symRef("loop")))))
	if ProcedureBodyIsCaptureSafe(nearerIsSafe, selfIn(t, env, "g"), env) {
		t.Error("a nearer harmless binder must not mask the unprovable body the local actually calls")
	}

	// Mirror image: the binder's h is harmless and the nearer one is not. The
	// local never calls the nearer h, so it stays provable — this is the control
	// that shows the case above fails on the scope and not on the shape.
	nearerIsUnsafe := fnWith("g", params(),
		letBinds(LetKindLetrec, []string{"h", "loop"}, []ValidatedExpr{
			lamOf(nil, call(symRef("car"), symRef("xs"))),
			lamOf(nil, call(symRef("h"))),
		}, letBinds(LetKindLet, []string{"h"}, []ValidatedExpr{
			lamOf(nil, call(symRef("car"), symRef("xs"))),
		}, call(symRef("loop")))))
	if !ProcedureBodyIsCaptureSafe(nearerIsUnsafe, selfIn(t, env, "g"), env) {
		t.Error("control: a local whose binder-scope callees are all provable must stay provable")
	}
}
