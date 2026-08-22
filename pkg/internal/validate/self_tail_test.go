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

// params builds a non-variadic parameter list from the given required names.
func params(names ...string) *ValidatedParams {
	p := &ValidatedParams{}
	for _, n := range names {
		p.Required = append(p.Required, syntax.NewSyntaxSymbol(n, nil))
	}
	return p
}

// variadicParams builds a parameter list with a rest parameter.
func variadicParams(req []string, rest string) *ValidatedParams {
	p := params(req...)
	p.Rest = syntax.NewSyntaxSymbol(rest, nil)
	return p
}

// ifx builds a ValidatedIf.
func ifx(test, conseq, alt ValidatedExpr) *ValidatedIf {
	return &ValidatedIf{
		formName: "if",
		Test:     test,
		Conseq:   conseq,
		Alt:      alt,
	}
}

// fnWith builds a function-form ValidatedDefine with explicit params.
func fnWith(name string, ps *ValidatedParams, body ...ValidatedExpr) *ValidatedDefine {
	d := defineFn(name, body...)
	d.params = ps
	return d
}

// TestBodyIsSelfTailReusable is the correctness core for OpSelfTailCall: it pins
// which closures may have their activation frame reused in place at a self-tail
// call. The predicate is the safety gate; any false positive here is silent
// corruption (see the escape-gated plan's kill criterion), so the negative cases
// are adversarial.
//
// Depth is no longer among the conditions (frame-reclaim Phase C): the op pops the
// intermediate let frames before rebinding, so this predicate answers only the
// in-body safety question. Two rows that used to refuse partly on depth now refuse
// on their own stated reason alone, and say so.
func TestBodyIsSelfTailReusable(t *testing.T) {
	letBind := func(name string) ValidatedLetBinding {
		return ValidatedLetBinding{Name: syntax.NewSyntaxSymbol(name, nil), Init: lit()}
	}

	tests := []struct {
		name string
		proc *ValidatedDefine
		self string
		want bool
	}{
		{
			name: "positive: named-let counter (self tail, depth 0)",
			proc: fnWith("loop", params("i", "n"),
				ifx(call(symRef(">="), symRef("i"), symRef("n")),
					symRef("i"),
					call(symRef("loop"), call(symRef("+"), symRef("i"), lit()), symRef("n")))),
			self: "loop", want: true,
		},
		{
			name: "positive: tail accumulator",
			proc: fnWith("sum", params("i", "acc"),
				ifx(call(symRef("="), symRef("i"), lit()),
					symRef("acc"),
					call(symRef("sum"), call(symRef("-"), symRef("i"), lit()),
						call(symRef("+"), symRef("acc"), symRef("i"))))),
			self: "sum", want: true,
		},
		{
			name: "positive: mixed — non-self tail call in the base branch is fine",
			proc: fnWith("f", params("x"),
				ifx(call(symRef("done?"), symRef("x")),
					call(symRef("other"), symRef("x")),
					call(symRef("f"), call(symRef("step"), symRef("x"))))),
			self: "f", want: true,
		},
		{
			name: "positive: self-tail call inside a begin",
			proc: fnWith("b", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					&ValidatedBegin{
						formName: "begin",
						body: []ValidatedExpr{
							call(symRef("effect"), symRef("i")),
							call(symRef("b"), call(symRef("+"), symRef("i"), lit())),
						},
					})),
			self: "b", want: true,
		},
		{
			name: "negative: call/cc anywhere in body",
			proc: fnWith("bad", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					call(symRef("bad"), call(symRef("call/cc"), symRef("k"))))),
			self: "bad", want: false,
		},
		{
			// Was a negative until phase 8 of the flat-closure arc. A LINKED
			// closure held its creating frame as its static link, so a lambda
			// handed to `save` could outlive the call while still reaching the
			// parameter frame. A flat closure's link is a structural root and its
			// free variables are copied at OpMakeClosure, so it never reaches that
			// frame — the clause that refused this had no remaining reason.
			//
			// This case being POSITIVE is the phase-8 assertion at the unit level;
			// pkg/wile's armedSelfTailSitesBaseline is the same claim end-to-end,
			// and pkg/wile/self_tail_box_test.go pins the per-iteration values a
			// captured loop parameter now has to produce.
			name: "positive: escaping lambda in body (flat closures do not pin the frame)",
			proc: fnWith("esc", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					call(symRef("esc"), call(symRef("save"), lam(symRef("i")))))),
			self: "esc", want: true,
		},
		{
			name: "negative: variadic params (no flat slot rebind)",
			proc: fnWith("v", variadicParams([]string{"i"}, "rest"),
				call(symRef("v"), symRef("i"))),
			self: "v", want: false,
		},
		{
			// Frame-reclaim Phase C. This was a NEGATIVE until 2026-08-14: v1's
			// OpSelfTailCall could rebind the parameter frame only while it was
			// mc.env, so a call under a let frame was refused here. The op now pops
			// the intermediate frames first, so the depth gate is gone and this walk
			// answers the smaller question — is there a self call in tail position.
			// The POP COUNT is codegen's (frameReuse.letDepth), not this walk's.
			name: "positive: self-tail call nested in a let (depth > 0, Phase C)",
			proc: fnWith("g", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					nestedLet([]ValidatedLetBinding{letBind("x")},
						call(symRef("g"), call(symRef("+"), symRef("i"), symRef("x")))))),
			self: "g", want: true,
		},

		{
			name: "negative: only a non-tail self-call (nothing reusable in tail)",
			proc: fnWith("nt", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					lit(),
					call(symRef("+"), call(symRef("nt"), call(symRef("-"), symRef("i"), lit())), lit()))),
			self: "nt", want: false,
		},
		{
			name: "negative: self appears only as a value, not an operator",
			proc: fnWith("a", params("xs"),
				call(symRef("map"), symRef("a"), symRef("xs"))),
			self: "a", want: false,
		},
		{
			name: "negative: arity-mismatch self-call is not rewritable",
			proc: fnWith("m", params("i", "n"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					call(symRef("m"), symRef("i")))),
			self: "m", want: false,
		},
		{
			name: "negative: no self-call at all (leaf)",
			proc: fnWith("leaf", params("x"),
				call(symRef("other"), symRef("x"))),
			self: "leaf", want: false,
		},
		{
			// This row refused for TWO reasons until Phase C removed the depth gate:
			// the call was at depth 1 AND the let binder shadows the self name. Only
			// the shadow reason is left, which is the one it was written for — so it
			// now discriminates rather than passing on the coarser condition.
			name: "negative: self-tail call shadowed by an inner let binding",
			proc: fnWith("s", params("i"),
				nestedLet([]ValidatedLetBinding{letBind("s")},
					call(symRef("s"), symRef("i")))),
			self: "s", want: false,
		},
		{
			// set! on the self name means a subsequent self-call must dispatch to
			// the new value — OpSelfTailCall's hardcoded jump-to-0 would be wrong.
			name: "negative: self name is set! in the body (binding is mutable)",
			proc: fnWith("loop", params("i"),
				&ValidatedBegin{
					formName: "begin",
					body: []ValidatedExpr{
						setBang("loop", symRef("other")),
						ifx(call(symRef("done?"), symRef("i")),
							symRef("i"),
							call(symRef("loop"), call(symRef("+"), symRef("i"), lit()))),
					},
				}),
			self: "loop", want: false,
		},
		{
			// A parameter named the same as the function shadows the self name in
			// the body, so (loop x) calls the PARAMETER, not the function — there is
			// no real self-tail call to rewrite.
			name: "negative: a parameter shadows the self name",
			proc: fnWith("loop", params("loop"),
				call(symRef("loop"), symRef("x"))),
			self: "loop", want: false,
		},
		{
			// A set! to a shadowing let binding of the same name does NOT mutate the
			// enclosing self, so reuse stays sound (precision: shadow-aware).
			name: "positive: set! targets a shadowing let binding, not self",
			proc: fnWith("s", params("i"),
				&ValidatedBegin{
					formName: "begin",
					body: []ValidatedExpr{
						nestedLet([]ValidatedLetBinding{letBind("s")}, setBang("s", lit())),
						ifx(call(symRef("done?"), symRef("i")),
							symRef("i"),
							call(symRef("s"), call(symRef("+"), symRef("i"), lit()))),
					},
				}),
			self: "s", want: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := bodyIsSelfTailReusable(tc.proc, tc.self, callccStub)
			if got != tc.want {
				t.Errorf("bodyIsSelfTailReusable = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestBodyIsFrameReleasable covers the fib-shaped release predicate: a body whose
// frame can be released at its tail calls because it never exposes the frame to a
// continuation (no capture, no escape, only self + capture-safe primitive callees).
// No self-tail call is required (fib's tail call is the foreign +).
func TestBodyIsFrameReleasable(t *testing.T) {
	// (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
	fib := fnWith("fib", params("n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("+"),
				call(symRef("fib"), call(symRef("-"), symRef("n"), lit())),
				call(symRef("fib"), call(symRef("-"), symRef("n"), lit())))))

	// Positive: <= + - imported (capture-safe ∧ Stable); fib is self.
	fibEnv := envWithImported(t, "<=", "+", "-")
	if !BodyIsFrameReleasable(fib, selfIn(t, fibEnv, "fib"), fibEnv) {
		t.Errorf("fib over capture-safe primitives + self must be frame-releasable")
	}

	// Negative: a tail call to another user function (mutual recursion) — refused
	// conservatively (the callee could capture).
	mutual := fnWith("ping", params("n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("pong"), call(symRef("-"), symRef("n"), lit()))))
	mutualEnv := envWithImported(t, "<=", "-", "pong")
	if BodyIsFrameReleasable(mutual, selfIn(t, mutualEnv, "ping"), mutualEnv) {
		t.Errorf("a tail call to another user function must not be frame-releasable (v1)")
	}

	// Negative: call/cc in the body pins the frame.
	captures := fnWith("c", params("n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("call/cc"), symRef("k"))))
	capturesEnv := envWithImported(t, "<=", "call/cc", "k")
	if BodyIsFrameReleasable(captures, selfIn(t, capturesEnv, "c"), capturesEnv) {
		t.Errorf("a body that calls call/cc must not be frame-releasable")
	}
}

// TestProcedureBodyIsCaptureSafe directly pins the predicate the prove-them path
// (compile_define.go) consults to stamp a define's binding CaptureSafe. The headline
// case is its deliberate DIFFERENCE from BodyIsFrameReleasable: an escaping-closure
// body is NOT frame-releasable (its own frame is pinned) yet IS capture-safe-as-callee
// (calling it cannot capture the caller's continuation). The negatives are the
// soundness boundary of the generalization — a capture operator or a procedure-invoking
// callee must leave a body un-trusted.
func TestProcedureBodyIsCaptureSafe(t *testing.T) {
	// Positive: body calls only capture-safe primitives + self.
	safe := fnWith("p", params("n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("p"), call(symRef("-"), symRef("n"), lit()))))
	safeEnv := envWithImported(t, "<=", "-")
	if !ProcedureBodyIsCaptureSafe(safe, selfIn(t, safeEnv, "p"), safeEnv) {
		t.Errorf("body over capture-safe primitives + self must be capture-safe-as-callee")
	}

	// KEY: an escaping-closure body is capture-safe-as-callee (escape is irrelevant to
	// callers) but NOT frame-releasable — the exact distinction the two predicates encode.
	escapes := fnWith("mk", params("n"), lam(lit()))
	env := envWithImported(t)
	if !ProcedureBodyIsCaptureSafe(escapes, selfIn(t, env, "mk"), env) {
		t.Errorf("a body that only returns a closure is capture-safe-as-callee (escape does not capture the caller)")
	}
	if BodyIsFrameReleasable(escapes, selfIn(t, env, "mk"), env) {
		t.Errorf("guard: the same escaping body must NOT be frame-releasable (its own frame is pinned)")
	}

	// Negative: a capture operator in the body.
	captures2 := fnWith("c", params("n"), call(symRef("call/cc"), symRef("k")))
	captures2Env := envWithImported(t, "call/cc", "k")
	if ProcedureBodyIsCaptureSafe(captures2, selfIn(t, captures2Env, "c"), captures2Env) {
		t.Errorf("a body that calls call/cc must NOT be capture-safe-as-callee")
	}

	// Negative: a procedure-invoking callee — map applies a user proc that may capture.
	usesMap := fnWith("u", params("f", "xs"), call(symRef("map"), symRef("f"), symRef("xs")))
	usesMapEnv := envWithImported(t, "map", "f", "xs")
	if ProcedureBodyIsCaptureSafe(usesMap, selfIn(t, usesMapEnv, "u"), usesMapEnv) {
		t.Errorf("a body calling a procedure-invoking primitive (map) must NOT be capture-safe-as-callee")
	}
}

// TestCalleeShadowedByOwnParameter pins the shadow-set seed in
// bodyCalleesAllCaptureSafe: the analysed procedure's OWN parameters must shadow a
// same-named global callee.
//
// The walk resolves every call operator through env.GetBinding against a FLAT env
// with no local frames, so the nameSet is what stands in for local resolution.
// Unseeded, a parameter sharing a capture-safe global's name resolved to that
// global and the body was wrongly trusted: (define (f car x) (car x)) stamped
// CaptureSafe, though `car` is bound to whatever the caller passes — including
// call/cc. That is a FALSE POSITIVE, the direction the kill criterion forbids, and
// no continuation or coroutine suite caught it. The sibling walkers already seeded
// their parameters (bodyIsSelfTailReusable, resolveCallEdges); this one did not.
//
// All three exported entry points are covered, since all three route through the
// same seed.
func TestCalleeShadowedByOwnParameter(t *testing.T) {
	env := envWithImported(t, "car", "<=")

	// (define (f car x) (car x)) — `car` is a PARAMETER, not the primitive.
	shadowed := fnWith("f", params("car", "x"), call(symRef("car"), symRef("x")))
	if ProcedureBodyIsCaptureSafe(shadowed, selfIn(t, env, "f"), env) {
		t.Errorf("a callee shadowed by the procedure's own parameter must NOT be capture-safe-as-callee")
	}
	if BodyIsFrameReleasable(shadowed, selfIn(t, env, "f"), env) {
		t.Errorf("a callee shadowed by the procedure's own parameter must NOT be frame-releasable")
	}

	// Self-tail path: (define (loop car n) (if (<= n 0) n (loop car (car n)))).
	// bodyIsSelfTailReusable already seeds params for self-call and set! detection;
	// the callee half did not, so this shape passed the exported predicate.
	loopShadowed := fnWith("loop", params("car", "n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("loop"), symRef("car"), call(symRef("car"), symRef("n")))))
	if BodyIsSelfTailReusable(loopShadowed, selfIn(t, env, "loop"), env) {
		t.Errorf("a loop whose callee is shadowed by its own parameter must NOT be self-tail-reusable")
	}

	// Positive controls: the same bodies with the parameter renamed are trusted
	// again, so the rejection above is the shadow and not the shape.
	unshadowed := fnWith("f", params("p", "x"), call(symRef("car"), symRef("x")))
	if !ProcedureBodyIsCaptureSafe(unshadowed, selfIn(t, env, "f"), env) {
		t.Errorf("an unshadowed capture-safe primitive callee must stay capture-safe-as-callee")
	}
	loopUnshadowed := fnWith("loop", params("p", "n"),
		ifx(call(symRef("<="), symRef("n"), lit()),
			symRef("n"),
			call(symRef("loop"), symRef("p"), call(symRef("car"), symRef("n")))))
	if !BodyIsSelfTailReusable(loopUnshadowed, selfIn(t, env, "loop"), env) {
		t.Errorf("an unshadowed capture-safe primitive callee must stay self-tail-reusable")
	}
}

// TestBodyIsSelfTailReusable_CalleeSafety covers the interprocedural half of
// capture-safety added at the exported boundary: a loop is reusable only if every
// call operator is the self name or a capture-safe, non-rebindable primitive. This
// is the fix for the map/for-each corruption — a loop that calls an unknown user
// callback could have that callee capture the continuation pinning the frame.
func TestBodyIsSelfTailReusable_CalleeSafety(t *testing.T) {
	// (define (loop i n) (if (>= i n) i (loop (+ i 1) n)))
	counter := fnWith("loop", params("i", "n"),
		ifx(call(symRef(">="), symRef("i"), symRef("n")),
			symRef("i"),
			call(symRef("loop"), call(symRef("+"), symRef("i"), lit()), symRef("n"))))

	// Positive: + and >= are imported (capture-safe whitelist ∧ Stable).
	counterEnv := envWithImported(t, "+", ">=")
	if !BodyIsSelfTailReusable(counter, selfIn(t, counterEnv, "loop"), counterEnv) {
		t.Errorf("counter over imported capture-safe primitives must be reusable")
	}

	// Negative: + is rebindable (not imported/Stable) — a set! could turn it into
	// a capturer, so the frame is no longer safe to reuse.
	rebindableEnv := envWithImported(t, ">=")
	if BodyIsSelfTailReusable(counter, selfIn(t, rebindableEnv, "loop"), rebindableEnv) {
		t.Errorf("a rebindable primitive callee must block reuse")
	}

	// Negative: the loop calls an unknown user function (the map/for-each hazard).
	// proc is imported here, but it is not on the capture-safe whitelist, so it may
	// capture the continuation that pins the frame.
	withCallback := fnWith("loop", params("i", "n"),
		ifx(call(symRef(">="), symRef("i"), symRef("n")),
			symRef("i"),
			call(symRef("loop"), call(symRef("proc"), symRef("i")), symRef("n"))))
	withCallbackEnv := envWithImported(t, ">=", "proc")
	if BodyIsSelfTailReusable(withCallback, selfIn(t, withCallbackEnv, "loop"), withCallbackEnv) {
		t.Errorf("a loop calling an unknown (non-capture-safe) callee must not be reusable")
	}

	// Negative: a computed operator — ((car fns) i) — could resolve to any
	// (capturing) procedure, so it disqualifies even though `car` itself is
	// capture-safe. Pins the non-*ValidatedSymbol operator branch.
	computedOp := fnWith("loop", params("i", "n"),
		ifx(call(symRef(">="), symRef("i"), symRef("n")),
			symRef("i"),
			&ValidatedBegin{
				formName: "begin",
				body: []ValidatedExpr{
					call(call(symRef("car"), symRef("fns")), symRef("i")),
					call(symRef("loop"), call(symRef("+"), symRef("i"), lit()), symRef("n")),
				},
			}))
	computedOpEnv := envWithImported(t, ">=", "+", "car", "fns")
	if BodyIsSelfTailReusable(computedOp, selfIn(t, computedOpEnv, "loop"), computedOpEnv) {
		t.Errorf("a loop with a computed-operator call must not be reusable")
	}
}

// TestResolveSelfTriState pins the three self shapes resolveSelf distinguishes, one
// of which is new and one of which had no coverage at all.
//
// The anonymous case is callback.go's: an argument lambda has no name, so no callee
// may be cleared as a self call, and the predicate must still answer on the body's
// own merits. The unresolvable case is the guard: a non-nil self the caller's env
// does not contain means the caller supplied the wrong env, and refusing turns that
// into a measurable arming-count drop instead of a silent loss at one clause.
func TestResolveSelfTriState(t *testing.T) {
	// (lambda (n) (- n 1)) — capture-safe body, no self reference.
	body := lam(call(symRef("-"), symRef("n"), lit()))
	body.params = params("n")

	t.Run("anonymous self admits a capture-safe body", func(t *testing.T) {
		env := envWithImported(t, "-")
		if !ProcedureBodyIsCaptureSafe(body, nil, env) {
			t.Error("a nil self must be the anonymous case, not a refusal: an argument " +
				"callback has no name and callback.go depends on this")
		}
	})

	t.Run("unresolvable self refuses", func(t *testing.T) {
		env := envWithImported(t, "-")
		absent := syntax.NewSyntaxSymbol("nowhere", nil)
		if ProcedureBodyIsCaptureSafe(body, absent, env) {
			t.Error("a non-nil self absent from env must refuse the whole predicate")
		}
		if BodyIsFrameReleasable(body, absent, env) {
			t.Error("BodyIsFrameReleasable must refuse an unresolvable self too")
		}
		if BodyIsSelfTailReusable(body, absent, env) {
			t.Error("BodyIsSelfTailReusable must refuse an unresolvable self too")
		}
	})

	t.Run("a same-spelled other binding is not the self", func(t *testing.T) {
		// (define (f n) (g n)) where `g` is a plain user binding: not capture-safe,
		// not the self. The pre-fix clause cleared a callee whose SPELLING matched the
		// self name; with identity comparison, only the resolved self binding clears.
		fn := fnWith("f", params("n"), call(symRef("g"), symRef("n")))
		env := envWithImported(t)
		selfIn(t, env, "g") // a `g` binding exists, but it is not f's self
		if ProcedureBodyIsCaptureSafe(fn, selfIn(t, env, "f"), env) {
			t.Error("a callee that is neither the self binding nor capture-safe must refuse")
		}
	})
}
