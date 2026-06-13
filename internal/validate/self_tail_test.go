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

	"github.com/aalpar/wile/internal/syntax"
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
		validatedBase: validatedBase{formName: "if"},
		Test:          test,
		Conseq:        conseq,
		Alt:           alt,
	}
}

// fnWith builds a function-form ValidatedDefine with explicit params.
func fnWith(name string, ps *ValidatedParams, body ...ValidatedExpr) *ValidatedDefine {
	d := defineFn(name, body...)
	d.params = ps
	return d
}

// TestBodyIsSelfTailReusable is the correctness core for OpSelfTailCall: it pins
// which closures may have their activation frame reused in place at a depth-0
// self-tail call. The predicate is the safety gate; any false positive here is
// silent corruption (see the escape-gated plan's kill criterion), so the negative
// cases are adversarial.
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
						validatedBase: validatedBase{formName: "begin"},
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
			name: "negative: escaping lambda in body",
			proc: fnWith("esc", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					call(symRef("esc"), call(symRef("save"), lam(symRef("i")))))),
			self: "esc", want: false,
		},
		{
			name: "negative: variadic params (no flat slot rebind)",
			proc: fnWith("v", variadicParams([]string{"i"}, "rest"),
				call(symRef("v"), symRef("i"))),
			self: "v", want: false,
		},
		{
			name: "negative: self-tail call nested in a let (depth > 0, v1 restriction)",
			proc: fnWith("g", params("i"),
				ifx(call(symRef("done?"), symRef("i")),
					symRef("i"),
					nestedLet([]ValidatedLetBinding{letBind("x")},
						call(symRef("g"), call(symRef("+"), symRef("i"), symRef("x")))))),
			self: "g", want: false,
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
					validatedBase: validatedBase{formName: "begin"},
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
			// A set! to a shadowing let binding of the same name does NOT mutate the
			// enclosing self, so reuse stays sound (precision: shadow-aware).
			name: "positive: set! targets a shadowing let binding, not self",
			proc: fnWith("s", params("i"),
				&ValidatedBegin{
					validatedBase: validatedBase{formName: "begin"},
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
