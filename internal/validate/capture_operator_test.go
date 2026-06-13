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

// callccStub treats the bare symbol named "call/cc" as the capture primitive.
// The production identity test lives in frame_reclaim_build.go; Layer A is
// parameterized by this callback so it can be tested in isolation.
func callccStub(s *syntax.SyntaxSymbol) bool {
	return s.Sym.Key == "call/cc"
}

func TestBodyReferencesCaptureOperator(t *testing.T) {
	// (call/cc k) — direct capture.
	direct := []ValidatedExpr{call(symRef("call/cc"), symRef("k"))}
	if !bodyReferencesCaptureOperator(direct, callccStub) {
		t.Fatalf("direct (call/cc k) must be detected")
	}

	// (f (call/cc k)) — capture nested inside an argument.
	nested := []ValidatedExpr{call(symRef("f"), call(symRef("call/cc"), symRef("k")))}
	if !bodyReferencesCaptureOperator(nested, callccStub) {
		t.Fatalf("nested (call/cc k) must be detected")
	}

	// (g (lambda () (call/cc k))) — capture inside a closure body.
	inClosure := []ValidatedExpr{call(symRef("g"), lam(call(symRef("call/cc"), symRef("k"))))}
	if !bodyReferencesCaptureOperator(inClosure, callccStub) {
		t.Fatalf("capture inside a closure body must be detected")
	}

	// (+ a b) — no capture operator.
	clean := []ValidatedExpr{call(symRef("+"), symRef("a"), symRef("b"))}
	if bodyReferencesCaptureOperator(clean, callccStub) {
		t.Fatalf("(+ a b) must not be flagged")
	}

	// call/cc as a bare reference, not in operator position, is NOT a call —
	// the predicate keys on the *operator* slot only.
	asArg := []ValidatedExpr{call(symRef("f"), symRef("call/cc"))}
	if bodyReferencesCaptureOperator(asArg, callccStub) {
		t.Fatalf("call/cc passed as an argument is not an invocation here")
	}
}

func TestBodyCreatesEscapingClosure(t *testing.T) {
	// (lambda () x) in body/return position — escapes.
	escaping := []ValidatedExpr{lam(symRef("x"))}
	if !bodyCreatesEscapingClosure(escaping) {
		t.Fatalf("a lambda in return position must be flagged as escaping")
	}

	// case-lambda in body position — escapes.
	escapingCase := []ValidatedExpr{caseLam(symRef("x"))}
	if !bodyCreatesEscapingClosure(escapingCase) {
		t.Fatalf("a case-lambda in return position must be flagged as escaping")
	}

	// ((lambda (x) x) 5) — immediately applied, does not escape.
	applied := []ValidatedExpr{call(lam(symRef("x")), symRef("5"))}
	if bodyCreatesEscapingClosure(applied) {
		t.Fatalf("an immediately-applied lambda must not be flagged")
	}

	// (f (lambda () x)) — lambda passed as an argument escapes (not the operator).
	asArg := []ValidatedExpr{call(symRef("f"), lam(symRef("x")))}
	if !bodyCreatesEscapingClosure(asArg) {
		t.Fatalf("a lambda passed as an argument must be flagged as escaping")
	}

	// ((lambda () (lambda () x))) — immediately-applied outer, but the inner
	// lambda is returned and escapes.
	nestedEscape := []ValidatedExpr{call(lam(lam(symRef("x"))))}
	if !bodyCreatesEscapingClosure(nestedEscape) {
		t.Fatalf("a lambda returned from an immediately-applied lambda must escape")
	}

	// (+ a b) — no closure at all.
	none := []ValidatedExpr{call(symRef("+"), symRef("a"), symRef("b"))}
	if bodyCreatesEscapingClosure(none) {
		t.Fatalf("no closure means no escape")
	}
}
