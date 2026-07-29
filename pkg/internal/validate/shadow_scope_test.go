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

// walkScopes runs the shadow walk over a body and returns, per call operator
// name, the nameSet in force at that call site. Two calls with the same operator
// name collapse to the last one seen, so probes are given distinct names.
func walkScopes(body []ValidatedExpr) map[string]nameSet {
	out := map[string]nameSet{}
	walkBodySeq(body, nameSet(nil), func(op ValidatedExpr, bound nameSet) {
		sym, ok := op.(*ValidatedSymbol)
		if !ok {
			return
		}
		out[sym.Symbol.Sym.Key] = bound
	})
	return out
}

// letOf builds a ValidatedLet of the given kind binding one name to one init.
func letOf(kind LetKind, name string, init ValidatedExpr, body ...ValidatedExpr) *ValidatedLet {
	return &ValidatedLet{
		validatedBase: validatedBase{formName: kind.String()},
		Kind:          kind,
		Bindings:      []ValidatedLetBinding{{Name: syntax.NewSyntaxSymbol(name, nil), Init: init}},
		body:          body,
	}
}

// lamOf builds a ValidatedLambda with the given required parameter names.
func lamOf(params []string, body ...ValidatedExpr) *ValidatedLambda {
	required := make([]*syntax.SyntaxSymbol, 0, len(params))
	for _, p := range params {
		required = append(required, syntax.NewSyntaxSymbol(p, nil))
	}
	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{Required: required},
			body:   body,
		},
	}
}

// TestShadowWalkRecordsLocalBindings pins what the widened nameSet records for
// each binder shape. No production predicate reads the localBinding yet — the
// walk still refuses on membership alone — so this is the only place the
// evidence is checked, and the place where the A-local rule's premises live.
func TestShadowWalkRecordsLocalBindings(t *testing.T) {
	// Named-let shape: (let loop ((i 0)) (loop ...)) validates to a letrec whose
	// single binding is the loop lambda, so the recursive call sits in the INIT.
	namedLetLam := lamOf(nil, call(symRef("in-init")))
	namedLet := letOf(LetKindLetrec, "loop", namedLetLam, call(symRef("in-body")))

	// The same shape as a plain let, where the name in its own init is the OUTER
	// binding and the initializer is therefore not evidence about it.
	plainLetLam := lamOf(nil, call(symRef("plain-in-init")))
	plainLet := letOf(LetKindLet, "f", plainLetLam, call(symRef("plain-in-body")))

	// A let whose body set!s the bound name: the init no longer describes what an
	// operator of that name resolves to.
	mutatedLam := lamOf(nil, lit())
	mutatedLet := letOf(LetKindLet, "g", mutatedLam,
		setBang("g", lit()), call(symRef("after-set")))

	// Non-procedure init: opaque, as a parameter is.
	valueLet := letOf(LetKindLet, "x", lit(), call(symRef("in-value-let")))

	// Internal defines: the function form is its own initializer, the value form
	// is opaque, and a set! of the name in the same sequence flags it.
	fnDefine := defineFn("helper", lit())
	valDefine := defineVal("konst", lit())
	mutDefine := defineFn("rebound", lit())
	defineBody := []ValidatedExpr{
		fnDefine, valDefine, mutDefine,
		setBang("rebound", lit()),
		call(symRef("in-define-body")),
	}

	// Parameters are opaque: the caller supplies the procedure.
	paramLam := lamOf([]string{"cb"}, call(symRef("in-param-body")))

	tests := []struct {
		name        string
		body        []ValidatedExpr
		probe       string
		subject     string
		wantInit    ValidatedBodyAndParams
		wantMutated bool
	}{{
		name: "letrec init sees its own binding", body: []ValidatedExpr{namedLet},
		probe: "in-init", subject: "loop", wantInit: namedLetLam,
	}, {
		name: "letrec body sees its own binding", body: []ValidatedExpr{namedLet},
		probe: "in-body", subject: "loop", wantInit: namedLetLam,
	}, {
		name: "plain let init is opaque", body: []ValidatedExpr{plainLet},
		probe: "plain-in-init", subject: "f", wantInit: nil,
	}, {
		name: "plain let body records the lambda", body: []ValidatedExpr{plainLet},
		probe: "plain-in-body", subject: "f", wantInit: plainLetLam,
	}, {
		name: "set! in the let body marks the binding mutated", body: []ValidatedExpr{mutatedLet},
		probe: "after-set", subject: "g", wantInit: mutatedLam, wantMutated: true,
	}, {
		name: "non-procedure init is opaque", body: []ValidatedExpr{valueLet},
		probe: "in-value-let", subject: "x", wantInit: nil,
	}, {
		name: "function-form internal define is its own init", body: defineBody,
		probe: "in-define-body", subject: "helper", wantInit: fnDefine,
	}, {
		name: "value-form internal define is opaque", body: defineBody,
		probe: "in-define-body", subject: "konst", wantInit: nil,
	}, {
		name: "set! of an internal define marks it mutated", body: defineBody,
		probe: "in-define-body", subject: "rebound", wantInit: mutDefine, wantMutated: true,
	}, {
		name: "a parameter is opaque", body: []ValidatedExpr{paramLam},
		probe: "in-param-body", subject: "cb", wantInit: nil,
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bound := walkScopes(tt.body)[tt.probe]
			if bound == nil {
				t.Fatalf("no call site with operator %q was walked", tt.probe)
			}
			// Membership is what every current consumer queries, and it must be
			// exactly what it was before the value type widened.
			if !bound.has(tt.subject) {
				t.Fatalf("%q not shadowed at %q", tt.subject, tt.probe)
			}
			got := bound[tt.subject]
			if got.init != tt.wantInit {
				t.Errorf("init for %q at %q = %v, want %v", tt.subject, tt.probe, got.init, tt.wantInit)
			}
			if got.mutated != tt.wantMutated {
				t.Errorf("mutated for %q at %q = %v, want %v", tt.subject, tt.probe, got.mutated, tt.wantMutated)
			}
		})
	}
}

// TestShadowWalkScopesDoNotLeak pins the copy-on-extend property the widened
// map must keep: a name bound in one let is not visible to its sibling.
func TestShadowWalkScopesDoNotLeak(t *testing.T) {
	left := letOf(LetKindLet, "a", lamOf(nil, lit()), call(symRef("in-left")))
	right := letOf(LetKindLet, "b", lamOf(nil, lit()), call(symRef("in-right")))
	scopes := walkScopes([]ValidatedExpr{left, right})

	if scopes["in-left"].has("b") {
		t.Error("binding from the right let leaked into the left let's scope")
	}
	if scopes["in-right"].has("a") {
		t.Error("binding from the left let leaked into the right let's scope")
	}
}
