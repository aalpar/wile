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
	"errors"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// buildNestedCall constructs (list (list (list ... 1 ...))) nested to the given
// depth as raw syntax, bypassing the parser. This is the programmatic-deep-syntax
// surface the parser's depth limit cannot reach (macro output, datum->syntax,
// quasiquote produce shapes like this).
func buildNestedCall(depth int) syntax.SyntaxValue {
	listSym := syntax.NewSyntaxSymbol("list", nil)
	var inner syntax.SyntaxValue = syntax.NewSyntaxObject(values.NewInteger(1), nil)
	for range depth {
		args := syntax.NewSyntaxCons(inner, syntax.SyntaxEmptyList, nil)
		inner = syntax.NewSyntaxCons(listSym, args, nil)
	}
	return inner
}

// buildNestedLambda constructs (lambda () (lambda () ... 1 ...)) nested to the
// given depth. Crucially, each lambda body is expanded by a *child* expander
// (expander_lambda.go), so this exercises depth accumulation across the
// child-expander boundary — the property a per-object counter would miss.
func buildNestedLambda(depth int) syntax.SyntaxValue {
	lambdaSym := syntax.NewSyntaxSymbol("lambda", nil)
	emptyFormals := syntax.SyntaxEmptyList
	var inner syntax.SyntaxValue = syntax.NewSyntaxObject(values.NewInteger(1), nil)
	for range depth {
		body := syntax.NewSyntaxCons(inner, syntax.SyntaxEmptyList, nil)
		args := syntax.NewSyntaxCons(emptyFormals, body, nil)
		inner = syntax.NewSyntaxCons(lambdaSym, args, nil)
	}
	return inner
}

func newDepthTestExpander(maxDepth int) *ExpanderTimeContinuation {
	env := newNamespace(environment.NewNamespace().Runtime())
	exp := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	exp.SetMaxDepth(maxDepth)
	return exp
}

// Deeply nested programmatic syntax must return a catchable
// ErrExpandDepthExceeded rather than crash with a fatal Go stack overflow.
func TestExpander_DepthLimit_Trips(t *testing.T) {
	exp := newDepthTestExpander(50)
	_, err := exp.ExpandExpression(buildNestedCall(200))
	if err == nil {
		t.Fatal("expected depth-limit error, got nil")
	}
	if !errors.Is(err, werr.ErrExpandDepthExceeded) {
		t.Fatalf("expected ErrExpandDepthExceeded, got: %v", err)
	}
}

// The default constructor bound (DefaultMaxExpandDepth) protects callers that
// never call SetMaxDepth — every production construction site relies on this.
func TestExpander_DepthLimit_DefaultProtects(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	exp := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	_, err := exp.ExpandExpression(buildNestedCall(DefaultMaxExpandDepth + 1))
	if !errors.Is(err, werr.ErrExpandDepthExceeded) {
		t.Fatalf("default bound should trip, got: %v", err)
	}
}

// Nesting within the limit must expand without error.
func TestExpander_DepthLimit_WithinLimitOK(t *testing.T) {
	exp := newDepthTestExpander(1000)
	q, err := exp.ExpandExpression(buildNestedCall(100))
	if err != nil {
		t.Fatalf("depth-100 nesting under a limit of 1000 should expand, got: %v", err)
	}
	if q == nil {
		t.Fatal("expected a syntax value, got nil")
	}
}

// THE shared-guard test. Nested lambdas spawn a fresh child ExpanderTimeContinuation
// per body (expander_lambda.go). A per-object depth counter would reset at each
// child and never trip; the depth must accumulate across the whole run. This
// asserts the bound trips through child-expander boundaries — i.e. that the
// guard is genuinely shared by pointer.
func TestExpander_DepthLimit_SharedAcrossChildExpanders(t *testing.T) {
	exp := newDepthTestExpander(50)
	_, err := exp.ExpandExpression(buildNestedLambda(200))
	if !errors.Is(err, werr.ErrExpandDepthExceeded) {
		t.Fatalf("nested lambdas must accumulate depth across child expanders; expected ErrExpandDepthExceeded, got: %v", err)
	}
}

// SetMaxDepth(0) disables the limit for callers with genuinely deep
// machine-generated syntax (bounded here so the test stays cheap).
func TestExpander_DepthLimit_Unlimited(t *testing.T) {
	exp := newDepthTestExpander(0)
	_, err := exp.ExpandExpression(buildNestedCall(200))
	if err != nil {
		t.Fatalf("disabled limit should expand, got: %v", err)
	}
}

// Context cancellation takes precedence over the depth bound: when a cancelled
// context and a depth-limit violation are both live at the same ExpandExpression
// entry, the caller must see ctx.Err(), not ErrExpandDepthExceeded. White-box:
// pre-inflate the shared guard to the limit so the next descent would trip, then
// cancel — this distinguishes the check ordering (depth-first would return the
// depth error instead).
func TestExpander_DepthLimit_CancellationWins(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	env := newNamespace(environment.NewNamespace().Runtime())
	exp := NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator())
	exp.SetMaxDepth(10)
	exp.depthGuard.depth = exp.depthGuard.max // next increment would exceed the bound
	_, err := exp.ExpandExpression(buildNestedCall(1))
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("cancellation must take precedence over the depth bound; expected context.Canceled, got: %v", err)
	}
}

// Depth must decrement on return, so reusing one expander for many shallow
// top-level expansions does not accumulate depth and falsely trip. Guards the
// `defer g.depth--` correctness — if the decrement were missing, depth would
// climb past the limit across the loop.
func TestExpander_DepthLimit_DecrementsOnReturn(t *testing.T) {
	exp := newDepthTestExpander(5)
	shallow := buildNestedCall(2) // depth ~2, well under 5
	for i := range 1000 {
		_, err := exp.ExpandExpression(shallow)
		if err != nil {
			t.Fatalf("iteration %d: shallow expansion must not trip (depth must decrement), got: %v", i, err)
		}
	}
}
