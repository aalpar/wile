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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// collectDefinesByName runs collectTopLevelDefines over a validated unit and
// returns the top-level defines keyed by name. Test helper.
func collectDefinesByName(expr ValidatedExpr) map[string]*ValidatedDefine {
	out := make(map[string]*ValidatedDefine)
	collectTopLevelDefines([]ValidatedExpr{expr}, func(d *ValidatedDefine) {
		out[d.name.Key()] = d
	})
	return out
}

func TestStableInUnit_NeverSetVsSet(t *testing.T) {
	// (begin (define a 1) (define b 2) (set! b 3))
	// a is defined-once and never set! → StableInUnit. b is set! → not.
	input := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), values.NewSymbol("a"), values.NewInteger(1)),
		values.List(values.NewSymbol("define"), values.NewSymbol("b"), values.NewInteger(2)),
		values.List(values.NewSymbol("set!"), values.NewSymbol("b"), values.NewInteger(3)),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if !defs["a"].StableInUnit {
		t.Fatalf("a is never set! → StableInUnit must be true")
	}
	if defs["b"].StableInUnit {
		t.Fatalf("b is set! in-unit → StableInUnit must be false")
	}
}

func TestStableInUnit_DefinedTwiceNotStable(t *testing.T) {
	// (begin (define f 1) (define f 2)) — defined twice → not defined-once.
	input := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), values.NewSymbol("f"), values.NewInteger(1)),
		values.List(values.NewSymbol("define"), values.NewSymbol("f"), values.NewInteger(2)),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if defs["f"].StableInUnit {
		t.Fatalf("f is defined twice in-unit → StableInUnit must be false")
	}
}

func TestStableInUnit_SetInsideLambdaBodyCounts(t *testing.T) {
	// (begin (define (h) 1) (define (use) (set! h 9)))
	// h is set! inside another define's body → still mutated in-unit → not stable.
	input := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"),
			values.List(values.NewSymbol("h")),
			values.NewInteger(1)),
		values.List(values.NewSymbol("define"),
			values.List(values.NewSymbol("use")),
			values.List(values.NewSymbol("set!"), values.NewSymbol("h"), values.NewInteger(9))),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if defs["h"].StableInUnit {
		t.Fatalf("h is set! inside use's body → StableInUnit must be false")
	}
	if !defs["use"].StableInUnit {
		t.Fatalf("use is defined-once and never set! → StableInUnit must be true")
	}
}

func TestStableInUnit_SetToLocalShadowStillMarksTopLevel(t *testing.T) {
	// (begin (define x 1) (let ((x 2)) (set! x 3)))
	// The set! targets the LOCAL shadow, not the top-level x. The conservative
	// by-name mark must STILL mark top-level x non-stable — this over-match is
	// the soundness margin protecting the frame-reclaim Stable stamp. If a
	// future change makes StableInUnit precise (only the resolved binding), this
	// test flips and catches the regression.
	input := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), values.NewSymbol("x"), values.NewInteger(1)),
		values.List(values.NewSymbol("let"),
			values.List(values.List(values.NewSymbol("x"), values.NewInteger(2))),
			values.List(values.NewSymbol("set!"), values.NewSymbol("x"), values.NewInteger(3))),
	)
	// A real env is required: validating the inner let creates a child frame,
	// which panics on a nil parent. The top-level defines still resolve as
	// globals (the validator does not create global bindings).
	env := environment.NewNamespace().Runtime()
	result := ValidateExpression(context.TODO(), env, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if defs["x"].StableInUnit {
		t.Fatalf("set! to a local shadow of x must conservatively mark top-level x non-stable")
	}
}

func TestStableInUnit_SingleTopLevelDefine(t *testing.T) {
	// A bare top-level (define g 5) (no begin wrapper) is defined-once, never set!.
	input := values.List(values.NewSymbol("define"), values.NewSymbol("g"), values.NewInteger(5))
	result := ValidateExpression(context.TODO(), nil, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if !defs["g"].StableInUnit {
		t.Fatalf("a bare never-set! top-level define must be StableInUnit")
	}
}
