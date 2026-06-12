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

	"github.com/aalpar/wile/values"
)

// collectDefinesByName walks a validated unit and indexes every ValidatedDefine
// by its symbol key.
func collectDefinesByName(expr ValidatedExpr) map[string]*ValidatedDefine {
	out := make(map[string]*ValidatedDefine)
	walkValidatedDefines(expr, func(d *ValidatedDefine) {
		if d.Name() != nil && d.Name().Sym != nil {
			out[d.Name().Sym.Key] = d
		}
	})
	return out
}

func TestValidate_StableInUnit(t *testing.T) {
	// (begin (define a 1) (define b 2) (set! b 3))
	// a is never set! -> StableInUnit true; b is set! -> false.
	unit := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), values.NewSymbol("a"), values.NewInteger(1)),
		values.List(values.NewSymbol("define"), values.NewSymbol("b"), values.NewInteger(2)),
		values.List(values.NewSymbol("set!"), values.NewSymbol("b"), values.NewInteger(3)),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(unit))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if !defs["a"].StableInUnit {
		t.Fatalf("a is never set! -> StableInUnit must be true")
	}
	if defs["b"].StableInUnit {
		t.Fatalf("b is set! in-unit -> StableInUnit must be false")
	}
}

func TestValidate_StableInUnit_RedefinedIsNotStable(t *testing.T) {
	// f defined twice in one unit -> not defined-once -> StableInUnit false.
	unit := values.List(
		values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), values.NewSymbol("f"), values.NewInteger(1)),
		values.List(values.NewSymbol("define"), values.NewSymbol("f"), values.NewInteger(2)),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(unit))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	defs := collectDefinesByName(result.Expr)
	if defs["f"].StableInUnit {
		t.Fatalf("f defined twice in-unit -> StableInUnit must be false")
	}
}
