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
	"slices"

	"context"
	"testing"

	"github.com/aalpar/wile/pkg/values"
)

// unitArityForList validates a (begin ...) unit built from vs and returns the
// collected arity table.
func unitArityForList(t *testing.T, vs ...values.Value) map[string]UnitArityInfo {
	t.Helper()
	input := values.List(append([]values.Value{values.NewSymbol("begin")}, vs...)...)
	result := ValidateExpression(context.TODO(), nil, makeSyntax(input))
	if !result.Ok() {
		t.Fatalf("unexpected validation errors: %v", result.Errors)
	}
	return UnitArityOf([]ValidatedExpr{result.Expr})
}

// arityDefine builds (define (name required... [. rest]) 1). An empty rest
// gives the proper-formals form.
func arityDefine(name string, required []string, rest string) values.Value {
	var head values.Value
	if rest == "" {
		head = values.EmptyList
	} else {
		head = values.NewSymbol(rest)
	}
	for _, r := range slices.Backward(required) {
		head = values.NewCons(values.NewSymbol(r), head)
	}
	head = values.NewCons(values.NewSymbol(name), head)
	return values.List(values.NewSymbol("define"), head, values.NewInteger(1))
}

func TestUnitArity_FixedAndVariadicDefines(t *testing.T) {
	arity := unitArityForList(t,
		arityDefine("h", []string{"x", "y"}, ""),
		arityDefine("g", []string{"a"}, "rest"),
		arityDefine("z", nil, ""),
		values.List(values.NewSymbol("define"), values.NewSymbol("n"), values.NewInteger(5)),
	)

	tcs := []struct {
		name  string
		want  UnitArityInfo
		found bool
	}{
		{name: "h", want: UnitArityInfo{RequiredCount: 2, Variadic: false}, found: true},
		{name: "g", want: UnitArityInfo{RequiredCount: 1, Variadic: true}, found: true},
		{name: "z", want: UnitArityInfo{RequiredCount: 0, Variadic: false}, found: true},
		// (define n 5) binds a non-procedure: no formals, so no entry.
		{name: "n", found: false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, found := arity[tc.name]
			if found != tc.found {
				t.Fatalf("%s: found=%v, want %v", tc.name, found, tc.found)
			}
			if found && got != tc.want {
				t.Fatalf("%s: got %+v, want %+v", tc.name, got, tc.want)
			}
		})
	}
}

// TestUnitArity_LambdaValueFormIsCollected covers (define name (lambda ...)),
// whose formals live on the lambda rather than on the define.
func TestUnitArity_LambdaValueFormIsCollected(t *testing.T) {
	// (define h (lambda (x y) 1))
	lam := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("y")),
		values.NewInteger(1),
	)
	arity := unitArityForList(t,
		values.List(values.NewSymbol("define"), values.NewSymbol("h"), lam),
	)

	got, found := arity["h"]
	if !found {
		t.Fatalf("h: (define h (lambda (x y) ...)) must be collected")
	}
	want := UnitArityInfo{RequiredCount: 2, Variadic: false}
	if got != want {
		t.Fatalf("h: got %+v, want %+v", got, want)
	}
}

// TestUnitArity_UnstableDefineIsExcluded is the gate that makes the string key
// safe. A name defined twice has no single answer, and — critically — two
// hygiene-distinct top-level bindings of the same name collide on this
// hygiene-dropping key. Both cases forfeit StableInUnit, so neither is
// collected and the call site falls through unchecked.
func TestUnitArity_UnstableDefineIsExcluded(t *testing.T) {
	tcs := []struct {
		name  string
		unit  []values.Value
		probe string
	}{
		{
			name: "defined twice with different arities",
			unit: []values.Value{
				arityDefine("f", []string{"x"}, ""),
				arityDefine("f", []string{"a", "b"}, ""),
			},
			probe: "f",
		},
		{
			name: "set! in unit",
			unit: []values.Value{
				arityDefine("f", []string{"x"}, ""),
				values.List(values.NewSymbol("set!"), values.NewSymbol("f"), values.NewInteger(9)),
			},
			probe: "f",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			arity := unitArityForList(t, tc.unit...)
			_, found := arity[tc.probe]
			if found {
				t.Fatalf("%s: an unstable define must not be collected", tc.probe)
			}
		})
	}
}
