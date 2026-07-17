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

package registry

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
)

// stubCallContext is a minimal machine.CallContext for validator unit tests.
// Only Arg is exercised; the other methods satisfy the interface but are
// not used during validation.
type stubCallContext struct {
	args []values.Value
}

func (p *stubCallContext) Arg(i int) values.Value                          { return p.args[i] }
func (p *stubCallContext) SetValue(values.Value)                           {}
func (p *stubCallContext) SetValues(...values.Value)                       {}
func (p *stubCallContext) Authorizer() security.Authorizer                 { return nil }
func (p *stubCallContext) Context() context.Context                        { return context.Background() }
func (p *stubCallContext) EnvironmentFrame() *environment.EnvironmentFrame { return nil }
func (p *stubCallContext) Thread() *values.Thread                          { return nil }

func (p *stubCallContext) ImmutableLiterals() *environment.ImmutableLiterals {
	return nil
}

// Compile-time assertion: stubCallContext satisfies CallContext.
var _ machine.CallContext = (*stubCallContext)(nil)

func newStub(args ...values.Value) *stubCallContext {
	return &stubCallContext{args: args}
}

func TestBuildValidator_ReturnsNil(t *testing.T) {
	tcs := []struct {
		name string
		spec PrimitiveSpec
	}{
		{"no param types", PrimitiveSpec{Name: "x", ParamCount: 1}},
		{"nil param types", PrimitiveSpec{Name: "x", ParamCount: 2, ParamTypes: nil}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(BuildValidator(tc.spec), qt.IsNil)
		})
	}
}

func TestBuildValidator_Fixed(t *testing.T) {
	spec := PrimitiveSpec{
		Name:       "string-length",
		ParamCount: 1,
		ParamTypes: []values.TypeConstraint{values.TypeString},
	}
	v := BuildValidator(spec)
	c := qt.New(t)
	c.Assert(v, qt.IsNotNil)

	tcs := []struct {
		name    string
		args    []values.Value
		wantErr bool
		match   string
	}{
		{"correct type", []values.Value{values.NewString("hi")}, false, ""},
		{"wrong type", []values.Value{values.NewInteger(42)}, true, "string-length: argument 0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			err := v(newStub(tc.args...))
			if tc.wantErr {
				c.Assert(err, qt.IsNotNil)
				c.Assert(err.Error(), qt.Contains, tc.match)
			} else {
				c.Assert(err, qt.IsNil)
			}
		})
	}
}

func TestBuildValidator_TypeAnyAccepts(t *testing.T) {
	// TypeAny accepts any value. Validator must not reject.
	spec := PrimitiveSpec{
		Name:       "id",
		ParamCount: 2,
		ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeString},
	}
	v := BuildValidator(spec)
	c := qt.New(t)

	// First arg is TypeAny -> anything passes. Second must be string.
	err := v(newStub(values.NewInteger(42), values.NewString("ok")))
	c.Assert(err, qt.IsNil)

	// Second arg wrong type.
	err = v(newStub(values.NewInteger(42), values.NewInteger(99)))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "id: argument 1")
}

func TestBuildValidator_Variadic(t *testing.T) {
	// string-append: ParamCount=1, IsVariadic=true. Arg(0) is the whole rest list.
	spec := PrimitiveSpec{
		Name:       "string-append",
		ParamCount: 1,
		IsVariadic: true,
		ParamTypes: []values.TypeConstraint{values.TypeString},
	}
	v := BuildValidator(spec)
	c := qt.New(t)
	c.Assert(v, qt.IsNotNil)

	// Rest list of strings - pass.
	rest := values.List(values.NewString("a"), values.NewString("b"))
	err := v(newStub(rest))
	c.Assert(err, qt.IsNil)

	// Rest list with non-string at position 1 - fail.
	rest = values.List(values.NewString("a"), values.NewInteger(42))
	err = v(newStub(rest))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "string-append: argument 1")

	// Empty rest list - pass (nothing to check).
	err = v(newStub(values.EmptyList))
	c.Assert(err, qt.IsNil)
}

// TestBuildValidator_VariadicShortParamTypes verifies that when
// len(ParamTypes) < ParamCount for a variadic spec, the last declared
// constraint applies to all unspecified fixed positions as well as to
// rest-list elements. PrimitiveSpec.Validate permits this shape (n in
// [1, ParamCount]); BuildValidator must honor it.
func TestBuildValidator_VariadicShortParamTypes(t *testing.T) {
	// ParamCount=3, IsVariadic=true, len(ParamTypes)=1 -> every arg
	// (two fixed positions + rest elements) must be TypeString.
	spec := PrimitiveSpec{
		Name:       "all-strings",
		ParamCount: 3,
		IsVariadic: true,
		ParamTypes: []values.TypeConstraint{values.TypeString},
	}
	v := BuildValidator(spec)
	c := qt.New(t)
	c.Assert(v, qt.IsNotNil)

	// All fixed positions + rest elements are strings: pass.
	rest := values.List(values.NewString("c"))
	err := v(newStub(values.NewString("a"), values.NewString("b"), rest))
	c.Assert(err, qt.IsNil)

	// Second fixed position (not covered by explicit types[1]) wrong: fail.
	err = v(newStub(values.NewString("a"), values.NewInteger(42), values.EmptyList))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "all-strings: argument 1")

	// Rest element wrong: fail.
	rest = values.List(values.NewInteger(99))
	err = v(newStub(values.NewString("a"), values.NewString("b"), rest))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "all-strings: argument 2")
}

func TestBuildValidator_VariadicWithFixedArgs(t *testing.T) {
	// Synthetic: ParamCount=2, IsVariadic=true.
	// Arg(0) is fixed (TypeNumber); Arg(1) is rest list of TypeNumber.
	spec := PrimitiveSpec{
		Name:       "sum-from",
		ParamCount: 2,
		IsVariadic: true,
		ParamTypes: []values.TypeConstraint{values.TypeNumber, values.TypeNumber},
	}
	v := BuildValidator(spec)
	c := qt.New(t)

	// Fixed arg correct, empty rest list - pass.
	err := v(newStub(values.NewInteger(10), values.EmptyList))
	c.Assert(err, qt.IsNil)

	// Fixed arg wrong type - fail on argument 0.
	err = v(newStub(values.NewString("bad"), values.EmptyList))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "sum-from: argument 0")

	// Fixed arg correct, rest arg with wrong type at position 2 - fail on argument 2.
	rest := values.List(values.NewInteger(1), values.NewString("bad"))
	err = v(newStub(values.NewInteger(10), rest))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "sum-from: argument 2")
}
