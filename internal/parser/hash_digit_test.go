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

package parser

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestHashDigit_Parser tests R7RS §7.1.1 # inexact digit placeholder parsing.
// The # character represents an unknown digit (treated as 0) and forces the
// resulting number to be inexact (Float).
func TestHashDigit_Parser(t *testing.T) {
	tcs := []struct {
		name   string
		in     string
		expect values.Value
	}{
		// Basic integer hash digits
		{
			name:   "1## -> 100.0",
			in:     "1##",
			expect: values.NewFloat(100.0),
		},
		{
			name:   "-1## -> -100.0",
			in:     "-1##",
			expect: values.NewFloat(-100.0),
		},
		{
			name:   "+1## -> 100.0",
			in:     "+1##",
			expect: values.NewFloat(100.0),
		},
		// Decimal fractions with hash digits
		{
			name:   "1.2## -> 1.2",
			in:     "1.2##",
			expect: values.NewFloat(1.2),
		},
		{
			name:   "1##.## -> 100.0",
			in:     "1##.##",
			expect: values.NewFloat(100.0),
		},
		{
			name:   ".5## -> 0.5",
			in:     ".5##",
			expect: values.NewFloat(0.5),
		},
		{
			name:   "1##. -> 100.0",
			in:     "1##.",
			expect: values.NewFloat(100.0),
		},
		// Rational with hash digits
		{
			name:   "1##/3 -> ~33.333",
			in:     "1##/3",
			expect: values.NewFloat(100.0 / 3.0),
		},
		{
			name:   "1/3## -> ~1/300",
			in:     "1/3##",
			expect: values.NewFloat(1.0 / 300.0),
		},
		// #e override: exact prefix converts hash-digit-forced-inexact back to exact
		{
			name:   "#e1## -> exact 100",
			in:     "#e1##",
			expect: values.NewInteger(100),
		},
		// Binary with hash digits
		{
			name:   "#b1# -> 2.0",
			in:     "#b1#",
			expect: values.NewFloat(2.0),
		},
		// Hexadecimal with hash digits
		{
			name:   "#xf# -> 240.0",
			in:     "#xf#",
			expect: values.NewFloat(240.0),
		},
		// Octal with hash digits
		{
			name:   "#o7# -> 56.0",
			in:     "#o7#",
			expect: values.NewFloat(56.0),
		},
		// Scientific notation with hash digits
		{
			name:   "1##e2 -> 10000.0",
			in:     "1##e2",
			expect: values.NewFloat(10000.0),
		},
		// Signed decimal fraction with hash digits
		{
			name:   "+.5## -> 0.5",
			in:     "+.5##",
			expect: values.NewFloat(0.5),
		},
		{
			name:   "-.5## -> -0.5",
			in:     "-.5##",
			expect: values.NewFloat(-0.5),
		},
		// Non-decimal base rationals (R7RS §7.1.1)
		{
			name:   "#x10/2 -> 8",
			in:     "#x10/2",
			expect: values.NewInteger(8),
		},
		{
			name:   "#o11/2 -> 9/2",
			in:     "#o11/2",
			expect: values.NewRational(9, 2),
		},
		{
			name:   "#b101/10 -> 5/2",
			in:     "#b101/10",
			expect: values.NewRational(5, 2),
		},
		{
			name:   "#x1#/2 -> 8.0 (hash digit forces inexact)",
			in:     "#x1#/2",
			expect: values.NewFloat(8.0),
		},
		{
			name:   "#x-a/5 -> -2",
			in:     "#x-a/5",
			expect: values.NewInteger(-2),
		},
		{
			name:   "#xa/b -> 10/11",
			in:     "#xa/b",
			expect: values.NewRational(10, 11),
		},
		{
			name:   "#x+ff/10 -> 255/16",
			in:     "#x+ff/10",
			expect: values.NewRational(255, 16),
		},
		{
			name:   "#o+7/2 -> 7/2",
			in:     "#o+7/2",
			expect: values.NewRational(7, 2),
		},
		{
			name:   "#b-110/11 -> -2",
			in:     "#b-110/11",
			expect: values.NewInteger(-2),
		},
		// Without hash: verify normal numbers still work
		{
			name:   "123 -> integer 123",
			in:     "123",
			expect: values.NewInteger(123),
		},
		{
			name:   "1.5 -> float 1.5",
			in:     "1.5",
			expect: values.NewFloat(1.5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironment().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			v := syn.UnwrapAll()
			c.Assert(v, valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestHashDigit_Inexactness verifies that hash digit numbers are inexact
// (Float type) and that #e prefix can override this to produce exact results.
func TestHashDigit_Inexactness(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 1## should be inexact (Float)
	p := NewParser(env, true, strings.NewReader("1##"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	v := syn.UnwrapAll()
	_, isFloat := v.(*values.Float)
	c.Assert(isFloat, qt.IsTrue, qt.Commentf("1## should produce Float, got %T", v))

	// #e1## should be exact (Integer)
	p = NewParser(env, true, strings.NewReader("#e1##"))
	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	v = syn.UnwrapAll()
	_, isInt := v.(*values.Integer)
	c.Assert(isInt, qt.IsTrue, qt.Commentf("#e1## should produce Integer, got %T", v))

	// #i1## should be inexact (Float) - #i on already-inexact is a no-op
	p = NewParser(env, true, strings.NewReader("#i1##"))
	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	v = syn.UnwrapAll()
	_, isFloat = v.(*values.Float)
	c.Assert(isFloat, qt.IsTrue, qt.Commentf("#i1## should produce Float, got %T", v))
}
