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

package wile_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func newPrecisionClassEngine(c *qt.C) *wile.Engine {
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	return engine
}

// TestReaderBigFloatIsDefaultPrecision pins the fixed precision class: every
// BigFloat the reader or string->number produces is DefaultBigFloatPrecision
// bits, whatever its spelling. Precision grown from the digit count (334 bits
// for a 100-digit literal, 1005 for #m of a 1000-bit integer) is what broke
// the write/read round trip (TestBigFloatWriteReadByEqv).
func TestReaderBigFloatIsDefaultPrecision(t *testing.T) {
	digits100 := "1." + strings.Repeat("3", 99)
	int1000 := "1" + strings.Repeat("0", 300) // 997 bits
	ratio := "1/" + strings.Repeat("7", 100)
	hexPast := "1" + strings.Repeat("0", 300) + ".8" // 16^300, past float64
	tcs := []string{
		"1.5l0",
		digits100 + "l0",
		"1e400",
		digits100 + "e400",
		"#m5",
		"#m1.5",
		"#m" + digits100,
		"#m#x1.8",
		"#m" + int1000,
		"#m#z" + int1000,
		"#m#e1/3",
		"#m#e" + ratio,
		"#x" + hexPast,
		"#b1" + strings.Repeat("0", 1100) + ".1",
		`(string->number "` + digits100 + `l0")`,
		`(string->number "1e400")`,
		`(string->number "#m` + int1000 + `")`,
		`(string->number "#m#e` + ratio + `")`,
		`(string->number "` + hexPast + `" 16)`,
		`(string->number "#x` + hexPast + `")`,
		`(read (open-input-string "` + digits100 + `l0"))`,
		// primitives over reader-class operands stay in the class
		"(sqrt #m2)",
		"(/ #m1 3)",
		"(exp #m1)",
		"(* #m1 (expt 10 400))",
		// an exact operand's inexact root is in the class too, not sized by
		// the operand (1329 bits here before)
		"(sqrt (- (expt 10 400) 1))",
	}
	for _, src := range tcs {
		t.Run(src[:min(len(src), 30)], func(t *testing.T) {
			c := qt.New(t)
			v, err := newPrecisionClassEngine(c).EvalMultiple(context.Background(), src)
			c.Assert(err, qt.IsNil)
			bf, ok := v.Internal().(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("%T", v.Internal()))
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
		})
	}
}

// TestBigFloatWriteReadByEqv is the R7RS 6.2.6 round trip, stated with eqv? as
// the report states it: number->string then string->number (or read) gives back
// the number. The writer prints the shortest digits at the value's precision,
// so the round trip holds exactly when the reader reads them at that precision.
// eqv? ignores precision; TestReaderBigFloatIsDefaultPrecision pins that half.
func TestBigFloatWriteReadByEqv(t *testing.T) {
	tcs := []string{
		"(/ #m1 3)",
		"(exp #m1)",
		"(sqrt #m2)",
		"1.2345678901234567890123456789012345678901234567890123456789012345678901234567890123456789l0",
		"1e400",
		"(* #m1 (expt 10 400))",
		"(- (/ #m2 7))",
		"(/ #m1 (expt 10 400))",
		"(sqrt (- (expt 10 400) 1))",
	}
	for _, src := range tcs {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			code := "(let* ((x " + src + ") (s (number->string x)) (y (string->number s)) (z (read (open-input-string s))))" +
				" (list (eqv? x y) (= x y) (eqv? x z) (= x z)))"
			v, err := newPrecisionClassEngine(c).EvalMultiple(context.Background(), code)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "(#t #t #t #t)")
		})
	}
}
