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

package values

import (
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
)

// NumericTowerCoverageTest tests all 245 type combinations (7 types × 7 operands × 5 operations)
// to document current behavior and ensure complete coverage after refactoring.
//
// Expected behaviors:
//   - success: operation completes and returns a valid Number
//   - panic:   operation panics (ErrNotANumber or ErrDivisionByZero)
//   - nil:     operation returns nil (BUG - should be converted to panic)

// testNumber holds a test value and its type name
type testNumber struct {
	name  string
	value Number
}

// makeTestNumbers creates one test value of each numeric type
func makeTestNumbers() []testNumber {
	return []testNumber{
		{"Integer", NewInteger(5)},
		{"BigInteger", NewBigIntegerFromInt64(5)},
		{"Float", NewFloat(5.0)},
		{"BigFloat", NewBigFloatFromFloat64(5.0)},
		{"Rational", NewRational(5, 1)},
		{"Complex", NewComplex(complex(5, 0))},
		{"BigComplex", NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(0))},
	}
}

// makeTestZeros creates zero values of each numeric type for division-by-zero testing
func makeTestZeros() []testNumber {
	return []testNumber{
		{"Integer", NewInteger(0)},
		{"BigInteger", NewBigIntegerFromInt64(0)},
		{"Float", NewFloat(0.0)},
		{"BigFloat", NewBigFloatFromFloat64(0.0)},
		{"Rational", NewRational(0, 1)},
		{"Complex", NewComplex(complex(0, 0))},
		{"BigComplex", NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), NewBigFloatFromFloat64(0))},
	}
}

// operationResult captures the result of an operation
type operationResult struct {
	success  bool
	isNil    bool
	panicked bool
	panicMsg string
}

// tryOperation safely executes an operation and captures the result
func tryOperation(op func() Number) (result operationResult) {
	defer func() {
		if r := recover(); r != nil {
			result.panicked = true
			result.panicMsg = fmt.Sprintf("%v", r)
		}
	}()

	res := op()
	if res == nil {
		result.isNil = true
	} else {
		result.success = true
	}
	return
}

// TestNumericTower_Add tests all type combinations for Add
// All combinations should succeed - any panic or nil is a buf
func TestNumericTower_Add(t *testing.T) {
	c := qt.New(t)
	numbers := makeTestNumbers()

	for _, receiver := range numbers {
		for _, operand := range numbers {
			name := fmt.Sprintf("%s+%s", receiver.name, operand.name)
			t.Run(name, func(t *testing.T) {
				result := tryOperation(func() Number {
					return receiver.value.Add(operand.value)
				})

				// All type combinations should succeed
				switch {
				case result.panicked:
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				case result.isNil:
					t.Errorf("NIL (BUG - should panic or succeed)")
				default:
					c.Assert(result.success, qt.IsTrue)
				}
			})
		}
	}
}

// TestNumericTower_Subtract tests all type combinations for Subtract
// All combinations should succeed - any panic or nil is a buf
func TestNumericTower_Subtract(t *testing.T) {
	c := qt.New(t)
	numbers := makeTestNumbers()

	for _, receiver := range numbers {
		for _, operand := range numbers {
			name := fmt.Sprintf("%s-%s", receiver.name, operand.name)
			t.Run(name, func(t *testing.T) {
				result := tryOperation(func() Number {
					return receiver.value.Subtract(operand.value)
				})

				switch {
				case result.panicked:
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				case result.isNil:
					t.Errorf("NIL (BUG - should panic or succeed)")
				default:
					c.Assert(result.success, qt.IsTrue)
				}
			})
		}
	}
}

// TestNumericTower_Multiply tests all type combinations for Multiply
// All combinations should succeed - any panic or nil is a buf
func TestNumericTower_Multiply(t *testing.T) {
	c := qt.New(t)
	numbers := makeTestNumbers()

	for _, receiver := range numbers {
		for _, operand := range numbers {
			name := fmt.Sprintf("%s*%s", receiver.name, operand.name)
			t.Run(name, func(t *testing.T) {
				result := tryOperation(func() Number {
					return receiver.value.Multiply(operand.value)
				})

				switch {
				case result.panicked:
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				case result.isNil:
					t.Errorf("NIL (BUG - should panic or succeed)")
				default:
					c.Assert(result.success, qt.IsTrue)
				}
			})
		}
	}
}

// TestNumericTower_Divide tests all type combinations for Divide
// All combinations should succeed - any panic or nil is a buf
func TestNumericTower_Divide(t *testing.T) {
	c := qt.New(t)
	numbers := makeTestNumbers()

	for _, receiver := range numbers {
		for _, operand := range numbers {
			name := fmt.Sprintf("%s/%s", receiver.name, operand.name)
			t.Run(name, func(t *testing.T) {
				result := tryOperation(func() Number {
					return receiver.value.Divide(operand.value)
				})

				switch {
				case result.panicked:
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				case result.isNil:
					t.Errorf("NIL (BUG - should panic or succeed)")
				default:
					c.Assert(result.success, qt.IsTrue)
				}
			})
		}
	}
}

// TestNumericTower_LessThan tests all type combinations for LessThan
// All combinations should succeed - any panic is a buf (missing type case)
func TestNumericTower_LessThan(t *testing.T) {
	numbers := makeTestNumbers()

	for _, receiver := range numbers {
		for _, operand := range numbers {
			name := fmt.Sprintf("%s<%s", receiver.name, operand.name)
			t.Run(name, func(t *testing.T) {
				var panicked bool
				var panicMsg string

				func() {
					defer func() {
						if r := recover(); r != nil {
							panicked = true
							panicMsg = fmt.Sprintf("%v", r)
						}
					}()
					_ = receiver.value.LessThan(operand.value)
				}()

				if panicked {
					t.Errorf("PANIC (BUG - missing type case): %s", panicMsg)
				}
			})
		}
	}
}

// TestNumericTower_DivideByZero tests division by zero for all types
// This test documents current behavior - nil returns are bugs that need fixing
func TestNumericTower_DivideByZero(t *testing.T) {
	numbers := makeTestNumbers()
	zeros := makeTestZeros()

	var nilCases []string

	for _, receiver := range numbers {
		for _, zero := range zeros {
			name := fmt.Sprintf("%s/zero_%s", receiver.name, zero.name)
			t.Run(name, func(t *testing.T) {
				result := tryOperation(func() Number {
					return receiver.value.Divide(zero.value)
				})

				// Division by zero should always panic, never return nil
				switch {
				case result.isNil:
					t.Logf("NIL (BUG) - should panic with ErrDivisionByZero")
					nilCases = append(nilCases, name)
				case result.panicked:
					// Expected behavior
					t.Logf("PANIC (correct): %s", result.panicMsg)
				case result.success:
					// This might happen for Float/BigFloat with IEEE infinity
					t.Logf("SUCCESS (IEEE infinity?)")
				}
			})
		}
	}

	// Report all nil cases at the end
	if len(nilCases) > 0 {
		t.Errorf("Division by zero returned nil (BUG) for %d cases: %v", len(nilCases), nilCases)
	}
}

// TestNumericTower_ResultTypes verifies that each type combination produces the expected result type.
// This tests the promotion matrix documented in plans/NUMERIC_TOWER_REFACTOR.md.
func TestNumericTower_ResultTypes(t *testing.T) {
	c := qt.New(t)

	// Create test values for receivers (a)
	// Using different values for receiver and operand to avoid simplification
	integerA := NewInteger(2)
	bigIntegerA := NewBigIntegerFromInt64(3)
	rationalA := NewRational(5, 2)
	floatA := NewFloat(2.5)
	bigFloatA := NewBigFloatFromFloat64(3.5)
	complexA := NewComplexFromParts(2.0, 1.0)
	bigComplexA := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(2), NewBigFloatFromFloat64(1))

	// Create test values for operands (b) - different values to avoid simplification
	integerB := NewInteger(7)
	bigIntegerB := NewBigIntegerFromInt64(11)
	rationalB := NewRational(3, 4)
	floatB := NewFloat(1.5)
	bigFloatB := NewBigFloatFromFloat64(2.5)
	complexB := NewComplexFromParts(3.0, 2.0)
	bigComplexB := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(3))

	// Expected result types for addition (same pattern for subtraction and multiplication)
	// Format: "ReceiverType+OperandType" -> "ExpectedResultType"
	expectedAdd := map[string]string{
		// Integer row
		"Integer+Integer":    "*values.Integer",
		"Integer+BigInteger": "*values.BigInteger",
		"Integer+Rational":   "*values.Rational",
		"Integer+Float":      "*values.Float",
		"Integer+BigFloat":   "*values.BigFloat",
		"Integer+Complex":    "*values.Complex",
		"Integer+BigComplex": "*values.BigComplex",
		// BigInteger row
		"BigInteger+Integer":    "*values.BigInteger",
		"BigInteger+BigInteger": "*values.BigInteger",
		"BigInteger+Rational":   "*values.Rational",
		"BigInteger+Float":      "*values.Float",
		"BigInteger+BigFloat":   "*values.BigFloat",
		"BigInteger+Complex":    "*values.Complex",
		"BigInteger+BigComplex": "*values.BigComplex",
		// Rational row
		"Rational+Integer":    "*values.Rational",
		"Rational+BigInteger": "*values.Rational",
		"Rational+Rational":   "*values.Rational",
		"Rational+Float":      "*values.Float",
		"Rational+BigFloat":   "*values.BigFloat",
		"Rational+Complex":    "*values.Complex",
		"Rational+BigComplex": "*values.BigComplex",
		// Float row
		"Float+Integer":    "*values.Float",
		"Float+BigInteger": "*values.Float",
		"Float+Rational":   "*values.Float",
		"Float+Float":      "*values.Float",
		"Float+BigFloat":   "*values.BigFloat",
		"Float+Complex":    "*values.Complex",
		"Float+BigComplex": "*values.BigComplex",
		// BigFloat row
		// Note: BigFloat+Complex returns Complex (loses precision) - see big_float.go
		"BigFloat+Integer":    "*values.BigFloat",
		"BigFloat+BigInteger": "*values.BigFloat",
		"BigFloat+Rational":   "*values.BigFloat",
		"BigFloat+Float":      "*values.BigFloat",
		"BigFloat+BigFloat":   "*values.BigFloat",
		"BigFloat+Complex":    "*values.Complex", // Loses BigFloat precision!
		"BigFloat+BigComplex": "*values.BigComplex",
		// Complex row
		"Complex+Integer":    "*values.Complex",
		"Complex+BigInteger": "*values.Complex",
		"Complex+Rational":   "*values.Complex",
		"Complex+Float":      "*values.Complex",
		"Complex+BigFloat":   "*values.BigComplex",
		"Complex+Complex":    "*values.Complex",
		"Complex+BigComplex": "*values.BigComplex",
		// BigComplex row
		"BigComplex+Integer":    "*values.BigComplex",
		"BigComplex+BigInteger": "*values.BigComplex",
		"BigComplex+Rational":   "*values.BigComplex",
		"BigComplex+Float":      "*values.BigComplex",
		"BigComplex+BigFloat":   "*values.BigComplex",
		"BigComplex+Complex":    "*values.BigComplex",
		"BigComplex+BigComplex": "*values.BigComplex",
	}

	receivers := []struct {
		name  string
		value Number
	}{
		{"Integer", integerA},
		{"BigInteger", bigIntegerA},
		{"Rational", rationalA},
		{"Float", floatA},
		{"BigFloat", bigFloatA},
		{"Complex", complexA},
		{"BigComplex", bigComplexA},
	}

	operands := []struct {
		name  string
		value Number
	}{
		{"Integer", integerB},
		{"BigInteger", bigIntegerB},
		{"Rational", rationalB},
		{"Float", floatB},
		{"BigFloat", bigFloatB},
		{"Complex", complexB},
		{"BigComplex", bigComplexB},
	}

	// Test Add
	for _, a := range receivers {
		for _, b := range operands {
			key := a.name + "+" + b.name
			t.Run("Add/"+key, func(t *testing.T) {
				result := a.value.Add(b.value)
				actualType := fmt.Sprintf("%T", result)
				expectedType := expectedAdd[key]
				c.Assert(actualType, qt.Equals, expectedType,
					qt.Commentf("Add: %s + %s", a.name, b.name))
			})
		}
	}

	// Test Subtract (same expected types as Add)
	for _, a := range receivers {
		for _, b := range operands {
			key := a.name + "+" + b.name // reuse the same key pattern
			t.Run("Subtract/"+a.name+"-"+b.name, func(t *testing.T) {
				result := a.value.Subtract(b.value)
				actualType := fmt.Sprintf("%T", result)
				expectedType := expectedAdd[key]
				c.Assert(actualType, qt.Equals, expectedType,
					qt.Commentf("Subtract: %s - %s", a.name, b.name))
			})
		}
	}

	// Test Multiply (same expected types as Add)
	for _, a := range receivers {
		for _, b := range operands {
			key := a.name + "+" + b.name
			t.Run("Multiply/"+a.name+"*"+b.name, func(t *testing.T) {
				result := a.value.Multiply(b.value)
				actualType := fmt.Sprintf("%T", result)
				expectedType := expectedAdd[key]
				c.Assert(actualType, qt.Equals, expectedType,
					qt.Commentf("Multiply: %s * %s", a.name, b.name))
			})
		}
	}
}

// TestNumericTower_DivisionResultTypes verifies division result types.
// Division has special rules:
// - Integer/Integer -> Rational if not exact, Integer if exact (e.g., 6/2=3)
// - We use values that don't divide evenly to test Rational result
func TestNumericTower_DivisionResultTypes(t *testing.T) {
	c := qt.New(t)

	// Receivers (a) - numerators
	integerA := NewInteger(5)
	bigIntegerA := NewBigIntegerFromInt64(7)
	rationalA := NewRational(5, 2)
	floatA := NewFloat(2.5)
	bigFloatA := NewBigFloatFromFloat64(3.5)
	complexA := NewComplexFromParts(2.0, 1.0)
	bigComplexA := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(2), NewBigFloatFromFloat64(1))

	// Operands (b) - denominators that won't divide evenly into receivers
	integerB := NewInteger(3)
	bigIntegerB := NewBigIntegerFromInt64(11)
	rationalB := NewRational(3, 4)
	floatB := NewFloat(1.5)
	bigFloatB := NewBigFloatFromFloat64(2.5)
	complexB := NewComplexFromParts(3.0, 2.0)
	bigComplexB := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(3))

	// Division result types
	// Note: Integer/Integer returns Rational when not exact (5/3), Integer when exact (6/2)
	expectedDiv := map[string]string{
		// Integer row - division by non-divisor produces Rational
		"Integer/Integer":    "*values.Rational", // 5/3 = 5/3
		"Integer/BigInteger": "*values.Rational", // 5/11 = 5/11
		"Integer/Rational":   "*values.Rational",
		"Integer/Float":      "*values.Float",
		"Integer/BigFloat":   "*values.BigFloat",
		"Integer/Complex":    "*values.Complex",
		"Integer/BigComplex": "*values.BigComplex",
		// BigInteger row
		"BigInteger/Integer":    "*values.Rational",
		"BigInteger/BigInteger": "*values.Rational",
		"BigInteger/Rational":   "*values.Rational",
		"BigInteger/Float":      "*values.Float",
		"BigInteger/BigFloat":   "*values.BigFloat",
		"BigInteger/Complex":    "*values.Complex",
		"BigInteger/BigComplex": "*values.BigComplex",
		// Rational row
		"Rational/Integer":    "*values.Rational",
		"Rational/BigInteger": "*values.Rational",
		"Rational/Rational":   "*values.Rational",
		"Rational/Float":      "*values.Float",
		"Rational/BigFloat":   "*values.BigFloat",
		"Rational/Complex":    "*values.Complex",
		"Rational/BigComplex": "*values.BigComplex",
		// Float row
		"Float/Integer":    "*values.Float",
		"Float/BigInteger": "*values.Float",
		"Float/Rational":   "*values.Float",
		"Float/Float":      "*values.Float",
		"Float/BigFloat":   "*values.BigFloat",
		"Float/Complex":    "*values.Complex",
		"Float/BigComplex": "*values.BigComplex",
		// BigFloat row
		// Note: BigFloat/Complex returns Complex (loses precision) - see big_float.go
		"BigFloat/Integer":    "*values.BigFloat",
		"BigFloat/BigInteger": "*values.BigFloat",
		"BigFloat/Rational":   "*values.BigFloat",
		"BigFloat/Float":      "*values.BigFloat",
		"BigFloat/BigFloat":   "*values.BigFloat",
		"BigFloat/Complex":    "*values.Complex", // Loses BigFloat precision!
		"BigFloat/BigComplex": "*values.BigComplex",
		// Complex row
		"Complex/Integer":    "*values.Complex",
		"Complex/BigInteger": "*values.Complex",
		"Complex/Rational":   "*values.Complex",
		"Complex/Float":      "*values.Complex",
		"Complex/BigFloat":   "*values.BigComplex",
		"Complex/Complex":    "*values.Complex",
		"Complex/BigComplex": "*values.BigComplex",
		// BigComplex row
		"BigComplex/Integer":    "*values.BigComplex",
		"BigComplex/BigInteger": "*values.BigComplex",
		"BigComplex/Rational":   "*values.BigComplex",
		"BigComplex/Float":      "*values.BigComplex",
		"BigComplex/BigFloat":   "*values.BigComplex",
		"BigComplex/Complex":    "*values.BigComplex",
		"BigComplex/BigComplex": "*values.BigComplex",
	}

	receivers := []struct {
		name  string
		value Number
	}{
		{"Integer", integerA},
		{"BigInteger", bigIntegerA},
		{"Rational", rationalA},
		{"Float", floatA},
		{"BigFloat", bigFloatA},
		{"Complex", complexA},
		{"BigComplex", bigComplexA},
	}

	operands := []struct {
		name  string
		value Number
	}{
		{"Integer", integerB},
		{"BigInteger", bigIntegerB},
		{"Rational", rationalB},
		{"Float", floatB},
		{"BigFloat", bigFloatB},
		{"Complex", complexB},
		{"BigComplex", bigComplexB},
	}

	for _, a := range receivers {
		for _, b := range operands {
			key := a.name + "/" + b.name
			t.Run(key, func(t *testing.T) {
				result := a.value.Divide(b.value)
				actualType := fmt.Sprintf("%T", result)
				expectedType := expectedDiv[key]
				c.Assert(actualType, qt.Equals, expectedType,
					qt.Commentf("Divide: %s / %s", a.name, b.name))
			})
		}
	}
}

// TestNumericTower_ExactnessPreservation verifies that exact + exact = exact
// and exact + inexact = inexact (R7RS §6.2.2 exactness contagion).
func TestNumericTower_ExactnessPreservation(t *testing.T) {
	c := qt.New(t)

	// Exact types
	exactInt := NewInteger(3)
	exactBigInt := NewBigIntegerFromInt64(5)
	exactRational := NewRational(1, 2)
	exactBigComplex := NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2))

	// Inexact types
	inexactFloat := NewFloat(3.0)
	inexactBigFloat := NewBigFloatFromFloat64(5.0)
	inexactComplex := NewComplexFromParts(1.0, 2.0)
	inexactBigComplex := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1), NewBigFloatFromFloat64(2))

	// Test exact + exact = exact
	t.Run("exact+exact=exact", func(t *testing.T) {
		// Integer + Integer
		result := exactInt.Add(exactInt)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("Integer + Integer"))

		// Integer + BigInteger
		result = exactInt.Add(exactBigInt)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("Integer + BigInteger"))

		// Integer + Rational
		result = exactInt.Add(exactRational)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("Integer + Rational"))

		// Rational + Rational
		result = exactRational.Add(exactRational)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("Rational + Rational"))

		// BigComplex(exact) + Integer
		result = exactBigComplex.Add(exactInt)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("BigComplex(exact) + Integer"))

		// BigComplex(exact) + Rational
		result = exactBigComplex.Add(exactRational)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("BigComplex(exact) + Rational"))

		// BigComplex(exact) + BigComplex(exact)
		result = exactBigComplex.Add(exactBigComplex)
		c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("BigComplex(exact) + BigComplex(exact)"))
	})

	// Test exact + inexact = inexact
	t.Run("exact+inexact=inexact", func(t *testing.T) {
		// Integer + Float
		result := exactInt.Add(inexactFloat)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Integer + Float"))

		// Integer + BigFloat
		result = exactInt.Add(inexactBigFloat)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Integer + BigFloat"))

		// Integer + Complex
		result = exactInt.Add(inexactComplex)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Integer + Complex"))

		// Rational + Float
		result = exactRational.Add(inexactFloat)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Rational + Float"))

		// BigComplex(exact) + Float
		result = exactBigComplex.Add(inexactFloat)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("BigComplex(exact) + Float"))

		// BigComplex(exact) + Complex
		result = exactBigComplex.Add(inexactComplex)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("BigComplex(exact) + Complex"))

		// BigComplex(exact) + BigComplex(inexact)
		result = exactBigComplex.Add(inexactBigComplex)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("BigComplex(exact) + BigComplex(inexact)"))
	})

	// Test inexact + inexact = inexact
	t.Run("inexact+inexact=inexact", func(t *testing.T) {
		result := inexactFloat.Add(inexactFloat)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Float + Float"))

		result = inexactComplex.Add(inexactComplex)
		c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("Complex + Complex"))
	})
}

// TestNumericTower_ExactComplexArithmetic verifies that arithmetic with exact complex
// numbers preserves exactness (the key behavior that direct dispatch handles correctly).
func TestNumericTower_ExactComplexArithmetic(t *testing.T) {
	c := qt.New(t)

	// Create exact complex: 3+4i using BigComplex with BigInteger parts
	exactComplex := NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4))
	c.Assert(exactComplex.IsExact(), qt.IsTrue, qt.Commentf("3+4i should be exact"))

	// exact complex + exact integer = exact complex
	t.Run("exact_complex+integer", func(t *testing.T) {
		result := exactComplex.Add(NewInteger(5))
		c.Assert(result.IsExact(), qt.IsTrue)
		// Result should be 8+4i
		bc, ok := result.(*BigComplex)
		c.Assert(ok, qt.IsTrue)
		c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(8))
		c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(4))
	})

	// exact complex + exact rational = exact complex
	t.Run("exact_complex+rational", func(t *testing.T) {
		result := exactComplex.Add(NewRational(1, 2))
		c.Assert(result.IsExact(), qt.IsTrue)
		// Result should be 7/2+4i (3.5+4i as exact)
		bc, ok := result.(*BigComplex)
		c.Assert(ok, qt.IsTrue)
		// Real part should be Rational 7/2
		rat, ok := bc.Real().(*Rational)
		c.Assert(ok, qt.IsTrue)
		c.Assert(rat.Num().Int64(), qt.Equals, int64(7))
		c.Assert(rat.Denom().Int64(), qt.Equals, int64(2))
	})

	// exact complex * exact complex = exact complex
	t.Run("exact_complex*exact_complex", func(t *testing.T) {
		// (3+4i) * (1+2i) = 3 + 6i + 4i + 8i² = 3 + 10i - 8 = -5 + 10i
		other := NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2))
		result := exactComplex.Multiply(other)
		c.Assert(result.IsExact(), qt.IsTrue)
		bc, ok := result.(*BigComplex)
		c.Assert(ok, qt.IsTrue)
		c.Assert(bc.Real().(*BigInteger).Int64(), qt.Equals, int64(-5))
		c.Assert(bc.Imag().(*BigInteger).Int64(), qt.Equals, int64(10))
	})

	// exact complex + inexact float = inexact
	t.Run("exact_complex+float=inexact", func(t *testing.T) {
		result := exactComplex.Add(NewFloat(1.5))
		c.Assert(result.IsExact(), qt.IsFalse)
	})
}

// TestNumericTower_CoverageMatrix prints a coverage matrix for documentation
func TestNumericTower_CoverageMatrix(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping coverage matrix in short mode")
	}

	numbers := makeTestNumbers()
	operations := []struct {
		name string
		op   func(a, b Number) Number
	}{
		{"Add", func(a, b Number) Number { return a.Add(b) }},
		{"Sub", func(a, b Number) Number { return a.Subtract(b) }},
		{"Mul", func(a, b Number) Number { return a.Multiply(b) }},
		{"Div", func(a, b Number) Number { return a.Divide(b) }},
	}

	for _, op := range operations {
		t.Logf("\n=== %s Coverage Matrix ===", op.name)
		header := "Receiver\\Operand"
		for _, n := range numbers {
			header += fmt.Sprintf("\t%s", n.name[:3])
		}
		t.Log(header)

		for _, receiver := range numbers {
			row := receiver.name[:3]
			for _, operand := range numbers {
				result := tryOperation(func() Number {
					return op.op(receiver.value, operand.value)
				})
				switch {
				case result.success:
					row += "\t✓"
				case result.isNil:
					row += "\tNIL"
				default:
					row += "\tPAN"
				}
			}
			t.Log(row)
		}
	}

	// LessThan matrix
	t.Log("\n=== LessThan Coverage Matrix ===")
	header := "Receiver\\Operand"
	for _, n := range numbers {
		header += fmt.Sprintf("\t%s", n.name[:3])
	}
	t.Log(header)

	for _, receiver := range numbers {
		row := receiver.name[:3]
		for _, operand := range numbers {
			var panicked bool
			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
					}
				}()
				_ = receiver.value.LessThan(operand.value)
			}()
			if panicked {
				row += "\tPAN"
			} else {
				row += "\t✓"
			}
		}
		t.Log(row)
	}
}
