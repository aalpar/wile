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
				if result.panicked {
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				} else if result.isNil {
					t.Errorf("NIL (BUG - should panic or succeed)")
				} else {
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

				if result.panicked {
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				} else if result.isNil {
					t.Errorf("NIL (BUG - should panic or succeed)")
				} else {
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

				if result.panicked {
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				} else if result.isNil {
					t.Errorf("NIL (BUG - should panic or succeed)")
				} else {
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

				if result.panicked {
					t.Errorf("PANIC (BUG - missing type case): %s", result.panicMsg)
				} else if result.isNil {
					t.Errorf("NIL (BUG - should panic or succeed)")
				} else {
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
				if result.isNil {
					t.Logf("NIL (BUG) - should panic with ErrDivisionByZero")
					nilCases = append(nilCases, name)
				} else if result.panicked {
					// Expected behavior
					t.Logf("PANIC (correct): %s", result.panicMsg)
				} else if result.success {
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
				if result.success {
					row += "\t✓"
				} else if result.isNil {
					row += "\tNIL"
				} else {
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
