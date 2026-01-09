// Copyright 2025 Aaron Alpar
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

package primitives_test

import (
	"context"
	"testing"

	"wile/runtime"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestGenerateTemporaries_CorrectCount(t *testing.T) {
	result, err := runSchemeCode(t, "(generate-temporaries '(a b c))")
	qt.Assert(t, err, qt.IsNil)

	pair, ok := result.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)

	// Count elements
	count := 0
	for !pair.IsEmptyList() {
		count++
		cdr := pair.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		pair = cdr.(*values.Pair)
	}
	qt.Assert(t, count, qt.Equals, 3)
}

func TestGenerateTemporaries_ReturnsSyntaxSymbols(t *testing.T) {
	result, err := runSchemeCode(t, "(generate-temporaries '(x))")
	qt.Assert(t, err, qt.IsNil)

	pair, ok := result.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)

	car := pair.Car()
	_, ok = car.(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestGenerateTemporaries_EmptyList(t *testing.T) {
	result, err := runSchemeCode(t, "(generate-temporaries '())")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, values.IsEmptyList(result), qt.IsTrue)
}

func TestGenerateTemporaries_AllUnique(t *testing.T) {
	result, err := runSchemeCode(t, "(generate-temporaries '(a b c d e))")
	qt.Assert(t, err, qt.IsNil)

	// Collect all symbol names
	names := make(map[string]bool)
	pair := result.(*values.Pair)
	for !pair.IsEmptyList() {
		sym := pair.Car().(*syntax.SyntaxSymbol)
		name := sym.Sym.Key
		qt.Assert(t, names[name], qt.IsFalse, qt.Commentf("duplicate name: %s", name))
		names[name] = true

		cdr := pair.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		pair = cdr.(*values.Pair)
	}
	qt.Assert(t, len(names), qt.Equals, 5)
}

func TestGenerateTemporaries_UniqueAcrossCalls(t *testing.T) {
	result1, err := runSchemeCode(t, "(generate-temporaries '(a b))")
	qt.Assert(t, err, qt.IsNil)

	result2, err := runSchemeCode(t, "(generate-temporaries '(x y))")
	qt.Assert(t, err, qt.IsNil)

	// Collect names from both results
	names := make(map[string]bool)

	pair1 := result1.(*values.Pair)
	for !pair1.IsEmptyList() {
		sym := pair1.Car().(*syntax.SyntaxSymbol)
		names[sym.Sym.Key] = true
		cdr := pair1.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		pair1 = cdr.(*values.Pair)
	}

	pair2 := result2.(*values.Pair)
	for !pair2.IsEmptyList() {
		sym := pair2.Car().(*syntax.SyntaxSymbol)
		name := sym.Sym.Key
		qt.Assert(t, names[name], qt.IsFalse, qt.Commentf("name from second call duplicates first: %s", name))
		names[name] = true
		cdr := pair2.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		pair2 = cdr.(*values.Pair)
	}

	qt.Assert(t, len(names), qt.Equals, 4)
}

func TestGenerateTemporaries_GensymPrefix(t *testing.T) {
	result, err := runSchemeCode(t, "(generate-temporaries '(a))")
	qt.Assert(t, err, qt.IsNil)

	pair := result.(*values.Pair)
	sym := pair.Car().(*syntax.SyntaxSymbol)
	qt.Assert(t, sym.Sym.Key[0], qt.Equals, byte('g'))
}

func TestGenerateTemporaries_PrimitiveExists(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	sym := env.InternSymbol(values.NewSymbol("generate-temporaries"))
	bnd := env.GetBinding(sym)
	qt.Assert(t, bnd, qt.IsNotNil)
}
