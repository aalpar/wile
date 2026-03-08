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

package bootstrap

import (
	"context"
	"sync"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ===========================================================================
// Independent Top-Level Environments
// ===========================================================================

// TestMultiEnv_PrimitiveMutationIsolation verifies that redefining a primitive
// in one top-level environment does not affect another.
func TestMultiEnv_PrimitiveMutationIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env1, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	env2, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// Redefine + in env1 to always return 0
	_, err = evalScheme(t, env1, `(define + (lambda args 0))`)
	c.Assert(err, qt.IsNil)

	// env1 sees the redefined +
	result, err := evalScheme(t, env1, `(+ 1 2)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(0))

	// env2 still has the original +
	result, err = evalScheme(t, env2, `(+ 1 2)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(3))
}

// TestMultiEnv_SymbolNonIdentityAcrossTopLevels verifies that the same symbol
// name interned in two independent top-level environments produces different
// pointers, since each has its own symbol intern table.
func TestMultiEnv_SymbolNonIdentityAcrossTopLevels(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env1, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	env2, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	sym1 := values.NewSymbol("cross-env-sym")
	sym2 := values.NewSymbol("cross-env-sym")

	// Structurally equal (same name)
	c.Assert(sym1.EqualTo(sym2), qt.IsTrue)

	// But different TopLevelEnvironments
	c.Assert(env1.TopLevelEnv() != env2.TopLevelEnv(), qt.IsTrue)
}

// TestMultiEnv_ConcurrentTopLevelUse verifies that two goroutines can use
// independent top-level environments simultaneously without interference.
func TestMultiEnv_ConcurrentTopLevelUse(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env1, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	env2, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	var wg sync.WaitGroup
	var result1, result2 values.Value
	var err1, err2 error

	wg.Add(2)
	go func() {
		defer wg.Done()
		// Define and compute in env1
		_, err1 = evalScheme(t, env1, `(define x 100)`)
		if err1 != nil {
			return
		}
		result1, err1 = evalScheme(t, env1, `(* x x)`)
	}()
	go func() {
		defer wg.Done()
		// Define and compute in env2
		_, err2 = evalScheme(t, env2, `(define x 200)`)
		if err2 != nil {
			return
		}
		result2, err2 = evalScheme(t, env2, `(* x x)`)
	}()
	wg.Wait()

	c.Assert(err1, qt.IsNil)
	c.Assert(err2, qt.IsNil)
	c.Assert(result1, valuestest.SchemeEquals, values.NewInteger(10000))
	c.Assert(result2, valuestest.SchemeEquals, values.NewInteger(40000))
}

// TestMultiEnv_UserMacroIsolation verifies that defining a macro in one
// top-level environment does not affect another.
func TestMultiEnv_UserMacroIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env1, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	env2, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// Define a macro in env1
	_, err = evalScheme(t, env1, `(define-syntax always-42
		(syntax-rules () ((always-42) 42)))`)
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env1, `(always-42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))

	// env2 does not have the macro
	_, err = evalScheme(t, env2, `(always-42)`)
	c.Assert(err, qt.IsNotNil)
}

// ===========================================================================
// Sibling Library Environments
// ===========================================================================

// TestMultiEnv_SiblingLibraryIsolation verifies that two library environments
// created from the same parent are isolated from each other.
func TestMultiEnv_SiblingLibraryIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib1, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	lib2, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Define in lib1
	_, err = evalScheme(t, lib1, `(define lib1-var 111)`)
	c.Assert(err, qt.IsNil)

	// Define in lib2
	_, err = evalScheme(t, lib2, `(define lib2-var 222)`)
	c.Assert(err, qt.IsNil)

	// lib1 sees its own var
	result, err := evalScheme(t, lib1, `lib1-var`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(111))

	// lib2 sees its own var
	result, err = evalScheme(t, lib2, `lib2-var`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(222))

	// lib1 does NOT see lib2's var
	_, err = evalScheme(t, lib1, `lib2-var`)
	c.Assert(err, qt.IsNotNil)

	// lib2 does NOT see lib1's var
	_, err = evalScheme(t, lib2, `lib1-var`)
	c.Assert(err, qt.IsNotNil)

	// Parent sees neither
	_, err = evalScheme(t, parent, `lib1-var`)
	c.Assert(err, qt.IsNotNil)
	_, err = evalScheme(t, parent, `lib2-var`)
	c.Assert(err, qt.IsNotNil)
}

// TestMultiEnv_SiblingLibrarySymbolIdentity verifies that symbols interned
// in sibling library environments are pointer-identical.
func TestMultiEnv_SiblingLibrarySymbolIdentity(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	_, err = NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	_, err = NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	sym1 := values.NewSymbol("shared-sym")
	sym2 := values.NewSymbol("shared-sym")

	// Symbols with same key are structurally equal
	c.Assert(sym1.EqualTo(sym2), qt.IsTrue, qt.Commentf("symbols with same key must be structurally equal"))
}

// TestMultiEnv_SiblingLibrarySharedPrimitives verifies that sibling libraries
// both have access to the same primitives.
func TestMultiEnv_SiblingLibrarySharedPrimitives(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib1, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	lib2, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Both libraries can use core primitives
	result, err := evalScheme(t, lib1, `(+ 10 20)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(30))

	result, err = evalScheme(t, lib2, `(+ 10 20)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(30))

	// Both libraries can use bootstrap macros
	result, err = evalScheme(t, lib1, `(let ((x 5)) (* x x))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(25))

	result, err = evalScheme(t, lib2, `(let ((x 5)) (* x x))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(25))
}

// TestMultiEnv_SiblingLibraryMacroIsolation verifies that a macro defined in
// one library does not leak to its sibling or parent.
func TestMultiEnv_SiblingLibraryMacroIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib1, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	lib2, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Define a macro in lib1
	_, err = evalScheme(t, lib1, `(define-syntax double
		(syntax-rules () ((double x) (+ x x))))`)
	c.Assert(err, qt.IsNil)

	// lib1 can use it
	result, err := evalScheme(t, lib1, `(double 21)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))

	// lib2 cannot
	_, err = evalScheme(t, lib2, `(double 21)`)
	c.Assert(err, qt.IsNotNil)

	// Parent cannot
	_, err = evalScheme(t, parent, `(double 21)`)
	c.Assert(err, qt.IsNotNil)
}

// ===========================================================================
// Nested Library Environments
// ===========================================================================

// TestMultiEnv_NestedLibraryCreation verifies that creating a library
// environment from another library environment (the real use case when a
// library imports another library) works correctly.
func TestMultiEnv_NestedLibraryCreation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	topLevel, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	outerLib, err := NewLibraryEnvironmentFrame(ctx, topLevel, nil)
	c.Assert(err, qt.IsNil)

	// Create a nested library from the outer library (simulates library import)
	innerLib, err := NewLibraryEnvironmentFrame(ctx, outerLib, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(innerLib, qt.IsNotNil)

	// All three share the same TopLevelEnvironment
	c.Assert(innerLib.TopLevelEnv(), qt.Equals, outerLib.TopLevelEnv())
	c.Assert(innerLib.TopLevelEnv(), qt.Equals, topLevel.TopLevelEnv())
}

// TestMultiEnv_NestedLibrarySymbolIdentity verifies that symbol interning
// works across three levels: top-level -> outer library -> inner library.
func TestMultiEnv_NestedLibrarySymbolIdentity(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	topLevel, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	outerLib, err := NewLibraryEnvironmentFrame(ctx, topLevel, nil)
	c.Assert(err, qt.IsNil)

	_, err = NewLibraryEnvironmentFrame(ctx, outerLib, nil)
	c.Assert(err, qt.IsNil)

	sym1 := values.NewSymbol("nested-sym")
	sym2 := values.NewSymbol("nested-sym")

	c.Assert(sym1.EqualTo(sym2), qt.IsTrue)
}

// TestMultiEnv_NestedLibraryBindingIsolation verifies that bindings at each
// level (top-level, outer library, inner library) are fully isolated.
func TestMultiEnv_NestedLibraryBindingIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	topLevel, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	outerLib, err := NewLibraryEnvironmentFrame(ctx, topLevel, nil)
	c.Assert(err, qt.IsNil)

	innerLib, err := NewLibraryEnvironmentFrame(ctx, outerLib, nil)
	c.Assert(err, qt.IsNil)

	// Define at each level
	_, err = evalScheme(t, topLevel, `(define level 'top)`)
	c.Assert(err, qt.IsNil)
	_, err = evalScheme(t, outerLib, `(define level 'outer)`)
	c.Assert(err, qt.IsNil)
	_, err = evalScheme(t, innerLib, `(define level 'inner)`)
	c.Assert(err, qt.IsNil)

	// Each sees its own binding
	result, err := evalScheme(t, topLevel, `level`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("top"))

	result, err = evalScheme(t, outerLib, `level`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("outer"))

	result, err = evalScheme(t, innerLib, `level`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("inner"))
}

// TestMultiEnv_NestedLibraryPrimitivesAvailable verifies that primitives and
// macros are available at all nesting levels.
func TestMultiEnv_NestedLibraryPrimitivesAvailable(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	topLevel, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	outerLib, err := NewLibraryEnvironmentFrame(ctx, topLevel, nil)
	c.Assert(err, qt.IsNil)

	innerLib, err := NewLibraryEnvironmentFrame(ctx, outerLib, nil)
	c.Assert(err, qt.IsNil)

	// Inner library has access to primitives and bootstrap macros
	result, err := evalScheme(t, innerLib, `(+ 1 2 3)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(6))

	result, err = evalScheme(t, innerLib, `(let ((x 10)) (cond ((> x 5) 'big) (else 'small)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewSymbol("big"))
}

// ===========================================================================
// Cross-Environment Value Passing
// ===========================================================================

// TestMultiEnv_ValuesCrossEnvironmentBoundary verifies that values created in
// one environment can be used in another that shares the same TopLevelEnvironment.
func TestMultiEnv_ValuesCrossEnvironmentBoundary(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Create a list in the parent
	_, err = evalScheme(t, parent, `(define data '(1 2 3))`)
	c.Assert(err, qt.IsNil)

	// Read it from parent, pass it to library via Go-level binding
	parentResult, err := evalScheme(t, parent, `data`)
	c.Assert(err, qt.IsNil)

	// Bind it in the library environment via direct global binding
	dataSym := values.NewSymbol("imported-data")
	gi, _ := lib.MaybeCreateOwnGlobalBinding(dataSym, environment.BindingTypeVariable)
	err = lib.SetOwnGlobalValue(gi, parentResult)
	c.Assert(err, qt.IsNil)

	// Library can operate on it
	result, err := evalScheme(t, lib, `(length imported-data)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(3))

	result, err = evalScheme(t, lib, `(car imported-data)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(1))
}

// TestMultiEnv_ClosureCapturesDefiningEnvironment verifies that a closure
// created in one environment resolves bindings in its defining environment,
// even when called from a different environment.
func TestMultiEnv_ClosureCapturesDefiningEnvironment(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Define a variable and a closure in the parent
	_, err = evalScheme(t, parent, `(define parent-x 100)`)
	c.Assert(err, qt.IsNil)
	_, err = evalScheme(t, parent, `(define get-parent-x (lambda () parent-x))`)
	c.Assert(err, qt.IsNil)

	// Read the closure from parent
	closureVal, err := evalScheme(t, parent, `get-parent-x`)
	c.Assert(err, qt.IsNil)

	// Bind the closure in the library
	fnSym := values.NewSymbol("get-parent-x")
	gi, _ := lib.MaybeCreateOwnGlobalBinding(fnSym, environment.BindingTypeVariable)
	err = lib.SetOwnGlobalValue(gi, closureVal)
	c.Assert(err, qt.IsNil)

	// Call it from the library — it should resolve parent-x in the parent env
	result, err := evalScheme(t, lib, `(get-parent-x)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(100))

	// Mutate parent-x in the parent and call again from library
	_, err = evalScheme(t, parent, `(set! parent-x 200)`)
	c.Assert(err, qt.IsNil)

	result, err = evalScheme(t, lib, `(get-parent-x)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(200))
}

// TestMultiEnv_ParameterObjectAcrossEnvironments verifies that a parameter
// object created in one environment can be parameterized in another.
func TestMultiEnv_ParameterObjectAcrossEnvironments(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Create a parameter in the parent
	_, err = evalScheme(t, parent, `(define my-param (make-parameter 10))`)
	c.Assert(err, qt.IsNil)

	// Read the parameter object
	paramVal, err := evalScheme(t, parent, `my-param`)
	c.Assert(err, qt.IsNil)

	// Bind it in the library
	paramSym := values.NewSymbol("my-param")
	gi, _ := lib.MaybeCreateOwnGlobalBinding(paramSym, environment.BindingTypeVariable)
	err = lib.SetOwnGlobalValue(gi, paramVal)
	c.Assert(err, qt.IsNil)

	// Library sees the default value
	result, err := evalSchemeEscape(t, lib, `(my-param)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(10))

	// Library can parameterize it
	result, err = evalSchemeEscape(t, lib, `(parameterize ((my-param 99)) (my-param))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(99))

	// After parameterize scope, parent still sees the default
	result, err = evalSchemeEscape(t, parent, `(my-param)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(10))
}

// ===========================================================================
// Concurrent Library Environment Use
// ===========================================================================

// TestMultiEnv_ConcurrentLibraryUse verifies that multiple goroutines can
// use sibling library environments concurrently.
func TestMultiEnv_ConcurrentLibraryUse(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	const numLibs = 4
	libs := make([]*environment.EnvironmentFrame, numLibs)
	for i := range libs {
		libs[i], err = NewLibraryEnvironmentFrame(ctx, parent, nil)
		c.Assert(err, qt.IsNil)
	}

	results := make([]values.Value, numLibs)
	errs := make([]error, numLibs)

	var wg sync.WaitGroup
	wg.Add(numLibs)
	for i := range numLibs {
		go func(idx int) {
			defer wg.Done()
			// Each goroutine defines and uses its own variable
			_, errs[idx] = evalScheme(t, libs[idx],
				`(define n `+string(rune('1'+idx))+`)`)
			if errs[idx] != nil {
				return
			}
			results[idx], errs[idx] = evalScheme(t, libs[idx], `(* n n)`)
		}(i)
	}
	wg.Wait()

	for i := range numLibs {
		c.Assert(errs[i], qt.IsNil, qt.Commentf("goroutine %d", i))
		expected := int64((i + 1) * (i + 1))
		c.Assert(results[i], valuestest.SchemeEquals, values.NewInteger(expected),
			qt.Commentf("goroutine %d", i))
	}
}

// ===========================================================================
// Library Environment Primitive Availability
// ===========================================================================

// TestMultiEnv_LibraryPrimitiveAvailability systematically verifies that a
// library environment has access to every category of primitive and extension
// registered by the runtime. One representative expression per registration
// category ensures the wiring is complete.
//
// Categories map to the registration functions in registry/core/register.go
// and the extensions in runtime/environment_tiny.go.
func TestMultiEnv_LibraryPrimitiveAvailability(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	parent, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	lib, err := NewLibraryEnvironmentFrame(ctx, parent, nil)
	c.Assert(err, qt.IsNil)

	// Core registration categories (registry/core/register.go)
	coreCases := []roundTripCase{
		// addPredicates
		{"predicate/null?", `(null? '())`, values.TrueValue},
		{"predicate/pair?", `(pair? '(1))`, values.TrueValue},
		{"predicate/number?", `(number? 42)`, values.TrueValue},
		{"predicate/string?", `(string? "x")`, values.TrueValue},
		{"predicate/zero?", `(zero? 0)`, values.TrueValue},

		// addEquality + addBoolean
		{"equality/eq?", `(eq? 'a 'a)`, values.TrueValue},
		{"equality/eqv?", `(eqv? 1 1)`, values.TrueValue},
		{"equality/equal?", `(equal? '(1 2) '(1 2))`, values.TrueValue},
		{"boolean/not", `(not #f)`, values.TrueValue},

		// addPairs
		{"pairs/cons", `(car (cons 1 2))`, values.NewInteger(1)},
		{"pairs/cdr", `(cdr (cons 1 2))`, values.NewInteger(2)},
		{"pairs/caar", `(caar '((10)))`, values.NewInteger(10)},

		// addLists
		{"lists/length", `(length '(a b c))`, values.NewInteger(3)},
		{"lists/append", `(car (append '(1) '(2)))`, values.NewInteger(1)},
		{"lists/reverse", `(car (reverse '(1 2 3)))`, values.NewInteger(3)},
		{"lists/memq", `(pair? (memq 'b '(a b c)))`, values.TrueValue},
		{"lists/assq", `(pair? (assq 'b '((a 1) (b 2))))`, values.TrueValue},

		// addArithmetic
		{"arith/add", `(+ 1 2 3)`, values.NewInteger(6)},
		{"arith/sub", `(- 10 3)`, values.NewInteger(7)},
		{"arith/mul", `(* 4 5)`, values.NewInteger(20)},
		{"arith/div", `(/ 10 2)`, values.NewInteger(5)},
		{"arith/abs", `(abs -7)`, values.NewInteger(7)},
		{"arith/min", `(min 3 1 4)`, values.NewInteger(1)},
		{"arith/max", `(max 3 1 4)`, values.NewInteger(4)},
		{"arith/gcd", `(gcd 12 8)`, values.NewInteger(4)},
		{"arith/quotient", `(quotient 7 2)`, values.NewInteger(3)},
		{"arith/exact", `(exact? 1)`, values.TrueValue},

		// addControl (runtime-only phase)
		{"control/apply", `(apply + '(1 2 3))`, values.NewInteger(6)},

		// addVectors
		{"vectors/vector-ref", `(vector-ref (vector 10 20 30) 1)`, values.NewInteger(20)},
		{"vectors/vector-length", `(vector-length (make-vector 5 0))`, values.NewInteger(5)},

		// addStrings
		{"strings/string-length", `(string-length "hello")`, values.NewInteger(5)},
		{"strings/string-append", `(string-append "a" "b")`, values.NewString("ab")},
		{"strings/substring", `(substring "hello" 1 3)`, values.NewString("el")},
		{"strings/number->string", `(number->string 42)`, values.NewString("42")},
		{"strings/string->number", `(string->number "42")`, values.NewInteger(42)},

		// addCharacters
		{"chars/char->integer", `(char->integer #\A)`, values.NewInteger(65)},
		{"chars/integer->char", `(integer->char 65)`, values.NewCharacter('A')},

		// addBytevectors
		{"bytevec/length", `(bytevector-length (make-bytevector 3 0))`, values.NewInteger(3)},
		{"bytevec/u8-ref", `(bytevector-u8-ref (bytevector 10 20 30) 1)`, values.NewInteger(20)},

		// addParameters (runtime-only phase)
		{"params/make-parameter", `(parameter? (make-parameter 10))`, values.TrueValue},

		// addPrompts (runtime-only phase)
		{"prompts/make-tag", `(continuation-prompt-tag? (make-continuation-prompt-tag))`, values.TrueValue},
		{"prompts/default-tag", `(continuation-prompt-tag? (default-continuation-prompt-tag))`, values.TrueValue},

		// addBoxes
		{"boxes/unbox", `(unbox (box 42))`, values.NewInteger(42)},
		{"boxes/box?", `(box? (box 1))`, values.TrueValue},

		// addHashtables
		{"hashtables/make", `(hashtable? (make-hashtable))`, values.TrueValue},
		{"hashtables/set-ref", `(let ((ht (make-hashtable))) (hashtable-set! ht 'k 99) (hashtable-ref ht 'k #f))`, values.NewInteger(99)},
	}

	// Extension categories (extensions/*)
	extensionCases := []roundTripCase{
		// extensions/io
		{"ext-io/display", `(let ((p (open-output-string))) (display "hi" p) (get-output-string p))`, values.NewString("hi")},
		{"ext-io/read", `(let ((p (open-input-string "42"))) (read p))`, values.NewInteger(42)},
		{"ext-io/write-char", `(let ((p (open-output-string))) (write-char #\Z p) (get-output-string p))`, values.NewString("Z")},
		{"ext-io/port?", `(port? (open-input-string "x"))`, values.TrueValue},
		{"ext-io/eof", `(eof-object? (read (open-input-string "")))`, values.TrueValue},

		// extensions/math
		{"ext-math/sqrt", `(= (sqrt 4) 2.0)`, values.TrueValue},
		{"ext-math/expt", `(expt 2 10)`, values.NewInteger(1024)},
		{"ext-math/floor", `(= (floor 3.7) 3.0)`, values.TrueValue},
		{"ext-math/sin", `(= (sin 0) 0.0)`, values.TrueValue},
		{"ext-math/exp", `(= (exp 0) 1.0)`, values.TrueValue},

		// extensions/exceptions
		{"ext-exc/error-object?", `(not (error-object? 42))`, values.TrueValue},
		{"ext-exc/read-error?", `(not (read-error? 42))`, values.TrueValue},
		{"ext-exc/file-error?", `(not (file-error? 42))`, values.TrueValue},

		// extensions/threads (SRFI-18)
		{"ext-threads/thread?", `(thread? (make-thread (lambda () 42)))`, values.TrueValue},
		{"ext-threads/mutex?", `(mutex? (make-mutex))`, values.TrueValue},
		{"ext-threads/condvar?", `(condition-variable? (make-condition-variable))`, values.TrueValue},
		{"ext-threads/time?", `(time? (current-time))`, values.TrueValue},

		// extensions/gointerop
		{"ext-gointerop/channel?", `(channel? (make-channel))`, values.TrueValue},
		{"ext-gointerop/atomic?", `(atomic? (make-atomic 0))`, values.TrueValue},
		{"ext-gointerop/wait-group?", `(wait-group? (make-wait-group))`, values.TrueValue},
		{"ext-gointerop/rw-mutex?", `(rw-mutex? (make-rw-mutex))`, values.TrueValue},
		{"ext-gointerop/once?", `(once? (make-once))`, values.TrueValue},

		// extensions/all (records, promises)
		{"ext-all/promise?", `(promise? (delay 1))`, values.TrueValue},

		// extensions/system
		{"ext-system/features", `(list? (features))`, values.TrueValue},
		{"ext-system/jiffies", `(> (current-jiffy) 0)`, values.TrueValue},
	}

	// Bootstrap macros (from addBootstrapMacros)
	bootstrapCases := []roundTripCase{
		{"macro/and", `(and #t #t #t)`, values.TrueValue},
		{"macro/or", `(or #f #f 42)`, values.NewInteger(42)},
		{"macro/let", `(let ((x 5)) x)`, values.NewInteger(5)},
		{"macro/let*", `(let* ((a 1) (b (+ a 1))) b)`, values.NewInteger(2)},
		{"macro/letrec", `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, values.NewInteger(120)},
		{"macro/cond", `(cond (#f 1) (#t 2))`, values.NewInteger(2)},
		{"macro/case", `(case 2 ((1) 'a) ((2) 'b))`, values.NewSymbol("b")},
		{"macro/when", `(when #t 42)`, values.NewInteger(42)},
		{"macro/unless", `(unless #f 42)`, values.NewInteger(42)},
		{"macro/do", `(do ((i 0 (+ i 1))) ((= i 3) i))`, values.NewInteger(3)},
	}

	allCases := make([]roundTripCase, 0, len(coreCases)+len(extensionCases)+len(bootstrapCases))
	allCases = append(allCases, coreCases...)
	allCases = append(allCases, extensionCases...)
	allCases = append(allCases, bootstrapCases...)

	for _, tt := range allCases {
		c.Run(tt.name, func(c *qt.C) {
			result, err := evalScheme(t, lib, tt.code)
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tt.code))
			c.Assert(result, valuestest.SchemeEquals, tt.expected, qt.Commentf("code: %s", tt.code))
		})
	}
}

// TestMultiEnv_NestedLibraryPrimitiveAvailability verifies that a library
// created from another library (the nested import case) has the same
// primitive availability as a library created directly from the top level.
func TestMultiEnv_NestedLibraryPrimitiveAvailability(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	topLevel, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	outerLib, err := NewLibraryEnvironmentFrame(ctx, topLevel, nil)
	c.Assert(err, qt.IsNil)

	innerLib, err := NewLibraryEnvironmentFrame(ctx, outerLib, nil)
	c.Assert(err, qt.IsNil)

	// One representative per major category, tested at the innermost nesting level.
	cases := []struct {
		name string
		code string
	}{
		// Core
		{"core/arithmetic", `(+ 1 2 3)`},
		{"core/predicates", `(null? '())`},
		{"core/pairs", `(car (cons 1 2))`},
		{"core/lists", `(length '(a b c))`},
		{"core/strings", `(string-length "abc")`},
		{"core/chars", `(char->integer #\A)`},
		{"core/vectors", `(vector-ref (vector 1) 0)`},
		{"core/bytevectors", `(bytevector-length (bytevector 1 2 3))`},
		{"core/boxes", `(unbox (box 42))`},
		{"core/hashtables", `(hashtable? (make-hashtable))`},
		{"core/apply", `(apply + '(1 2))`},
		{"core/parameters", `(parameter? (make-parameter 1))`},
		{"core/prompts", `(continuation-prompt-tag? (make-continuation-prompt-tag))`},
		// Extensions
		{"ext/io", `(get-output-string (open-output-string))`},
		{"ext/math", `(expt 2 3)`},
		{"ext/exceptions", `(error-object? 42)`},
		{"ext/threads", `(mutex? (make-mutex))`},
		{"ext/gointerop", `(channel? (make-channel))`},
		{"ext/promises", `(promise? (delay 1))`},
		{"ext/system", `(list? (features))`},
		// Bootstrap macros
		{"macro/and", `(and 1 2 3)`},
		{"macro/let", `(let ((x 1)) x)`},
		{"macro/cond", `(cond (#t 1))`},
		{"macro/when", `(when #t 1)`},
		{"macro/do", `(do ((i 0 (+ i 1))) ((= i 1) i))`},
	}

	for _, tt := range cases {
		c.Run(tt.name, func(c *qt.C) {
			_, err := evalScheme(t, innerLib, tt.code)
			c.Assert(err, qt.IsNil, qt.Commentf("code: %s", tt.code))
		})
	}
}
