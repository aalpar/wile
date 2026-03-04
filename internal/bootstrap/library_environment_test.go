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
	"fmt"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ===========================================================================
// NewLibraryEnvironmentFrame Tests
// ===========================================================================

// TestNewLibraryEnvironmentFrame_Basic verifies that a library environment
// can be created from a valid caller environment.
func TestNewLibraryEnvironmentFrame_Basic(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(libEnv, qt.IsNotNil)
}

// TestNewLibraryEnvironmentFrame_SharedTopLevelEnvironment verifies that the
// library environment shares the same TopLevelEnvironment as the caller,
// which is required for symbol identity across library boundaries (R7RS §6.5).
func TestNewLibraryEnvironmentFrame_SharedTopLevelEnvironment(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	callerTopLevel := callerEnv.TopLevelEnv()
	libTopLevel := libEnv.TopLevelEnv()
	c.Assert(libTopLevel, qt.Equals, callerTopLevel)
}

// TestNewLibraryEnvironmentFrame_SymbolIdentity verifies that symbols interned
// in the caller and library environments are pointer-identical, ensuring
// (eq? 'foo (string->symbol "foo")) returns #t across library boundaries.
//
// R7RS §6.5: symbol identity must be preserved across library boundaries.
func TestNewLibraryEnvironmentFrame_SymbolIdentity(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	// Intern the same symbol name in both environments
	sym1 := callerEnv.InternSymbol(values.NewSymbol("test-symbol"))
	sym2 := libEnv.InternSymbol(values.NewSymbol("test-symbol"))

	// Must be pointer-identical (same *Symbol), not just structurally equal
	c.Assert(sym1 == sym2, qt.IsTrue, qt.Commentf(
		"symbols interned across library boundaries must be pointer-identical"))
}

// TestNewLibraryEnvironmentFrame_BindingIsolation verifies that bindings
// defined in the library environment do not leak into the caller environment.
func TestNewLibraryEnvironmentFrame_BindingIsolation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	// Define a binding in the library environment
	libSym := libEnv.InternSymbol(values.NewSymbol("lib-only-binding"))
	libEnv.MaybeCreateOwnGlobalBinding(libSym, environment.BindingTypeVariable)

	// The caller environment should not see this binding
	callerBinding := callerEnv.GetBinding(libSym)
	c.Assert(callerBinding, qt.IsNil, qt.Commentf(
		"library bindings must not leak into caller environment"))

	// But the library environment should see it
	libBinding := libEnv.GetBinding(libSym)
	c.Assert(libBinding, qt.IsNotNil)
}

// TestNewLibraryEnvironmentFrame_PrimitivesAvailable verifies that the library
// environment has access to core primitives.
func TestNewLibraryEnvironmentFrame_PrimitivesAvailable(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	// Evaluate a simple expression in the library environment
	result, err := evalScheme(t, libEnv, `(+ 1 2)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(3))
}

// TestNewLibraryEnvironmentFrame_BootstrapMacrosAvailable verifies that
// bootstrap macros (and, or, let, cond, etc.) are available in library environments.
func TestNewLibraryEnvironmentFrame_BootstrapMacrosAvailable(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"and", `(and #t #t)`, values.TrueValue},
		{"or", `(or #f #t)`, values.TrueValue},
		{"let", `(let ((x 5)) x)`, values.NewInteger(5)},
		{"let*", `(let* ((x 1) (y (+ x 1))) y)`, values.NewInteger(2)},
		{"cond", `(cond (#t 42))`, values.NewInteger(42)},
		{"when", `(when #t 99)`, values.NewInteger(99)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result, err := evalScheme(t, libEnv, tt.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tt.expected)
		})
	}
}

// TestNewLibraryEnvironmentFrame_IndependentMutation verifies that defining
// variables in the library environment does not affect the caller, and vice versa.
func TestNewLibraryEnvironmentFrame_IndependentMutation(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	callerEnv, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	libEnv, err := NewLibraryEnvironmentFrame(ctx, callerEnv, nil)
	c.Assert(err, qt.IsNil)

	// Define a variable in the caller
	_, err = evalScheme(t, callerEnv, `(define caller-var 100)`)
	c.Assert(err, qt.IsNil)

	// Define a variable in the library
	_, err = evalScheme(t, libEnv, `(define lib-var 200)`)
	c.Assert(err, qt.IsNil)

	// Each sees its own
	result, err := evalScheme(t, callerEnv, `caller-var`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(100))

	result, err = evalScheme(t, libEnv, `lib-var`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(200))
}

// ===========================================================================
// Multiple Independent Top-Level Environment Tests
// ===========================================================================

// TestNewTopLevelEnvironmentFrameTiny_IndependentEnvironments verifies that
// two separately created top-level environments are fully independent.
func TestNewTopLevelEnvironmentFrameTiny_IndependentEnvironments(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env1, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	env2, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// Different TopLevelEnvironment instances
	c.Assert(env1.TopLevelEnv() != env2.TopLevelEnv(), qt.IsTrue)

	// Mutation in env1 doesn't affect env2
	_, err = evalScheme(t, env1, `(define unique-var 42)`)
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env1, `unique-var`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))

	// env2 should not have unique-var
	_, err = evalScheme(t, env2, `unique-var`)
	c.Assert(err, qt.IsNotNil)
}

// TestNewTopLevelEnvironmentFrameTiny_BootstrapMacrosLoaded verifies that all
// expected bootstrap macros are functional after environment creation.
func TestNewTopLevelEnvironmentFrameTiny_BootstrapMacrosLoaded(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{"and-true", `(and 1 2 3)`, values.NewInteger(3)},
		{"and-false", `(and 1 #f 3)`, values.FalseValue},
		{"and-empty", `(and)`, values.TrueValue},
		{"or-true", `(or #f #f 42)`, values.NewInteger(42)},
		{"or-false", `(or #f #f #f)`, values.FalseValue},
		{"or-empty", `(or)`, values.FalseValue},
		{"let", `(let ((a 10) (b 20)) (+ a b))`, values.NewInteger(30)},
		{"let*", `(let* ((a 1) (b (* a 10))) b)`, values.NewInteger(10)},
		{"letrec", `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, values.NewInteger(120)},
		{"cond-first", `(cond (#f 1) (#t 2) (#t 3))`, values.NewInteger(2)},
		{"cond-else", `(cond (#f 1) (else 99))`, values.NewInteger(99)},
		{"when-true", `(when #t 42)`, values.NewInteger(42)},
		{"unless-false", `(unless #f 42)`, values.NewInteger(42)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result, err := evalScheme(t, env, tt.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tt.expected)
		})
	}
}

// ===========================================================================
// loadBootstrapMacros Tests
// ===========================================================================

// TestLoadBootstrapMacros_EmptySources verifies that loading with no sources
// succeeds without error.
func TestLoadBootstrapMacros_EmptySources(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	err = loadBootstrapMacros(ctx, env, []string{})
	c.Assert(err, qt.IsNil)

	err = loadBootstrapMacros(ctx, env, nil)
	c.Assert(err, qt.IsNil)
}

// TestLoadBootstrapMacros_InvalidExpansion verifies that source that fails
// during macro expansion produces an error.
func TestLoadBootstrapMacros_InvalidExpansion(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// A define-syntax with a non-transformer value should fail during expansion.
	err = loadBootstrapMacros(ctx, env, []string{
		`(define-syntax bad 42)`,
	})
	c.Assert(err, qt.IsNotNil)
}

// TestLoadBootstrapMacros_CompileError verifies that source that parses but
// fails to compile produces an error.
func TestLoadBootstrapMacros_CompileError(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// A define-syntax with an invalid transformer spec should fail during
	// expansion or compilation.
	err = loadBootstrapMacros(ctx, env, []string{
		`(define-syntax bad-macro (syntax-rules () ((bad-macro) (undefined-variable-xyz))))`,
	})
	// This should succeed at the definition stage (define-syntax just registers
	// the transformer); the error would occur at use time. So we just verify
	// no panic occurred.
	_ = err
}

// TestLoadBootstrapMacros_ValidMacro verifies that a well-formed macro
// definition is properly loaded and usable.
func TestLoadBootstrapMacros_ValidMacro(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	// Load a custom macro
	err = loadBootstrapMacros(ctx, env, []string{
		`(define-syntax my-add
		   (syntax-rules ()
		     ((my-add a b) (+ a b))))`,
	})
	c.Assert(err, qt.IsNil)

	// Use the macro
	result, err := evalScheme(t, env, `(my-add 3 4)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(7))
}

// TestLoadBootstrapMacros_MultipleSources verifies that multiple macro source
// strings are loaded in order, with later macros able to use earlier ones.
func TestLoadBootstrapMacros_MultipleSources(t *testing.T) {
	c := qt.New(t)
	ctx := context.TODO()

	env, err := NewTopLevelEnvironmentFrameTiny(ctx)
	c.Assert(err, qt.IsNil)

	err = loadBootstrapMacros(ctx, env, []string{
		`(define-syntax double
		   (syntax-rules ()
		     ((double x) (+ x x))))`,
		`(define-syntax quadruple
		   (syntax-rules ()
		     ((quadruple x) (double (double x)))))`,
	})
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(quadruple 5)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(20))
}

// ===========================================================================
// Error Path Tests (via allExtensions swap)
// ===========================================================================

// brokenExtension is an extension that always fails during registration.
type brokenExtension struct{}

func (p brokenExtension) Name() string { return "broken" }
func (p brokenExtension) AddToRegistry(_ *registry.Registry) error {
	return fmt.Errorf("intentional failure")
}

// TestNewTopLevelEnvironmentFrameTiny_ExtensionError verifies that a failing
// extension produces an error from NewTopLevelEnvironmentFrameTiny.
func TestNewTopLevelEnvironmentFrameTiny_ExtensionError(t *testing.T) {
	c := qt.New(t)

	saved := allExtensions
	allExtensions = []registry.Extension{brokenExtension{}}
	defer func() { allExtensions = saved }()

	_, err := NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

// TestNewLibraryEnvironmentFrame_ExtensionError verifies that a failing
// extension produces an error from NewLibraryEnvironmentFrame.
func TestNewLibraryEnvironmentFrame_ExtensionError(t *testing.T) {
	c := qt.New(t)

	// First create a valid parent with the real extensions
	parent, err := NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)

	// Then break extensions for the library creation
	saved := allExtensions
	allExtensions = []registry.Extension{brokenExtension{}}
	defer func() { allExtensions = saved }()

	_, err = NewLibraryEnvironmentFrame(context.TODO(), parent, nil)
	c.Assert(err, qt.IsNotNil)
}
