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

package wile

import (
	"context"
	"fmt"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"

	qt "github.com/frankban/quicktest"
)

// TestInitOrder_UnbootstrappedNamespace verifies that a namespace with a
// registry but no bootstrap (no applyBaseEnvironment call) produces an
// error when trying to evaluate code that depends on primitive bindings.
//
// NewEngine accepts such a namespace (it only checks that a registry exists),
// but evaluation must fail because primitive bindings (like "+") are installed
// by applyBaseEnvironment during bootstrap. Core syntax forms (if, define,
// lambda) are hardcoded in the compiler's type switch and work without
// bootstrap, but any expression referencing a primitive will fail.
func TestInitOrder_UnbootstrappedNamespace(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	c.Assert(err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)
	// Deliberately skip applyBaseEnvironment — no syntax compilers, no
	// primitive bindings, no bootstrap macros.

	eng, err := NewEngine(ctx, WithNamespace(ns))
	c.Assert(err, qt.IsNil)

	// Core forms like "if" are hardcoded in the compiler's type switch, so
	// (if #t 1 2) compiles without bootstrap. But primitive bindings like "+"
	// are installed by applyBaseEnvironment, so any expression referencing a
	// primitive must fail.
	expr, parseErr := eng.Parse(ctx, "(+ 1 2)")
	c.Assert(parseErr, qt.IsNil)

	// Evaluation must fail. Without bootstrap, "+" has no binding — the
	// compiler cannot resolve it.
	_, evalErr := eng.Eval(ctx, expr)
	c.Assert(evalErr, qt.IsNotNil,
		qt.Commentf("eval should fail on unbootstrapped namespace, got nil error"))
}

// TestInitOrder_LibraryWithoutBootstrap verifies that enabling the library
// system on an unbootstrapped namespace fails. The failure may occur at
// engine creation (if setupLibrarySystem needs bootstrap artifacts) or at
// import time (if library parsing requires define-library syntax).
func TestInitOrder_LibraryWithoutBootstrap(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	c.Assert(err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)
	// No bootstrap — same as above.

	// Wrap engine creation + eval in a recover block because the missing
	// bootstrap state may cause a panic rather than a clean error return.
	var creationErr error
	var evalErr error
	panicked := false
	panicVal := ""

	func() {
		defer func() {
			r := recover()
			if r != nil {
				panicked = true
				panicVal = fmt.Sprintf("%v", r)
			}
		}()

		var eng *Engine
		eng, creationErr = NewEngine(ctx, WithNamespace(ns), WithLibraryPaths("."))
		if creationErr != nil {
			return
		}

		// If creation succeeded, try to import a library — this requires
		// the expander to understand define-library, which comes from bootstrap.
		expr, parseErr := eng.Parse(ctx, `(import (scheme base))`)
		if parseErr != nil {
			evalErr = parseErr
			return
		}
		_, evalErr = eng.Eval(ctx, expr)
	}()

	// At least one of these must be true: creation failed, eval failed, or
	// something panicked. A fully successful path means the test's premise
	// is wrong and needs investigation.
	failed := creationErr != nil || evalErr != nil || panicked
	c.Assert(failed, qt.IsTrue,
		qt.Commentf("expected failure from library system without bootstrap; "+
			"creationErr=%v, evalErr=%v, panicked=%v (%s)",
			creationErr, evalErr, panicked, panicVal))
}
