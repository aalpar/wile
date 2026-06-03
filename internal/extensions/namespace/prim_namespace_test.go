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

package namespace_test

import (
	"context"
	"testing"

	wile "github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithLibraryPaths(),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng
}

func schemeEval(t *testing.T, eng *wile.Engine, code string) wile.Value {
	t.Helper()
	ctx := context.Background()
	result, err := eng.EvalMultiple(ctx, code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

func schemeEvalExpectError(t *testing.T, eng *wile.Engine, code string) {
	t.Helper()
	ctx := context.Background()
	expr, err := eng.Parse(ctx, code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = eng.Eval(ctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestNamespaceQ(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace? (interaction-environment))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	result = schemeEval(t, eng, `(namespace? 42)`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceName(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace-name (interaction-environment))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"interaction-environment"`)
}

func TestMakeNamespace(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(namespace? (make-namespace))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	// Empty namespace has no bindings
	result = schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns '+))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceDefineAndRef(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'x 42)
		(namespace-ref ns 'x))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

func TestNamespaceRefDefault(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 'nonexistent 'default))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "default")
}

func TestNamespaceRefError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 'nonexistent))`)
}

func TestNamespaceBound(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'x 1)
		(namespace-bound? ns 'x))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")

	result = schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns 'nonexistent))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceUndefine(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'y 10)
		(namespace-undefine! ns 'y)
		(namespace-bound? ns 'y))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#f")
}

func TestNamespaceBoundNames(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 'a 1)
		(namespace-define! ns 'b 2)
		(let ((names (namespace-bound-names ns)))
			(and (memq 'a names) (memq 'b names) #t)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

func TestNamespaceDerive(t *testing.T) {
	eng := newEngine(t)

	result := schemeEval(t, eng, `(let ((ns (make-namespace)))
		(namespace? (namespace-derive ns)))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

func TestNamespaceNameEmpty(t *testing.T) {
	eng := newEngine(t)

	// make-namespace creates a namespace with name "namespace"
	result := schemeEval(t, eng, `(namespace-name (make-namespace))`)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"namespace"`)
}

func TestMakeNamespaceErrors(t *testing.T) {
	eng := newEngine(t)

	// Invalid import spec
	schemeEvalExpectError(t, eng, `(make-namespace 42)`)
}

func TestNamespaceRequireErrors(t *testing.T) {
	eng := newEngine(t)

	// Not a namespace
	schemeEvalExpectError(t, eng, `(namespace-require 42 '(scheme base))`)

	// Invalid import spec
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-require ns 42))`)
}

func TestNamespaceDefineSymbolError(t *testing.T) {
	eng := newEngine(t)

	// Not a symbol for second arg
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-define! ns 42 "value"))`)
}

func TestNamespaceBoundSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-bound? ns 42))`)
}

func TestNamespaceUndefineSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-undefine! ns 42))`)
}

func TestNamespaceRefSymbolError(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-ref ns 42))`)
}

func TestNamespaceRequireLibraryNotFound(t *testing.T) {
	eng := newEngine(t)

	// Library not found
	schemeEvalExpectError(t, eng, `(let ((ns (make-namespace)))
		(namespace-require ns '(nonexistent library)))`)
}

func TestMakeNamespaceLibraryNotFound(t *testing.T) {
	eng := newEngine(t)

	// Library not found during make-namespace
	schemeEvalExpectError(t, eng, `(make-namespace '(nonexistent library))`)
}

func TestMakeNamespaceImportSetError(t *testing.T) {
	eng := newEngine(t)

	// An import-set modifier naming a binding the library does not export
	// exercises ImportSpecInto's ApplyToExports failure branch (shared with
	// the environment and namespace-require primitives).
	schemeEvalExpectError(t, eng, `(make-namespace '(only (scheme base) totally-not-a-real-binding))`)
}

func TestNamespaceTypeErrors(t *testing.T) {
	eng := newEngine(t)

	schemeEvalExpectError(t, eng, `(namespace-name 42)`)
	schemeEvalExpectError(t, eng, `(namespace-define! 42 'x 1)`)
	schemeEvalExpectError(t, eng, `(namespace-ref 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-bound? 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-undefine! 42 'x)`)
	schemeEvalExpectError(t, eng, `(namespace-bound-names 42)`)
	schemeEvalExpectError(t, eng, `(namespace-derive 42)`)
	schemeEvalExpectError(t, eng, `(namespace-require 42 '(scheme base))`)
}
