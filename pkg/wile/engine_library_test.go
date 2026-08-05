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
	"testing"

	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"

	qt "github.com/frankban/quicktest"
)

func TestExtensionAsLibrary_Import(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (wile math))
		(sqrt 4)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "2")
}

func TestExtensionAsLibrary_Only(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// sqrt should work
	result, err := engine.EvalMultiple(ctx, `
		(import (only (wile math) sqrt))
		(sqrt 16)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "4")
}

func TestExtensionAsLibrary_Prefix(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (prefix (wile math) m:))
		(m:sqrt 9)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestExtensionAsLibrary_Rename(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (rename (wile math) (sqrt square-root)))
		(square-root 25)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "5")
}

func TestExtensionAsLibrary_NoRuntimePrimitives(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Extension that registers no runtime primitives (compile-time only binding).
	compileOnly := registry.NewExtension("compileonly", func(r *registry.PrimitiveRegistry) error {
		r.AddBinding("my-special-form")
		return nil
	})

	engine, err := NewEngine(ctx,
		WithExtension(compileOnly),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// (wile compileonly) should not exist as a library since it has no runtime primitives.
	_, err = engine.EvalMultiple(ctx, `(import (wile compileonly))`)
	c.Assert(err, qt.IsNotNil)
}

// mockLibraryNamerExtension implements both Extension and LibraryNamer.
type mockLibraryNamerExtension struct {
	libName []string
}

func (m *mockLibraryNamerExtension) Name() string {
	return "custom"
}

func (m *mockLibraryNamerExtension) AddToRegistry(r *registry.PrimitiveRegistry) error {
	r.AddPrimitive(registry.PrimitiveSpec{
		Name:       "custom-fn",
		ParamCount: 1,
		Impl: machine.ForeignFunction(func(mc machine.CallContext) error {
			mc.SetValue(nil)
			return nil
		}),
	}, registry.PhaseSetRuntime)
	return nil
}

func (m *mockLibraryNamerExtension) LibraryName() []string {
	return m.libName
}

func TestExtensionAsLibrary_CustomLibraryName(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ext := &mockLibraryNamerExtension{
		libName: []string{"myorg", "utils"},
	}

	engine, err := NewEngine(ctx,
		WithExtension(ext),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// The library should be registered under (myorg utils), not (wile custom).
	libReg := engine.Environment().LibraryRegistry()
	c.Assert(libReg, qt.IsNotNil)

	// Cast to machine.LibraryRegistry to call Lookup is complex from here,
	// so test via the library being importable by its custom name.
	// Since the Impl is nil, we can't actually call the function, but
	// just importing without error proves the library was registered correctly.
	// A nil Impl would panic at call time, but importing is fine.
	_, err = engine.EvalMultiple(ctx, `(import (myorg utils))`)
	c.Assert(err, qt.IsNil)
}

// TestExtensionAsLibrary_EmptyLibraryNameFallsBack verifies the Phase-2
// semantic: a LibraryNamer that returns an empty slice falls back to the
// (wile <name>) default rather than erroring. This applies uniformly to
// both *ExtensionFunc with an unset WithLibraryName slot and to custom
// types that happen to return [].
func TestExtensionAsLibrary_EmptyLibraryNameFallsBack(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ext := &mockLibraryNamerExtension{libName: nil}

	engine, err := NewEngine(ctx,
		WithExtension(ext),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (wile custom))`)
	c.Assert(err, qt.IsNil)
}

func TestExtensionAsLibrary_NotEnabled(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Without WithLibraryPaths(), library system is not enabled.
	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
	)
	c.Assert(err, qt.IsNil)

	// import should fail since the library system is not configured.
	_, err = engine.EvalMultiple(ctx, `(import (wile math))`)
	c.Assert(err, qt.IsNotNil)
}
