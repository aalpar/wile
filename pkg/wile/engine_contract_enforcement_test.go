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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/files"
)

// TestContractEnforcement_EndToEnd verifies the full stack: the
// WithContractEnforcement engine option installs validators built from
// PrimitiveSpec.ParamTypes (via registry.BuildValidator), which then run
// through the dispatch path in ForeignClosure and reject wrong-typed args
// with a wrapped error mentioning the primitive name and argument index.
//
// The files extension is the proving ground: every primitive declares
// ParamTypes, so each call exercises the validator. A wrong-typed first
// argument must be caught *before* the implementation runs (i.e., the
// user never sees a RequireArg error when enforcement is on, because the
// validator fires first).
func TestContractEnforcement_EndToEnd(t *testing.T) {
	ctx := context.Background()

	t.Run("wrong type rejected with position", func(t *testing.T) {
		c := qt.New(t)
		engine, err := NewEngine(ctx, WithContractEnforcement(), WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		_, err = engine.EvalMultiple(ctx, `(file-exists? 42)`)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Contains, "file-exists?")
		c.Assert(err.Error(), qt.Contains, "argument 0")
	})

	t.Run("correct type passes validator", func(t *testing.T) {
		c := qt.New(t)
		engine, err := NewEngine(ctx, WithContractEnforcement(), WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		// Nonexistent path returns #f — the validator doesn't reject it.
		_, err = engine.EvalMultiple(ctx, `(file-exists? "/nonexistent/path/that/should/not/be/there")`)
		c.Assert(err, qt.IsNil)
	})

	t.Run("enforcement off still rejects wrong type via RequireArg", func(t *testing.T) {
		c := qt.New(t)
		// No WithContractEnforcement: validators are not installed, but the
		// primitive's own RequireArg still catches the mismatch. Important
		// sanity check — disabling enforcement must not let wrong types
		// silently succeed.
		engine, err := NewEngine(ctx, WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		_, err = engine.EvalMultiple(ctx, `(file-exists? 42)`)
		c.Assert(err, qt.IsNotNil)
		// The two rejections are distinguishable, which is what makes the
		// pre-built-namespace test below able to tell enforcement apart from
		// the primitive's own guard. The validator says "argument 0" (0-based,
		// registry vocabulary) and names Scheme types; RequireArg says
		// "argument 1" and names a Go type.
		c.Assert(err.Error(), qt.Contains, "expected a string but got *values.Integer")
	})
}

// TestContractEnforcement_PreBuiltNamespace closes the gap TODO.md filed under
// "WithContractEnforcement is SPLIT across the WithNamespace line".
//
// Before: the flag lived on engineConfig, and the pre-built-namespace path
// skipped bootstrapNamespace — so applyBaseEnvironment never saw it and the BASE
// ENVIRONMENT went unenforced, while library environments and later
// RegisterPrimitive calls were enforced. Enforcement was partial and silent.
//
// After: the flag lives on the Namespace, all three binding sites read it, and
// the option is namespace-consumed — so this is the only spelling, and it covers
// the base environment. file-exists? is a base-environment primitive here (bound
// by applyBaseEnvironment from the files extension), which is precisely the site
// that used to be missed.
func TestContractEnforcement_PreBuiltNamespace(t *testing.T) {
	ctx := context.Background()

	t.Run("namespace carries enforcement into the base environment", func(t *testing.T) {
		c := qt.New(t)
		ns, err := NewNamespace(ctx, WithContractEnforcement(), WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		engine, err := NewEngineWithNamespace(ctx, ns)
		c.Assert(err, qt.IsNil)

		_, err = engine.EvalMultiple(ctx, `(file-exists? 42)`)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Contains, "file-exists?")
		c.Assert(err.Error(), qt.Contains, "argument 0")
		c.Assert(err.Error(), qt.Contains, "expected string, got integer")
	})

	t.Run("a namespace without enforcement does not gain it", func(t *testing.T) {
		c := qt.New(t)
		// The discriminating arm: without this, the assertion above would pass
		// on any engine that rejects a wrong-typed argument at all, which every
		// engine does via RequireArg.
		ns, err := NewNamespace(ctx, WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		engine, err := NewEngineWithNamespace(ctx, ns)
		c.Assert(err, qt.IsNil)

		_, err = engine.EvalMultiple(ctx, `(file-exists? 42)`)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Contains, "expected a string but got *values.Integer")
	})

	t.Run("two engines over one namespace agree", func(t *testing.T) {
		c := qt.New(t)
		// The point of moving the flag off the Engine: a shared namespace is one
		// setting, not one per engine. This was previously representable at the
		// RegisterPrimitive site alone, and inconsistently.
		ns, err := NewNamespace(ctx, WithContractEnforcement(), WithExtension(files.Extension))
		c.Assert(err, qt.IsNil)

		for _, name := range []string{"first", "second"} {
			engine, engErr := NewEngineWithNamespace(ctx, ns)
			c.Assert(engErr, qt.IsNil)

			_, evalErr := engine.EvalMultiple(ctx, `(file-exists? 42)`)
			c.Assert(evalErr, qt.IsNotNil, qt.Commentf("engine %s", name))
			c.Assert(evalErr.Error(), qt.Contains, "expected string, got integer",
				qt.Commentf("engine %s did not enforce", name))
		}
	})
}
