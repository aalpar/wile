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
	})
}
