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

package machine_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileInclude tests include compilation by writing a temporary Scheme
// file and including it.
//
// Source: compile_time_continuation_include.go (CompileInclude,
// compileIncludeImpl, processFormsWithLetrecSemantics).
func TestCompileInclude(t *testing.T) {
	// Create a temp directory and write a Scheme file
	tmpDir := t.TempDir()
	schemeFile := filepath.Join(tmpDir, "included.scm")
	err := os.WriteFile(schemeFile, []byte("(define included-val 99)\n"), 0644)
	qt.Assert(t, err, qt.IsNil)

	env := newFullRuntimeEnv(t)

	// Set the SCHEME_INCLUDE_PATH so the file resolver can find the file
	t.Setenv("SCHEME_INCLUDE_PATH", tmpDir)

	// Step 1: Include the file
	sv := parseSchemeExprExt(t, env, `(include "included.scm")`)
	cont, contErr := newTopLevelThunkExt(sv, env)
	qt.Assert(t, contErr, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	runErr := mc.Run()
	qt.Assert(t, runErr, qt.IsNil)

	// Step 2: Reference the included binding
	sv = parseSchemeExprExt(t, env, `included-val`)
	cont, contErr = newTopLevelThunkExt(sv, env)
	qt.Assert(t, contErr, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	runErr = mc.Run()
	qt.Assert(t, runErr, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

// TestCompileIncludeMultipleForms tests that include processes multiple
// forms in a file with letrec* semantics.
func TestCompileIncludeMultipleForms(t *testing.T) {
	tmpDir := t.TempDir()
	schemeFile := filepath.Join(tmpDir, "multi.scm")
	err := os.WriteFile(schemeFile, []byte("(define a 10)\n(define b (+ a 20))\n"), 0644)
	qt.Assert(t, err, qt.IsNil)

	env := newFullRuntimeEnv(t)
	t.Setenv("SCHEME_INCLUDE_PATH", tmpDir)

	// Include the file
	sv := parseSchemeExprExt(t, env, `(include "multi.scm")`)
	cont, contErr := newTopLevelThunkExt(sv, env)
	qt.Assert(t, contErr, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	runErr := mc.Run()
	qt.Assert(t, runErr, qt.IsNil)

	// Check that both bindings are visible and b uses a
	sv = parseSchemeExprExt(t, env, `b`)
	cont, contErr = newTopLevelThunkExt(sv, env)
	qt.Assert(t, contErr, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	runErr = mc.Run()
	qt.Assert(t, runErr, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(30))
}
