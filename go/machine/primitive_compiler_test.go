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


package machine

import (
	"wile/environment"
	"wile/syntax"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestPrimitiveCompiler_SchemeString(t *testing.T) {
	fn := func(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	pc := NewPrimitiveCompiler("test-prim", fn)
	qt.Assert(t, pc.SchemeString(), qt.Equals, "#<primitive-compiler:test-prim>")
}

func TestPrimitiveCompiler_IsVoid(t *testing.T) {
	fn := func(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	pc := NewPrimitiveCompiler("test-prim", fn)
	qt.Assert(t, pc.IsVoid(), qt.IsFalse)
}

func TestPrimitiveCompiler_EqualTo(t *testing.T) {
	fn1 := func(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	fn2 := func(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}

	pc1 := NewPrimitiveCompiler("test-prim", fn1)
	pc2 := NewPrimitiveCompiler("test-prim", fn2)
	pc3 := NewPrimitiveCompiler("other-prim", fn1)

	// Same name should be equal
	qt.Assert(t, pc1.EqualTo(pc2), qt.IsTrue)
	// Different name should not be equal
	qt.Assert(t, pc1.EqualTo(pc3), qt.IsFalse)
	qt.Assert(t, pc1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestLookupPrimitiveCompiler(t *testing.T) {
	env := environment.NewTipTopEnvironmentFrame()
	RegisterPrimitiveCompilers(env) //nolint:errcheck

	// Should find built-in primitives
	metaSym := env.InternSymbol(values.NewSymbol("meta"))
	metaPc := LookupPrimitiveCompiler(env, metaSym, nil)
	qt.Assert(t, metaPc, qt.IsNotNil)

	includeSym := env.InternSymbol(values.NewSymbol("include"))
	includePc := LookupPrimitiveCompiler(env, includeSym, nil)
	qt.Assert(t, includePc, qt.IsNotNil)

	// Should return nil for non-existent primitive
	nonExistentSym := env.InternSymbol(values.NewSymbol("nonexistent-primitive"))
	nonExistent := LookupPrimitiveCompiler(env, nonExistentSym, nil)
	qt.Assert(t, nonExistent, qt.IsNil)
}

func TestLookupPrimitiveCompiler_PhaseEnvironment(t *testing.T) {
	env := environment.NewTipTopEnvironmentFrame()

	// Register primitives in the compile environment
	RegisterPrimitiveCompilers(env) //nolint:errcheck

	// Should find primitives in the compile phase
	defineSyntaxSym := env.InternSymbol(values.NewSymbol("define-syntax"))
	defineSyntaxPc := LookupPrimitiveCompiler(env, defineSyntaxSym, nil)
	qt.Assert(t, defineSyntaxPc, qt.IsNotNil)
}
