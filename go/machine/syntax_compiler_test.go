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

func TestSyntaxCompiler_SchemeString(t *testing.T) {
	fn := func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	pc := NewSyntaxCompiler("test-prim", fn)
	qt.Assert(t, pc.SchemeString(), qt.Equals, "#<syntax-compiler:test-prim>")
}

func TestSyntaxCompiler_IsVoid(t *testing.T) {
	fn := func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	pc := NewSyntaxCompiler("test-prim", fn)
	qt.Assert(t, pc.IsVoid(), qt.IsFalse)
}

func TestSyntaxCompiler_EqualTo(t *testing.T) {
	fn1 := func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}
	fn2 := func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
		return nil
	}

	pc1 := NewSyntaxCompiler("test-prim", fn1)
	pc2 := NewSyntaxCompiler("test-prim", fn2)
	pc3 := NewSyntaxCompiler("other-prim", fn1)

	// Same name should be equal
	qt.Assert(t, pc1.EqualTo(pc2), qt.IsTrue)
	// Different name should not be equal
	qt.Assert(t, pc1.EqualTo(pc3), qt.IsFalse)
	qt.Assert(t, pc1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestLookupSyntaxCompiler(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Should find built-in syntax compilers
	metaSym := env.InternSymbol(values.NewSymbol("meta"))
	metaPc := LookupSyntaxCompiler(env, metaSym, nil)
	qt.Assert(t, metaPc, qt.IsNotNil)

	includeSym := env.InternSymbol(values.NewSymbol("include"))
	includePc := LookupSyntaxCompiler(env, includeSym, nil)
	qt.Assert(t, includePc, qt.IsNotNil)

	// Should return nil for non-existent syntax compiler
	nonExistentSym := env.InternSymbol(values.NewSymbol("nonexistent-primitive"))
	nonExistent := LookupSyntaxCompiler(env, nonExistentSym, nil)
	qt.Assert(t, nonExistent, qt.IsNil)
}

func TestLookupSyntaxCompiler_PhaseEnvironment(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Register syntax compilers in the compile environment
	err := RegisterSyntaxCompilers(env) //nolint:errcheck
	qt.Assert(t, err, qt.IsNil)

	// Should find syntax compilers in the compile phase
	defineSyntaxSym := env.InternSymbol(values.NewSymbol("define-syntax"))
	defineSyntaxPc := LookupSyntaxCompiler(env, defineSyntaxSym, nil)
	qt.Assert(t, defineSyntaxPc, qt.IsNotNil)
}
