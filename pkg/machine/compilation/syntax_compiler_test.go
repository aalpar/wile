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

package compilation

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

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
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Should find built-in syntax compilers
	metaSym := values.NewSymbol("meta")
	metaPc := LookupSyntaxCompiler(env, metaSym, nil)
	qt.Assert(t, metaPc, qt.IsNotNil)

	includeSym := values.NewSymbol("include")
	includePc := LookupSyntaxCompiler(env, includeSym, nil)
	qt.Assert(t, includePc, qt.IsNotNil)

	// Should return nil for non-existent syntax compiler
	nonExistentSym := values.NewSymbol("nonexistent-primitive")
	nonExistent := LookupSyntaxCompiler(env, nonExistentSym, nil)
	qt.Assert(t, nonExistent, qt.IsNil)
}

// The compilers live in the ambient tier, which every frame's ranked probe
// reaches as T3. That is what LookupSyntaxCompiler's doc claims, and it has two
// halves: a same-phase user binding at T1 outranks the compiler FROM THAT FRAME,
// and the shadow reaches no further, because an exact-phase slot is not a
// candidate at any other phase at all.
func TestLookupSyntaxCompiler_SamePhaseShadowOutranksAmbient(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("define-syntax")
	qt.Assert(t, LookupSyntaxCompiler(env, sym, nil), qt.IsNotNil)

	// A user (define define-syntax …) at phase 0: an exact-phase MUTABLE slot,
	// a distinct binding from the ambient one because coordinates are half of
	// binding identity (CreateGlobalBindingAt).
	_, created := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
	qt.Assert(t, created, qt.IsTrue)
	qt.Assert(t, LookupSyntaxCompiler(env, sym, nil), qt.IsNil,
		qt.Commentf("T1 outranks the ambient T3 compiler at the shadowed phase"))

	// Phase 1 has no slot of the name, so the ambient compiler still answers.
	expand := env.AtPhase(environment.PhaseExpand)
	qt.Assert(t, LookupSyntaxCompiler(expand, sym, nil), qt.IsNotNil,
		qt.Commentf("a phase-0 shadow is not a candidate at phase 1"))
}
