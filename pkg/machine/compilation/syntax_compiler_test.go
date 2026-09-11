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
	metaPc := LookupPhaseBinding[*SyntaxCompiler](env, metaSym, nil)
	qt.Assert(t, metaPc, qt.IsNotNil)

	includeSym := values.NewSymbol("include")
	includePc := LookupPhaseBinding[*SyntaxCompiler](env, includeSym, nil)
	qt.Assert(t, includePc, qt.IsNotNil)

	// Should return nil for non-existent syntax compiler
	nonExistentSym := values.NewSymbol("nonexistent-primitive")
	nonExistent := LookupPhaseBinding[*SyntaxCompiler](env, nonExistentSym, nil)
	qt.Assert(t, nonExistent, qt.IsNil)
}

// A user binding of a compiler's name at the SAME phase outranks the compiler,
// and the shadow reaches no further than that phase.
//
// Both halves changed shape when the ambient tier was deleted, and neither
// changed answer. The compiler now sits at (phase 0, sealed) rather than at
// (ANY, sealed), so the contest at phase 0 is tierExactMutable over
// tierExactSealed at one coordinate instead of tierExactMutable over the deleted
// ambient tier. And a phase-0 slot was never a
// candidate at phase 1; what changed is that the compiler is not one either, so a
// bare phase-1 probe now finds NOTHING rather than the compiler.
//
// What restores the compiler at phase 1 is a bulk row, and the row is sealed-tier
// restricted (NewSealedStoreBulkSource), so it carries the compiler up without
// carrying the user's mutable shadow with it. That is the replacement for
// "ambient, minus whatever the local phase shadows".
func TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("define-syntax")
	qt.Assert(t, LookupPhaseBinding[*SyntaxCompiler](env, sym, nil), qt.IsNotNil)

	// A user (define define-syntax …) at phase 0: an exact-phase MUTABLE slot,
	// a distinct binding from the sealed one because coordinates are half of
	// binding identity (CreateGlobalBindingAt).
	_, created := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
	qt.Assert(t, created, qt.IsTrue)
	qt.Assert(t, LookupPhaseBinding[*SyntaxCompiler](env, sym, nil), qt.IsNil,
		qt.Commentf("tierExactMutable outranks the tierExactSealed compiler at the shadowed phase"))

	// Phase 1 reaches neither: the shadow is a phase-0 slot, and so is the
	// compiler.
	expand := env.AtPhase(environment.PhaseExpand)
	qt.Assert(t, LookupPhaseBinding[*SyntaxCompiler](expand, sym, nil), qt.IsNil,
		qt.Commentf("no coordinate is phase-blind any more, so phase 1 supplies nothing of its own"))

	// The phase-1 bulk row supplies the SEALED compiler and leaves the mutable
	// shadow at phase 0 where it was written.
	store := env.Namespace().Store()
	src := environment.NewSealedStoreBulkSource(store, environment.PhaseRuntime, environment.BaseSourceName())
	store.InstallBulkRow(src, nil, environment.PhaseExpand, true, environment.BulkOriginLanguage)
	qt.Assert(t, LookupPhaseBinding[*SyntaxCompiler](expand, sym, nil), qt.IsNotNil,
		qt.Commentf("the row is sealed-tier restricted, so a phase-0 mutable shadow does not ride it up"))
}
