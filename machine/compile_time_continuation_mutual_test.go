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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// Helper function to build syntax lists
func syntaxList(sctx *syntax.SourceContext, items ...syntax.SyntaxValue) *syntax.SyntaxPair {
	if len(items) == 0 {
		panic("syntaxList requires at least one element")
	}
	if len(items) == 1 {
		return syntax.NewSyntaxCons(items[0], syntax.SyntaxEmptyList, sctx)
	}
	return syntax.NewSyntaxCons(items[0], syntaxList(sctx, items[1:]...), sctx)
}

func TestCompileContext_CompileDefine_MutualRecursion_NotSupported(t *testing.T) {
	// NOTE: This test documents that mutual recursion via forward references is NOT supported.
	// Only self-recursion is enabled by the early binding fix.
	// For true mutual recursion, use letrec or define both functions with lambda first.

	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// Attempting mutual recursion with function definition form
	// (define (f x) (g x))  ; g is not defined yet - will fail
	// (define (g x) (f x))

	// Build (define (f x) (g x))
	prog1 := syntaxList(sctx,
		syntax.NewSyntaxSymbol("define", sctx),
		syntaxList(sctx,
			syntax.NewSyntaxSymbol("f", sctx),
			syntax.NewSyntaxSymbol("x", sctx)),
		syntaxList(sctx,
			syntax.NewSyntaxSymbol("g", sctx),
			syntax.NewSyntaxSymbol("x", sctx)))

	// Try to compile first function - should error because g is not defined yet
	tpl1 := NewNativeTemplate(0, 0, false)
	cctx1 := NewCompiletimeContinuation(tpl1, env)
	ectx := context.Background()
	econt1 := NewExpanderTimeContinuation(ectx, env)
	prog1Syntax, err := econt1.ExpandExpression(prog1)
	qt.Assert(t, err, qt.IsNil)
	cnt := NewCompileTimeCallContext(context.Background(), false)
	err = cctx1.CompileExpression(cnt, prog1Syntax)
	// This SHOULD fail - mutual recursion requires both functions to be pre-declared
	qt.Assert(t, err, qt.IsNotNil, qt.Commentf("Should fail: forward references not supported"))
	qt.Assert(t, err.Error(), qt.Contains, "no such binding")
}

func TestCompileContext_CompileDefine_SelfRecursion_FunctionForm(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// Test self-recursion with function definition form
	// Simplified: (define (fact n) (fact n))
	prog := syntaxList(sctx,
		syntax.NewSyntaxSymbol("define", sctx),
		syntaxList(sctx,
			syntax.NewSyntaxSymbol("fact", sctx),
			syntax.NewSyntaxSymbol("n", sctx)),
		syntaxList(sctx,
			syntax.NewSyntaxSymbol("fact", sctx),
			syntax.NewSyntaxSymbol("n", sctx)))

	// Compile the function - should not error even though fact references itself
	tpl := NewNativeTemplate(0, 0, false)
	cctx := NewCompiletimeContinuation(tpl, env)
	ectx := context.Background()
	econt := NewExpanderTimeContinuation(ectx, env)
	progSyntax, err := econt.ExpandExpression(prog)
	qt.Assert(t, err, qt.IsNil)
	cnt := NewCompileTimeCallContext(context.Background(), false)
	err = cctx.CompileExpression(cnt, progSyntax)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("Should be able to compile fact referencing itself"))

	// Execute the define
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Verify the function is defined
	factSym := values.NewSymbol("fact")
	factGi := env.GetGlobalIndex(factSym)
	qt.Assert(t, factGi, qt.IsNotNil, qt.Commentf("fact should be defined"))
}
