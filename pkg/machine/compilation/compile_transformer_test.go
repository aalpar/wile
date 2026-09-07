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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestCompileTransformerValue_SyntaxRules(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (syntax-rules () ((my-const) 42))
	transformer := values.List(
		values.NewSymbol("syntax-rules"),
		values.EmptyList,
		values.List(
			values.List(values.NewSymbol("my-const")),
			values.NewInteger(42),
		),
	)
	transformerStx := mustDatumToSyntax(sctx, transformer)

	result, err := compileTransformerValue(context.Background(), env, transformerStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)
	_, isClosure := result.(*machine.MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue, qt.Commentf("%T", result))
}

func TestCompileTransformerValue_Lambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (stx) (quote 42))
	transformer := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("stx")),
		values.List(values.NewSymbol("quote"), values.NewInteger(42)),
	)
	transformerStx := mustDatumToSyntax(sctx, transformer)

	result, err := compileTransformerValue(context.Background(), env, transformerStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)
	_, isClosure := result.(*machine.MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue, qt.Commentf("%T", result))
}

func TestCompileTransformerValue_ERMacroTransformer(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (er-macro-transformer (lambda (form rename compare) form))
	transformer := values.List(
		values.NewSymbol("er-macro-transformer"),
		values.List(
			values.NewSymbol("lambda"),
			values.List(values.NewSymbol("form"), values.NewSymbol("rename"), values.NewSymbol("compare")),
			values.NewSymbol("form"),
		),
	)
	transformerStx := mustDatumToSyntax(sctx, transformer)

	result, err := compileTransformerValue(context.Background(), env, transformerStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)

	_, isER := result.(*ERMacroTransformer)
	qt.Assert(t, isER, qt.IsTrue)
}

// TestCompileTransformerValue_MacroUseExpandsToALambda is the §5.2 shape the head
// switch refused: the right-hand side is a macro USE, and only expanding it
// reveals the lambda underneath.
func TestCompileTransformerValue_MacroUseExpandsToALambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (define-syntax my-er (syntax-rules () ((_) (lambda (stx) (quote 7)))))
	// stored one phase up, so the transformer right-hand side below sees it.
	defineSyntax := values.List(
		values.NewSymbol("define-syntax"),
		values.NewSymbol("my-er"),
		values.List(
			values.NewSymbol("syntax-rules"),
			values.EmptyList,
			values.List(
				values.List(values.NewSymbol("_")),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("stx")),
					values.List(values.NewSymbol("quote"), values.NewInteger(7)),
				),
			),
		),
	)
	dsStx := mustDatumToSyntax(sctx, defineSyntax).(*syntax.SyntaxPair)
	err := compileDefineSyntaxFromSyntax(context.Background(), env, dsStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)

	// (my-er) as the transformer expression.
	useStx := mustDatumToSyntax(sctx, values.List(values.NewSymbol("my-er")))
	result, err := compileTransformerValue(context.Background(), env, useStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)
	_, isClosure := result.(*machine.MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue, qt.Commentf("%T", result))
}

// TestCompileTransformerValue_NonProcedureIsStoredBare (Q4): P0.4 removed the
// admission ladder P0.1 introduced. A right-hand side that evaluates to a
// non-procedure comes back as itself, for the caller to store bare; refusing it
// is the job of whatever tries to USE it as a transformer.
func TestCompileTransformerValue_NonProcedureIsStoredBare(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	transformerStx := mustDatumToSyntax(sctx, values.NewInteger(42))

	result, err := compileTransformerValue(context.Background(), env, transformerStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)
	n, ok := result.(*values.Integer)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("%T", result))
	qt.Assert(t, n.Value, qt.Equals, int64(42))
}

// TestProceduralMacroExpandTimePath tests that procedural macros work through
// the expand-time path (used by load/include/REPL)
func TestProceduralMacroExpandTimePath(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// Define a simple procedural macro that returns a constant
	// (define-syntax my-const
	//   (lambda (stx) (datum->syntax stx 42)))
	//
	// For this test, we'll use a simpler transformer that just returns a quoted value
	// since datum->syntax would need to be compiled into the environment

	// Compile: (define-syntax my-const (lambda (stx) '42))
	defineSyntaxExpr := values.List(
		values.NewSymbol("define-syntax"),
		values.NewSymbol("my-const"),
		values.List(
			values.NewSymbol("lambda"),
			values.List(values.NewSymbol("stx")),
			values.List(values.NewSymbol("quote"), values.NewInteger(42)),
		),
	)
	defineSyntaxStx := mustDatumToSyntax(sctx, defineSyntaxExpr).(*syntax.SyntaxPair)

	// Use the expand-time path to compile the define-syntax
	err := compileDefineSyntaxFromSyntax(context.Background(), env, defineSyntaxStx, nil, machine.NewVMMacroEvaluator())
	qt.Assert(t, err, qt.IsNil)

	// Verify the macro is stored in the expand environment
	expandEnv := env.Expand()
	idx := expandEnv.GetGlobalIndex(values.NewSymbol("my-const"))
	qt.Assert(t, idx, qt.IsNotNil)

	binding := expandEnv.GetGlobalBinding(idx)
	qt.Assert(t, binding, qt.IsNotNil)
	qt.Assert(t, binding.BindingType(), qt.Equals, environment.BindingTypeSyntax)

	// Verify the value is a closure
	val := binding.Value()
	_, isClosure := val.(*machine.MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue)
}
