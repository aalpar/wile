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
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestCompileTransformerToMachineClosure_SyntaxRules(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
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
	transformerStx := schemeutil.DatumToSyntaxValue(context.Background(), sctx, transformer)

	closure, err := compileTransformerToMachineClosure(context.Background(), env, transformerStx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, closure, qt.IsNotNil)
}

func TestCompileTransformerToMachineClosure_Lambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (stx) (quote 42))
	transformer := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("stx")),
		values.List(values.NewSymbol("quote"), values.NewInteger(42)),
	)
	transformerStx := schemeutil.DatumToSyntaxValue(context.Background(), sctx, transformer)

	closure, err := compileTransformerToMachineClosure(context.Background(), env, transformerStx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, closure, qt.IsNotNil)
}

func TestCompileTransformerToMachineClosure_UnsupportedType(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (unsupported-keyword ...)
	transformer := values.List(
		values.NewSymbol("unsupported-keyword"),
		values.NewInteger(42),
	)
	transformerStx := schemeutil.DatumToSyntaxValue(context.Background(), sctx, transformer)

	closure, err := compileTransformerToMachineClosure(context.Background(), env, transformerStx)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, closure, qt.IsNil)
	qt.Assert(t, err.Error(), qt.Contains, "unsupported transformer type")
}

func TestCompileTransformerToMachineClosure_NotAPair(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// Just a symbol, not a pair
	transformerStx := schemeutil.DatumToSyntaxValue(context.Background(), sctx, values.NewSymbol("not-a-list"))

	closure, err := compileTransformerToMachineClosure(context.Background(), env, transformerStx)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, closure, qt.IsNil)
	qt.Assert(t, err.Error(), qt.Contains, "transformer must be a list")
}

// TestProceduralMacroExpandTimePath tests that procedural macros work through
// the expand-time path (used by load/include/REPL)
func TestProceduralMacroExpandTimePath(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
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
	defineSyntaxStx := schemeutil.DatumToSyntaxValue(context.Background(), sctx, defineSyntaxExpr).(*syntax.SyntaxPair)

	// Use the expand-time path to compile the define-syntax
	err := compileDefineSyntaxFromSyntax(context.Background(), env, defineSyntaxStx)
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
	_, isClosure := val.(*MachineClosure)
	qt.Assert(t, isClosure, qt.IsTrue)
}
