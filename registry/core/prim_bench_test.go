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

package core_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
)

// primBenchCase defines a single primitive benchmark. Setup code is eval'd
// once before the benchmark loop; code is the expression being measured.
type primBenchCase struct {
	name  string
	setup string
	code  string
}

var primBenchCases = []primBenchCase{
	// Arithmetic
	{"Add", "", "(+ 1 2)"},
	{"Multiply", "", "(* 6 7)"},
	{"Compare", "", "(< 1 2)"},

	// List operations
	{"Cons", "", "(cons 1 2)"},
	{"Car", "(define bench-pair (cons 1 2))", "(car bench-pair)"},
	{"Length", "", "(length '(1 2 3 4 5))"},
	{"Append", "", "(append '(1 2) '(3 4))"},
	{"Reverse", "", "(reverse '(1 2 3 4 5))"},
	{"MakeList", "", "(make-list 5 0)"},
	{"ListTail", "(define bench-list '(1 2 3 4 5))", "(list-tail bench-list 3)"},
	{"ListCopy", "(define bench-list5 '(1 2 3 4 5))", "(list-copy bench-list5)"},

	// Predicates
	{"NullQ", "", "(null? '())"},
	{"PairQ", "", "(pair? '(1))"},

	// Equality
	{"EqQ", "", "(eq? 'a 'a)"},
	{"EqualQ", "", "(equal? '(1 2 3) '(1 2 3))"},

	// Vectors
	{"VectorRef", "(define bench-vec (vector 1 2 3 4 5))", "(vector-ref bench-vec 2)"},

	// Strings
	{"StringLength", "", `(string-length "hello world")`},
	{"StringRef", "", `(string-ref "hello" 2)`},
}

// BenchmarkPrimitiveCall measures per-primitive VM execution by pre-compiling
// each expression once and benchmarking only the Run phase. This isolates
// primitive dispatch, argument extraction, and result boxing from parse/
// expand/compile overhead.
func BenchmarkPrimitiveCall(b *testing.B) {
	for _, tc := range primBenchCases {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			env, err := bootstrap.NewNamespaceFrameTiny(ctx)
			if err != nil {
				b.Fatal(err)
			}
			if tc.setup != "" {
				benchEval(ctx, b, env, tc.setup)
			}

			tpl := benchCompile(ctx, b, env, tc.code)

			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				mc := machine.NewMachineContext(
					ctx,
					machine.NewMachineContinuation(nil, tpl, env),
				)
				err = mc.RunWithEscapeHandling()
				if err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// benchCompile parses, expands, and compiles a Scheme expression, returning
// the compiled template for repeated execution.
func benchCompile(ctx context.Context, b *testing.B, env *environment.EnvironmentFrame, code string) *machine.NativeTemplate {
	b.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		b.Fatal(err)
	}

	expanded, err := machine.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	if err != nil {
		b.Fatal(err)
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(ctx, false)
	err = machine.NewCompiletimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	if err != nil {
		b.Fatal(err)
	}
	tpl.Optimize()
	return tpl
}

// benchEval compiles and executes Scheme code once (used for setup).
func benchEval(ctx context.Context, b *testing.B, env *environment.EnvironmentFrame, code string) {
	b.Helper()
	tpl := benchCompile(ctx, b, env, code)
	mc := machine.NewMachineContext(
		ctx,
		machine.NewMachineContinuation(nil, tpl, env),
	)
	err := mc.RunWithEscapeHandling()
	if err != nil {
		b.Fatal(err)
	}
}
