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
)

// benchCase defines a single benchmark expression with optional setup code.
// Setup is eval'd once before the benchmark loop; code is the expression
// being measured.
type benchCase struct {
	name  string
	setup string // scheme code eval'd before benchmarking (empty = none)
	code  string // expression to benchmark
}

var benchCases = []benchCase{
	{"Simple", "", "(+ 1 2)"},
	{"Arithmetic", "", "(+ (* 2 3) (- 10 5))"},
	{"Lambda", "", "((lambda (x) (+ x 1)) 42)"},
	{"Let", "", "(let ((x 10) (y 20)) (+ x y))"},
	{"List", "", "(length (list 1 2 3 4 5))"},
	{"Cond", "", `(cond ((< 5 3) 'less) ((> 5 3) 'greater) (else 'equal))`},
	{
		"Fibonacci",
		"(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
		"(fib 10)",
	},
	{
		"TailRecursion",
		"(define (sum n acc) (if (<= n 0) acc (sum (- n 1) (+ acc n))))",
		"(sum 100 0)",
	},
	{
		"Macro",
		`(define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...) #f))))`,
		"(when #t (+ 1 2))",
	},
}

// BenchmarkEval measures end-to-end evaluation (parse + expand + compile + run).
func BenchmarkEval(b *testing.B) {
	for _, tc := range benchCases {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				b.Fatal(err)
			}
			if tc.setup != "" {
				_, err = engine.Eval(ctx, engine.MustParse(ctx, tc.setup))
				if err != nil {
					b.Fatal(err)
				}
			}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err = engine.Eval(ctx, engine.MustParse(ctx, tc.code))
				if err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// BenchmarkRun measures VM-only execution by pre-compiling once and
// benchmarking just Engine.Run(). Isolates VM dispatch from parse/expand/
// compile overhead.
func BenchmarkRun(b *testing.B) {
	for _, tc := range benchCases {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				b.Fatal(err)
			}
			if tc.setup != "" {
				_, err = engine.Eval(ctx, engine.MustParse(ctx, tc.setup))
				if err != nil {
					b.Fatal(err)
				}
			}
			compiled, err := engine.Compile(ctx, engine.MustParse(ctx, tc.code))
			if err != nil {
				b.Fatal(err)
			}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err = engine.Run(ctx, compiled)
				if err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// BenchmarkCompile measures compilation without execution.
func BenchmarkCompile(b *testing.B) {
	engine, err := NewEngine(context.Background())
	if err != nil {
		b.Fatal(err)
	}
	code := "(lambda (x y) (+ x y))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err = engine.Compile(context.Background(), engine.MustParse(context.Background(), code))
		if err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkZebraPuzzle benchmarks the zebra/Einstein puzzle via Schelog.
// This is a brute-force constraint satisfaction problem that exercises heavy
// backtracking with occurs-check enabled. Setup (engine creation, library
// loading) happens before ResetTimer so only the solve is measured.
//
// Libraries are loaded via Scheme's (include ...) which uses letrec*
// semantics for forward references within included files.
//
// Skipped in short mode — a single iteration takes minutes in a bytecode
// interpreter. Use `make profile-zebra` for dedicated profiling.
func BenchmarkZebraPuzzle(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping zebra puzzle benchmark in short mode (use make profile-zebra)")
	}
	// include resolves paths via SCHEME_INCLUDE_PATH. The example sources live
	// at the module root; this package (and the test's CWD) is at pkg/wile/.
	b.Setenv("SCHEME_INCLUDE_PATH", "../../")

	engine, err := NewEngine(context.Background())
	if err != nil {
		b.Fatal(err)
	}

	ctx := context.Background()

	setup := `
		(include "examples/logic/schelog/schelog.scm")
		(include "examples/logic/schelog/puzzle.scm")
		(include "examples/logic/schelog/houses.scm")
		(set! *schelog-use-occurs-check?* #t)
	`
	_, err = engine.EvalMultiple(ctx, setup)
	if err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err = engine.Eval(ctx, engine.MustParse(ctx, "(solve-puzzle %houses)"))
		if err != nil {
			b.Fatal(err)
		}
	}
	b.StopTimer()
	b.Logf("VM counters (last iteration):\n%s", engine.LastCounters())
}
