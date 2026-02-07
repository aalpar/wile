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

// BenchmarkEvalSimple benchmarks end-to-end simple expression evaluation
func BenchmarkEvalSimple(b *testing.B) {
	engine, _ := NewEngine()
	code := "(+ 1 2)"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkEvalArithmetic benchmarks nested arithmetic
func BenchmarkEvalArithmetic(b *testing.B) {
	engine, _ := NewEngine()
	code := "(+ (* 2 3) (- 10 5))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkEvalLambda benchmarks lambda creation and application
func BenchmarkEvalLambda(b *testing.B) {
	engine, _ := NewEngine()
	code := "((lambda (x) (+ x 1)) 42)"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkEvalList benchmarks list operations
func BenchmarkEvalList(b *testing.B) {
	engine, _ := NewEngine()
	code := "(length (list 1 2 3 4 5))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkEvalFibonacci benchmarks recursive Fibonacci
func BenchmarkEvalFibonacci(b *testing.B) {
	engine, _ := NewEngine()
	_, _ = engine.Eval(context.TODO(), "(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), "(fib 10)")
	}
}

// BenchmarkEvalTailRecursion benchmarks tail recursion
func BenchmarkEvalTailRecursion(b *testing.B) {
	engine, _ := NewEngine()
	_, _ = engine.Eval(context.TODO(), "(define (sum n acc) (if (<= n 0) acc (sum (- n 1) (+ acc n))))")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), "(sum 100 0)")
	}
}

// BenchmarkCompile benchmarks compilation without execution
func BenchmarkCompile(b *testing.B) {
	engine, _ := NewEngine()
	code := "(lambda (x y) (+ x y))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Compile(context.Background(), code)
	}
}

// BenchmarkEvalMacro benchmarks macro expansion and execution
func BenchmarkEvalMacro(b *testing.B) {
	engine, _ := NewEngine()
	_, _ = engine.Eval(context.TODO(), `
		(define-syntax when
			(syntax-rules ()
				((when test body ...)
				 (if test (begin body ...) #f))))
	`)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), "(when #t (+ 1 2))")
	}
}

// BenchmarkEvalLet benchmarks let binding
func BenchmarkEvalLet(b *testing.B) {
	engine, _ := NewEngine()
	code := "(let ((x 10) (y 20)) (+ x y))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkEvalCond benchmarks cond expressions
func BenchmarkEvalCond(b *testing.B) {
	engine, _ := NewEngine()
	code := "(cond ((< 5 3) 'less) ((> 5 3) 'greater) (else 'equal))"
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = engine.Eval(context.TODO(), code)
	}
}

// BenchmarkZebraPuzzle benchmarks the zebra/Einstein puzzle via Schelog.
// This is a brute-force constraint satisfaction problem that exercises heavy
// backtracking with occurs-check enabled. Setup (engine creation, library
// loading) happens before ResetTimer so only the solve is measured.
//
// Libraries are loaded via Scheme's (include ...) which uses letrec*
// semantics for forward references within included files.
func BenchmarkZebraPuzzle(b *testing.B) {
	// include resolves paths via SCHEME_INCLUDE_PATH.
	b.Setenv("SCHEME_INCLUDE_PATH", ".")

	engine, err := NewEngine()
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
		_, err = engine.Eval(ctx, "(solve-puzzle %houses)")
		if err != nil {
			b.Fatal(err)
		}
	}
	b.StopTimer()
	b.Logf("VM counters (last iteration):\n%s", engine.LastCounters())
}
