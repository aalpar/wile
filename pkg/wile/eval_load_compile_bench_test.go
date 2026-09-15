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
	"os"
	"path/filepath"
	"strconv"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

// evalLoadCompileBenchFile is loaded by BenchmarkEvalLoadCompile/Load. Its
// procedures are closure templates; its last form is top-level code with calls
// to promoted primitives, the part only the load path compiles. The defines are
// internal so the file can be loaded repeatedly under the immutable top level.
const evalLoadCompileBenchFile = `
(let ()
  (define (sq x) (* x x))
  (define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc (sq n)))))
  (loop 10 0))
(+ (car (cons 1 2)) (cdr (cons 3 4)) (vector-ref (vector 5 6) 1))
`

// BenchmarkEvalLoadCompile measures code that reaches the compiler through the
// eval, load, and compile primitives rather than through Engine.Eval. Each case
// pays that primitive's expand/compile per iteration except Thunk, which
// compiles once and then runs the returned procedure's template repeatedly.
func BenchmarkEvalLoadCompile(b *testing.B) {
	path := filepath.Join(b.TempDir(), "bench.scm")
	err := os.WriteFile(path, []byte(evalLoadCompileBenchFile), 0o600)
	if err != nil {
		b.Fatal(err)
	}
	cases := []benchCase{
		{
			"Eval",
			"",
			"(eval '(let ((p (cons 3 4))) (+ (car p) (cdr p) (vector-ref (vector 5 6) 1))))",
		},
		{
			"Load",
			"",
			"(load " + strconv.Quote(path) + ")",
		},
		{
			"Thunk",
			"(define bench-p (cons 3 4)) (define bench-t (compile '(+ (car bench-p) (cdr bench-p))))",
			"(let loop ((i 0)) (if (< i 1000) (begin (bench-t) (loop (+ i 1)))))",
		},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithSourceOS())
			if err != nil {
				b.Fatal(err)
			}
			defer engine.Close()
			if tc.setup != "" {
				_, err = engine.EvalMultiple(ctx, tc.setup)
				if err != nil {
					b.Fatalf("setup: %v", err)
				}
			}
			expr, err := engine.Parse(ctx, tc.code)
			if err != nil {
				b.Fatalf("parse: %v", err)
			}
			cc, err := engine.Compile(ctx, expr)
			if err != nil {
				b.Fatalf("compile: %v", err)
			}
			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err = engine.Run(ctx, cc)
				if err != nil {
					b.Fatalf("run: %v", err)
				}
			}
		})
	}
}
