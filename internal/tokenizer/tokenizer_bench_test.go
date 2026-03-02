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

package tokenizer

import (
	"strings"
	"testing"
)

// BenchmarkTokenize measures pure tokenizer throughput for representative
// Scheme inputs. Reports bytes/sec so regressions are visible independent
// of input size.
func BenchmarkTokenize(b *testing.B) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			"Atom",
			"42",
		},
		{
			"SimpleList",
			"(+ 1 2 3 4 5)",
		},
		{
			"NestedLambda",
			"((lambda (x y) (+ (* x x) (* y y))) 3 4)",
		},
		{
			"StringLiteral",
			`"hello world, this is a string with escapes: \n\t\\\""`,
		},
		{
			"MixedProgram",
			`(define (fibonacci n)
  (let loop ((i 0) (a 0) (b 1))
    (if (= i n) a
        (loop (+ i 1) b (+ a b)))))

(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define result (map fibonacci '(0 1 2 3 4 5 6 7 8 9 10)))`,
		},
		{
			"NumberHeavy",
			"(+ 123456789 3.14159 1/3 2+3i #xff #b1010 1e10 -42)",
		},
	}

	for _, tc := range tcs {
		b.Run(tc.name, func(b *testing.B) {
			b.SetBytes(int64(len(tc.input)))
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, _ = Tokenize(tc.input, false)
			}
		})
	}
}

// BenchmarkTokenizeLargeInput measures tokenizer throughput on a larger input
// constructed by repeating a representative program. This stresses the
// tokenizer's steady-state performance and slice growth behavior.
func BenchmarkTokenizeLargeInput(b *testing.B) {
	fragment := `(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
`
	input := strings.Repeat(fragment, 100)
	b.SetBytes(int64(len(input)))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = Tokenize(input, false)
	}
}
