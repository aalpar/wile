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
	"strconv"
	"strings"
	"testing"
)

// Macro-expansion cost, as a spendable budget.
//
// Nothing in the existing bench set measures it. BenchmarkCompile compiles
// `(lambda (x y) (+ x y))`, which invokes no macro at all;
// compilation.BenchmarkCompilePhase runs expansion once in setup and explicitly
// excludes it from the timer, precisely so it can isolate codegen dispatch.
// Expansion has therefore never been on a scale.
//
// It needs to be on one before any decision to move expander logic out of Go
// and into Scheme, because expansion time is what such a move spends. A budget
// that cannot be measured cannot be spent deliberately — it can only be spent
// by accident.
//
// The corpus is graded so one run yields a ratio rather than a bare number:
//
//	none    — no macro; the floor, matching BenchmarkCompile's shape
//	derived — bootstrap-derived forms only (cond/case/do/let-values), each a
//	          syntax-rules macro from bootstrap_macros.scm
//	user    — a user syntax-rules macro with literals and a recursive tail
//	ellipsis— nested ellipsis, the pattern matcher's expensive case
//	dense   — all of the above stacked, approximating real library source
//
// Report `dense / none` as the headline: it is the multiplier a change to the
// expander is charged against.
var expansionBenchCorpus = []struct {
	name  string
	setup string
	code  string
}{
	{
		name: "none",
		code: `(lambda (x y) (let ((a x)) (if a (+ a y) y)))`,
	},
	{
		name: "derived",
		code: `(lambda (n)
		         (do ((i 0 (+ i 1)) (acc '() (cons i acc)))
		             ((= i n) (cond ((null? acc) 'empty)
		                            ((null? (cdr acc)) 'one)
		                            (else (case (car acc)
		                                    ((0 1 2) 'low)
		                                    ((3 4 5) 'mid)
		                                    (else 'high)))))))`,
	},
	{
		name: "user",
		setup: `(define-syntax my-or
		          (syntax-rules ()
		            ((_) #f)
		            ((_ e) e)
		            ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))`,
		code: `(lambda (a b c d e f) (my-or a b c d e f))`,
	},
	{
		name: "ellipsis",
		setup: `(define-syntax my-let*
		          (syntax-rules ()
		            ((_ () body ...) (begin body ...))
		            ((_ ((n v) rest ...) body ...)
		             (let ((n v)) (my-let* (rest ...) body ...)))))
		        (define-syntax table
		          (syntax-rules ()
		            ((_ (k v ...) ...) (list (list 'k (list v ...)) ...))))`,
		code: `(lambda () (my-let* ((a 1) (b 2) (c 3) (d 4))
		                    (table (x 1 2 3) (y 4 5) (z 6 7 8 9))))`,
	},
	{
		name: "dense",
		setup: `(define-syntax swap!
		          (syntax-rules ()
		            ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
		        (define-syntax while
		          (syntax-rules ()
		            ((_ test body ...)
		             (let loop () (when test body ... (loop))))))`,
		code: `(lambda (xs n)
		         (let-values (((p q) (values 0 1)))
		           (do ((i 0 (+ i 1)))
		               ((= i n) (cond ((> p q) 'gt) ((< p q) 'lt) (else 'eq)))
		             (while (< p q) (swap! p q))
		             (case i ((0) (set! p 1)) ((1) (set! q 2)) (else (set! p q))))))`,
	},
}

// BenchmarkExpandAndCompile times parse-excluded expand+compile over the graded
// corpus. Engine.Compile runs expansion and codegen together — there is no
// public expand-only entry point — so this measures the cost a caller actually
// waits on, which is the right altitude for a budget.
//
// Parsing runs once per case in setup and is excluded, so the delta between
// cases is expansion plus the codegen it induces, not reader throughput.
func BenchmarkExpandAndCompile(b *testing.B) {
	for _, tc := range expansionBenchCorpus {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				b.Fatal(err)
			}
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

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err = engine.Compile(ctx, expr)
				if err != nil {
					b.Fatalf("compile: %v", err)
				}
			}
		})
	}
}

// bootstrapShapedSource returns n define-syntax forms and n define forms, in the
// shape bootstrap_macros.scm and bootstrap_procedures.scm actually use: small
// syntax-rules macros with a recursive clause, and small procedures. Each form
// is one logical unit, so the reported ns/op divided by n is a per-form rate.
func bootstrapShapedSource(n int) string {
	var sb strings.Builder
	for i := range n {
		id := strconv.Itoa(i)
		sb.WriteString("(define-syntax m" + id + `
		   (syntax-rules ()
		     ((_) '())
		     ((_ a b ...) (cons a (m` + id + ` b ...)))))
		 (define (p` + id + ` x y) (if (< x y) (+ x y) (- x y)))
		`)
	}
	return sb.String()
}

// BenchmarkBootstrapMarginalCost measures what one more bootstrap-shaped form
// costs at engine construction.
//
// This is the number that prices moving Go logic into Scheme. Bootstrap is not
// cached across engines — LoadBootstrapCore re-parses, re-expands and
// re-compiles bootstrap_macros.scm, bootstrap_procedures.scm and
// bootstrap_macros_late.scm on every NewEngine, with no memoization anywhere in
// pkg/internal/bootstrap or compilation/load_bootstrap.go. So every line that
// migrates into bootstrap Scheme is paid again per engine, and startup rather
// than expansion throughput is the binding constraint on such a migration.
//
// Engine construction is excluded from the timer so the result is the marginal
// cost alone; BenchmarkEngineStartup supplies the fixed baseline it adds to.
// Divide ns/op by the form count for a per-form rate, then multiply by the
// number of forms a proposed migration would add.
func BenchmarkBootstrapMarginalCost(b *testing.B) {
	counts := []int{8, 32}
	for _, n := range counts {
		b.Run("forms-"+strconv.Itoa(n), func(b *testing.B) {
			ctx := context.Background()
			source := bootstrapShapedSource(n)

			b.ReportAllocs()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				b.StopTimer()
				engine, err := NewEngine(ctx, WithMutableTopLevel())
				if err != nil {
					b.Fatal(err)
				}
				b.StartTimer()

				_, err = engine.EvalMultiple(ctx, source)
				if err != nil {
					b.Fatalf("load %d forms: %v", n, err)
				}
			}
		})
	}
}
