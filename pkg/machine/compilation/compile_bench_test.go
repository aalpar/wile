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
	"bufio"
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// compileBenchCorpus is a synthetic but representative set of forms that route
// through the registry most often: if / let-family / lambda / define density,
// nested, so the benchmark exercises Tier-1 dispatch under realistic recursion
// rather than a tight single-form loop (per the "micro-benchmarks mislead"
// caution in memory/). The gate is on aggregate compile throughput.
var compileBenchCorpus = []string{
	`(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))`,
	`(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))`,
	`(lambda (x y) (let ((a (+ x y)) (b (- x y))) (let* ((c (* a b)) (d (+ c 1))) (if (> d 0) c d))))`,
	`(define (sum3 a b c) (letrec ((go (lambda (acc xs) (if (null? xs) acc (go (+ acc (car xs)) (cdr xs)))))) (go 0 (list a b c))))`,
	`(let loop ((i 0) (acc 0)) (if (< i 10) (loop (+ i 1) (+ acc i)) acc))`,
	`(lambda (f g x) (let ((y (f x))) (if y (g y) (begin (set! x 0) x))))`,
	`(define (classify n) (if (< n 0) 'neg (if (= n 0) 'zero 'pos)))`,
	`(letrec* ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))`,
}

// newCompileBenchEnv extends the minimal compile-time namespace with placeholder
// primitive bindings for the runtime operators used in compileBenchCorpus.
// The compiler requires each free identifier to resolve to a known binding;
// actual callable values are not needed since the benchmark only times the
// compile phase, not execution.
func newCompileBenchEnv() *environment.EnvironmentFrame {
	env := newNamespace(environment.NewNamespace().Runtime())
	for _, name := range []string{"<", ">", "=", "+", "-", "*", "null?", "car", "cdr", "list"} {
		env.MaybeCreateOwnGlobalBinding(values.NewSymbol(name), environment.BindingTypePrimitive, nil)
	}
	return env
}

func parseForBench(b *testing.B, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.TODO())
	if err != nil {
		b.Fatalf("parse %q: %v", code, err)
	}
	return sv
}

// BenchmarkCompilePhase times the compile phase (validated-expr -> bytecode)
// only. Parse, expand, and validate all run once in setup and are excluded from
// the timer; the loop calls compileValidated directly on the pre-validated
// forms, so the delta between the pre-refactor (type-switch) commit and the
// post-refactor (registry) commit isolates dispatch cost rather than burying it
// under a per-iteration validation pass. Compare via:
//
//	benchstat old.txt new.txt
func BenchmarkCompilePhase(b *testing.B) {
	env := newCompileBenchEnv()
	eval := machine.NewVMMacroEvaluator()

	validated := make([]validate.ValidatedExpr, 0, len(compileBenchCorpus))
	for _, code := range compileBenchCorpus {
		prog := parseForBench(b, env, code)
		econt := NewExpanderTimeContinuation(context.Background(), env, eval)
		ex, err := econt.ExpandExpression(prog)
		if err != nil {
			b.Fatalf("expand %q: %v", code, err)
		}
		result := validate.ValidateExpression(context.Background(), env, ex)
		if !result.Ok() {
			b.Fatalf("validate %q: %v", code, result.Error())
		}
		validated = append(validated, result.Expr)
	}

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, ve := range validated {
			tpl := machine.NewNativeTemplate(0, 0, false)
			cctx := NewCompileTimeContinuation(tpl, env, eval)
			cnt := NewCompileTimeCallContext(context.Background(), false)
			err := cctx.compileValidated(cnt, ve)
			if err != nil {
				b.Fatalf("compile: %v", err)
			}
		}
	}
}
