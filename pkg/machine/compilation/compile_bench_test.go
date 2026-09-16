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
		env.MaybeCreateOwnGlobalBinding(values.NewSymbol(name), environment.BindingTypeVariable, nil)
	}
	return env
}

// condCaseBenchCorpus defines cond and case locally, verbatim from
// pkg/registry/core/bootstrap_macros.scm, because newCompileBenchEnv's minimal
// namespace never loads stdlib bootstrap (see newNamespace); compileBenchCorpus
// has no cond/case at all, so neither BenchmarkValidatePhase nor
// BenchmarkFrontEndPhase can see the per-clause else/=> literal-resolution cost
// the renamed-auxiliary-keyword fix (pkg/internal/match) added.
//
// Three clause shapes, chosen because they cost differently under that fix:
//
//   - flag-chain / arrow-chain: each non-final clause's test is a bare
//     identifier, or uses the (test => proc) idiom. This is the shape that
//     puts a real symbol at cond's else/=> literal-comparison position for
//     every clause tried — match.go's ByteCodeCompareCar only calls
//     literalMatcher when the input at a literal position is itself a
//     *syntax.SyntaxSymbol (pkg/internal/match/match.go, the isLiteral/
//     inputIsSym guard around line 348), so this shape pays one binding
//     resolution per clause, i.e. O(N).
//   - compound-test: each clause's test is a compound expression, (< n K),
//     matching compileBenchCorpus's own if/let style. The else/=> position
//     never sees a symbol here, so this shape never reaches literalMatcher at
//     all — kept as a control to contrast against flag-chain/arrow-chain.
//   - tag5 (case): every non-final clause's head is a LIST, (atoms ...), never
//     a symbol, so unlike cond's flag-chain shape, case's else-literal check
//     is reached (and pays one resolution) only once, for the terminal else
//     clause, regardless of N — case's per-clause cost from this fix is O(1),
//     not O(N).
var condCaseBenchCorpus = []string{
	`(define-syntax cond
	   (syntax-rules (else =>)
	     ((cond (else result1 result2 ...)) (begin result1 result2 ...))
	     ((cond (test => result)) (let ((temp test)) (if temp (result temp))))
	     ((cond (test => result) clause1 clause2 ...)
	      (let ((temp test)) (if temp (result temp) (cond clause1 clause2 ...))))
	     ((cond (test)) test)
	     ((cond (test) clause1 clause2 ...)
	      (let ((temp test)) (if temp temp (cond clause1 clause2 ...))))
	     ((cond (test result1 result2 ...)) (if test (begin result1 result2 ...)))
	     ((cond (test result1 result2 ...) clause1 clause2 ...)
	      (if test (begin result1 result2 ...) (cond clause1 clause2 ...)))))`,
	`(define-syntax case
	   (syntax-rules (else =>)
	     ((case (key ...) clauses ...) (let ((atom-key (key ...))) (case atom-key clauses ...)))
	     ((case key (else => result)) (result key))
	     ((case key (else result1 result2 ...)) (begin result1 result2 ...))
	     ((case key ((atoms ...) => result)) (if (memv key '(atoms ...)) (result key)))
	     ((case key ((atoms ...) => result) clause clauses ...)
	      (if (memv key '(atoms ...)) (result key) (case key clause clauses ...)))
	     ((case key ((atoms ...) result1 result2 ...)) (if (memv key '(atoms ...)) (begin result1 result2 ...)))
	     ((case key ((atoms ...) result1 result2 ...) clause clauses ...)
	      (if (memv key '(atoms ...)) (begin result1 result2 ...) (case key clause clauses ...)))))`,
	`(define (flag-chain a b c d e) (cond (a 'a) (b 'b) (c 'c) (d 'd) (else 'other)))`,
	`(define (arrow-chain a b) (cond (a => car) (b => cdr) (else 'other)))`,
	`(define (compound-test n) (cond ((< n 0) 'neg) ((< n 10) 'small) ((< n 100) 'mid) (else 'large)))`,
	`(define (tag5 n) (case n ((0) 'zero) ((1) 'one) ((2) 'two) ((3) 'three) (else 'other)))`,
}

// quasiquoteBenchCorpus exists for the same reason condCaseBenchCorpus does, one
// subsystem over: compileBenchCorpus and condCaseBenchCorpus contain zero
// quasiquote between them, so neither BenchmarkValidatePhase nor
// BenchmarkFrontEndPhase can see the quasiquote change at all — a measurement
// taken on them and reported as "no delta for the marker fix" is reporting on a
// corpus the fix never touches.
//
// Both walks the fix moved are in here, because they cost at different places:
//
//   - the EXPANDER's (quasi_expand.go), one headName per template pair head, now
//     a binding resolution rather than a string compare;
//   - VALIDATE's (opaque_subtree.go), one markerName per pair head in the same
//     template, plus the same walk again per enclosing region from the boxing
//     pass's reference index (ref_index.go).
//
// So the benchmark runs the full parse → expand → validate → compile, not expand
// alone as BenchmarkCondClauseLiteralExpand does.
//
// The shapes are chosen for head density rather than size: a template whose pairs
// are mostly data still pays one resolution per head. row/deep are ordinary
// element templates, nest reaches depth 2 (where an unquote does NOT fire and the
// walk keeps descending), splice takes the append path, dotted is the
// (a unquote b) spine shape both walks special-case, and vec is the vector arm.
var quasiquoteBenchCorpus = []string{
	`(define (row a b) (quasiquote (x (unquote a) y (unquote b) z)))`,
	`(define (nest a) (quasiquote (1 (quasiquote (2 (unquote (unquote a)))) 3)))`,
	`(define (splice xs) (quasiquote (head (unquote-splicing xs) tail)))`,
	`(define (dotted a b) (quasiquote (a unquote b)))`,
	`(define (deep a) (quasiquote ((k1 (unquote a)) (k2 (quote lit)) (k3 (unquote (+ a 1))))))`,
	`(define (vec a b) (quasiquote #(x (unquote a) y (unquote b) z)))`,
}

// newQuasiquoteBenchEnv extends newCompileBenchEnv with the list-construction
// operators quasiquote SYNTHESIZES — quasiHead finds no sealed binding in this
// minimal namespace, so the references it emits are ordinary free ones the
// compiler still has to resolve. Its own function, so it cannot perturb
// BenchmarkValidatePhase/BenchmarkFrontEndPhase's environment.
func newQuasiquoteBenchEnv() *environment.EnvironmentFrame {
	env := newCompileBenchEnv()
	for _, name := range []string{"cons", "append", "list->vector"} {
		env.MaybeCreateOwnGlobalBinding(values.NewSymbol(name), environment.BindingTypeVariable, nil)
	}
	return env
}

// newCondCaseBenchEnv extends newCompileBenchEnv with "memv", which case's
// bootstrap expansion references; kept as its own function so this benchmark's
// setup never changes BenchmarkValidatePhase/BenchmarkFrontEndPhase's own
// environment.
func newCondCaseBenchEnv() *environment.EnvironmentFrame {
	env := newCompileBenchEnv()
	env.MaybeCreateOwnGlobalBinding(values.NewSymbol("memv"), environment.BindingTypeVariable, nil)
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
