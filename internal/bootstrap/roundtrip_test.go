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

package bootstrap

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// evalSchemeEscape is like evalScheme but uses RunWithEscapeHandling to support
// call/cc invocations, guard, with-exception-handler + raise, dynamic-wind
// escapes, and delimited continuations (prompts).
func evalSchemeEscape(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(context.TODO())
	if err != nil {
		return nil, err
	}

	ectx := context.Background()
	expanded, err := machine.NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(context.Background(), false)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	err = mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}

	return mc.GetValue(), nil
}

// roundTripCase defines a single round-trip test: Scheme code in, expected value out.
type roundTripCase struct {
	name     string
	code     string
	expected values.Value
}

// evalFunc selects between evalScheme and evalSchemeEscape.
type evalFunc func(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error)

// runRoundTrips creates one environment and runs all cases through the full
// parse -> expand -> compile -> execute pipeline.
func runRoundTrips(t *testing.T, cases []roundTripCase) {
	runRoundTripsWith(t, evalScheme, cases)
}

// runRoundTripsEscape is like runRoundTrips but uses RunWithEscapeHandling.
func runRoundTripsEscape(t *testing.T, cases []roundTripCase) {
	runRoundTripsWith(t, evalSchemeEscape, cases)
}

func runRoundTripsWith(t *testing.T, eval evalFunc, cases []roundTripCase) {
	t.Helper()
	c := qt.New(t)
	env, err := NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)

	for _, tt := range cases {
		c.Run(tt.name, func(c *qt.C) {
			result, err := eval(t, env, tt.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tt.expected)
		})
	}
}

// runRoundTripsBool is like runRoundTrips but all cases are expected to return #t.
func runRoundTripsBool(t *testing.T, cases []struct{ name, code string }) {
	runRoundTripsBoolWith(t, evalScheme, cases)
}

// runRoundTripsBoolEscape is like runRoundTripsBool but uses RunWithEscapeHandling.
func runRoundTripsBoolEscape(t *testing.T, cases []struct{ name, code string }) {
	runRoundTripsBoolWith(t, evalSchemeEscape, cases)
}

func runRoundTripsBoolWith(t *testing.T, eval evalFunc, cases []struct{ name, code string }) {
	t.Helper()
	c := qt.New(t)
	env, err := NewTopLevelEnvironmentFrameTiny(context.TODO())
	c.Assert(err, qt.IsNil)

	for _, tt := range cases {
		c.Run(tt.name, func(c *qt.C) {
			result, err := eval(t, env, tt.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, qt.Equals, values.TrueValue)
		})
	}
}

// ===========================================================================
// Arithmetic and Numeric Operations
// ===========================================================================

func TestRoundTrip_Arithmetic(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"add", `(+ 1 2)`, values.NewInteger(3)},
		{"add-multi", `(+ 1 2 3 4 5)`, values.NewInteger(15)},
		{"add-zero", `(+)`, values.NewInteger(0)},
		{"sub", `(- 10 3)`, values.NewInteger(7)},
		{"sub-negate", `(- 5)`, values.NewInteger(-5)},
		{"mul", `(* 6 7)`, values.NewInteger(42)},
		{"mul-zero", `(*)`, values.NewInteger(1)},
		{"div", `(/ 10 2)`, values.NewInteger(5)},
		{"abs-pos", `(abs 42)`, values.NewInteger(42)},
		{"abs-neg", `(abs -7)`, values.NewInteger(7)},
		{"min", `(min 3 1 4 1 5)`, values.NewInteger(1)},
		{"max", `(max 3 1 4 1 5)`, values.NewInteger(5)},
		{"quotient", `(quotient 13 4)`, values.NewInteger(3)},
		{"remainder", `(remainder 13 4)`, values.NewInteger(1)},
		{"modulo", `(modulo 13 4)`, values.NewInteger(1)},
		{"gcd", `(gcd 12 8)`, values.NewInteger(4)},
		{"lcm", `(lcm 4 6)`, values.NewInteger(12)},
	})
}

func TestRoundTrip_NumericComparisons(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"lt", `(< 1 2 3)`},
		{"gt", `(> 3 2 1)`},
		{"le", `(<= 1 1 2)`},
		{"ge", `(>= 3 3 1)`},
		{"eq", `(= 5 5 5)`},
		{"zero", `(zero? 0)`},
		{"positive", `(positive? 1)`},
		{"negative", `(negative? -1)`},
		{"even", `(even? 4)`},
		{"odd", `(odd? 3)`},
	})
}

func TestRoundTrip_Exactness(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"exact-int", `(exact? 1)`},
		{"inexact-float", `(inexact? 1.0)`},
		{"exact->inexact", `(= (exact->inexact 1) 1.0)`},
		{"inexact->exact", `(= (inexact->exact 1.0) 1)`},
	})
}

// ===========================================================================
// Type Predicates
// ===========================================================================

func TestRoundTrip_TypePredicates(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"number", `(number? 42)`},
		{"not-number", `(not (number? "hello"))`},
		{"integer", `(integer? 5)`},
		{"real", `(real? 3.14)`},
		{"complex", `(complex? 1+2i)`},
		{"string", `(string? "hello")`},
		{"not-string", `(not (string? 42))`},
		{"char", `(char? #\A)`},
		{"boolean-true", `(boolean? #t)`},
		{"boolean-false", `(boolean? #f)`},
		{"pair", `(pair? '(1 . 2))`},
		{"not-pair-null", `(not (pair? '()))`},
		{"null", `(null? '())`},
		{"not-null", `(not (null? '(1)))`},
		{"symbol", `(symbol? 'foo)`},
		{"vector", `(vector? (vector 1 2 3))`},
		{"bytevector", `(bytevector? (bytevector 1 2 3))`},
		{"procedure", `(procedure? car)`},
		{"procedure-lambda", `(procedure? (lambda (x) x))`},
	})
}

// ===========================================================================
// Equality
// ===========================================================================

func TestRoundTrip_Equality(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"eq-bool", `(eq? #t #t)`},
		{"eq-symbol", `(eq? 'foo 'foo)`},
		{"eq-empty", `(eq? '() '())`},
		{"eqv-int", `(eqv? 42 42)`},
		{"eqv-char", `(eqv? #\A #\A)`},
		{"equal-list", `(equal? '(1 2 3) '(1 2 3))`},
		{"equal-string", `(equal? "hello" "hello")`},
		{"equal-vector", `(equal? (vector 1 2) (vector 1 2))`},
		{"not-equal", `(not (equal? '(1 2) '(1 3)))`},
		{"boolean-eq", `(boolean=? #t #t)`},
		{"symbol-eq", `(symbol=? 'abc 'abc)`},
		{"not-fn", `(not #f)`},
		{"not-fn-true", `(not (not #t))`},
	})
}

// ===========================================================================
// Pairs and Lists
// ===========================================================================

func TestRoundTrip_PairsAndLists(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"cons", `(cons 1 2)`, values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{"car", `(car '(1 2 3))`, values.NewInteger(1)},
		{"cdr-car", `(car (cdr '(1 2 3)))`, values.NewInteger(2)},
		{"list-length", `(length '(1 2 3))`, values.NewInteger(3)},
		{"list-ref", `(list-ref '(a b c d) 2)`, values.NewSymbol("c")},
	})
}

func TestRoundTrip_ListOperations(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"list-construct", `(equal? (list 1 2 3) '(1 2 3))`},
		{"append", `(equal? (append '(1 2) '(3 4)) '(1 2 3 4))`},
		{"reverse", `(equal? (reverse '(1 2 3)) '(3 2 1))`},
		{"list-tail", `(equal? (list-tail '(a b c d) 2) '(c d))`},
		{"list-copy", `(equal? (list-copy '(1 2 3)) '(1 2 3))`},
		{"make-list", `(equal? (make-list 3 0) '(0 0 0))`},
		{"memq", `(equal? (memq 'b '(a b c)) '(b c))`},
		{"memv", `(equal? (memv 2 '(1 2 3)) '(2 3))`},
		{"assq", `(equal? (assq 'b '((a 1) (b 2) (c 3))) '(b 2))`},
		{"assv", `(equal? (assv 2 '((1 a) (2 b) (3 c))) '(2 b))`},
		{"list-set", `(let ((ls (list 1 2 3))) (list-set! ls 1 99) (equal? ls '(1 99 3)))`},
		{"set-car", `(let ((p (cons 1 2))) (set-car! p 10) (= (car p) 10))`},
		{"set-cdr", `(let ((p (cons 1 2))) (set-cdr! p 20) (= (cdr p) 20))`},
		{"cxr-cadr", `(= (cadr '(1 2 3)) 2)`},
		{"cxr-caddr", `(= (caddr '(1 2 3)) 3)`},
		{"cxr-caar", `(= (caar '((10 20) 30)) 10)`},
	})
}

// ===========================================================================
// Strings
// ===========================================================================

func TestRoundTrip_Strings(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"length", `(string-length "hello")`, values.NewInteger(5)},
		{"ref", `(string-ref "hello" 1)`, values.NewCharacter('e')},
		{"append", `(string-append "foo" "bar")`, values.NewString("foobar")},
		{"number->string", `(number->string 42)`, values.NewString("42")},
		{"string->number", `(string->number "42")`, values.NewInteger(42)},
		{"substring", `(substring "hello" 1 3)`, values.NewString("el")},
		{"string->symbol", `(string->symbol "foo")`, values.NewSymbol("foo")},
		{"symbol->string", `(symbol->string 'foo)`, values.NewString("foo")},
		{"make-string", `(make-string 3 #\x)`, values.NewString("xxx")},
		{"string-copy", `(string-copy "hello")`, values.NewString("hello")},
	})
}

func TestRoundTrip_StringComparisons(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"lt", `(string<? "abc" "abd")`},
		{"gt", `(string>? "abd" "abc")`},
		{"eq", `(string=? "abc" "abc")`},
		{"le", `(string<=? "abc" "abc")`},
		{"ge", `(string>=? "abc" "abc")`},
	})
}

// ===========================================================================
// Characters
// ===========================================================================

func TestRoundTrip_Characters(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"char->integer", `(char->integer #\A)`, values.NewInteger(65)},
		{"integer->char", `(integer->char 65)`, values.NewCharacter('A')},
	})
}

func TestRoundTrip_CharComparisons(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"eq", `(char=? #\A #\A)`},
		{"lt", `(char<? #\A #\B)`},
		{"gt", `(char>? #\B #\A)`},
		{"le", `(char<=? #\A #\A)`},
		{"ge", `(char>=? #\B #\A)`},
	})
}

// ===========================================================================
// Vectors
// ===========================================================================

func TestRoundTrip_Vectors(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"ref", `(vector-ref (vector 10 20 30) 1)`, values.NewInteger(20)},
		{"length", `(vector-length (make-vector 5 0))`, values.NewInteger(5)},
	})
}

func TestRoundTrip_VectorOperations(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"vector-set", `(let ((v (vector 1 2 3))) (vector-set! v 1 99) (= (vector-ref v 1) 99))`},
		{"vector->list", `(equal? (vector->list (vector 1 2 3)) '(1 2 3))`},
		{"list->vector", `(equal? (vector->list (list->vector '(a b c))) '(a b c))`},
		{"vector-fill", `(let ((v (make-vector 3 0))) (vector-fill! v 7) (= (vector-ref v 0) 7))`},
		{"vector-copy", `(equal? (vector->list (vector-copy (vector 1 2 3))) '(1 2 3))`},
	})
}

// ===========================================================================
// Bytevectors
// ===========================================================================

func TestRoundTrip_Bytevectors(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"u8-ref", `(bytevector-u8-ref (bytevector 10 20 30) 1)`, values.NewInteger(20)},
		{"length", `(bytevector-length (make-bytevector 5 0))`, values.NewInteger(5)},
	})
}

func TestRoundTrip_BytevectorOperations(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"u8-set", `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) (= (bytevector-u8-ref bv 1) 99))`},
		{"utf8-roundtrip", `(equal? (utf8->string (string->utf8 "hello")) "hello")`},
		{"copy", `(let ((bv (bytevector-copy (bytevector 1 2 3)))) (= (bytevector-u8-ref bv 2) 3))`},
	})
}

// ===========================================================================
// Boxes
// ===========================================================================

func TestRoundTrip_Boxes(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"unbox", `(unbox (box 42))`, values.NewInteger(42)},
	})
}

func TestRoundTrip_BoxOperations(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"box-pred", `(box? (box 1))`},
		{"not-box", `(not (box? 42))`},
		{"set-box", `(let ((b (box 1))) (set-box! b 99) (= (unbox b) 99))`},
	})
}

// ===========================================================================
// Hashtables
// ===========================================================================

func TestRoundTrip_Hashtables(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"ref", `(let ((ht (make-hashtable))) (hashtable-set! ht 'key 42) (hashtable-ref ht 'key #f))`, values.NewInteger(42)},
		{"size", `(let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-set! ht 'b 2) (hashtable-size ht))`, values.NewInteger(2)},
		{"ref-default", `(hashtable-ref (make-hashtable) 'missing 99)`, values.NewInteger(99)},
	})
}

func TestRoundTrip_HashtableOperations(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"pred", `(hashtable? (make-hashtable))`},
		{"not-ht", `(not (hashtable? 42))`},
		{"delete", `(let ((ht (make-hashtable)))
			(hashtable-set! ht 'k 1)
			(hashtable-delete! ht 'k)
			(= (hashtable-size ht) 0))`},
		{"keys", `(let ((ht (make-hashtable)))
			(hashtable-set! ht 'a 1)
			(= (length (hashtable-keys ht)) 1))`},
		{"values", `(let ((ht (make-hashtable)))
			(hashtable-set! ht 'a 42)
			(= (car (hashtable-values ht)) 42))`},
		{"copy", `(let* ((ht (make-hashtable))
			(_ (hashtable-set! ht 'x 10))
			(ht2 (hashtable-copy ht)))
			(= (hashtable-ref ht2 'x #f) 10))`},
		{"clear", `(let ((ht (make-hashtable)))
			(hashtable-set! ht 'a 1)
			(hashtable-clear! ht)
			(= (hashtable-size ht) 0))`},
	})
}

// ===========================================================================
// Control Flow: apply, call/cc, values, call-with-values
// ===========================================================================

func TestRoundTrip_Apply(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"simple", `(apply + '(1 2 3))`, values.NewInteger(6)},
		{"prefix-args", `(apply + 1 2 '(3 4 5))`, values.NewInteger(15)},
		{"lambda", `(apply (lambda (x y) (+ x y)) '(3 4))`, values.NewInteger(7)},
	})
}

func TestRoundTrip_CallCC(t *testing.T) {
	runRoundTripsEscape(t, []roundTripCase{
		{"normal-return", `(call/cc (lambda (k) 42))`, values.NewInteger(42)},
		{"immediate-escape", `(call/cc (lambda (k) (k 99)))`, values.NewInteger(99)},
		{"skip-computation", `(+ 1 (call/cc (lambda (k) (+ 100 (k 10)))))`, values.NewInteger(11)},
		{"full-name", `(call-with-current-continuation (lambda (k) (k 77)))`, values.NewInteger(77)},
	})
}

func TestRoundTrip_Values(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"single", `(values 42)`, values.NewInteger(42)},
		{"call-with-values-add", `(call-with-values (lambda () (values 1 2)) +)`, values.NewInteger(3)},
		{"call-with-values-list", `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))`, values.NewInteger(6)},
		{"single-producer", `(call-with-values (lambda () 42) (lambda (x) (* x 2)))`, values.NewInteger(84)},
	})
}

// ===========================================================================
// Dynamic-wind
// ===========================================================================

func TestRoundTrip_DynamicWind(t *testing.T) {
	runRoundTripsEscape(t, []roundTripCase{
		{"returns-thunk-result",
			`(dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f))`,
			values.NewInteger(42)},
		{"before-runs-first",
			`(let ((v (make-vector 1 0)))
			   (dynamic-wind
			     (lambda () (vector-set! v 0 1))
			     (lambda () (vector-ref v 0))
			     (lambda () (vector-set! v 0 2))))`,
			values.NewInteger(1)},
	})
}

func TestRoundTrip_DynamicWindOrder(t *testing.T) {
	runRoundTripsBoolEscape(t, []struct{ name, code string }{
		{"execution-order",
			`(equal?
			   (let ((log '()))
			     (dynamic-wind
			       (lambda () (set! log (cons 'before log)))
			       (lambda () (set! log (cons 'during log)) 'result)
			       (lambda () (set! log (cons 'after log))))
			     (reverse log))
			   '(before during after))`},
		{"after-on-escape",
			`(let ((after-ran #f))
			   (call/cc (lambda (k)
			     (dynamic-wind
			       (lambda () #f)
			       (lambda () (k 'escaped))
			       (lambda () (set! after-ran #t)))))
			   after-ran)`},
	})
}

// ===========================================================================
// Delimited Continuations (Prompts)
// ===========================================================================

func TestRoundTrip_Prompts(t *testing.T) {
	runRoundTripsEscape(t, []roundTripCase{
		{"normal-return",
			`(let ((tag (make-continuation-prompt-tag 'test)))
			   (call-with-continuation-prompt
			     (lambda () 99)
			     tag
			     (lambda (v) v)))`,
			values.NewInteger(99)},
		{"abort-single",
			`(let ((tag (make-continuation-prompt-tag 'test)))
			   (call-with-continuation-prompt
			     (lambda () (abort-current-continuation tag 42))
			     tag
			     (lambda (v) v)))`,
			values.NewInteger(42)},
		{"abort-multi",
			`(let ((tag (make-continuation-prompt-tag 'test)))
			   (call-with-continuation-prompt
			     (lambda () (abort-current-continuation tag 1 2 3))
			     tag
			     (lambda (a b c) (+ a b c))))`,
			values.NewInteger(6)},
		{"composable",
			`(let ((tag (make-continuation-prompt-tag 'test)))
			   (call-with-continuation-prompt
			     (lambda ()
			       (+ 1 (call-with-composable-continuation
			               (lambda (k) (k 10))
			               tag)))
			     tag
			     #f))`,
			values.NewInteger(11)},
	})
}

func TestRoundTrip_PromptPredicates(t *testing.T) {
	runRoundTripsBoolEscape(t, []struct{ name, code string }{
		{"tag-pred", `(continuation-prompt-tag? (make-continuation-prompt-tag))`},
		{"default-tag", `(continuation-prompt-tag? (default-continuation-prompt-tag))`},
		{"not-tag", `(not (continuation-prompt-tag? 42))`},
	})
}

// ===========================================================================
// Exception Handling
// ===========================================================================

func TestRoundTrip_Guard(t *testing.T) {
	runRoundTripsEscape(t, []roundTripCase{
		{"else-clause",
			`(guard (exn (else 'caught)) (raise 'error))`,
			values.NewSymbol("caught")},
		{"test-clause",
			`(guard (exn ((number? exn) 'was-number) (else 'other)) (raise 123))`,
			values.NewSymbol("was-number")},
		{"no-exception",
			`(guard (exn (else 'error)) (+ 1 2))`,
			values.NewInteger(3)},
		{"exception-value",
			`(guard (exn (else exn)) (raise 42))`,
			values.NewInteger(42)},
		{"multiple-clauses",
			`(guard (exn
			   ((string? exn) 'was-string)
			   ((number? exn) 'was-number)
			   (else 'other))
			  (raise "boom"))`,
			values.NewSymbol("was-string")},
	})
}

func TestRoundTrip_RaiseContinuable(t *testing.T) {
	runRoundTripsEscape(t, []roundTripCase{
		{"handler-return",
			`(with-exception-handler
			   (lambda (e) (+ e 100))
			   (lambda () (raise-continuable 5)))`,
			values.NewInteger(105)},
		{"resume-computation",
			`(with-exception-handler
			   (lambda (e) (* e 2))
			   (lambda () (+ (raise-continuable 7) 3)))`,
			values.NewInteger(17)},
	})
}

func TestRoundTrip_ErrorObjects(t *testing.T) {
	runRoundTripsBoolEscape(t, []struct{ name, code string }{
		{"error-object",
			`(call/cc (lambda (escape)
			   (with-exception-handler
			     (lambda (e) (escape (error-object? e)))
			     (lambda () (error "test")))))`},
		{"error-message",
			`(call/cc (lambda (escape)
			   (with-exception-handler
			     (lambda (e) (escape (equal? (error-object-message e) "hello")))
			     (lambda () (error "hello")))))`},
		{"error-irritants",
			`(call/cc (lambda (escape)
			   (with-exception-handler
			     (lambda (e) (escape (equal? (error-object-irritants e) '(a b))))
			     (lambda () (error "msg" 'a 'b)))))`},
		{"not-error-object", `(not (error-object? 42))`},
		{"not-read-error", `(not (read-error? 42))`},
		{"not-file-error", `(not (file-error? 42))`},
	})
}

// ===========================================================================
// I/O: String Ports
// ===========================================================================

func TestRoundTrip_StringPorts(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"read-integer",
			`(let ((p (open-input-string "42"))) (read p))`,
			values.NewInteger(42)},
		{"read-symbol",
			`(let ((p (open-input-string "foo"))) (read p))`,
			values.NewSymbol("foo")},
		{"display-string",
			`(let ((p (open-output-string))) (display "hello" p) (get-output-string p))`,
			values.NewString("hello")},
		{"display-number",
			`(let ((p (open-output-string))) (display 42 p) (get-output-string p))`,
			values.NewString("42")},
		{"write-number",
			`(let ((p (open-output-string))) (write 42 p) (get-output-string p))`,
			values.NewString("42")},
		{"multi-display",
			`(let ((p (open-output-string)))
			   (display "a" p) (display "b" p) (display "c" p)
			   (get-output-string p))`,
			values.NewString("abc")},
	})
}

func TestRoundTrip_StringPortPredicates(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"input-port", `(input-port? (open-input-string "test"))`},
		{"output-port", `(output-port? (open-output-string))`},
		{"port-input", `(port? (open-input-string "x"))`},
		{"port-output", `(port? (open-output-string))`},
		{"eof-empty", `(eof-object? (read (open-input-string "")))`},
		{"eof-past-end", `(let ((p (open-input-string "42"))) (read p) (eof-object? (read p)))`},
	})
}

func TestRoundTrip_CharacterIO(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"write-char",
			`(let ((p (open-output-string)))
			   (write-char #\A p) (write-char #\B p)
			   (get-output-string p))`,
			values.NewString("AB")},
		{"read-char",
			`(let ((p (open-input-string "A"))) (read-char p))`,
			values.NewCharacter('A')},
		{"peek-char",
			`(let ((p (open-input-string "X")))
			   (peek-char p)
			   (read-char p))`,
			values.NewCharacter('X')},
	})
}

// ===========================================================================
// Math / Transcendentals
// ===========================================================================

func TestRoundTrip_Math(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"sqrt", `(= (sqrt 4) 2.0)`},
		{"expt-int", `(= (expt 2 10) 1024)`},
		{"floor", `(= (floor 3.7) 3.0)`},
		{"ceiling", `(= (ceiling 3.2) 4.0)`},
		{"truncate", `(= (truncate 3.7) 3.0)`},
		{"round", `(= (round 3.5) 4.0)`},
		{"sin-zero", `(= (sin 0) 0.0)`},
		{"cos-zero", `(= (cos 0) 1.0)`},
		{"exp-zero", `(= (exp 0) 1.0)`},
		{"log-one", `(= (log 1) 0.0)`},
		{"atan-zero", `(= (atan 0) 0.0)`},
		{"finite", `(finite? 1.0)`},
		{"infinite", `(infinite? +inf.0)`},
		{"nan", `(nan? +nan.0)`},
	})
}

// ===========================================================================
// Parameters
// ===========================================================================

func TestRoundTrip_Parameters(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"basic",
			`(let ((p (make-parameter 10))) (p))`,
			values.NewInteger(10)},
		{"parameterize",
			`(let ((p (make-parameter 10)))
			   (parameterize ((p 20)) (p)))`,
			values.NewInteger(20)},
		{"restore-after",
			`(let ((p (make-parameter 10)))
			   (parameterize ((p 20)) #f)
			   (p))`,
			values.NewInteger(10)},
	})
}

func TestRoundTrip_ParameterPredicates(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"parameter", `(parameter? (make-parameter 1))`},
		{"not-parameter", `(not (parameter? 42))`},
	})
}

// ===========================================================================
// Promises
// ===========================================================================

func TestRoundTrip_Promises(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"force-delay", `(force (delay 42))`, values.NewInteger(42)},
		{"force-expression", `(force (delay (+ 1 2)))`, values.NewInteger(3)},
	})
}

func TestRoundTrip_PromisePredicates(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"promise", `(promise? (delay 1))`},
		{"not-promise", `(not (promise? 42))`},
	})
}

// ===========================================================================
// Records
// ===========================================================================

func TestRoundTrip_Records(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"define-and-create",
			`(begin
			   (define-record-type <point>
			     (make-point x y)
			     point?
			     (x point-x)
			     (y point-y))
			   (point? (make-point 3 4)))`},
		{"accessor",
			`(begin
			   (define-record-type <pair-rec>
			     (make-pair-rec a b)
			     pair-rec?
			     (a pair-rec-a)
			     (b pair-rec-b))
			   (= (pair-rec-a (make-pair-rec 10 20)) 10))`},
		{"not-record",
			`(begin
			   (define-record-type <thing>
			     (make-thing v)
			     thing?
			     (v thing-v))
			   (not (thing? 42)))`},
	})
}

// ===========================================================================
// Map and For-Each
// ===========================================================================

func TestRoundTrip_Map(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"single-list", `(equal? (map (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))`},
		{"two-lists", `(equal? (map + '(1 2 3) '(10 20 30)) '(11 22 33))`},
		{"empty", `(null? (map (lambda (x) x) '()))`},
		{"with-car", `(equal? (map car '((a b) (c d) (e f))) '(a c e))`},
		{"three-lists", `(equal? (map + '(1 2) '(10 20) '(100 200)) '(111 222))`},
	})
}

func TestRoundTrip_ForEach(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"side-effects",
			`(let ((result '()))
			   (for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
			   (equal? result '(3 2 1)))`},
		{"two-lists",
			`(let ((result '()))
			   (for-each (lambda (x y) (set! result (cons (+ x y) result))) '(1 2 3) '(10 20 30))
			   (equal? result '(33 22 11)))`},
		{"empty-no-effect",
			`(let ((called #f))
			   (for-each (lambda (x) (set! called #t)) '())
			   (not called))`},
	})
}

// ===========================================================================
// Do (Iteration)
// ===========================================================================

func TestRoundTrip_Do(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"sum-loop",
			`(do ((i 0 (+ i 1))
			      (sum 0 (+ sum i)))
			     ((= i 5) sum))`,
			values.NewInteger(10)},
		{"collect",
			`(do ((i 3 (- i 1))
			      (ls '() (cons i ls)))
			     ((= i 0) ls))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
	})
}

// ===========================================================================
// Case Expression
// ===========================================================================

func TestRoundTrip_Case(t *testing.T) {
	runRoundTrips(t, []roundTripCase{
		{"match-first",
			`(case (+ 1 1)
			   ((1) 'one)
			   ((2) 'two)
			   ((3) 'three))`,
			values.NewSymbol("two")},
		{"else-clause",
			`(case 99
			   ((1) 'one)
			   (else 'other))`,
			values.NewSymbol("other")},
		{"multi-datum",
			`(case 'b
			   ((a b c) 'first)
			   ((d e f) 'second))`,
			values.NewSymbol("first")},
	})
}

// ===========================================================================
// Quasiquote
// ===========================================================================

func TestRoundTrip_Quasiquote(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"basic", "(equal? `(1 2 3) '(1 2 3))"},
		{"unquote", "(equal? `(1 ,(+ 1 1) 3) '(1 2 3))"},
		{"splicing", "(equal? `(1 ,@(list 2 3) 4) '(1 2 3 4))"},
		{"nested", "(let ((x 10)) (equal? `(a ,x b) '(a 10 b)))"},
	})
}

// ===========================================================================
// System Interface
// ===========================================================================

func TestRoundTrip_System(t *testing.T) {
	runRoundTripsBool(t, []struct{ name, code string }{
		{"features-list", `(list? (features))`},
		{"features-has-r7rs", `(if (memq 'r7rs (features)) #t #f)`},
		{"jiffies-positive", `(> (current-jiffy) 0)`},
		{"jiffies-per-second", `(> (jiffies-per-second) 0)`},
	})
}
