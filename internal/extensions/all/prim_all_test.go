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

package all_test

import (
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// Record Primitives — Low-Level API
// =============================================================================

func TestMakeRecordType(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"returns record type", `(record-type? (make-record-type 'point '(x y)))`, values.TrueValue},
		{"not a record", `(record? (make-record-type 'point '(x y)))`, values.FalseValue},
		{"empty fields", `(record-type? (make-record-type 'empty '()))`, values.TrueValue},
		{"single field", `(record-type? (make-record-type 'wrapper '(value)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"name not symbol", `(make-record-type "point" '(x y))`},
		{"field not symbol", `(make-record-type 'point '(x "y"))`},
		{"wrong arity zero", `(make-record-type)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestRecordTypeQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"true for record type", `(record-type? (make-record-type 'foo '(a)))`, values.TrueValue},
		{"false for integer", `(record-type? 42)`, values.FalseValue},
		{"false for string", `(record-type? "hello")`, values.FalseValue},
		{"false for list", `(record-type? '(1 2))`, values.FalseValue},
		{"false for boolean", `(record-type? #t)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestRecordQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"true for record instance", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y))))
			  (record? (ctor 1 2)))`, values.TrueValue},
		{"false for record type", `(record? (make-record-type 'foo '(a)))`, values.FalseValue},
		{"false for integer", `(record? 42)`, values.FalseValue},
		{"false for string", `(record? "hello")`, values.FalseValue},
		{"false for pair", `(record? '(1 . 2))`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestRecordTypeAccessor(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// record-type returns the RecordType of a record instance
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"returns record type", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (p (ctor 1 2)))
			  (record-type? (record-type p)))`, values.TrueValue},
		{"same identity as original", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (p (ctor 1 2)))
			  (eq? rt (record-type p)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a record", `(record-type 42)`},
		{"record type not a record", `(record-type (make-record-type 'foo '()))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestRecordConstructor(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"construct and verify is record", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y))))
			  (record? (ctor 10 20)))`, values.TrueValue},
		{"partial constructor", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x)))
			       (p (ctor 10))
			       (get-x (record-accessor rt 'x))
			       (get-y (record-accessor rt 'y)))
			  (get-x p))`, values.NewInteger(10)},
		{"partial constructor default field is false", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x)))
			       (p (ctor 10))
			       (get-y (record-accessor rt 'y)))
			  (get-y p))`, values.FalseValue},
		{"empty constructor", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '()))
			       (p (ctor))
			       (get-x (record-accessor rt 'x)))
			  (get-x p))`, values.FalseValue},
		{"constructor field ordering", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(y x)))
			       (p (ctor 20 10))
			       (get-x (record-accessor rt 'x))
			       (get-y (record-accessor rt 'y)))
			  (+ (get-x p) (get-y p)))`, values.NewInteger(30)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a record type", `(record-constructor 42 '(x))`},
		{"unknown field", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-constructor rt '(z)))`},
		{"field tags not symbols", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-constructor rt '("x")))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestRecordPredicate(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"true for own type", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (pred (record-predicate rt)))
			  (pred (ctor 1 2)))`, values.TrueValue},
		{"false for different record type", `
			(let* ((rt1 (make-record-type 'point '(x y)))
			       (rt2 (make-record-type 'color '(r g b)))
			       (ctor2 (record-constructor rt2 '(r g b)))
			       (pred1 (record-predicate rt1)))
			  (pred1 (ctor2 255 0 0)))`, values.FalseValue},
		{"false for non-record", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (pred (record-predicate rt)))
			  (pred 42))`, values.FalseValue},
		{"false for string", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (pred (record-predicate rt)))
			  (pred "hello"))`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a record type", `(record-predicate 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestRecordAccessor(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"access first field", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (get-x (record-accessor rt 'x)))
			  (get-x (ctor 10 20)))`, values.NewInteger(10)},
		{"access second field", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (get-y (record-accessor rt 'y)))
			  (get-y (ctor 10 20)))`, values.NewInteger(20)},
		{"access string field", `
			(let* ((rt (make-record-type 'person '(name age)))
			       (ctor (record-constructor rt '(name age)))
			       (get-name (record-accessor rt 'name)))
			  (get-name (ctor "Alice" 30)))`, values.NewString("Alice")},
		{"access boolean field", `
			(let* ((rt (make-record-type 'flag '(value)))
			       (ctor (record-constructor rt '(value)))
			       (get-val (record-accessor rt 'value)))
			  (get-val (ctor #t)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a record type", `(record-accessor 42 'x)`},
		{"field tag not symbol", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-accessor rt "x"))`},
		{"unknown field", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-accessor rt 'z))`},
		{"accessor on wrong record type", `
			(let* ((rt1 (make-record-type 'point '(x y)))
			       (rt2 (make-record-type 'color '(r g b)))
			       (ctor2 (record-constructor rt2 '(r g b)))
			       (get-x (record-accessor rt1 'x)))
			  (get-x (ctor2 255 0 0)))`},
		{"accessor on non-record", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (get-x (record-accessor rt 'x)))
			  (get-x 42))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestRecordModifier(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"modify and read back", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (get-x (record-accessor rt 'x))
			       (set-x! (record-modifier rt 'x))
			       (p (ctor 10 20)))
			  (set-x! p 99)
			  (get-x p))`, values.NewInteger(99)},
		{"modify second field", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (get-y (record-accessor rt 'y))
			       (set-y! (record-modifier rt 'y))
			       (p (ctor 10 20)))
			  (set-y! p 99)
			  (get-y p))`, values.NewInteger(99)},
		{"modify preserves other fields", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (get-x (record-accessor rt 'x))
			       (set-y! (record-modifier rt 'y))
			       (p (ctor 10 20)))
			  (set-y! p 99)
			  (get-x p))`, values.NewInteger(10)},
		{"modify type change", `
			(let* ((rt (make-record-type 'box '(value)))
			       (ctor (record-constructor rt '(value)))
			       (get-val (record-accessor rt 'value))
			       (set-val! (record-modifier rt 'value))
			       (b (ctor 42)))
			  (set-val! b "hello")
			  (get-val b))`, values.NewString("hello")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a record type", `(record-modifier 42 'x)`},
		{"field tag not symbol", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-modifier rt "x"))`},
		{"unknown field", `
			(let ((rt (make-record-type 'point '(x y))))
			  (record-modifier rt 'z))`},
		{"modifier on wrong record type", `
			(let* ((rt1 (make-record-type 'point '(x y)))
			       (rt2 (make-record-type 'color '(r g b)))
			       (ctor2 (record-constructor rt2 '(r g b)))
			       (set-x! (record-modifier rt1 'x)))
			  (set-x! (ctor2 255 0 0) 42))`},
		{"modifier on non-record", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (set-x! (record-modifier rt 'x)))
			  (set-x! 42 99))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Records — High-Level define-record-type Macro
// =============================================================================

func TestDefineRecordType(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"construct and access", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (let ((p (make-point 3 4)))
			    (+ (point-x p) (point-y p))))`, values.NewInteger(7)},
		{"predicate true", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (point? (make-point 1 2)))`, values.TrueValue},
		{"predicate false for non-record", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (point? 42))`, values.FalseValue},
		{"predicate false for different record type", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (define-record-type <color>
			    (make-color r g b)
			    color?
			    (r color-r)
			    (g color-g)
			    (b color-b))
			  (point? (make-color 255 0 0)))`, values.FalseValue},
		{"mutable field", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x point-set-x!)
			    (y point-y point-set-y!))
			  (let ((p (make-point 3 4)))
			    (point-set-x! p 10)
			    (point-x p)))`, values.NewInteger(10)},
		{"mutation preserves other fields", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x point-set-x!)
			    (y point-y))
			  (let ((p (make-point 3 4)))
			    (point-set-x! p 10)
			    (point-y p)))`, values.NewInteger(4)},
		{"mixed mutable and immutable fields", `
			(begin
			  (define-record-type <entry>
			    (make-entry key value)
			    entry?
			    (key entry-key)
			    (value entry-value entry-set-value!))
			  (let ((e (make-entry 'name "Alice")))
			    (entry-set-value! e "Bob")
			    (entry-value e)))`, values.NewString("Bob")},
		{"record with single field", `
			(begin
			  (define-record-type <wrapper>
			    (make-wrapper val)
			    wrapper?
			    (val wrapper-val))
			  (wrapper-val (make-wrapper 42)))`, values.NewInteger(42)},
		{"nested records", `
			(begin
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (define-record-type <line>
			    (make-line start end)
			    line?
			    (start line-start)
			    (end line-end))
			  (let ((l (make-line (make-point 0 0) (make-point 3 4))))
			    (point-x (line-end l))))`, values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// =============================================================================
// Promise Primitives
// =============================================================================

func TestPromiseQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"delay creates promise", `(promise? (delay 1))`, values.TrueValue},
		{"make-promise creates promise", `(promise? (make-promise 42))`, values.TrueValue},
		{"make-promise on promise", `(promise? (make-promise (delay 1)))`, values.TrueValue},
		{"integer not promise", `(promise? 1)`, values.FalseValue},
		{"string not promise", `(promise? "hello")`, values.FalseValue},
		{"list not promise", `(promise? '(1 2 3))`, values.FalseValue},
		{"boolean not promise", `(promise? #f)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestMakePromise(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"wraps value", `(force (make-promise 42))`, values.NewInteger(42)},
		{"wraps computed value", `(force (make-promise (+ 1 2)))`, values.NewInteger(3)},
		{"wraps string", `(force (make-promise "hello"))`, values.NewString("hello")},
		{"passes promise through", `(force (make-promise (delay 99)))`, values.NewInteger(99)},
		{"make-promise is idempotent on promises", `
			(let ((p (delay 42)))
			  (eq? p (make-promise p)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestForce(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"force delayed value", `(force (delay 42))`, values.NewInteger(42)},
		{"force delayed arithmetic", `(force (delay (+ 1 2)))`, values.NewInteger(3)},
		{"force non-promise passthrough", `(force 5)`, values.NewInteger(5)},
		{"force string passthrough", `(force "hello")`, values.NewString("hello")},
		{"force memoizes result", `
			(let ((count 0))
			  (let ((p (delay (begin (set! count (+ count 1)) count))))
			    (force p)
			    (force p)
			    (force p)
			    count))`, values.NewInteger(1)},
		{"force nested delay", `(force (delay (delay 10)))`, values.NewInteger(10)},
		{"force list result", `(force (delay (cons 1 (cons 2 '()))))`,
			values.List(values.NewInteger(1), values.NewInteger(2))},
		{"force boolean", `(force (delay #t))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestDelayForce(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"simple delay-force", `(force (delay-force (make-promise 5)))`, values.NewInteger(5)},
		{"delay-force with delay", `(force (delay-force (delay 10)))`, values.NewInteger(10)},
		{"delay-force chain", `(force (delay-force (delay-force (make-promise 7))))`, values.NewInteger(7)},
		{"delay-force recursive iteration", `
			(begin
			  (define (stream-count n limit)
			    (if (>= n limit)
			        (delay n)
			        (delay-force (stream-count (+ n 1) limit))))
			  (force (stream-count 0 100)))`, values.NewInteger(100)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestMakeLazyPromise(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// %make-lazy-promise is the internal primitive used by the delay macro.
	// It creates a promise from a thunk (zero-argument lambda).
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"creates promise from thunk", `(promise? (%make-lazy-promise (lambda () 42)))`, values.TrueValue},
		{"force evaluates thunk", `(force (%make-lazy-promise (lambda () (+ 1 2))))`, values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// =============================================================================
// R7RS Promise Semantics (§4.2.5 examples)
// =============================================================================

func TestR7RSPromiseSemantics(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// R7RS §4.2.5: Memoization — forcing a promise multiple times
		// returns the same result without re-evaluating.
		{"memoization side effect counted once", `
			(let ((count 0))
			  (define p (delay (begin (set! count (+ count 1)) count)))
			  (force p)
			  (force p)
			  count)`, values.NewInteger(1)},

		// R7RS §4.2.5: force on a non-promise returns the value.
		{"force on non-promise", `(force 42)`, values.NewInteger(42)},

		// Delay-force enables safe recursive forcing (tail-call style).
		{"delay-force tail recursion", `
			(begin
			  (define (loop n)
			    (if (= n 0)
			        (delay 'done)
			        (delay-force (loop (- n 1)))))
			  (force (loop 50)))`, values.NewSymbol("done")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// =============================================================================
// Opaque Records
// =============================================================================

func TestMakeOpaqueRecordType(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"is a record type", `(record-type? (make-opaque-record-type 'stack '(items)))`, values.TrueValue},
		{"record? false for opaque instance", `
			(let* ((rt (make-opaque-record-type 'stack '(items)))
			       (ctor (record-constructor rt '(items)))
			       (s (ctor '(1 2 3))))
			  (record? s))`, values.FalseValue},
		{"record? true for normal instance", `
			(let* ((rt (make-record-type 'point '(x y)))
			       (ctor (record-constructor rt '(x y)))
			       (p (ctor 1 2)))
			  (record? p))`, values.TrueValue},
		{"type-specific predicate works", `
			(let* ((rt (make-opaque-record-type 'stack '(items)))
			       (pred (record-predicate rt))
			       (ctor (record-constructor rt '(items)))
			       (s (ctor '(1 2 3))))
			  (pred s))`, values.TrueValue},
		{"type-specific predicate rejects other types", `
			(let* ((rt (make-opaque-record-type 'stack '(items)))
			       (pred (record-predicate rt)))
			  (pred 42))`, values.FalseValue},
		{"accessor works on opaque record", `
			(let* ((rt (make-opaque-record-type 'stack '(items)))
			       (ctor (record-constructor rt '(items)))
			       (get-items (record-accessor rt 'items))
			       (s (ctor '(1 2 3))))
			  (get-items s))`, values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"modifier works on opaque record", `
			(let* ((rt (make-opaque-record-type 'stack '(items)))
			       (ctor (record-constructor rt '(items)))
			       (get-items (record-accessor rt 'items))
			       (set-items! (record-modifier rt 'items))
			       (s (ctor '(1 2 3))))
			  (set-items! s '(4 5))
			  (get-items s))`, values.List(values.NewInteger(4), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestOpaqueRecordTypeError(t *testing.T) {
	engine := newEngine(t)

	// record-type should error on opaque records
	evalExpectError(t, engine, `
		(let* ((rt (make-opaque-record-type 'stack '(items)))
		       (ctor (record-constructor rt '(items)))
		       (s (ctor '(1 2 3))))
		  (record-type s))`)
}

func TestDefineOpaqueRecordType(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"construct and access", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items))
			  (stack-items (make-stack '(1 2 3))))`,
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{"predicate true", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items))
			  (stack? (make-stack '())))`, values.TrueValue},
		{"predicate false for non-record", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items))
			  (stack? 42))`, values.FalseValue},
		{"record? false for opaque", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items))
			  (record? (make-stack '())))`, values.FalseValue},
		{"mutable opaque field", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items set-stack-items!))
			  (let ((s (make-stack '(1 2))))
			    (set-stack-items! s '(3 4 5))
			    (stack-items s)))`,
			values.List(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))},
		{"opaque and normal records coexist", `
			(begin
			  (define-opaque-record-type <stack>
			    (make-stack items)
			    stack?
			    (items stack-items))
			  (define-record-type <point>
			    (make-point x y)
			    point?
			    (x point-x)
			    (y point-y))
			  (list (record? (make-stack '()))
			        (record? (make-point 1 2))
			        (stack? (make-point 1 2))
			        (point? (make-stack '()))))`,
			values.List(values.FalseValue, values.TrueValue, values.FalseValue, values.FalseValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}
