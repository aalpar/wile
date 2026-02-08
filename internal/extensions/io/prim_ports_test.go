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

package io_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the I/O extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(
		wile.WithExtension(extio.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

// =============================================================================
// Port Predicates — port?, input-port?, output-port?
// =============================================================================

func TestPortPredicate(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// port?
		{"string input port is port", `(port? (open-input-string "x"))`, values.TrueValue},
		{"string output port is port", `(port? (open-output-string))`, values.TrueValue},
		{"bytevector input port is port", `(port? (open-input-bytevector #u8(1)))`, values.TrueValue},
		{"bytevector output port is port", `(port? (open-output-bytevector))`, values.TrueValue},
		{"integer is not port", `(port? 42)`, values.FalseValue},
		{"string is not port", `(port? "hello")`, values.FalseValue},
		{"boolean is not port", `(port? #t)`, values.FalseValue},
		{"eof is not port", `(port? (eof-object))`, values.FalseValue},
		// input-port?
		{"string input port is input", `(input-port? (open-input-string "x"))`, values.TrueValue},
		{"bytevector input port is input", `(input-port? (open-input-bytevector #u8(1)))`, values.TrueValue},
		{"string output port is not input", `(input-port? (open-output-string))`, values.FalseValue},
		{"bytevector output port is not input", `(input-port? (open-output-bytevector))`, values.FalseValue},
		{"integer is not input port", `(input-port? 42)`, values.FalseValue},
		// output-port?
		{"string output port is output", `(output-port? (open-output-string))`, values.TrueValue},
		{"bytevector output port is output", `(output-port? (open-output-bytevector))`, values.TrueValue},
		{"string input port is not output", `(output-port? (open-input-string "x"))`, values.FalseValue},
		{"bytevector input port is not output", `(output-port? (open-input-bytevector #u8(1)))`, values.FalseValue},
		{"integer is not output port", `(output-port? 42)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Textual/Binary Port Predicates — textual-port?, binary-port?
// =============================================================================

func TestTextualBinaryPortPredicate(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// textual-port?
		{"string input port is textual", `(textual-port? (open-input-string "x"))`, values.TrueValue},
		{"string output port is textual", `(textual-port? (open-output-string))`, values.TrueValue},
		{"bytevector input port not textual", `(textual-port? (open-input-bytevector #u8(1)))`, values.FalseValue},
		{"bytevector output port not textual", `(textual-port? (open-output-bytevector))`, values.FalseValue},
		{"integer is not textual port", `(textual-port? 42)`, values.FalseValue},
		// binary-port?
		{"bytevector input port is binary", `(binary-port? (open-input-bytevector #u8(1)))`, values.TrueValue},
		{"bytevector output port is binary", `(binary-port? (open-output-bytevector))`, values.TrueValue},
		{"string input port not binary", `(binary-port? (open-input-string "x"))`, values.FalseValue},
		{"string output port not binary", `(binary-port? (open-output-string))`, values.FalseValue},
		{"integer is not binary port", `(binary-port? 42)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Port Lifecycle — input-port-open?, output-port-open?
// =============================================================================

func TestPortOpenPredicate(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// input-port-open?
		{"fresh string input port is open",
			`(input-port-open? (open-input-string "x"))`, values.TrueValue},
		{"closed string input port is not open",
			`(let ((p (open-input-string "x"))) (close-port p) (input-port-open? p))`, values.FalseValue},
		{"fresh bytevector input port is open",
			`(input-port-open? (open-input-bytevector #u8(1)))`, values.TrueValue},
		{"closed bytevector input port is not open",
			`(let ((p (open-input-bytevector #u8(1)))) (close-port p) (input-port-open? p))`, values.FalseValue},
		// output-port-open?
		{"fresh string output port is open",
			`(output-port-open? (open-output-string))`, values.TrueValue},
		{"closed string output port is not open",
			`(let ((p (open-output-string))) (close-port p) (output-port-open? p))`, values.FalseValue},
		{"fresh bytevector output port is open",
			`(output-port-open? (open-output-bytevector))`, values.TrueValue},
		{"closed bytevector output port is not open",
			`(let ((p (open-output-bytevector))) (close-port p) (output-port-open? p))`, values.FalseValue},
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
		{"input-port-open? on integer", `(input-port-open? 42)`},
		{"input-port-open? on string", `(input-port-open? "hello")`},
		{"input-port-open? on output port", `(input-port-open? (open-output-string))`},
		{"output-port-open? on integer", `(output-port-open? 42)`},
		{"output-port-open? on string", `(output-port-open? "hello")`},
		{"output-port-open? on input port", `(output-port-open? (open-input-string "x"))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Close Port — close-port, close-input-port, close-output-port
// =============================================================================

func TestClosePort(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"close-port on input port",
			`(let ((p (open-input-string "x"))) (close-port p) (input-port-open? p))`, values.FalseValue},
		{"close-port on output port",
			`(let ((p (open-output-string))) (close-port p) (output-port-open? p))`, values.FalseValue},
		{"close-input-port on input port",
			`(let ((p (open-input-string "x"))) (close-input-port p) (input-port-open? p))`, values.FalseValue},
		{"close-output-port on output port",
			`(let ((p (open-output-string))) (close-output-port p) (output-port-open? p))`, values.FalseValue},
		{"close-port on bytevector input port",
			`(let ((p (open-input-bytevector #u8(1)))) (close-port p) (input-port-open? p))`, values.FalseValue},
		{"close-port on bytevector output port",
			`(let ((p (open-output-bytevector))) (close-port p) (output-port-open? p))`, values.FalseValue},
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
		{"close-port on integer", `(close-port 42)`},
		{"close-port on string", `(close-port "hello")`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// EOF — eof-object, eof-object?
// =============================================================================

func TestEofObject(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"eof-object is eof", `(eof-object? (eof-object))`, values.TrueValue},
		{"integer is not eof", `(eof-object? 42)`, values.FalseValue},
		{"boolean is not eof", `(eof-object? #f)`, values.FalseValue},
		{"string is not eof", `(eof-object? "hello")`, values.FalseValue},
		{"empty list is not eof", `(eof-object? '())`, values.FalseValue},
		{"read-char on empty port returns eof",
			`(eof-object? (read-char (open-input-string "")))`, values.TrueValue},
		{"read-u8 on empty bytevector port returns eof",
			`(eof-object? (read-u8 (open-input-bytevector #u8())))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// String Ports — open-input-string, open-output-string, get-output-string
// =============================================================================

func TestStringPorts(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// open-input-string: reading
		{"read first char from string port",
			`(equal? (read-char (open-input-string "hello")) #\h)`, values.TrueValue},
		{"read line from string port",
			`(equal? (read-line (open-input-string "hello")) "hello")`, values.TrueValue},
		{"empty string port yields eof on read-char",
			`(eof-object? (read-char (open-input-string "")))`, values.TrueValue},
		// open-input-string: type checks
		{"string input port is textual",
			`(textual-port? (open-input-string "test"))`, values.TrueValue},
		{"string input port is input",
			`(input-port? (open-input-string "test"))`, values.TrueValue},
		// open-output-string + get-output-string
		{"write and get string",
			`(let ((p (open-output-string)))
			   (write-string "hello" p)
			   (equal? (get-output-string p) "hello"))`, values.TrueValue},
		{"write multiple strings",
			`(let ((p (open-output-string)))
			   (write-string "hello" p)
			   (write-string " world" p)
			   (equal? (get-output-string p) "hello world"))`, values.TrueValue},
		{"empty output string",
			`(equal? (get-output-string (open-output-string)) "")`, values.TrueValue},
		{"write-char to string port",
			`(let ((p (open-output-string)))
			   (write-char #\A p)
			   (write-char #\B p)
			   (equal? (get-output-string p) "AB"))`, values.TrueValue},
		// open-output-string: type checks
		{"string output port is textual",
			`(textual-port? (open-output-string))`, values.TrueValue},
		{"string output port is output",
			`(output-port? (open-output-string))`, values.TrueValue},
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
		{"open-input-string with non-string", `(open-input-string 42)`},
		{"get-output-string with input port", `(get-output-string (open-input-string "x"))`},
		{"get-output-string with non-port", `(get-output-string 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Bytevector Ports — open-input-bytevector, open-output-bytevector,
//
//	get-output-bytevector
//
// =============================================================================

func TestBytevectorPorts(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// open-input-bytevector: reading
		{"read first byte",
			`(equal? (read-u8 (open-input-bytevector #u8(65 66 67))) 65)`, values.TrueValue},
		{"empty bytevector port yields eof on read-u8",
			`(eof-object? (read-u8 (open-input-bytevector #u8())))`, values.TrueValue},
		// open-input-bytevector: type checks
		{"bytevector input port is binary",
			`(binary-port? (open-input-bytevector #u8(1 2 3)))`, values.TrueValue},
		{"bytevector input port is input",
			`(input-port? (open-input-bytevector #u8(1)))`, values.TrueValue},
		// open-output-bytevector + get-output-bytevector
		{"write and get bytevector",
			`(let ((p (open-output-bytevector)))
			   (write-u8 65 p)
			   (write-u8 66 p)
			   (equal? (get-output-bytevector p) #u8(65 66)))`, values.TrueValue},
		{"empty output bytevector",
			`(let ((p (open-output-bytevector)))
			   (equal? (get-output-bytevector p) #u8()))`, values.TrueValue},
		{"write-bytevector and get",
			`(let ((p (open-output-bytevector)))
			   (write-bytevector #u8(1 2 3) p)
			   (equal? (get-output-bytevector p) #u8(1 2 3)))`, values.TrueValue},
		// open-output-bytevector: type checks
		{"bytevector output port is binary",
			`(binary-port? (open-output-bytevector))`, values.TrueValue},
		{"bytevector output port is output",
			`(output-port? (open-output-bytevector))`, values.TrueValue},
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
		{"open-input-bytevector with non-bytevector", `(open-input-bytevector 42)`},
		{"get-output-bytevector with input port", `(get-output-bytevector (open-input-bytevector #u8(1)))`},
		{"get-output-bytevector with non-port", `(get-output-bytevector 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// call-with-port
// =============================================================================

func TestCallWithPort(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"returns procedure result",
			`(equal? (call-with-port (open-input-string "hello")
			           (lambda (p) (read-char p)))
			         #\h)`, values.TrueValue},
		{"input port closed after return",
			`(let ((p (open-input-string "hello")))
			   (call-with-port p (lambda (port) (read-char port)))
			   (input-port-open? p))`, values.FalseValue},
		{"output port closed after return",
			`(let ((p (open-output-string)))
			   (call-with-port p (lambda (port) (write-string "hi" port)))
			   (output-port-open? p))`, values.FalseValue},
		{"returns string result via read-line",
			`(equal? (call-with-port (open-input-string "hello world")
			           (lambda (p) (read-line p)))
			         "hello world")`, values.TrueValue},
		{"accumulates output then closes",
			`(equal? (call-with-port (open-output-string)
			           (lambda (port)
			             (write-string "abc" port)
			             (get-output-string port)))
			         "abc")`, values.TrueValue},
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
		{"non-procedure second arg", `(call-with-port (open-input-string "x") 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}
