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
	"bytes"
	"context"
	"errors"
	"io"
	"strings"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// TestConcurrentMapAccess_T1 verifies that concurrent access to Tokenizers
// and Parsers maps does not cause races or panics.
// This test addresses T1 from the architectural review.
func TestConcurrentMapAccess_T1(t *testing.T) {
	c := qt.New(t)

	// Reset state before and after test
	extio.ResetState()
	extio.InitState()
	defer extio.ResetState()

	// Create multiple ports that will be accessed concurrently
	numPorts := 10
	ports := make([]*values.PortObject, numPorts)
	for i := range numPorts {
		ports[i] = values.NewStringInputPortWithBuffer(bytes.NewBufferString("(+ 1 2) (+ 3 4)"))
	}

	// Number of concurrent goroutines per operation type
	numGoroutines := 20

	var wg sync.WaitGroup

	// Test 1: Concurrent parser creation and access (PrimRead pattern)
	t.Run("concurrent parser access", func(t *testing.T) {
		for i := range numGoroutines {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]

				// Simulate PrimRead: get or create parser
				rr, _ := port.AsRuneReader()
				extio.ExportCacheMu.Lock()
				prss, ok := (*extio.ExportParsers)[port]
				if !ok || prss == nil {
					prss = parser.NewParser(nil, true, rr)
					(*extio.ExportParsers)[port] = prss
				}
				extio.ExportCacheMu.Unlock()

				c.Assert(prss, qt.Not(qt.IsNil))
			}(i)
		}
		wg.Wait()
	})

	// Test 2: Concurrent delete operations (closePort pattern)
	t.Run("concurrent delete", func(t *testing.T) {
		// Pre-populate maps
		for _, port := range ports {
			rr, _ := port.AsRuneReader()
			extio.ExportCacheMu.Lock()
			(*extio.ExportParsers)[port] = parser.NewParser(nil, true, rr)
			extio.ExportCacheMu.Unlock()
		}

		// Concurrently delete entries
		for i := range numGoroutines {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]

				// Simulate closePort: evict cached state
				extio.ExportEvictPortCache(port)
			}(i)
		}
		wg.Wait()
	})

	// Test 3: Concurrent mixed operations (read, write, delete)
	t.Run("concurrent mixed operations", func(t *testing.T) {
		for i := 0; i < numGoroutines*3; i++ {
			wg.Add(1)
			opType := i % 3
			go func(portIdx, op int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]

				switch op {
				case 0:
					// Read operation
					extio.ExportCacheMu.Lock()
					_ = (*extio.ExportParsers)[port]
					extio.ExportCacheMu.Unlock()
				case 1:
					// Write operation
					rr, _ := port.AsRuneReader()
					extio.ExportCacheMu.Lock()
					(*extio.ExportParsers)[port] = parser.NewParser(nil, true, rr)
					extio.ExportCacheMu.Unlock()
				case 2:
					// Delete operation
					extio.ExportEvictPortCache(port)
				}
			}(i, opType)
		}
		wg.Wait()
	})

	// Test 4: Concurrent InitState calls (should be idempotent and safe)
	t.Run("concurrent InitState", func(t *testing.T) {
		extio.ResetState()
		for range numGoroutines {
			wg.Add(1)
			go func() {
				defer wg.Done()
				extio.InitState()
			}()
		}
		wg.Wait()

		// Verify maps were initialized exactly once
		c.Assert(*extio.ExportTokenizers, qt.Not(qt.IsNil))
		c.Assert(*extio.ExportParsers, qt.Not(qt.IsNil))
	})
}

// =============================================================================
// Allocation Limit Tests (M11)
// =============================================================================

func TestReadStringAllocationLimit(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Below limit: should succeed
		{"k equals zero",
			`(equal? (read-string 0 (open-input-string "hello")) "")`,
			values.TrueValue},
		{"k equals one",
			`(equal? (read-string 1 (open-input-string "hello")) "h")`,
			values.TrueValue},
		{"k equals 1000",
			`(string? (read-string 1000 (open-input-string "hello")))`,
			values.TrueValue},
		{"k at limit boundary", // 100MB / 4 bytes per rune = 26,214,400
			`(string? (read-string 26214400 (open-input-string "x")))`,
			values.TrueValue},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	// Above limit: should error
	errs := []struct {
		name string
		code string
	}{
		{"k just over limit", `(read-string 26214401 (open-input-string "x"))`},
		{"k equals 100 million", `(read-string 100000000 (open-input-string "x"))`},
		{"k equals 1 billion", `(read-string 1000000000 (open-input-string "x"))`},
	}

	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadBytevectorAllocationLimit(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Below limit: should succeed
		{"k equals zero",
			`(equal? (read-bytevector 0 (open-input-bytevector #u8(1 2 3))) #u8())`,
			values.TrueValue},
		{"k equals one",
			`(equal? (read-bytevector 1 (open-input-bytevector #u8(1 2 3))) #u8(1))`,
			values.TrueValue},
		{"k equals 1000",
			`(bytevector? (read-bytevector 1000 (open-input-bytevector #u8(1))))`,
			values.TrueValue},
		{"k at limit boundary", // 100MB = 104,857,600 bytes
			`(bytevector? (read-bytevector 104857600 (open-input-bytevector #u8(1))))`,
			values.TrueValue},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	// Above limit: should error
	errs := []struct {
		name string
		code string
	}{
		{"k just over limit", `(read-bytevector 104857601 (open-input-bytevector #u8(1)))`},
		{"k equals 1 billion", `(read-bytevector 1000000000 (open-input-bytevector #u8(1)))`},
	}

	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadAllocationLimitErrorMessages(t *testing.T) {
	engine := newEngine(t)

	// Verify read-string error messages are informative
	_, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `(read-string 1000000000 (open-input-string "x"))`))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
	qt.Assert(t, err.Error(), qt.Contains, "100 MB")

	// Verify read-bytevector error messages are informative
	_, err = engine.Eval(context.Background(), engine.MustParse(context.Background(), `(read-bytevector 1000000000 (open-input-bytevector #u8(1)))`))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
	qt.Assert(t, err.Error(), qt.Contains, "100 MB")
}

// =============================================================================
// Phase 1 — 0% functions with most code paths
// =============================================================================

func TestRead(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer",
			`(equal? (read (open-input-string "42")) 42)`,
			values.TrueValue},
		{"symbol",
			`(equal? (read (open-input-string "hello")) 'hello)`,
			values.TrueValue},
		{"list",
			`(equal? (read (open-input-string "(1 2 3)")) '(1 2 3))`,
			values.TrueValue},
		{"string",
			`(equal? (read (open-input-string "\"abc\"")) "abc")`,
			values.TrueValue},
		{"boolean true",
			`(equal? (read (open-input-string "#t")) #t)`,
			values.TrueValue},
		{"boolean false",
			`(equal? (read (open-input-string "#f")) #f)`,
			values.TrueValue},
		{"vector",
			`(equal? (read (open-input-string "#(1 2 3)")) #(1 2 3))`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (read (open-input-string "")))`,
			values.TrueValue},
		{"successive reads",
			`(let ((p (open-input-string "1 2")))
			   (let ((a (read p)) (b (read p)))
			     (and (equal? a 1) (equal? b 2))))`,
			values.TrueValue},
		{"successive read then eof",
			`(let ((p (open-input-string "42")))
			   (read p)
			   (eof-object? (read p)))`,
			values.TrueValue},
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
		{"binary port", `(read (open-input-bytevector #u8(1 2 3)))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWrite(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer",
			`(let ((p (open-output-string)))
			   (write 42 p)
			   (equal? (get-output-string p) "42"))`,
			values.TrueValue},
		{"quoted string",
			`(let ((p (open-output-string)))
			   (write "hello" p)
			   (equal? (get-output-string p) "\"hello\""))`,
			values.TrueValue},
		{"list",
			`(let ((p (open-output-string)))
			   (write '(1 2 3) p)
			   (equal? (get-output-string p) "(1 2 3)"))`,
			values.TrueValue},
		{"boolean",
			`(let ((p (open-output-string)))
			   (write #t p)
			   (equal? (get-output-string p) "#t"))`,
			values.TrueValue},
		{"character",
			`(let ((p (open-output-string)))
			   (write #\A p)
			   (equal? (get-output-string p) "#\\A"))`,
			values.TrueValue},
		{"nested structure",
			`(let ((p (open-output-string)))
			   (write '(1 (2 3) 4) p)
			   (equal? (get-output-string p) "(1 (2 3) 4)"))`,
			values.TrueValue},
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
		{"non-port arg", `(write 42 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestDisplay(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer",
			`(let ((p (open-output-string)))
			   (display 42 p)
			   (equal? (get-output-string p) "42"))`,
			values.TrueValue},
		{"unquoted string",
			`(let ((p (open-output-string)))
			   (display "hello" p)
			   (equal? (get-output-string p) "hello"))`,
			values.TrueValue},
		{"bare character",
			`(let ((p (open-output-string)))
			   (display #\A p)
			   (equal? (get-output-string p) "A"))`,
			values.TrueValue},
		{"list",
			`(let ((p (open-output-string)))
			   (display '(1 2 3) p)
			   (equal? (get-output-string p) "(1 2 3)"))`,
			values.TrueValue},
		{"symbol",
			`(let ((p (open-output-string)))
			   (display 'hello p)
			   (equal? (get-output-string p) "hello"))`,
			values.TrueValue},
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
		{"non-port arg", `(display 42 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestNewline(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"newline to port",
			`(let ((p (open-output-string)))
			   (newline p)
			   (equal? (get-output-string p) "\n"))`,
			values.TrueValue},
		{"between writes",
			`(let ((p (open-output-string)))
			   (display "a" p)
			   (newline p)
			   (display "b" p)
			   (equal? (get-output-string p) "a\nb"))`,
			values.TrueValue},
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
		{"non-port arg", `(newline 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestPeekChar(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"peek first char",
			`(equal? (peek-char (open-input-string "hello")) #\h)`,
			values.TrueValue},
		{"peek then read same char",
			`(let ((p (open-input-string "hello")))
			   (let ((peeked (peek-char p)) (read-val (read-char p)))
			     (equal? peeked read-val)))`,
			values.TrueValue},
		{"peek twice returns same",
			`(let ((p (open-input-string "abc")))
			   (let ((a (peek-char p)) (b (peek-char p)))
			     (equal? a b)))`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (peek-char (open-input-string "")))`,
			values.TrueValue},
		{"peek does not advance",
			`(let ((p (open-input-string "xy")))
			   (peek-char p)
			   (peek-char p)
			   (equal? (read-char p) #\x))`,
			values.TrueValue},
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
		{"binary port", `(peek-char (open-input-bytevector #u8(1 2 3)))`},
		{"closed port",
			`(let ((p (open-input-string "hello")))
			   (close-port p)
			   (peek-char p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestPeekU8(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"peek first byte",
			`(equal? (peek-u8 (open-input-bytevector #u8(65 66 67))) 65)`,
			values.TrueValue},
		{"peek then read same byte",
			`(let ((p (open-input-bytevector #u8(42))))
			   (let ((peeked (peek-u8 p)) (read-val (read-u8 p)))
			     (equal? peeked read-val)))`,
			values.TrueValue},
		{"peek twice returns same",
			`(let ((p (open-input-bytevector #u8(10 20))))
			   (let ((a (peek-u8 p)) (b (peek-u8 p)))
			     (equal? a b)))`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (peek-u8 (open-input-bytevector #u8())))`,
			values.TrueValue},
		{"peek does not advance",
			`(let ((p (open-input-bytevector #u8(99 100))))
			   (peek-u8 p)
			   (peek-u8 p)
			   (equal? (read-u8 p) 99))`,
			values.TrueValue},
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
		{"textual port", `(peek-u8 (open-input-string "hello"))`},
		{"closed port",
			`(let ((p (open-input-bytevector #u8(1 2 3))))
			   (close-port p)
			   (peek-u8 p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Phase 2 — Remaining 0% functions
// =============================================================================

func TestWriteSimple(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer",
			`(let ((p (open-output-string)))
			   (write-simple 42 p)
			   (equal? (get-output-string p) "42"))`,
			values.TrueValue},
		{"quoted string",
			`(let ((p (open-output-string)))
			   (write-simple "hello" p)
			   (equal? (get-output-string p) "\"hello\""))`,
			values.TrueValue},
		{"list",
			`(let ((p (open-output-string)))
			   (write-simple '(1 2 3) p)
			   (equal? (get-output-string p) "(1 2 3)"))`,
			values.TrueValue},
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
		{"non-port", `(write-simple 42 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWriteShared(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer",
			`(let ((p (open-output-string)))
			   (write-shared 42 p)
			   (equal? (get-output-string p) "42"))`,
			values.TrueValue},
		{"list",
			`(let ((p (open-output-string)))
			   (write-shared '(1 2 3) p)
			   (equal? (get-output-string p) "(1 2 3)"))`,
			values.TrueValue},
		{"string",
			`(let ((p (open-output-string)))
			   (write-shared "abc" p)
			   (equal? (get-output-string p) "\"abc\""))`,
			values.TrueValue},
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
		{"non-port", `(write-shared 42 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadToken(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"returns non-eof for integer",
			`(not (eof-object? (read-token (open-input-string "42"))))`,
			values.TrueValue},
		{"returns non-eof for symbol",
			`(not (eof-object? (read-token (open-input-string "hello"))))`,
			values.TrueValue},
		{"empty returns eof",
			`(eof-object? (read-token (open-input-string "")))`,
			values.TrueValue},
		{"successive tokens then eof",
			`(let ((p (open-input-string "1 2")))
			   (read-token p)
			   (read-token p)
			   (eof-object? (read-token p)))`,
			values.TrueValue},
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
		{"binary port", `(read-token (open-input-bytevector #u8(1 2 3)))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadSyntax(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"reads syntax object",
			`(let ((s (read-syntax (open-input-string "42"))))
			   (not (eof-object? s)))`,
			values.TrueValue},
		{"empty returns eof",
			`(eof-object? (read-syntax (open-input-string "")))`,
			values.TrueValue},
		{"successive reads",
			`(let ((p (open-input-string "1 2")))
			   (let ((a (read-syntax p)) (b (read-syntax p)))
			     (and (not (eof-object? a)) (not (eof-object? b)))))`,
			values.TrueValue},
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
		{"binary port", `(read-syntax (open-input-bytevector #u8(1 2 3)))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestCharReadyQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"non-empty port",
			`(char-ready? (open-input-string "hello"))`,
			values.TrueValue},
		{"empty port",
			`(char-ready? (open-input-string ""))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestU8ReadyQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"non-empty port",
			`(u8-ready? (open-input-bytevector #u8(1 2)))`,
			values.TrueValue},
		{"empty port",
			`(u8-ready? (open-input-bytevector #u8()))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestFlushOutputPort(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"flush string port",
			`(let ((p (open-output-string)))
			   (write-string "hello" p)
			   (flush-output-port p)
			   (equal? (get-output-string p) "hello"))`,
			values.TrueValue},
		{"flush bytevector port",
			`(let ((p (open-output-bytevector)))
			   (write-u8 42 p)
			   (flush-output-port p)
			   (equal? (get-output-bytevector p) #u8(42)))`,
			values.TrueValue},
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
		{"non-port", `(flush-output-port 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Phase 3 — Improve partial coverage
// =============================================================================

func TestReadLine(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"simple line",
			`(equal? (read-line (open-input-string "hello")) "hello")`,
			values.TrueValue},
		{"line with newline",
			`(equal? (read-line (open-input-string "hello\nworld")) "hello")`,
			values.TrueValue},
		{"empty line",
			`(equal? (read-line (open-input-string "\nworld")) "")`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (read-line (open-input-string "")))`,
			values.TrueValue},
		{"successive reads",
			`(let ((p (open-input-string "a\nb\nc")))
			   (let ((a (read-line p)) (b (read-line p)) (c (read-line p)))
			     (and (equal? a "a") (equal? b "b") (equal? c "c"))))`,
			values.TrueValue},
		{"CRLF line ending",
			`(equal? (read-line (open-input-string "hello\r\nworld")) "hello")`,
			values.TrueValue},
		{"lone CR",
			`(equal? (read-line (open-input-string "hello\rworld")) "hello")`,
			values.TrueValue},
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
		{"binary port", `(read-line (open-input-bytevector #u8(1 2 3)))`},
		{"closed port",
			`(let ((p (open-input-string "hello")))
			   (close-port p)
			   (read-line p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadChar(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"ASCII",
			`(equal? (read-char (open-input-string "A")) #\A)`,
			values.TrueValue},
		{"unicode",
			`(equal? (read-char (open-input-string "λ")) #\λ)`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (read-char (open-input-string "")))`,
			values.TrueValue},
		{"successive chars",
			`(let ((p (open-input-string "ab")))
			   (let ((a (read-char p)) (b (read-char p)))
			     (and (equal? a #\a) (equal? b #\b))))`,
			values.TrueValue},
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
		{"binary port", `(read-char (open-input-bytevector #u8(65)))`},
		{"closed port",
			`(let ((p (open-input-string "hello")))
			   (close-port p)
			   (read-char p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWriteChar(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"write char to string port",
			`(let ((p (open-output-string)))
			   (write-char #\Z p)
			   (equal? (get-output-string p) "Z"))`,
			values.TrueValue},
		{"write unicode char",
			`(let ((p (open-output-string)))
			   (write-char #\λ p)
			   (equal? (get-output-string p) "λ"))`,
			values.TrueValue},
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
		{"non-character arg", `(write-char 42)`},
		{"non-port arg", `(write-char #\A 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadStringCoverage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"read k from longer string",
			`(equal? (read-string 3 (open-input-string "hello")) "hel")`,
			values.TrueValue},
		{"fewer than k chars",
			`(equal? (read-string 10 (open-input-string "hi")) "hi")`,
			values.TrueValue},
		{"k equals zero returns empty string",
			`(equal? (read-string 0 (open-input-string "hello")) "")`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (read-string 5 (open-input-string "")))`,
			values.TrueValue},
		{"successive reads",
			`(let ((p (open-input-string "abcd")))
			   (let ((a (read-string 2 p)) (b (read-string 2 p)))
			     (and (equal? a "ab") (equal? b "cd"))))`,
			values.TrueValue},
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
		{"negative k", `(read-string -1 (open-input-string "hello"))`},
		{"non-integer k", `(read-string "5" (open-input-string "hello"))`},
		{"binary port", `(read-string 5 (open-input-bytevector #u8(1 2 3)))`},
		{"closed port",
			`(let ((p (open-input-string "hello")))
			   (close-port p)
			   (read-string 3 p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWriteStringCoverage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"full string",
			`(let ((p (open-output-string)))
			   (write-string "hello" p)
			   (equal? (get-output-string p) "hello"))`,
			values.TrueValue},
		{"with start",
			`(let ((p (open-output-string)))
			   (write-string "hello" p 2)
			   (equal? (get-output-string p) "llo"))`,
			values.TrueValue},
		{"with start and end",
			`(let ((p (open-output-string)))
			   (write-string "hello" p 1 3)
			   (equal? (get-output-string p) "el"))`,
			values.TrueValue},
		{"empty range",
			`(let ((p (open-output-string)))
			   (write-string "hello" p 2 2)
			   (equal? (get-output-string p) ""))`,
			values.TrueValue},
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
		{"non-string", `(write-string 42 (open-output-string))`},
		{"non-port second arg", `(write-string "hello" 42)`},
		{"binary port", `(write-string "hello" (open-output-bytevector))`},
		{"start greater than end", `(write-string "hello" (open-output-string) 3 1)`},
		{"start out of bounds", `(write-string "hi" (open-output-string) 10)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWriteU8Coverage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"byte 0",
			`(let ((p (open-output-bytevector)))
			   (write-u8 0 p)
			   (equal? (get-output-bytevector p) #u8(0)))`,
			values.TrueValue},
		{"byte 255",
			`(let ((p (open-output-bytevector)))
			   (write-u8 255 p)
			   (equal? (get-output-bytevector p) #u8(255)))`,
			values.TrueValue},
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
		{"byte 256", `(write-u8 256 (open-output-bytevector))`},
		{"byte -1", `(write-u8 -1 (open-output-bytevector))`},
		{"non-integer", `(write-u8 "x" (open-output-bytevector))`},
		{"textual port", `(write-u8 65 (open-output-string))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestWriteU8_ByteRangeSentinel(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"byte 256", `(write-u8 256 (open-output-bytevector))`},
		{"byte -1", `(write-u8 -1 (open-output-bytevector))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), tc.code))
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNotAByte), qt.IsTrue)
		})
	}
}

func TestWriteBytevectorCoverage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"full bytevector",
			`(let ((p (open-output-bytevector)))
			   (write-bytevector #u8(1 2 3) p)
			   (equal? (get-output-bytevector p) #u8(1 2 3)))`,
			values.TrueValue},
		{"with start",
			`(let ((p (open-output-bytevector)))
			   (write-bytevector #u8(1 2 3 4 5) p 2)
			   (equal? (get-output-bytevector p) #u8(3 4 5)))`,
			values.TrueValue},
		{"with start and end",
			`(let ((p (open-output-bytevector)))
			   (write-bytevector #u8(1 2 3 4 5) p 1 3)
			   (equal? (get-output-bytevector p) #u8(2 3)))`,
			values.TrueValue},
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
		{"non-bytevector", `(write-bytevector "abc" (open-output-bytevector))`},
		{"start greater than end", `(write-bytevector #u8(1 2 3) (open-output-bytevector) 2 1)`},
		{"textual port", `(write-bytevector #u8(1 2 3) (open-output-string))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestReadU8Coverage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"single byte",
			`(equal? (read-u8 (open-input-bytevector #u8(42))) 42)`,
			values.TrueValue},
		{"successive bytes",
			`(let ((p (open-input-bytevector #u8(10 20))))
			   (let ((a (read-u8 p)) (b (read-u8 p)))
			     (and (equal? a 10) (equal? b 20))))`,
			values.TrueValue},
		{"empty port returns eof",
			`(eof-object? (read-u8 (open-input-bytevector #u8())))`,
			values.TrueValue},
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
		{"textual port", `(read-u8 (open-input-string "hello"))`},
		{"closed port",
			`(let ((p (open-input-bytevector #u8(1 2 3))))
			   (close-port p)
			   (read-u8 p))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Phase 4 — Cache lifecycle
// =============================================================================

// NOTE: TestParserCacheEviction accesses package-level state (cacheMu, Parsers,
// Tokenizers) and must NOT use t.Parallel(). Go tests within the same package
// run sequentially by default, so no additional synchronization is needed
// beyond the existing cacheMu locking.
func TestParserCacheEviction(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("read to eof evicts parser", func(t *testing.T) {
		// Read until EOF triggers parser eviction
		eval(t, engine, `(let ((p (open-input-string "42")))
		   (read p)
		   (read p))`) // second read hits EOF

		// After EOF, the Parsers map should have no lingering entry
		// for that port. We verify indirectly: a fresh port reuses
		// the read path without stale state.
		result := eval(t, engine, `(equal? (read (open-input-string "99")) 99)`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("read-token to eof evicts tokenizer", func(t *testing.T) {
		eval(t, engine, `(let ((p (open-input-string "hello")))
		   (read-token p)
		   (read-token p))`) // second read-token hits EOF

		// Verify a fresh port works after eviction
		result := eval(t, engine, `(not (eof-object? (read-token (open-input-string "world"))))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("cache maps empty after eof", func(t *testing.T) {
		// Snapshot cache sizes before our operation
		extio.ExportCacheMu.RLock()
		parsersBefore := len(*extio.ExportParsers)
		tokenizersBefore := len(*extio.ExportTokenizers)
		extio.ExportCacheMu.RUnlock()

		// Read to EOF on a port — should add then evict the parser entry
		eng := newEngine(t)
		eval(t, eng, `(let ((p (open-input-string "1")))
		   (read p)
		   (read p))`)

		// After EOF eviction, cache should be back to pre-test size
		extio.ExportCacheMu.RLock()
		parsersAfter := len(*extio.ExportParsers)
		tokenizersAfter := len(*extio.ExportTokenizers)
		extio.ExportCacheMu.RUnlock()

		c.Assert(parsersAfter, qt.Equals, parsersBefore)
		c.Assert(tokenizersAfter, qt.Equals, tokenizersBefore)
	})
}

// =============================================================================
// Default port (no port arg) paths
// =============================================================================

// TestDefaultOutputPort exercises the "no port arg" code path where
// write/display/newline/write-char/write-simple/write-shared/flush-output-port
// fall through to the current output port.
//
// NOTE: This test modifies global state (current output port) and must NOT
// use t.Parallel(). Go tests within the same package run sequentially by
// default, so no additional synchronization is needed.
func TestDefaultOutputPort(t *testing.T) {
	// Redirect the current output port to io.Discard to avoid polluting
	// test output while still exercising the default port code path.
	extio.SetCurrentOutputPort(values.NewCharacterOutputPortFromWriter(io.Discard))
	defer extio.ResetCurrentOutputPort()

	engine := newEngine(t)

	// Each of these calls the function without a port argument,
	// exercising the IsEmptyList() → GetCurrentOutputPort() branch.
	codes := []struct {
		name string
		code string
	}{
		{"write no port", `(write 42)`},
		{"display no port", `(display 42)`},
		{"newline no port", `(newline)`},
		{"write-simple no port", `(write-simple 42)`},
		{"write-shared no port", `(write-shared 42)`},
		{"flush-output-port no port", `(flush-output-port)`},
		{"write-string no port", `(write-string "hi")`},
		{"write-char no port", `(write-char #\A)`},
	}
	for _, tc := range codes {
		t.Run(tc.name, func(t *testing.T) {
			eval(t, engine, tc.code)
		})
	}
}

// TestDefaultInputPortCharReady exercises char-ready? with no port arg.
func TestDefaultInputPortCharReady(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := eval(t, engine, `(char-ready?)`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

// TestDefaultInputPortRead exercises the "no port arg" path for read-char
// by redirecting the current input port to a string reader.
//
// NOTE: This test modifies global state (current input port) and must NOT
// use t.Parallel(). Go tests within the same package run sequentially by
// default, so no additional synchronization is needed.
func TestDefaultInputPortRead(t *testing.T) {
	c := qt.New(t)

	port := values.NewCharacterInputPortFromReader(strings.NewReader("hello"))
	extio.SetCurrentInputPort(port)
	defer extio.ResetCurrentInputPort()

	engine := newEngine(t)
	result := eval(t, engine, `(equal? (read-char) #\h)`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

// TestBinaryPortNoArgError verifies that binary I/O functions error
// when no binary port is provided.
func TestBinaryPortNoArgError(t *testing.T) {
	engine := newEngine(t)

	errs := []struct {
		name string
		code string
	}{
		{"read-u8 no args", `(read-u8)`},
		{"peek-u8 no args", `(peek-u8)`},
		{"write-u8 no port", `(write-u8 42)`},
		{"write-bytevector no port", `(write-bytevector #u8(1 2 3))`},
		{"read-bytevector no port", `(read-bytevector 5)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// C5: read-bytevector / read-bytevector! return full data, not short reads
// =============================================================================

func TestReadBytevectorFullRead(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Partial read at EOF: request more bytes than available, get what's there.
		// Before the io.ReadFull fix, a short internal buffer could return fewer bytes.
		{"read-bytevector partial at EOF",
			`(let* ((data (make-bytevector 100 42))
			        (port (open-input-bytevector data))
			        (result (read-bytevector 200 port)))
			   (bytevector-length result))`,
			values.NewInteger(100)},
		// Exact read: request exactly the available bytes.
		{"read-bytevector exact",
			`(let* ((data (make-bytevector 50 7))
			        (port (open-input-bytevector data))
			        (result (read-bytevector 50 port)))
			   (bytevector-length result))`,
			values.NewInteger(50)},
		// After partial read, next read returns EOF.
		{"read-bytevector then EOF",
			`(let* ((data (make-bytevector 5 1))
			        (port (open-input-bytevector data)))
			   (read-bytevector 5 port)
			   (eof-object? (read-bytevector 1 port)))`,
			values.TrueValue},
		// L4: read-string 0 returns "" not eof-object (R7RS §6.13.2).
		{"read-string 0 returns empty string",
			`(equal? (read-string 0 (open-input-string "hello")) "")`,
			values.TrueValue},
		{"read-string 0 on empty port",
			`(equal? (read-string 0 (open-input-string "")) "")`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal().EqualTo(tc.want), qt.IsTrue,
				qt.Commentf("got %v, want %v", result.Internal().SchemeString(), tc.want.SchemeString()))
		})
	}
}
