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

package io

import (
	"bytes"
	"context"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/values"
)

// TestConcurrentMapAccess_T1 verifies that concurrent access to Tokenizers
// and Parsers maps does not cause races or panics.
// This test addresses T1 from the architectural review.
func TestConcurrentMapAccess_T1(t *testing.T) {
	c := qt.New(t)

	// Reset state before and after test
	ResetState()
	InitState()
	defer ResetState()

	// Create multiple ports that will be accessed concurrently
	numPorts := 10
	ports := make([]*values.StringInputPort, numPorts)
	for i := 0; i < numPorts; i++ {
		ports[i] = values.NewStringInputPortWithBuffer(bytes.NewBufferString("(+ 1 2) (+ 3 4)"))
	}

	// Number of concurrent goroutines per operation type
	numGoroutines := 20

	var wg sync.WaitGroup

	// Test 1: Concurrent parser creation and access (PrimRead pattern)
	t.Run("concurrent parser access", func(t *testing.T) {
		for i := 0; i < numGoroutines; i++ {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]

				// Simulate PrimRead: get or create parser
				cacheMu.Lock()
				prss, ok := Parsers[port]
				if !ok || prss == nil {
					prss = parser.NewParser(nil, true, port)
					Parsers[port] = prss
				}
				cacheMu.Unlock()

				c.Assert(prss, qt.Not(qt.IsNil))
			}(i)
		}
		wg.Wait()
	})

	// Test 2: Concurrent delete operations (closePort pattern)
	t.Run("concurrent delete", func(t *testing.T) {
		// Pre-populate maps
		for _, port := range ports {
			cacheMu.Lock()
			Parsers[port] = parser.NewParser(nil, true, port)
			cacheMu.Unlock()
		}

		// Concurrently delete entries
		for i := 0; i < numGoroutines; i++ {
			wg.Add(1)
			go func(portIdx int) {
				defer wg.Done()
				port := ports[portIdx%numPorts]

				// Simulate closePort: delete from maps
				cacheMu.Lock()
				delete(Tokenizers, port)
				delete(Parsers, port)
				cacheMu.Unlock()
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
					cacheMu.Lock()
					_ = Parsers[port]
					cacheMu.Unlock()
				case 1:
					// Write operation
					cacheMu.Lock()
					Parsers[port] = parser.NewParser(nil, true, port)
					cacheMu.Unlock()
				case 2:
					// Delete operation
					cacheMu.Lock()
					delete(Parsers, port)
					delete(Tokenizers, port)
					cacheMu.Unlock()
				}
			}(i, opType)
		}
		wg.Wait()
	})

	// Test 4: Concurrent InitState calls (should be idempotent and safe)
	t.Run("concurrent InitState", func(t *testing.T) {
		ResetState()
		for i := 0; i < numGoroutines; i++ {
			wg.Add(1)
			go func() {
				defer wg.Done()
				InitState()
			}()
		}
		wg.Wait()

		// Verify maps were initialized exactly once
		c.Assert(Tokenizers, qt.Not(qt.IsNil))
		c.Assert(Parsers, qt.Not(qt.IsNil))
	})
}

// newEngine creates a Wile engine with the I/O extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(Extension),
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
			`(eof-object? (read-string 0 (open-input-string "hello")))`,
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
	_, err := engine.Eval(context.Background(), `(read-string 1000000000 (open-input-string "x"))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
	qt.Assert(t, err.Error(), qt.Contains, "100 MB")

	// Verify read-bytevector error messages are informative
	_, err = engine.Eval(context.Background(), `(read-bytevector 1000000000 (open-input-bytevector #u8(1)))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
	qt.Assert(t, err.Error(), qt.Contains, "100 MB")
}
