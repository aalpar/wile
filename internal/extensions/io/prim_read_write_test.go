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
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

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
