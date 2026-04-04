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

package environment

import (
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

// TestConcurrentGlobalAccess_T2 verifies that concurrent access to
// GlobalEnvironmentFrame maps and slices does not cause races or panics.
// This test addresses T2 from the architectural review.
func TestConcurrentGlobalAccess_T2(t *testing.T) {
	c := qt.New(t)

	// Number of concurrent goroutines per operation type
	numGoroutines := 20

	var wg sync.WaitGroup

	// Test 1: Concurrent CreateGlobalBinding (define pattern)
	t.Run("concurrent CreateGlobalBinding", func(t *testing.T) {
		topLevel := NewNamespace()
		env := topLevel.Runtime()

		for i := range numGoroutines {
			wg.Add(1)
			go func(idx int) {
				defer wg.Done()
				// Each goroutine creates a unique binding
				sym := values.NewSymbol("var" + string(rune('A'+idx)))
				_, created := env.global.CreateGlobalBinding(sym, BindingTypeVariable)
				c.Assert(created, qt.IsTrue)
			}(i)
		}
		wg.Wait()

		// Verify all bindings were created
		c.Assert(len(env.global.Keys()), qt.Equals, numGoroutines)
	})

	// Test 2: Concurrent GetGlobalIndex (lookup pattern)
	t.Run("concurrent GetGlobalIndex", func(t *testing.T) {
		topLevel := NewNamespace()
		env := topLevel.Runtime()

		// Pre-populate with bindings
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			env.global.CreateGlobalBinding(sym, BindingTypeVariable)
		}

		for i := range numGoroutines {
			wg.Add(1)
			go func(idx int) {
				defer wg.Done()
				sym := values.NewSymbol("var" + string(rune('A'+(idx%10))))
				gi := env.global.GetGlobalIndex(sym)
				c.Assert(gi, qt.Not(qt.IsNil))
			}(i)
		}
		wg.Wait()
	})

	// Test 3: Concurrent SetOwnGlobalValue (set! pattern)
	t.Run("concurrent SetOwnGlobalValue", func(t *testing.T) {
		topLevel := NewNamespace()
		env := topLevel.Runtime()

		// Pre-populate with bindings
		symbols := make([]*values.Symbol, 10)
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			symbols[i] = sym
			gi, _ := env.global.CreateGlobalBinding(sym, BindingTypeVariable)
			_ = env.global.SetOwnGlobalValue(gi, values.NewInteger(0))
		}

		for i := range numGoroutines {
			wg.Add(1)
			go func(idx int) {
				defer wg.Done()
				gi := env.global.GetGlobalIndex(symbols[idx%10])
				err := env.global.SetOwnGlobalValue(gi, values.NewInteger(int64(idx)))
				c.Assert(err, qt.IsNil)
			}(i)
		}
		wg.Wait()

		// Verify all bindings have values
		for i := range 10 {
			gi := env.global.GetGlobalIndex(symbols[i])
			bd := env.global.GetOwnGlobalBinding(gi)
			c.Assert(bd, qt.Not(qt.IsNil))
			c.Assert(bd.Value(), qt.Not(qt.IsNil))
		}
	})

	// Test 4: Concurrent mixed operations (create, lookup, update)
	t.Run("concurrent mixed operations", func(t *testing.T) {
		topLevel := NewNamespace()
		env := topLevel.Runtime()

		// Pre-populate with some bindings
		symbols := make([]*values.Symbol, 10)
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			symbols[i] = sym
			gi, _ := env.global.CreateGlobalBinding(sym, BindingTypeVariable)
			_ = env.global.SetOwnGlobalValue(gi, values.NewInteger(int64(i)))
		}

		for i := 0; i < numGoroutines*3; i++ {
			wg.Add(1)
			opType := i % 3
			go func(idx, op int) {
				defer wg.Done()

				switch op {
				case 0:
					// Create operation
					sym := values.NewSymbol("newvar" + string(rune('A'+idx)))
					env.global.CreateGlobalBinding(sym, BindingTypeVariable)
				case 1:
					// Lookup operation
					gi := env.global.GetGlobalIndex(symbols[idx%10])
					if gi != nil {
						_ = env.global.GetOwnGlobalBinding(gi)
					}
				case 2:
					// Update operation
					gi := env.global.GetGlobalIndex(symbols[idx%10])
					if gi != nil {
						_ = env.global.SetOwnGlobalValue(gi, values.NewInteger(int64(idx)))
					}
				}
			}(i, opType)
		}
		wg.Wait()
	})

	// Test 5: Concurrent Copy operations
	t.Run("concurrent Copy", func(t *testing.T) {
		topLevel := NewNamespace()
		env := topLevel.Runtime()

		// Pre-populate with bindings
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			gi, _ := env.global.CreateGlobalBinding(sym, BindingTypeVariable)
			_ = env.global.SetOwnGlobalValue(gi, values.NewInteger(int64(i)))
		}

		for range numGoroutines {
			wg.Add(1)
			go func() {
				defer wg.Done()
				copied := env.global.Copy()
				c.Assert(copied, qt.Not(qt.IsNil))
				c.Assert(len(copied.Keys()), qt.Equals, 10)
			}()
		}
		wg.Wait()
	})

	// Test 6: Concurrent EqualTo operations
	t.Run("concurrent EqualTo", func(t *testing.T) {
		topLevel := NewNamespace()
		env1 := topLevel.Runtime()
		env2 := topLevel.NewChildRuntime()

		// Pre-populate both with same bindings
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			gi1, _ := env1.global.CreateGlobalBinding(sym, BindingTypeVariable)
			gi2, _ := env2.global.CreateGlobalBinding(sym, BindingTypeVariable)
			_ = env1.global.SetOwnGlobalValue(gi1, values.NewInteger(int64(i)))
			_ = env2.global.SetOwnGlobalValue(gi2, values.NewInteger(int64(i)))
		}

		for range numGoroutines {
			wg.Add(1)
			go func() {
				defer wg.Done()
				// Note: they won't be equal because they're different instances,
				// but we're testing that EqualTo doesn't race
				_ = env1.global.EqualTo(env2.global)
			}()
		}
		wg.Wait()
	})

	// Test 7: Concurrent EnvironmentFrame.resolveGlobal (variable lookup pattern)
	t.Run("concurrent resolveGlobal", func(t *testing.T) {
		topLevel := NewNamespace()
		parent := topLevel.Runtime()
		child := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), parent)

		// Pre-populate parent with bindings
		symbols := make([]*values.Symbol, 10)
		for i := range 10 {
			sym := values.NewSymbol("var" + string(rune('A'+i)))
			symbols[i] = sym
			gi, _ := parent.global.CreateGlobalBinding(sym, BindingTypeVariable)
			_ = parent.global.SetOwnGlobalValue(gi, values.NewInteger(int64(i)))
		}

		for i := range numGoroutines {
			wg.Add(1)
			go func(idx int) {
				defer wg.Done()
				// Use GetBinding which internally calls resolveGlobal
				bd := child.GetBinding(symbols[idx%10], nil)
				c.Assert(bd, qt.Not(qt.IsNil))
			}(i)
		}
		wg.Wait()
	})
}
