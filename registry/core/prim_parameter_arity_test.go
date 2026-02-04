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

package core_test

import "testing"

// TestParameters_ArityErrors verifies that parameter primitives reject
// wrong argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestParameters_ArityErrors(t *testing.T) {
	// Fixed 1 arg: parameter?
	t.Run("parameter? zero args", func(t *testing.T) {
		runSchemeCodeExpectError(t, "(parameter?)")
	})
	t.Run("parameter? two args", func(t *testing.T) {
		runSchemeCodeExpectError(t, "(parameter? 1 2)")
	})

	// Variadic with minimum 1 arg: make-parameter
	t.Run("make-parameter zero args", func(t *testing.T) {
		runSchemeCodeExpectError(t, "(make-parameter)")
	})
}
