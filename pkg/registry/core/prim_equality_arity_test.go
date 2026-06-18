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

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestEquality_ArityErrors verifies that equality and boolean primitives
// reject wrong argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestEquality_ArityErrors(t *testing.T) {
	// Fixed 2 args: eq?, eqv?, equal?
	fixedArity2 := []string{
		"eq?",
		"eqv?",
		"equal?",
	}
	for _, name := range fixedArity2 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
		t.Run(name+" one arg", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" 1)")
		})
		t.Run(name+" three args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" 1 2 3)")
		})
	}

	// Fixed 1 arg: not
	t.Run("not zero args", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(not)")
	})
	t.Run("not two args", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(not 1 2)")
	})

	// Variadic with minimum 2 args: boolean=?, symbol=?
	variadicMin1 := []string{
		"boolean=?",
		"symbol=?",
	}
	for _, name := range variadicMin1 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
	}
}
