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

// TestControl_ArityErrors verifies that control primitives reject wrong
// argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestControl_ArityErrors(t *testing.T) {
	// Fixed 1 arg: call/cc, call-with-current-continuation
	fixedArity1 := []string{
		"call/cc",
		"call-with-current-continuation",
	}
	for _, name := range fixedArity1 {
		t.Run(name+" zero args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+")")
		})
		t.Run(name+" two args", func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, "("+name+" 1 2)")
		})
	}

	// Fixed 2 args: call-with-values
	t.Run("call-with-values zero args", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(call-with-values)")
	})
	t.Run("call-with-values one arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(call-with-values 1)")
	})
	t.Run("call-with-values three args", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, "(call-with-values 1 2 3)")
	})
}
