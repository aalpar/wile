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

// TestPredicates_ArityErrors verifies that all core predicates reject
// wrong argument counts. The VM enforces arity automatically via
// machine_context.go ("expected %d arguments, got %d").
func TestPredicates_ArityErrors(t *testing.T) {
	predicates := []string{
		// 19 type predicates
		"void?",
		"null?",
		"pair?",
		"boolean?",
		"number?",
		"integer?",
		"real?",
		"rational?",
		"complex?",
		"exact?",
		"inexact?",
		"exact-integer?",
		"symbol?",
		"string?",
		"char?",
		"vector?",
		"bytevector?",
		"procedure?",
		"list?",
		// 5 numeric predicates
		"zero?",
		"positive?",
		"negative?",
		"odd?",
		"even?",
	}

	for _, pred := range predicates {
		t.Run(pred+" zero args", func(t *testing.T) {
			runSchemeCodeExpectError(t, "("+pred+")")
		})
		t.Run(pred+" two args", func(t *testing.T) {
			runSchemeCodeExpectError(t, "("+pred+" 1 2)")
		})
	}
}
