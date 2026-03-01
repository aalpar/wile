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

	"github.com/aalpar/wile/registry/testhelpers"
)

// TestGenerateTemporaries_H7Regression tests fix for H7: generate-temporaries
// should return a proper error for non-list arguments, not panic.
//
// Bug: Unchecked type assertion arg.(values.Tuple).Length() caused Go panic.
// Fix: Check arg type before assertion, return ErrNotAList with context.
func TestGenerateTemporaries_H7Regression(t *testing.T) {
	t.Run("valid list input", func(t *testing.T) {
		code := `
			(let ((temps (generate-temporaries '(a b c))))
			  (and (= (length temps) 3)
			       (identifier? (car temps))
			       (identifier? (cadr temps))
			       (identifier? (caddr temps))))
		`
		testhelpers.RunSchemeCodeExpectTrue(t, code)
	})

	t.Run("empty list", func(t *testing.T) {
		code := `
			(let ((temps (generate-temporaries '())))
			  (and (list? temps)
			       (= (length temps) 0)))
		`
		testhelpers.RunSchemeCodeExpectTrue(t, code)
	})

	t.Run("single element list", func(t *testing.T) {
		code := `
			(let ((temps (generate-temporaries '(x))))
			  (and (= (length temps) 1)
			       (identifier? (car temps))))
		`
		testhelpers.RunSchemeCodeExpectTrue(t, code)
	})

	// H7: Error cases - should return proper Scheme errors, not panic
	t.Run("error: string argument", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(generate-temporaries "not-a-list")`)
	})

	t.Run("error: number argument", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(generate-temporaries 42)`)
	})

	t.Run("error: symbol argument", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(generate-temporaries 'not-a-list)`)
	})

	t.Run("error: vector argument", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(generate-temporaries #(a b c))`)
	})

	t.Run("uniqueness", func(t *testing.T) {
		// Each call should generate unique identifiers
		code := `
			(let ((temps1 (generate-temporaries '(a b)))
			      (temps2 (generate-temporaries '(c d))))
			  ; Identifiers should be unique
			  (not (free-identifier=? (car temps1) (car temps2))))
		`
		testhelpers.RunSchemeCodeExpectTrue(t, code)
	})
}
