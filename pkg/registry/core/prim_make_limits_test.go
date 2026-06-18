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
	"github.com/aalpar/wile/pkg/values"
)

// An element count above MaxMakeLength (2^32) must be rejected before any
// allocation, so an embedder running untrusted Scheme cannot OOM the host.
// 5000000000 > 2^32 (4294967296) and still fits in int64, so it reaches the
// guard rather than promoting to a bignum.
func TestMakeConstructors_RejectOversized(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "make-vector", Code: `(make-vector 5000000000)`},
		{Name: "make-string", Code: `(make-string 5000000000)`},
		{Name: "make-bytevector", Code: `(make-bytevector 5000000000)`},
		{Name: "make-list", Code: `(make-list 5000000000)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			if err == nil {
				t.Fatalf("%s: oversized count should error, got nil", tc.Name)
			}
		})
	}
}

// Counts within the limit still allocate normally.
func TestMakeConstructors_AllowNormal(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "make-vector", Code: `(vector-length (make-vector 8))`},
		{Name: "make-string", Code: `(string-length (make-string 8 #\a))`},
		{Name: "make-bytevector", Code: `(bytevector-length (make-bytevector 8 0))`},
		{Name: "make-list", Code: `(length (make-list 8))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			if err != nil {
				t.Fatalf("%s: should allocate, got: %v", tc.Name, err)
			}
			n, ok := result.(*values.Integer)
			if !ok || n.Value != 8 {
				t.Fatalf("%s: want length 8, got %v", tc.Name, result)
			}
		})
	}
}
