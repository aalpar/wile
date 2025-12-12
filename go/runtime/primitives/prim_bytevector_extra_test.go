// Copyright 2025 Aaron Alpar
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

package primitives_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBytevectorCopyBang(t *testing.T) {
	tcs := []struct {
		name           string
		code           string
		expectedOutput string
	}{
		{
			name: "basic copy",
			code: `(let ((to (make-bytevector 5 0))
			            (from (bytevector 1 2 3)))
			         (bytevector-copy! to 0 from)
			         to)`,
			expectedOutput: "#u8( 1 2 3 0 0 )",
		},
		{
			name: "copy with offset",
			code: `(let ((to (make-bytevector 5 0))
			            (from (bytevector 1 2 3)))
			         (bytevector-copy! to 2 from)
			         to)`,
			expectedOutput: "#u8( 0 0 1 2 3 )",
		},
		{
			name: "copy with start and end",
			code: `(let ((to (make-bytevector 5 0))
			            (from (bytevector 1 2 3 4 5)))
			         (bytevector-copy! to 1 from 1 3)
			         to)`,
			expectedOutput: "#u8( 0 2 3 0 0 )",
		},
		{
			name: "copy entire bytevector to beginning",
			code: `(let ((to (make-bytevector 4 255))
			            (from (bytevector 10 20 30 40)))
			         (bytevector-copy! to 0 from)
			         to)`,
			expectedOutput: "#u8( 10 20 30 40 )",
		},
		{
			name: "copy single byte",
			code: `(let ((to (make-bytevector 3 0))
			            (from (bytevector 42)))
			         (bytevector-copy! to 1 from)
			         to)`,
			expectedOutput: "#u8( 0 42 0 )",
		},
		{
			name: "copy with start only",
			code: `(let ((to (make-bytevector 4 0))
			            (from (bytevector 10 20 30 40)))
			         (bytevector-copy! to 0 from 2)
			         to)`,
			expectedOutput: "#u8( 30 40 0 0 )",
		},
		{
			name: "copy to end of destination",
			code: `(let ((to (make-bytevector 5 0))
			            (from (bytevector 100 200)))
			         (bytevector-copy! to 3 from)
			         to)`,
			expectedOutput: "#u8( 0 0 0 100 200 )",
		},
		{
			name: "copy zero bytes with start equals end",
			code: `(let ((to (make-bytevector 3 0))
			            (from (bytevector 1 2 3)))
			         (bytevector-copy! to 0 from 1 1)
			         to)`,
			expectedOutput: "#u8( 0 0 0 )",
		},
		{
			name: "copy overlapping regions same bytevector",
			code: `(let ((bv (bytevector 1 2 3 4 5)))
			         (bytevector-copy! bv 2 bv 0 3)
			         bv)`,
			expectedOutput: "#u8( 1 2 1 2 3 )",
		},
		{
			name: "copy with middle section",
			code: `(let ((to (make-bytevector 6 0))
			            (from (bytevector 10 20 30 40 50)))
			         (bytevector-copy! to 1 from 1 4)
			         to)`,
			expectedOutput: "#u8( 0 20 30 40 0 0 )",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expectedOutput)
		})
	}
}

func TestBytevectorCopyBangErrors(t *testing.T) {
	tcs := []struct {
		name        string
		code        string
		expectError bool
	}{
		{
			name:        "destination not a bytevector",
			code:        `(bytevector-copy! "not-bv" 0 (bytevector 1 2 3))`,
			expectError: true,
		},
		{
			name:        "source not a bytevector",
			code:        `(bytevector-copy! (make-bytevector 5) 0 "not-bv")`,
			expectError: true,
		},
		{
			name:        "at index not an integer",
			code:        `(bytevector-copy! (make-bytevector 5) "not-int" (bytevector 1 2 3))`,
			expectError: true,
		},
		{
			name:        "negative at index",
			code:        `(bytevector-copy! (make-bytevector 5) -1 (bytevector 1 2 3))`,
			expectError: true,
		},
		{
			name:        "not enough space in destination",
			code:        `(bytevector-copy! (make-bytevector 3) 0 (bytevector 1 2 3 4 5))`,
			expectError: true,
		},
		{
			name:        "at index too large",
			code:        `(bytevector-copy! (make-bytevector 5) 4 (bytevector 1 2 3))`,
			expectError: true,
		},
		{
			name:        "start index out of bounds",
			code:        `(bytevector-copy! (make-bytevector 5) 0 (bytevector 1 2 3) 5)`,
			expectError: true,
		},
		{
			name:        "end index less than start",
			code:        `(bytevector-copy! (make-bytevector 5) 0 (bytevector 1 2 3) 2 1)`,
			expectError: true,
		},
		{
			name:        "end index out of bounds",
			code:        `(bytevector-copy! (make-bytevector 5) 0 (bytevector 1 2 3) 0 10)`,
			expectError: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			if tc.expectError {
				qt.Assert(t, err, qt.Not(qt.IsNil))
			} else {
				qt.Assert(t, err, qt.IsNil)
			}
		})
	}
}

func TestBytevectorCopyBangReturnValue(t *testing.T) {
	code := `(let ((to (make-bytevector 5 0))
	               (from (bytevector 1 2 3)))
	          (bytevector-copy! to 0 from))`

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue, qt.Commentf("bytevector-copy! should return void"))
}
