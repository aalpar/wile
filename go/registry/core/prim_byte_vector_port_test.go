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

package core_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// Bytevector Port Tests (R7RS §6.13.1)

func TestOpenInputBytevector(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "input-port? on bytevector input port",
			code: `(input-port? (open-input-bytevector #u8(1 2 3)))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on bytevector input port",
			code: `(port? (open-input-bytevector #u8(1 2 3)))`,
			out:  values.TrueValue,
		},
		{
			name: "output-port? returns false for input bytevector port",
			code: `(output-port? (open-input-bytevector #u8(1 2 3)))`,
			out:  values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestOpenOutputBytevector(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "output-port? on bytevector output port",
			code: `(output-port? (open-output-bytevector))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on bytevector output port",
			code: `(port? (open-output-bytevector))`,
			out:  values.TrueValue,
		},
		{
			name: "input-port? returns false for output bytevector port",
			code: `(input-port? (open-output-bytevector))`,
			out:  values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestGetOutputBytevector(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-bytevector)))
			(display "Hi" p)
			(get-output-bytevector p))
	`)
	qt.Assert(t, err, qt.IsNil)
	// "Hi" as bytes: H=72, i=105
	expected := values.ByteVector{{Value: 72}, {Value: 105}}
	qt.Assert(t, result, values.SchemeEquals, &expected)
}
