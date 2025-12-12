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

	"wile/values"
)

func TestBytevectorCopyFullCopy(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8( 1 2 3 4 5 )")
}

func TestBytevectorCopyWithStart(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5) 2)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8( 3 4 5 )")
}

func TestBytevectorCopyWithStartAndEnd(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5) 1 3)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8( 2 3 )")
}

func TestBytevectorCopyEmptyBytevector(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8()")
}

func TestBytevectorCopyStartZero(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5) 0)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8( 1 2 3 4 5 )")
}

func TestBytevectorCopyStartAtEnd(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5) 5)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8()")
}

func TestBytevectorCopyEmptyRange(t *testing.T) {
	result, err := runSchemeCode(t, `(bytevector-copy (bytevector 1 2 3 4 5) 2 2)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "#u8()")
}

func TestBytevectorCopyIndependence(t *testing.T) {
	code := `(let* ((bv1 (bytevector 1 2 3 4 5))
	              (bv2 (bytevector-copy bv1)))
	         (bytevector-u8-set! bv2 0 99)
	         (bytevector-u8-ref bv1 0))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(1))
}

func TestBytevectorCopyPartialIndependence(t *testing.T) {
	code := `(let* ((bv1 (bytevector 10 20 30 40 50))
	              (bv2 (bytevector-copy bv1 1 4)))
	         (bytevector-u8-set! bv2 0 99)
	         (list (bytevector-u8-ref bv1 1) (bytevector-u8-ref bv2 0)))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.List(values.NewInteger(20), values.NewInteger(99)))
}
