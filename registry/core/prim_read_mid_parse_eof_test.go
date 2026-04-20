// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestReadMidParseEOFRaisesReadError verifies R7RS §6.13.2: mid-parse EOF
// inside an unterminated compound datum raises a read-error?-satisfying
// exception rather than returning the EOF object.
func TestReadMidParseEOFRaisesReadError(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "unterminated open paren",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "(")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "unterminated list with elements",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "(1 2 3")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "nested unterminated",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "(1 (2 (3")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "unterminated vector",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "#(1 2")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "unterminated bytevector",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "#u8(1 2")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "improper list truncated after dot",
			Code:     `(guard (e (#t (read-error? e))) (read (open-input-string "(1 .")))`,
			Expected: values.TrueValue,
		},
		// Regression: clean EOF at token boundary still returns the EOF object
		{
			Name:     "clean EOF returns eof-object",
			Code:     `(eof-object? (read (open-input-string "")))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "clean EOF after complete form",
			Code:     `(let ((p (open-input-string "(1 2 3)"))) (read p) (eof-object? (read p)))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
