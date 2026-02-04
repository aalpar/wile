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

package tokenizer

import (
	"io"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestHashDigit_Tokenizer tests R7RS §7.1.1 # inexact digit placeholder
// tokenization. The # character can appear after real digits in number
// literals, representing an unknown digit. Its presence sets the hashDigit
// flag which forces the resulting number to be inexact.
func TestHashDigit_Tokenizer(t *testing.T) {
	tcs := []struct {
		name string
		in   string
		typ  TokenizerState
		src  string
		hash bool
		err  error
	}{
		// Unsigned integers with hash digits
		{
			name: "unsigned integer with hash",
			in:   "1##",
			typ:  TokenizerStateUnsignedInteger,
			src:  "1##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "unsigned integer no hash",
			in:   "123",
			typ:  TokenizerStateUnsignedInteger,
			src:  "123",
			hash: false,
			err:  io.EOF,
		},
		// Signed integers with hash digits
		{
			name: "signed integer with hash",
			in:   "-1##",
			typ:  TokenizerStateSignedInteger,
			src:  "-1##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "positive signed integer with hash",
			in:   "+1##",
			typ:  TokenizerStateSignedInteger,
			src:  "+1##",
			hash: true,
			err:  io.EOF,
		},
		// Unsigned decimal fractions with hash digits
		{
			name: "decimal fraction hash in fraction",
			in:   "1.2##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1.2##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "integer hash then dot (production 4)",
			in:   "1##.",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1##.",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "integer hash then dot then hash (production 4)",
			in:   "1##.##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1##.##",
			hash: true,
			err:  io.EOF,
		},
		// Dot-initial decimal with hash
		{
			name: "dot-initial decimal with hash",
			in:   ".5##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  ".5##",
			hash: true,
			err:  io.EOF,
		},
		// Rational fractions with hash digits
		{
			name: "rational numerator hash",
			in:   "1##/3",
			typ:  TokenizerStateUnsignedRationalFraction,
			src:  "1##/3",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "rational denominator hash",
			in:   "1/3##",
			typ:  TokenizerStateUnsignedRationalFraction,
			src:  "1/3##",
			hash: true,
			err:  io.EOF,
		},
		// Scientific notation with hash digits
		{
			name: "scientific notation hash in mantissa",
			in:   "1##e2",
			typ:  TokenizerStateUnsignedScientificNotation,
			src:  "1##e2",
			hash: true,
			err:  io.EOF,
		},
		// Base-2 (binary) with hash digits
		{
			name: "binary with hash",
			in:   "1#",
			typ:  TokenizerStateUnsignedIntegerBase2,
			src:  "1#",
			hash: true,
			err:  io.EOF,
		},
		// Base-16 (hex) with hash digits
		{
			name: "hex with hash",
			in:   "f#",
			typ:  TokenizerStateUnsignedIntegerBase16,
			src:  "f#",
			hash: true,
			err:  io.EOF,
		},
		// Signed decimal fraction with hash
		{
			name: "signed decimal fraction with hash",
			in:   "+.5##",
			typ:  TokenizerStateSignedDecimalFraction,
			src:  "+.5##",
			hash: true,
			err:  io.EOF,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			var input string
			// For base-N tests, we need to set up the tokenizer with the right radix
			switch tc.typ {
			case TokenizerStateUnsignedIntegerBase2:
				input = "#b" + tc.in
			case TokenizerStateUnsignedIntegerBase16:
				input = "#x" + tc.in
			default:
				input = tc.in
			}
			ts, err := Tokenize(input, false)
			c.Assert(err, qt.ErrorIs, tc.err)
			// For base-N, first token is the marker, second is the number
			var tok Token
			switch tc.typ {
			case TokenizerStateUnsignedIntegerBase2, TokenizerStateUnsignedIntegerBase16:
				c.Assert(len(ts), qt.Equals, 2)
				tok = ts[1]
			default:
				c.Assert(len(ts) >= 1, qt.IsTrue)
				tok = ts[0]
			}
			st := tok.(*SimpleToken)
			c.Assert(st.typ, qt.Equals, tc.typ)
			c.Assert(st.String(), qt.Equals, tc.src)
			c.Assert(tok.HasHashDigit(), qt.Equals, tc.hash)
		})
	}
}
