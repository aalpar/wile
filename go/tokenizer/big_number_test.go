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

package tokenizer

import (
	"fmt"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestTokenizer_BigInteger(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			bs:    "#m123",
			scan:  "#m123",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#M456",
			scan:  "#M456",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m-789",
			scan:  "#m-789",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m+42",
			scan:  "#m+42",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m12345678901234567890",
			scan:  "#m12345678901234567890",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m0",
			scan:  "#m0",
			err0:  io.EOF,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m123 abc",
			scan:  "#m123",
			err0:  nil,
			state: TokenizerStateBigInteger,
		},
		{
			bs:    "#m123)",
			scan:  "#m123",
			err0:  nil,
			state: TokenizerStateBigInteger,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.span()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.scan)
		})
	}
}

func TestTokenizer_BigFloat(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			bs:    "#z3.14159265358979323846",
			scan:  "#z3.14159265358979323846",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#Z2.71828182845904523536",
			scan:  "#Z2.71828182845904523536",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z-1.5",
			scan:  "#z-1.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z+42.0",
			scan:  "#z+42.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z123",
			scan:  "#z123",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z.5",
			scan:  "#z.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z1e10",
			scan:  "#z1e10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z1.5e-10",
			scan:  "#z1.5e-10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z3.14E+20",
			scan:  "#z3.14E+20",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z0.0",
			scan:  "#z0.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z1.23 abc",
			scan:  "#z1.23",
			err0:  nil,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#z1.23)",
			scan:  "#z1.23",
			err0:  nil,
			state: TokenizerStateBigFloat,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.span()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.scan)
		})
	}
}
