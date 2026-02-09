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
			bs:    "#z123",
			scan:  "#z123",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#Z456",
			scan:  "#Z456",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z-789",
			scan:  "#z-789",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z+42",
			scan:  "#z+42",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z12345678901234567890",
			scan:  "#z12345678901234567890",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z0",
			scan:  "#z0",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z123 abc",
			scan:  "#z123",
			err0:  nil,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z123)",
			scan:  "#z123",
			err0:  nil,
			state: TokenizerStateBigIntegerBase10,
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
			bs:    "#m3.14159265358979323846",
			scan:  "#m3.14159265358979323846",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#M2.71828182845904523536",
			scan:  "#M2.71828182845904523536",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#M-1.5",
			scan:  "#M-1.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m+42.0",
			scan:  "#m+42.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m123",
			scan:  "#m123",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m.5",
			scan:  "#m.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1e10",
			scan:  "#m1e10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.5e-10",
			scan:  "#m1.5e-10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m3.14E+20",
			scan:  "#m3.14E+20",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m0.0",
			scan:  "#m0.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.23 abc",
			scan:  "#m1.23",
			err0:  nil,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.23)",
			scan:  "#m1.23",
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
