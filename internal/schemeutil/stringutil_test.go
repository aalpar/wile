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

package schemeutil

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestToLowerASCII(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		in   byte
		want byte
	}{
		{"uppercase A", 'A', 'a'},
		{"uppercase Z", 'Z', 'z'},
		{"lowercase a", 'a', 'a'},
		{"lowercase z", 'z', 'z'},
		{"digit", '5', '5'},
		{"space", ' ', ' '},
		{"symbol", '+', '+'},
		{"high byte", 0xFF, 0xFF},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := ToLowerASCII(tt.in)
			c.Assert(got, qt.Equals, tt.want)
		})
	}
}

func TestHasPrefixCI(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name   string
		s      string
		prefix string
		want   bool
	}{
		{"exact match", "Hello", "Hello", true},
		{"case insensitive", "Hello", "HELLO", true},
		{"partial match", "HelloWorld", "Hello", true},
		{"no match", "Hello", "World", false},
		{"prefix too long", "Hi", "Hello", false},
		{"empty prefix", "Hello", "", true},
		{"empty string", "", "Hello", false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := HasPrefixCI(tt.s, tt.prefix)
			c.Assert(got, qt.Equals, tt.want)
		})
	}
}

func TestHasSuffixCI(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name   string
		s      string
		suffix string
		want   bool
	}{
		{"exact match", "Hello", "Hello", true},
		{"case insensitive", "Hello", "HELLO", true},
		{"partial match", "WorldHello", "Hello", true},
		{"no match", "Hello", "World", false},
		{"suffix too long", "Hi", "Hello", false},
		{"empty suffix", "Hello", "", true},
		{"empty string", "", "Hello", false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := HasSuffixCI(tt.s, tt.suffix)
			c.Assert(got, qt.Equals, tt.want)
		})
	}
}

func TestTrimPrefixCI(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name   string
		s      string
		prefix string
		want   string
	}{
		{"exact match", "Hello", "Hello", ""},
		{"lowercase prefix", "Hello", "hello", ""},
		{"uppercase prefix", "hello", "HELLO", ""},
		{"mixed case", "HeLLo", "hElLo", ""},
		{"prefix with remainder", "HelloWorld", "hello", "World"},
		{"no match", "Hello", "World", "Hello"},
		{"empty prefix", "Hello", "", "Hello"},
		{"empty string", "", "hello", ""},
		{"prefix longer than string", "Hi", "Hello", "Hi"},
		{"non-ascii unchanged", "Helloé", "hello", "é"},
		{"numbers exact", "123abc", "123", "abc"},
		{"numbers no match", "123abc", "124", "123abc"},
		{"inf.0 lowercase", "+inf.0", "+inf.0", ""},
		{"inf.0 mixed case", "+InF.0", "+inf.0", ""},
		{"inf.0 uppercase", "+INF.0", "+inf.0", ""},
		{"nan.0 lowercase", "+nan.0", "+nan.0", ""},
		{"nan.0 mixed case", "+NaN.0", "+nan.0", ""},
		{"nan.0 uppercase", "+NAN.0", "+nan.0", ""},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := TrimPrefixCI(tt.s, tt.prefix)
			c.Assert(got, qt.Equals, tt.want)
		})
	}
}

func TestTrimSuffixCI(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name   string
		s      string
		suffix string
		want   string
	}{
		{"exact match", "Hello", "Hello", ""},
		{"lowercase suffix", "Hello", "hello", ""},
		{"uppercase suffix", "hello", "HELLO", ""},
		{"mixed case", "HeLLo", "hElLo", ""},
		{"suffix with remainder", "WorldHello", "hello", "World"},
		{"no match", "Hello", "World", "Hello"},
		{"empty suffix", "Hello", "", "Hello"},
		{"empty string", "", "hello", ""},
		{"suffix longer than string", "Hi", "Hello", "Hi"},
		{"imaginary i lowercase", "123i", "i", "123"},
		{"imaginary I uppercase", "123I", "i", "123"},
		{"imaginary i with uppercase input", "123i", "I", "123"},
		{"inf.0 lowercase", "+inf.0", ".0", "+inf"},
		{"numbers exact", "abc123", "123", "abc"},
		{"numbers no match", "abc123", "124", "abc123"},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := TrimSuffixCI(tt.s, tt.suffix)
			c.Assert(got, qt.Equals, tt.want)
		})
	}
}
