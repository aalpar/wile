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
			got := toLowerASCII(tt.in)
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
			got := hasPrefixCI(tt.s, tt.prefix)
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
			got := hasSuffixCI(tt.s, tt.suffix)
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

func TestNormalizeExponentMarker(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		s    string
		want string
	}{
		{"short marker s", "1s10", "1e10"},
		{"short marker f uppercase", "1.5F3", "1.5e3"},
		{"short marker d", "2d-4", "2e-4"},
		{"short marker l", "9L9", "9e9"},
		{"e left untouched", "1e10", "1e10"},
		{"E left untouched", "1E10", "1E10"},
		{"no marker", "1.5", "1.5"},
		{"only first marker folded", "1s2s3", "1e2s3"},
		{"empty", "", ""},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(NormalizeExponentMarker(tt.s), qt.Equals, tt.want)
		})
	}
}

func TestIndexExponentMarker(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		s    string
		want int
	}{
		{"lowercase e", "1e10", 1},
		{"uppercase E", "1E10", 1},
		{"short marker s", "1s10", 1},
		{"short marker D uppercase", "2.5D3", 3},
		{"marker after decimal", "12.34e5", 5},
		{"no marker plain int", "123", -1},
		{"no marker decimal", "1.5", -1},
		{"empty", "", -1},
		{"first of several", "1e2e3", 1},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(IndexExponentMarker(tt.s), qt.Equals, tt.want)
		})
	}
}
