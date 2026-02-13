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

// TestPositionTrackingWithNewlines verifies that byte positions are tracked correctly
// across newlines. This test catches the H8 bug where NewLine() double-counts byte positions.
func TestPositionTrackingWithNewlines(t *testing.T) {
	c := qt.New(t)

	// Test string: "a\nb\nc" (5 bytes: 'a' at 0, '\n' at 1, 'b' at 2, '\n' at 3, 'c' at 4)
	input := "a\nb\nc"
	tokens, err := Tokenize(input, false)
	c.Assert(err, qt.ErrorIs, io.EOF)
	c.Assert(len(tokens), qt.Equals, 3)

	// Token 'a' should be at index 0
	c.Assert(tokens[0].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[0].Value(), qt.Equals, "a")
	c.Assert(tokens[0].Start().Index(), qt.Equals, 0, qt.Commentf("Token 'a' should start at byte 0"))
	c.Assert(tokens[0].End().Index(), qt.Equals, 1, qt.Commentf("Token 'a' should end at byte 1"))

	// Token 'b' should be at index 2 (after "a\n")
	c.Assert(tokens[1].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[1].Value(), qt.Equals, "b")
	c.Assert(tokens[1].Start().Index(), qt.Equals, 2, qt.Commentf("Token 'b' should start at byte 2"))
	c.Assert(tokens[1].End().Index(), qt.Equals, 3, qt.Commentf("Token 'b' should end at byte 3"))

	// Token 'c' should be at index 4 (after "a\nb\n")
	c.Assert(tokens[2].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[2].Value(), qt.Equals, "c")
	c.Assert(tokens[2].Start().Index(), qt.Equals, 4, qt.Commentf("Token 'c' should start at byte 4"))
	c.Assert(tokens[2].End().Index(), qt.Equals, 5, qt.Commentf("Token 'c' should end at byte 5"))
}

// TestPositionTrackingWithTabs verifies that byte positions are tracked correctly
// with tab characters. This test catches the H8 bug where Tab() double-counts byte positions.
func TestPositionTrackingWithTabs(t *testing.T) {
	c := qt.New(t)

	// Test string: "a\tb\tc" (5 bytes: 'a' at 0, '\t' at 1, 'b' at 2, '\t' at 3, 'c' at 4)
	input := "a\tb\tc"
	tokens, err := Tokenize(input, false)
	c.Assert(err, qt.ErrorIs, io.EOF)
	c.Assert(len(tokens), qt.Equals, 3)

	// Token 'a' should be at index 0
	c.Assert(tokens[0].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[0].Value(), qt.Equals, "a")
	c.Assert(tokens[0].Start().Index(), qt.Equals, 0, qt.Commentf("Token 'a' should start at byte 0"))
	c.Assert(tokens[0].End().Index(), qt.Equals, 1, qt.Commentf("Token 'a' should end at byte 1"))

	// Token 'b' should be at index 2 (after "a\t")
	c.Assert(tokens[1].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[1].Value(), qt.Equals, "b")
	c.Assert(tokens[1].Start().Index(), qt.Equals, 2, qt.Commentf("Token 'b' should start at byte 2"))
	c.Assert(tokens[1].End().Index(), qt.Equals, 3, qt.Commentf("Token 'b' should end at byte 3"))

	// Token 'c' should be at index 4 (after "a\tb\t")
	c.Assert(tokens[2].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[2].Value(), qt.Equals, "c")
	c.Assert(tokens[2].Start().Index(), qt.Equals, 4, qt.Commentf("Token 'c' should start at byte 4"))
	c.Assert(tokens[2].End().Index(), qt.Equals, 5, qt.Commentf("Token 'c' should end at byte 5"))
}

// TestPositionTrackingMixed verifies correct tracking with both newlines and tabs
func TestPositionTrackingMixed(t *testing.T) {
	c := qt.New(t)

	// Test string: "a\n\tb" (4 bytes: 'a' at 0, '\n' at 1, '\t' at 2, 'b' at 3)
	input := "a\n\tb"
	tokens, err := Tokenize(input, false)
	c.Assert(err, qt.ErrorIs, io.EOF)
	c.Assert(len(tokens), qt.Equals, 2)

	// Token 'a' should be at index 0
	c.Assert(tokens[0].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[0].Value(), qt.Equals, "a")
	c.Assert(tokens[0].Start().Index(), qt.Equals, 0)

	// Token 'b' should be at index 3 (after "a\n\t")
	c.Assert(tokens[1].Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(tokens[1].Value(), qt.Equals, "b")
	c.Assert(tokens[1].Start().Index(), qt.Equals, 3, qt.Commentf("Token 'b' should start at byte 3"))
	c.Assert(tokens[1].End().Index(), qt.Equals, 4)
}
