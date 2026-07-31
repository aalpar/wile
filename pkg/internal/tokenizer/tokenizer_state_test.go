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
	"strconv"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestTokenizerStateNamesAreComplete is the ratchet the const block's comment
// promises: a state added without a tokenizerStateNames entry would render as
// the empty string inside a reader diagnostic ("tokenizer state "), which reads
// as a formatting bug rather than a missing table row.
//
// Names must also be unique — two states sharing one name make a diagnostic
// ambiguous about which scanning path failed.
func TestTokenizerStateNamesAreComplete(t *testing.T) {
	c := qt.New(t)

	seen := make(map[string]TokenizerState, tokenizerStateCount)
	for i := range int(tokenizerStateCount) {
		s := TokenizerState(i)
		name := s.String()
		c.Assert(name, qt.Not(qt.Equals), "",
			qt.Commentf("TokenizerState(%d) has no tokenizerStateNames entry", int(s)))
		prev, dup := seen[name]
		c.Assert(dup, qt.IsFalse,
			qt.Commentf("TokenizerState(%d) and TokenizerState(%d) share the name %q",
				int(prev), int(s), name))
		seen[name] = s
	}
}

// TestTokenizerStateStringOutOfRange pins that an unnamed value still
// identifies itself. A fixed "unknown" would make a user's bug report
// unactionable; the number is the one fact worth keeping.
func TestTokenizerStateStringOutOfRange(t *testing.T) {
	c := qt.New(t)

	c.Assert(tokenizerStateCount.String(), qt.Equals,
		"tokenizer-state-"+strconv.Itoa(int(tokenizerStateCount)))
	c.Assert(TokenizerState(-1).String(), qt.Equals, "tokenizer-state--1")
}
