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

package parser

import (
	"unicode"
	"unicode/utf8"
)

// runesEqualFold returns true if two runes are equal under case-folding.
// Returns false if either rune is an error.
func runesEqualFold(r0, r1 rune) bool {
	if r0 == utf8.RuneError || r1 == utf8.RuneError {
		return false
	}
	return unicode.ToLower(r0) == unicode.ToLower(r1)
}

// TrimPrefixFolded performs a case-insensitive trim of the specified prefix from the string s.
// If s does not start with the prefix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimPrefix but case-insensitive.
func TrimPrefixFolded(s, prefix string) string {
	if len(prefix) == 0 || len(prefix) > len(s) {
		return s
	}
	i := 0
	j := 0
	for j < len(prefix) {
		if i >= len(s) {
			return s
		}
		r0, n0 := utf8.DecodeRuneInString(s[i:])
		r1, n1 := utf8.DecodeRuneInString(prefix[j:])
		if !runesEqualFold(r0, r1) {
			return s
		}
		i += n0
		j += n1
	}
	return s[i:]
}

// TrimSuffixFolded performs a case-insensitive trim of the specified suffix from the string s.
// If s does not end with the suffix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimSuffix but case-insensitive.
func TrimSuffixFolded(s, suffix string) string {
	if len(suffix) > len(s) || len(suffix) == 0 {
		return s
	}
	lasti := len(s)
	lastj := len(suffix)
	for lastj > 0 {
		r0, n0 := utf8.DecodeLastRuneInString(s[:lasti])
		r1, n1 := utf8.DecodeLastRuneInString(suffix[:lastj])
		if !runesEqualFold(r0, r1) {
			return s
		}
		lasti -= n0
		lastj -= n1
	}
	return s[:lasti]
}
