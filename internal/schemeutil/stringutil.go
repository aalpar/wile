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

import "strings"

// NormalizeExponentMarker replaces an R7RS short float exponent marker
// (s, S, f, F, d, D, l, L) with 'e' so that strconv.ParseFloat can parse the
// number. R7RS §7.1.1: all exponent markers have the same meaning in Wile.
// Only the first marker is replaced; strings without one are returned unchanged.
func NormalizeExponentMarker(s string) string {
	idx := strings.IndexAny(s, "sSfFdDlL")
	if idx == -1 {
		return s
	}
	q := []byte(s)
	q[idx] = 'e'
	return string(q)
}

// ToLowerASCII converts an ASCII byte to lowercase.
// Non-ASCII bytes are returned unchanged.
func ToLowerASCII(c byte) byte {
	if c >= 'A' && c <= 'Z' {
		return c + ('a' - 'A')
	}
	return c
}

// HasPrefixCI reports whether s begins with prefix, using ASCII case-insensitive
// comparison. Only ASCII letters (A-Z, a-z) are treated as case-insensitive;
// all other bytes must match exactly.
func HasPrefixCI(s, prefix string) bool {
	if len(s) < len(prefix) {
		return false
	}
	for i := 0; i < len(prefix); i++ {
		if ToLowerASCII(s[i]) != ToLowerASCII(prefix[i]) {
			return false
		}
	}
	return true
}

// HasSuffixCI reports whether s ends with suffix, using ASCII case-insensitive
// comparison. Only ASCII letters (A-Z, a-z) are treated as case-insensitive;
// all other bytes must match exactly.
func HasSuffixCI(s, suffix string) bool {
	if len(s) < len(suffix) {
		return false
	}
	start := len(s) - len(suffix)
	for i := 0; i < len(suffix); i++ {
		if ToLowerASCII(s[start+i]) != ToLowerASCII(suffix[i]) {
			return false
		}
	}
	return true
}

// TrimPrefixCI returns s without the provided leading prefix string,
// using ASCII case-insensitive comparison. If s doesn't start with prefix
// (case-insensitively), s is returned unchanged.
func TrimPrefixCI(s, prefix string) string {
	if HasPrefixCI(s, prefix) {
		return s[len(prefix):]
	}
	return s
}

// TrimSuffixCI returns s without the provided trailing suffix string,
// using ASCII case-insensitive comparison. If s doesn't end with suffix
// (case-insensitively), s is returned unchanged.
func TrimSuffixCI(s, suffix string) string {
	if HasSuffixCI(s, suffix) {
		return s[:len(s)-len(suffix)]
	}
	return s
}
