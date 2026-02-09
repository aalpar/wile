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

package values

import (
	"strings"
	"unicode"
)

var (
	_ Value    = (*Symbol)(nil)
	_ Hashable = (*Symbol)(nil)
)

// Symbol represents a Scheme symbol.
type Symbol struct {
	Key string
}

// NewSymbol creates a new symbol with the given key.
func NewSymbol(key string) *Symbol {
	q := &Symbol{Key: key}
	return q
}

// HashCode returns a hash of the symbol's key.
func (p *Symbol) HashCode() uint64 {
	return hashString(0x1, p.Key)
}

// Datum returns the symbol's key string.
func (p *Symbol) Datum() string {
	return p.Key
}

// Copy returns a copy of the symbol.
func (p *Symbol) Copy() Value {
	q := &Symbol{Key: p.Key}
	return q
}

// IsVoid returns true if the symbol is nil.
func (p *Symbol) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the symbols have equal keys.
func (p *Symbol) EqualTo(v Value) bool {
	if other, ok := v.(*Symbol); ok { //nolint:gocritic
		return p.Key == other.Key
	}
	return false
}

// SchemeString returns the R7RS external representation of the symbol.
//
// R7RS §7.1.1: Identifiers that can be represented without bars are written
// bare. Otherwise, they are enclosed in vertical bars with only \ and |
// characters escaped.
func (p *Symbol) SchemeString() string {
	if isBareIdentifier(p.Key) {
		return p.Key
	}
	var sb strings.Builder
	sb.WriteByte('|')
	for _, r := range p.Key {
		if r == '\\' || r == '|' {
			sb.WriteByte('\\')
		}
		sb.WriteRune(r)
	}
	sb.WriteByte('|')
	return sb.String()
}

// isBareIdentifier checks whether a symbol name can be written without bars.
//
// R7RS §7.1.1: An identifier is either:
//   - <initial> <subsequent>*
//   - <peculiar identifier>: +, -, ..., or <explicit sign> <sign subsequent> <subsequent>*
//
// Additionally, identifiers that could be misread as number literals must be barred.
func isBareIdentifier(name string) bool {
	if len(name) == 0 {
		return false
	}

	runes := []rune(name)

	// Check special peculiar identifiers
	if name == "+" || name == "-" || name == "..." {
		return true
	}

	// First character must be an <initial>
	if isInitial(runes[0]) {
		// Remaining characters must be <subsequent>
		for _, r := range runes[1:] {
			if !isSubsequent(r) {
				return false
			}
		}
		return true
	}

	// <explicit sign> <sign subsequent> <subsequent>*
	if (runes[0] == '+' || runes[0] == '-') && len(runes) >= 2 {
		if isSignSubsequent(runes[1]) {
			for _, r := range runes[2:] {
				if !isSubsequent(r) {
					return false
				}
			}
			// Must not be misread as a number literal
			return !isNumberLiteral(name)
		}
		// <explicit sign> . <dot subsequent> <subsequent>*
		if runes[1] == '.' && len(runes) >= 3 && isDotSubsequent(runes[2]) {
			for _, r := range runes[3:] {
				if !isSubsequent(r) {
					return false
				}
			}
			return !isNumberLiteral(name)
		}
		return false
	}

	// . <dot subsequent> <subsequent>*
	if runes[0] == '.' && len(runes) >= 2 && isDotSubsequent(runes[1]) {
		for _, r := range runes[2:] {
			if !isSubsequent(r) {
				return false
			}
		}
		return true
	}

	return false
}

// isInitial checks if r is an R7RS <initial>: letter or special initial.
//
// R7RS §7.1.1: <initial> -> <letter> | <special initial>
// <special initial> -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
func isInitial(r rune) bool {
	if unicode.IsLetter(r) {
		return true
	}
	return strings.ContainsRune("!$%&*/:<=>?^_~", r)
}

// isSubsequent checks if r is an R7RS <subsequent>: initial, digit, or special subsequent.
//
// R7RS §7.1.1: <subsequent> -> <initial> | <digit> | <special subsequent>
// <special subsequent> -> + | - | . | @
func isSubsequent(r rune) bool {
	if isInitial(r) || unicode.IsDigit(r) {
		return true
	}
	return r == '+' || r == '-' || r == '.' || r == '@'
}

// isSignSubsequent checks if r is an R7RS <sign subsequent>.
//
// R7RS §7.1.1: <sign subsequent> -> <initial> | + | -  | @
func isSignSubsequent(r rune) bool {
	return isInitial(r) || r == '+' || r == '-' || r == '@'
}

// isDotSubsequent checks if r is an R7RS <dot subsequent>.
//
// R7RS §7.1.1: <dot subsequent> -> <sign subsequent> | .
func isDotSubsequent(r rune) bool {
	return isSignSubsequent(r) || r == '.'
}

// isNumberLiteral checks if name could be misread as a number literal.
// This catches +i, -i, +inf.0, -inf.0, +nan.0, -nan.0, and number-like identifiers.
// Also catches identifiers starting with +/-  followed by digits, dots, or
// the special prefixes inf/nan (which the tokenizer recognizes case-insensitively).
func isNumberLiteral(name string) bool {
	if len(name) < 2 || (name[0] != '+' && name[0] != '-') {
		return false
	}
	lower := strings.ToLower(name)
	switch lower {
	case "+i", "-i", "+inf.0", "-inf.0", "+nan.0", "-nan.0":
		return true
	}
	c := name[1]
	// +<digit> or -<digit>
	if c >= '0' && c <= '9' {
		return true
	}
	// +.<digit> or -.<digit>
	if c == '.' && len(name) >= 3 && name[2] >= '0' && name[2] <= '9' {
		return true
	}
	// +inf... or +nan... (case-insensitive prefix check)
	rest := lower[1:]
	if strings.HasPrefix(rest, "inf") || strings.HasPrefix(rest, "nan") {
		return true
	}
	return false
}
