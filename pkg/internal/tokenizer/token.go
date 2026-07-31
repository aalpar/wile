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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// Token is the interface for tokenizer output tokens.
type Token interface {
	Type() TokenizerState
	Start() syntax.SourceIndexes
	End() syntax.SourceIndexes
	String() string
	Value() string      // Returns processed value (e.g., with escape sequences converted)
	HasHashDigit() bool // R7RS §7.1.1: whether # appeared as inexact digit placeholder
	Radix() int         // Effective parse base for integer and decimal-fraction tokens (2/8/10/16); 0 otherwise
}

// SimpleToken is the concrete implementation of Token used by the tokenizer.
type SimpleToken struct {
	istart syntax.SourceIndexes
	iend   syntax.SourceIndexes
	typ    TokenizerState
	hash   bool // R7RS §7.1.1: whether # appeared as inexact digit placeholder
	rad    int  // effective parse base for integer tokens (2/8/10/16); 0 otherwise
	src    string
	val    string // Processed value (e.g., escape sequences converted)
}

// NewSimpleToken creates a new SimpleToken with the given type, source, value,
// position, hash-digit flag, and radix. radix is the effective parse base for
// integer tokens (2/8/10/16) and 0 for tokens where base is not meaningful.
func NewSimpleToken(typ TokenizerState, src, val string, sti, eni *syntax.SourceIndexes, hash bool, radix int) *SimpleToken {
	q := &SimpleToken{
		istart: *sti,
		iend:   *eni,
		typ:    typ,
		hash:   hash,
		rad:    radix,
		src:    src,
		val:    val,
	}
	return q
}

// Type returns the token type.
func (p *SimpleToken) Type() TokenizerState {
	return p.typ
}

func (p *SimpleToken) String() string {
	return p.src
}

// Value returns the processed value of the token (e.g., with escape sequences converted).
func (p *SimpleToken) Value() string {
	return p.val
}

// Start returns the source position where the token begins.
func (p *SimpleToken) Start() syntax.SourceIndexes {
	return p.istart
}

// End returns the source position where the token ends.
func (p *SimpleToken) End() syntax.SourceIndexes {
	return p.iend
}

// HasHashDigit returns true if the token contained # as an inexact digit placeholder.
// R7RS §7.1.1: # can appear in place of digits after at least one real digit,
// representing an unknown digit (treated as 0). Its presence forces the number to be inexact.
func (p *SimpleToken) HasHashDigit() bool {
	return p.hash
}

// Radix returns the effective parse base for an integer or decimal-fraction
// token (2, 8, 10, or 16). It is 0 for tokens where base is not meaningful
// (non-numeric tokens, and numeric shapes other than those two). The tokenizer
// records the base here so the parser reads it directly rather than re-deriving
// a base literal from the token state.
//
// Fractions carry it for the same reason integers do, plus one of their own:
// the reader's #e converts a decimal literal from its digits, and must not do
// that to a hex one (makeExactLiteral, pkg/parser).
func (p *SimpleToken) Radix() int {
	return p.rad
}

// SchemeString returns the Scheme representation of the token.
func (p *SimpleToken) SchemeString() string {
	q := fmt.Sprintf("<simple-token %q %d:%d %d>", p.src, p.istart, p.iend, p.typ)
	return q
}

// IsVoid returns true if the token is nil.
func (p *SimpleToken) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this token equals the given value.
func (p *SimpleToken) EqualTo(v values.Value) bool {
	other, ok := v.(*SimpleToken)
	if !ok {
		return false
	}
	if p.typ != other.typ {
		return false
	}
	if p.istart != other.istart {
		return false
	}
	if p.iend != other.iend {
		return false
	}
	if p.src != other.src {
		return false
	}
	return true
}
