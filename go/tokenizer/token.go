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

	"wile/syntax"
	"wile/values"
)

// Token is the interface for tokenizer output tokens.
type Token interface {
	Type() TokenizerState
	Start() syntax.SourceIndexes
	End() syntax.SourceIndexes
	String() string
	Value() string // Returns processed value (e.g., with escape sequences converted)
}

// SimpleToken is the concrete implementation of Token used by the tokenizer.
type SimpleToken struct {
	istart syntax.SourceIndexes
	iend   syntax.SourceIndexes
	typ    TokenizerState
	sign   bool // Whether the token has an explicit sign (+ or -)
	rad    int
	src    string
	val    string // Processed value (e.g., escape sequences converted)
}

// NewSimpleToken creates a new SimpleToken with the given type, source, value, and position.
func NewSimpleToken(typ TokenizerState, src, val string, sti, eni *syntax.SourceIndexes, signed bool, rad int) *SimpleToken {
	q := &SimpleToken{
		istart: *sti,
		iend:   *eni,
		typ:    typ,
		sign:   signed,
		rad:    rad,
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
	// For string tokens, always use val (which may be empty for "")
	// For other tokens, fall back to src if val is not set
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
