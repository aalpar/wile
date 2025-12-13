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

type Token interface {
	Type() TokenizerState
	Start() syntax.SourceIndexes
	End() syntax.SourceIndexes
	String() string
	Value() string // Returns processed value (e.g., with escape sequences converted)
}

type SimpleToken struct {
	istart syntax.SourceIndexes
	iend   syntax.SourceIndexes
	typ    TokenizerState
	neg    bool
	sign   bool
	rad    int
	src    string
	val    string // Processed value (e.g., escape sequences converted)
}

func NewSimpleToken(typ TokenizerState, src, val string, sti, eni *syntax.SourceIndexes, neg, sign bool, rad int) *SimpleToken {
	q := &SimpleToken{
		istart: *sti,
		iend:   *eni,
		typ:    typ,
		sign:   sign,
		neg:    neg,
		rad:    rad,
		src:    src,
		val:    val,
	}
	return q
}

func (p *SimpleToken) Type() TokenizerState {
	return p.typ
}

func (p *SimpleToken) String() string {
	return p.src
}

func (p *SimpleToken) Value() string {
	// For string tokens, always use val (which may be empty for "")
	// For other tokens, fall back to src if val is not set
	if p.typ == TokenizerStateStringEnd {
		return p.val
	}
	if p.val != "" {
		return p.val
	}
	return p.src
}

func (p *SimpleToken) Start() syntax.SourceIndexes {
	return p.istart
}

func (p *SimpleToken) End() syntax.SourceIndexes {
	return p.iend
}

func (p *SimpleToken) SchemeString() string {
	q := fmt.Sprintf("<simple-token %q %d:%d %d>", p.src, p.istart, p.iend, p.typ)
	return q
}

func (p *SimpleToken) IsVoid() bool {
	return p == nil
}

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
