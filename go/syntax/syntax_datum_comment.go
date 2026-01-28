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

package syntax

import (
	"fmt"

	"wile/values"
)

var (
	_ values.Value = (*SyntaxDatumComment)(nil)
	_ SyntaxValue  = (*SyntaxDatumComment)(nil)
)

// SyntaxDatumComment represents a datum comment (#;datum).
type SyntaxDatumComment struct {
	Label         string
	Value         SyntaxValue
	sourceContext *SourceContext
}

// NewSyntaxDatumComment creates a new datum comment.
func NewSyntaxDatumComment(label string, value SyntaxValue, sctx *SourceContext) *SyntaxDatumComment {
	return &SyntaxDatumComment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

// AddScope returns the comment unchanged (comments don't participate in hygiene).
func (p *SyntaxDatumComment) AddScope(_ *Scope) SyntaxValue {
	return p
}

// SourceContext returns the source context of the comment.
func (p *SyntaxDatumComment) SourceContext() *SourceContext {
	return p.sourceContext
}

// IsVoid returns true if the comment is nil.
func (p *SyntaxDatumComment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumComment) Unwrap() values.Value {
	return p.Value
}

// UnwrapAll recursively unwraps the commented value.
func (p *SyntaxDatumComment) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// EqualTo compares datum comments by label and value.
func (p *SyntaxDatumComment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumComment)
	if !ok {
		return false
	}
	if p.Label != other.Label {
		return false
	}
	return p.Value.UnwrapAll().EqualTo(other.Value.UnwrapAll())
}

// SchemeString returns a string representation of the datum comment.
func (p *SyntaxDatumComment) SchemeString() string {
	return fmt.Sprintf("%s %s", p.Label, p.Value.SchemeString())
}
