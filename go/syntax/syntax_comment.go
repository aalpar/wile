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
	"wile/values"
)

var (
	_ values.Value = (*SyntaxComment)(nil)
	_ SyntaxValue  = (*SyntaxComment)(nil)
)

type SyntaxComment struct {
	Text          string
	sourceContext *SourceContext
}

func (p *SyntaxComment) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxComment) Unwrap() values.Value {
	return values.NewString(p.Text)
}

func (p *SyntaxComment) UnwrapAll() values.Value {
	return values.NewString(p.Text)
}

func NewSyntaxComment(text string, sctx *SourceContext) *SyntaxComment {
	return &SyntaxComment{
		Text:          text,
		sourceContext: sctx,
	}
}

func (p *SyntaxComment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxComment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxComment)
	if !ok {
		return false
	}
	if p.Text != other.Text {
		return false
	}
	return true
}

func (p *SyntaxComment) SchemeString() string {
	return p.Text
}
