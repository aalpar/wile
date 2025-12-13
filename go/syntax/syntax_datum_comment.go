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

type SyntaxDatumComment struct {
	Label         string
	Value         SyntaxValue
	sourceContext *SourceContext
}

func (p *SyntaxDatumComment) SourceContext() *SourceContext {
	return p.sourceContext
}

func NewSyntaxDatumComment(label string, value SyntaxValue, sctx *SourceContext) *SyntaxDatumComment {
	return &SyntaxDatumComment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumComment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumComment) Unwrap() values.Value {
	return p.Value
}

func (p *SyntaxDatumComment) UnwrapAll() values.Value {
	sv, ok := p.Value.(SyntaxValue)
	if ok {
		return sv.UnwrapAll()
	}
	return p.Value
}

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

func (p *SyntaxDatumComment) SchemeString() string {
	return fmt.Sprintf("%s %s", p.Label, p.Value.SchemeString())
}
