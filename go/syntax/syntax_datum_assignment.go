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
	_ values.Value = (*SyntaxDatumLabelAssignment)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabelAssignment)(nil)
)

type SyntaxDatumLabelAssignment struct {
	Label         int
	Value         values.Value
	sourceContext *SourceContext
}

func (p *SyntaxDatumLabelAssignment) SourceContext() *SourceContext {
	return p.sourceContext
}

func NewSyntaxDatumLabelAssignment(label int, value values.Value, sctx *SourceContext) *SyntaxDatumLabelAssignment {
	return &SyntaxDatumLabelAssignment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumLabelAssignment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumLabelAssignment) Unwrap() values.Value {
	return p.Value
}

func (p *SyntaxDatumLabelAssignment) UnwrapAll() values.Value {
	q := p.Value
	sv, ok := p.Value.(SyntaxValue)
	if ok {
		q = sv.UnwrapAll()
	}
	return q
}

func (p *SyntaxDatumLabelAssignment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumLabelAssignment)
	if !ok {
		return false
	}
	return p == other
}

func (p *SyntaxDatumLabelAssignment) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
