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
	_ values.Value = (*SyntaxDatumLabel)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabel)(nil)
)

type SyntaxDatumLabel struct {
	Label         int
	sourceContext *SourceContext
}

func (p *SyntaxDatumLabel) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxDatumLabel) Unwrap() values.Value {
	return values.NewInteger(int64(p.Label))
}

func (p *SyntaxDatumLabel) UnwrapAll() values.Value {
	return values.NewInteger(int64(p.Label))
}

func NewSyntaxDatumLabel(label int, sctx *SourceContext) *SyntaxDatumLabel {
	return &SyntaxDatumLabel{
		Label:         label,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumLabel) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumLabel) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumLabel)
	if !ok {
		return false
	}
	if p.Label != other.Label {
		return false
	}
	return true
}

func (p *SyntaxDatumLabel) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
