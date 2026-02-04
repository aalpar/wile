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

	"github.com/aalpar/wile/values"
)

var (
	_ values.Value = (*SyntaxDatumLabel)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabel)(nil)
)

// SyntaxDatumLabel represents a datum label reference (#n#).
type SyntaxDatumLabel struct {
	Label         int
	sourceContext *SourceContext
}

// AddScope returns the label unchanged (labels don't participate in hygiene).
func (p *SyntaxDatumLabel) AddScope(_ *Scope) SyntaxValue {
	return p
}

// SourceContext returns the source context of the label.
func (p *SyntaxDatumLabel) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxDatumLabel) Unwrap() values.Value {
	return values.NewInteger(int64(p.Label))
}

// UnwrapAll returns the label number as an integer value.
func (p *SyntaxDatumLabel) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// NewSyntaxDatumLabel creates a new datum label reference with the given number.
func NewSyntaxDatumLabel(label int, sctx *SourceContext) *SyntaxDatumLabel {
	return &SyntaxDatumLabel{
		Label:         label,
		sourceContext: sctx,
	}
}

// IsVoid returns true if the label is nil.
func (p *SyntaxDatumLabel) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the labels have the same number.
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

// SchemeString returns the label number as a string.
func (p *SyntaxDatumLabel) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
