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
	_ values.Value = (*SyntaxDatumLabelAssignment)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabelAssignment)(nil)
)

// SyntaxDatumLabelAssignment represents a datum label assignment (#n=datum).
type SyntaxDatumLabelAssignment struct {
	Label         int
	Value         values.Value
	sourceContext *SourceContext
}

// AddScope returns the assignment unchanged (labels don't participate in hygiene).
func (p *SyntaxDatumLabelAssignment) AddScope(_ *Scope) SyntaxValue {
	return p
}

// SourceContext returns the source context of the assignment.
func (p *SyntaxDatumLabelAssignment) SourceContext() *SourceContext {
	return p.sourceContext
}

// NewSyntaxDatumLabelAssignment creates a new datum label assignment.
func NewSyntaxDatumLabelAssignment(label int, value values.Value, sctx *SourceContext) *SyntaxDatumLabelAssignment {
	return &SyntaxDatumLabelAssignment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

// IsVoid returns true if the assignment is nil.
func (p *SyntaxDatumLabelAssignment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumLabelAssignment) Unwrap() values.Value {
	return p.Value
}

// UnwrapAll recursively unwraps the assigned value.
func (p *SyntaxDatumLabelAssignment) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// EqualTo returns true if both assignments are the same object.
func (p *SyntaxDatumLabelAssignment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumLabelAssignment)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the label.
func (p *SyntaxDatumLabelAssignment) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
