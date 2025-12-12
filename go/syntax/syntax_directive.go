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
	_ values.Value = (*SyntaxDirective)(nil)
	_ SyntaxValue  = (*SyntaxDirective)(nil)
)

type SyntaxDirective struct {
	Name          string
	sourceContext *SourceContext
}

func (p *SyntaxDirective) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxDirective) Unwrap() values.Value {
	return values.NewString(p.Name)
}

func (p *SyntaxDirective) UnwrapAll() values.Value {
	return values.NewString(p.Name)
}

func NewSyntaxDirective(name string, sctx *SourceContext) *SyntaxDirective {
	return &SyntaxDirective{
		Name:          name,
		sourceContext: sctx,
	}
}

func (p *SyntaxDirective) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDirective) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDirective)
	if !ok {
		return false
	}
	if p.Name != other.Name {
		return false
	}
	return true
}

func (p *SyntaxDirective) SchemeString() string {
	return fmt.Sprintf("%s", p.Name)
}
