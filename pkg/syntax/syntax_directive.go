// Copyright 2026 Aaron Alpar
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
	"github.com/aalpar/wile/pkg/values"
)

var (
	_ values.Value = (*SyntaxDirective)(nil)
	_ SyntaxValue  = (*SyntaxDirective)(nil)
)

// SyntaxDirective represents a reader directive (#!fold-case, etc.).
type SyntaxDirective struct {
	Name string
	syntaxBase
}

// AddScope returns the directive unchanged (directives don't participate in hygiene).
func (p *SyntaxDirective) AddScope(_ *Scope) SyntaxValue {
	return p
}

func (p *SyntaxDirective) Unwrap() values.Value {
	return values.NewString(p.Name)
}

// UnwrapAll returns the directive name as a string value.
func (p *SyntaxDirective) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// NewSyntaxDirective creates a new reader directive with the given name.
func NewSyntaxDirective(name string, sctx *SourceContext) *SyntaxDirective {
	return &SyntaxDirective{
		Name:       name,
		syntaxBase: values.NewSyntaxBase(sctx),
	}
}

// IsVoid returns true if the directive is nil.
func (p *SyntaxDirective) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both directives have the same name.
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

// SchemeString returns the directive name.
func (p *SyntaxDirective) SchemeString() string {
	return p.Name
}
