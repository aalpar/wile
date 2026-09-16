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

package environment

import "github.com/aalpar/wile/pkg/values"

// FormDenotation is implemented by the value of a keyword binding that names the
// special form the keyword denotes. It is what lets a renamed or prefixed import
// keep its meaning: the name travels with the binding's value, so dispatch reads
// the form off the resolved binding instead of off the spelling at the use site.
type FormDenotation interface {
	FormName() string
}

// FormKeyword is the value of a compile-time-only keyword binding (if, lambda,
// quote, ...). It exists only to name the form; a reference in value position is
// still refused by the compiler, which keys on BindingType, not on this value.
type FormKeyword struct {
	name string
}

// NewFormKeyword returns the keyword value denoting the special form name.
func NewFormKeyword(name string) *FormKeyword {
	return &FormKeyword{name: name}
}

// FormName implements FormDenotation.
func (p *FormKeyword) FormName() string {
	return p.name
}

// SchemeString implements values.Value.
func (p *FormKeyword) SchemeString() string {
	return "#<keyword:" + p.name + ">"
}

// IsVoid implements values.Value.
func (*FormKeyword) IsVoid() bool {
	return false
}

// EqualTo implements values.Value. Two keyword values are equal when they denote
// the same form, which is what import conflict detection needs: every owner mints
// its own value for the same keyword.
func (p *FormKeyword) EqualTo(other values.Value) bool {
	o, ok := other.(*FormKeyword)
	return ok && o.name == p.name
}

// DenotedForm returns the special form b denotes, or "" when b is nil, is a
// variable, or holds no form-naming value. A variable never denotes a form, even
// if its value happens to be a keyword value: a (define x ...) is not a keyword.
func DenotedForm(b *Binding) string {
	if b == nil {
		return ""
	}
	if b.BindingType() == BindingTypeVariable {
		return ""
	}
	fd, ok := b.Value().(FormDenotation)
	if !ok {
		return ""
	}
	return fd.FormName()
}
