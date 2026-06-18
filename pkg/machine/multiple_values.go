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

package machine

import (
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/values"
)

// MultipleValues represents multiple return values from a function.
type MultipleValues []values.Value

// NewMultipleValues creates a new MultipleValues from the given values.
func NewMultipleValues(values ...values.Value) MultipleValues {
	return values
}

// Len returns the number of values in the MultipleValues.
func (p MultipleValues) Len() int {
	return len(p)
}

// Copy creates a copy of the MultipleValues.
func (p MultipleValues) Copy() MultipleValues {
	return slices.Clone(p)
}

// IsVoid returns true if the MultipleValues represents 'void' - either
func (p MultipleValues) IsVoid() bool {
	if len(p) == 0 {
		return true
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return true
	}
	return false
}

// SchemeString returns the Scheme representation of the MultipleValues.
func (p MultipleValues) SchemeString() string {
	q := strings.Builder{}
	if len(p) == 0 {
		return values.SpecialVoid
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return values.SpecialVoid
	}
	q.WriteString(p[0].SchemeString())
	for i, v := range p[1:] {
		if i != 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
	}
	return q.String()
}

// EqualTo checks if the MultipleValues is equal to another value.
func (p MultipleValues) EqualTo(o values.Value) bool {
	v, ok := o.(MultipleValues)
	if !ok {
		return false
	}
	if len(p) != len(v) {
		return false
	}
	for i := range p {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}
