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

package values

import "fmt"

var _ Value = (*Box)(nil)

// Box represents a mutable Scheme box (container).
type Box struct {
	Value Value
}

// NewBox creates a new box containing the given value.
func NewBox(v Value) *Box {
	q := &Box{
		Value: v,
	}
	return q
}

// Datum returns the boxed value.
func (p *Box) Datum() Value {
	return p.Value
}

// Unbox returns the boxed value.
func (p *Box) Unbox() Value {
	return p.Value
}

// IsVoid returns true if the box is nil.
func (p *Box) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the boxes contain equal values.
func (p *Box) EqualTo(v Value) bool {
	other, ok := v.(*Box)
	if !ok {
		return false
	}
	if p == other {
		return true
	}
	if p == nil && other == nil {
		return true
	}
	if p == nil || other == nil {
		return false
	}
	if p.Value == nil && other.Value == nil {
		return true
	}
	if p.Value == nil || other.Value == nil {
		return false
	}
	return p.Value.EqualTo(other.Value)
}

// SchemeString returns the Scheme representation of the box.
func (p *Box) SchemeString() string {
	return fmt.Sprintf("#&%s", p.Value.SchemeString())
}
