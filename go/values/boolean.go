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

package values

var (
	_ Value = (*Boolean)(nil)

	FalseValue = NewBoolean(false)
	TrueValue  = NewBoolean(true)
)

type Boolean struct {
	Value bool
}

func NewBoolean(v bool) *Boolean {
	q := &Boolean{Value: v}
	return q
}

func (p *Boolean) Datum() bool {
	return p.Value
}

func (p *Boolean) IsVoid() bool {
	return p == nil
}

func (p *Boolean) EqualTo(v Value) bool {
	if other, ok := v.(*Boolean); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Boolean) SchemeString() string {
	if !p.Value {
		return "#f"
	}
	return "#t"
}
