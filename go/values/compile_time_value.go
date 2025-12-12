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

import "fmt"

var (
	_ Value = (*CompileTimeValue)(nil)
)

// CompileTimeValue wraps a value that is stored in the expand phase
// but accessible during macro expansion. This enables compile-time
// computation via define-for-syntax and begin-for-syntax.
type CompileTimeValue struct {
	Value Value
}

func NewCompileTimeValue(v Value) *CompileTimeValue {
	return &CompileTimeValue{
		Value: v,
	}
}

func (p *CompileTimeValue) Unwrap() Value {
	return p.Value
}

func (p *CompileTimeValue) IsVoid() bool {
	return p == nil
}

func (p *CompileTimeValue) EqualTo(v Value) bool {
	other, ok := v.(*CompileTimeValue)
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

func (p *CompileTimeValue) SchemeString() string {
	return fmt.Sprintf("#<compile-time-value %s>", p.Value.SchemeString())
}
