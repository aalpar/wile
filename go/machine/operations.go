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


package machine

import (
	"wile/values"
)

type Operations []Operation

func NewOperations(ops ...Operation) Operations {
	return ops
}

func (p Operations) Length() int {
	return len(p)
}
func (p Operations) SchemeString() string {
	return "#<machine-operations>"
}

func (p Operations) IsVoid() bool {
	return p == nil
}

func (p Operations) EqualTo(o values.Value) bool {
	v, ok := o.(Operations)
	if !ok {
		return false
	}
	if len(p) != len(v) {
		return false
	}
	for i := 0; i < len(p); i++ {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}

func (p Operations) Copy() Operations {
	q := NewOperations(p...)
	return q
}
