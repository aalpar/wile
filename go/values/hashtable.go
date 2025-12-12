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

import (
	"fmt"
)

var (
	_ Value = (*Hashtable)(nil)
)

type Hashtable struct {
	Value map[string]Value
}

func NewHashtable(v map[string]Value) *Hashtable {
	q := &Hashtable{
		Value: v,
	}
	return q
}

func (p *Hashtable) Datum() map[string]Value {
	return p.Value
}

func (p *Hashtable) IsVoid() bool {
	return p == nil
}

func (p *Hashtable) EqualTo(o Value) bool {
	v, ok := o.(*Hashtable)
	if !ok {
		return false
	}
	if len(p.Value) != len(v.Value) {
		return false
	}
	for k := range p.Value {
		_, ok = v.Value[k]
		if !ok {
			return false
		}
	}
	for k := range v.Value {
		_, ok = p.Value[k]
		if !ok {
			return false
		}
	}
	for k := range p.Value {
		if !p.Value[k].EqualTo(v.Value[k]) {
			return false
		}
	}
	return true
}

func (p *Hashtable) SchemeString() string {
	return fmt.Sprintf("%v", p.Value)
}
