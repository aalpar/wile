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

var _ Value = (*Hashtable)(nil)

// Hashtable represents a Scheme hash table mapping strings to values.
type Hashtable struct {
	Value map[string]Value
}

// NewHashtable creates a new hash table from the given map.
func NewHashtable(v map[string]Value) *Hashtable {
	q := &Hashtable{
		Value: v,
	}
	return q
}

// Datum returns the underlying map.
func (p *Hashtable) Datum() map[string]Value {
	return p.Value
}

// IsVoid returns true if this hash table is nil.
func (p *Hashtable) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both hash tables have equal contents.
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

// SchemeString returns the Scheme representation of this hash table.
func (p *Hashtable) SchemeString() string {
	return fmt.Sprintf("%v", p.Value)
}
