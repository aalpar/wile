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

// KeyValue represents a key-value pair.
type KeyValue struct {
	Key   string
	Value Value
}

// NewKeyValue creates a new key-value pair.
func NewKeyValue(key string, value Value) *KeyValue {
	return &KeyValue{
		Key:   key,
		Value: value,
	}
}

// Datum returns the key-value as a map.
func (p *KeyValue) Datum() map[string]Value {
	return map[string]Value{p.Key: p.Value}
}
