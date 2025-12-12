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
	_ Value = (*Symbol)(nil)
)

type Symbol struct {
	Key string
}

func NewSymbol(key string) *Symbol {
	q := &Symbol{Key: key}
	return q
}

func (p *Symbol) Datum() string {
	return p.Key
}

func (p *Symbol) Copy() Value {
	q := &Symbol{Key: p.Key}
	return q
}

func (p *Symbol) IsVoid() bool {
	return p == nil
}

func (p *Symbol) EqualTo(v Value) bool {
	if other, ok := v.(*Symbol); ok {
		return p.Key == other.Key
	}
	return false
}

func (p *Symbol) SchemeString() string {
	return p.Key
}
