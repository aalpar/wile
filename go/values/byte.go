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
	_ Value = (*Byte)(nil)
)

type Byte struct {
	Value uint8
}

func NewByte(v uint8) *Byte {
	q := &Byte{Value: v}
	return q
}

func (p *Byte) Datum() uint8 {
	return p.Value
}

func (p *Byte) IsVoid() bool {
	return p == nil
}

func (p *Byte) EqualTo(v Value) bool {
	if other, ok := v.(*Byte); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Byte) SchemeString() string {
	return fmt.Sprintf("%d", p.Value)
}
