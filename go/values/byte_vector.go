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
	"strings"
)

var (
	_ Value = (*ByteVector)(nil)
)

type ByteVector []Byte

func NewByteVector(vs ...*Integer) *ByteVector {
	if len(vs) == 0 {
		return &ByteVector{}
	}
	q := ByteVector(make([]Byte, len(vs)))
	for i := range vs {
		// FIXME: handle overflow, ugly but necessary
		b := NewByte(uint8((*vs[i]).Value))
		q[i] = *b
	}
	return &q
}

func (p *ByteVector) Datum() []Byte {
	return *p
}

func (p *ByteVector) IsVoid() bool {
	return p == nil
}

func (p *ByteVector) EqualTo(v Value) bool {
	other, ok := v.(*ByteVector)
	if !ok {
		return false
	}
	if len(*p) != len(*other) {
		return false
	}
	for i := range *p {
		if (*p)[i].Value != (*other)[i].Value {
			return false
		}
	}
	return true
}

func (p *ByteVector) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("#u8(")
	if len(*p) > 0 {
		q.WriteString(" ")
		q.WriteString(fmt.Sprintf("%s", (*p)[0].SchemeString()))
		for _, v := range (*p)[1:] {
			q.WriteString(" ")
			q.WriteString(fmt.Sprintf("%s", v.SchemeString()))
		}
		q.WriteString(" ")
	}
	q.WriteString(")")
	return q.String()
}
