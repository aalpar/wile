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
	"bytes"
	"fmt"
)

var (
	_ Value = (*BytevectorOutputPort)(nil)
)

type BytevectorOutputPort struct {
	buffer *bytes.Buffer
}

func NewBytevectorOutputPort() *BytevectorOutputPort {
	return &BytevectorOutputPort{buffer: &bytes.Buffer{}}
}

func (p *BytevectorOutputPort) Write(data []byte) (int, error) {
	return p.buffer.Write(data)
}

func (p *BytevectorOutputPort) WriteByte(b byte) error {
	return p.buffer.WriteByte(b)
}

func (p *BytevectorOutputPort) GetBytevector() []byte {
	return p.buffer.Bytes()
}

func (p *BytevectorOutputPort) Datum() *bytes.Buffer {
	return p.buffer
}

func (p *BytevectorOutputPort) IsVoid() bool {
	return p == nil
}

func (p *BytevectorOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BytevectorOutputPort); ok {
		return p.buffer == other.buffer
	}
	return false
}

func (p *BytevectorOutputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-output-port %p>", p.buffer)
}
