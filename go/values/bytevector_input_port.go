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
	_ Value = (*BytevectorInputPort)(nil)
)

type BytevectorInputPort struct {
	reader *bytes.Reader
}

func NewBytevectorInputPort(data []byte) *BytevectorInputPort {
	return &BytevectorInputPort{reader: bytes.NewReader(data)}
}

func (p *BytevectorInputPort) Read(data []byte) (int, error) {
	return p.reader.Read(data)
}

func (p *BytevectorInputPort) ReadByte() (byte, error) {
	return p.reader.ReadByte()
}

func (p *BytevectorInputPort) UnreadByte() error {
	return p.reader.UnreadByte()
}

func (p *BytevectorInputPort) Datum() *bytes.Reader {
	return p.reader
}

func (p *BytevectorInputPort) IsVoid() bool {
	return p == nil
}

func (p *BytevectorInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BytevectorInputPort); ok {
		return p.reader == other.reader
	}
	return false
}

func (p *BytevectorInputPort) SchemeString() string {
	return fmt.Sprintf("<bytevector-input-port %p>", p.reader)
}
