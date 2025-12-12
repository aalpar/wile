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
	"io"
)

var (
	_ Value = (*BinaryInputPort)(nil)
)

type BinaryInputPort struct {
	Value io.Reader
}

func NewBinaryInputPort(rdr io.Reader) *BinaryInputPort {
	return &BinaryInputPort{Value: rdr}
}

func (p *BinaryInputPort) Read(buf []byte) (int, error) {
	return p.Value.Read(buf)
}

func (p *BinaryInputPort) Datum() io.Reader {
	return p.Value
}

func (p *BinaryInputPort) IsVoid() bool {
	return p == nil
}

func (p *BinaryInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BinaryInputPort); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *BinaryInputPort) SchemeString() string {
	return fmt.Sprintf("<binary-input-port %p>", p.Value)
}
