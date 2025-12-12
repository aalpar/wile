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
	_ Value = (*BinaryOutputPort)(nil)
)

type BinaryOutputPort struct {
	Value io.Writer
}

func NewBinaryOutputPort(wrt io.Writer) *BinaryOutputPort {
	return &BinaryOutputPort{Value: wrt}
}

func (p *BinaryOutputPort) Write(buf []byte) (int, error) {
	return p.Value.Write(buf)
}

func (p *BinaryOutputPort) Datum() io.Writer {
	return p.Value
}

func (p *BinaryOutputPort) IsVoid() bool {
	return p == nil
}

func (p *BinaryOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*BinaryOutputPort); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *BinaryOutputPort) SchemeString() string {
	return fmt.Sprintf("<binary-output-port %p>", p.Value)
}
