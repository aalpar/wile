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
	"bufio"
	"fmt"
	"io"
)

var (
	_ Value = (*CharacterInputPort)(nil)
)

type CharacterInputPort struct {
	Value io.RuneReader
}

func NewCharacterInputPortFromReader(rdr io.Reader) *CharacterInputPort {
	q := &CharacterInputPort{Value: bufio.NewReader(rdr)}
	return q
}

func NewCharacterInputPort(rdr io.RuneReader) *CharacterInputPort {
	q := &CharacterInputPort{Value: rdr}
	return q
}

func (p *CharacterInputPort) Datum() io.RuneReader {
	return p.Value
}

func (p *CharacterInputPort) IsVoid() bool {
	return p == nil
}

func (p *CharacterInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*CharacterInputPort); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *CharacterInputPort) SchemeString() string {
	return fmt.Sprintf("<character-input-port %p>", p.Value)
}
