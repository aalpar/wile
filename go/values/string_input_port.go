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
	_ Value = (*StringInputPort)(nil)
)

type StringInputPort struct {
	reader *strings.Reader
}

func NewStringInputPort(s string) *StringInputPort {
	return &StringInputPort{reader: strings.NewReader(s)}
}

func (p *StringInputPort) ReadRune() (rune, int, error) {
	return p.reader.ReadRune()
}

func (p *StringInputPort) UnreadRune() error {
	return p.reader.UnreadRune()
}

func (p *StringInputPort) Datum() *strings.Reader {
	return p.reader
}

func (p *StringInputPort) IsVoid() bool {
	return p == nil
}

func (p *StringInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*StringInputPort); ok {
		return p.reader == other.reader
	}
	return false
}

func (p *StringInputPort) SchemeString() string {
	return fmt.Sprintf("<string-input-port %p>", p.reader)
}
