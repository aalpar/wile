// Copyright 2026 Aaron Alpar
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

	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ Value    = (*Byte)(nil)
	_ Hashable = (*Byte)(nil)
)

// Byte represents a Scheme byte value (0-255).
type Byte struct {
	Value uint8
}

// NewByte creates a new byte value.
func NewByte(v uint8) *Byte {
	q := &Byte{Value: v}
	return q
}

// HashCode returns a hash of the byte value.
func (p *Byte) HashCode() uint64 {
	return hashUint64(0x8, uint64(p.Value))
}

// IsVoid returns true if the byte is nil.
func (p *Byte) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the bytes have equal values.
func (p *Byte) EqualTo(v Value) bool {
	other, ok := v.(*Byte)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the byte.
func (p *Byte) SchemeString() string {
	return fmt.Sprintf("%d", p.Value)
}

// ValidateByteValue checks that an integer is in the byte range [0, 255].
// Returns a wrapped werr.ErrNotAByte error if the value is out of range.
func ValidateByteValue(v *Integer, name string, desc string) error {
	if v.Value < 0 || v.Value > 255 {
		return werr.WrapForeignErrorf(werr.ErrNotAByte,
			"%s: %s must be a byte (0-255)", name, desc)
	}
	return nil
}
