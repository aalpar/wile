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
	"sync/atomic"
)

var (
	_ Value = (*OpaqueValue)(nil)

	opaqueValueIDCounter atomic.Uint64
)

// OpaqueValue wraps an arbitrary Go object as a Scheme value.
// Construction is Go-only via NewOpaqueValue. The inner value
// is accessible only from Go via Unwrap.
type OpaqueValue struct {
	tag string
	id  uint64
	val any
}

// NewOpaqueValue creates a new opaque value with the given tag and inner value.
func NewOpaqueValue(tag string, val any) *OpaqueValue {
	id := opaqueValueIDCounter.Add(1)
	return &OpaqueValue{
		tag: tag,
		id:  id,
		val: val,
	}
}

// OpaqueTag returns the tag string identifying this opaque value's kind.
func (p *OpaqueValue) OpaqueTag() string {
	if p == nil {
		return ""
	}
	return p.tag
}

// Unwrap returns the inner Go value. Go-only — not exposed to Scheme.
func (p *OpaqueValue) Unwrap() any {
	if p == nil {
		return nil
	}
	return p.val
}

// SchemeString returns the Scheme representation of this opaque value.
func (p *OpaqueValue) SchemeString() string {
	if p == nil {
		return "#<opaque:void>"
	}
	return fmt.Sprintf("#<%s:%d>", p.tag, p.id)
}

// IsVoid returns true if this opaque value is nil.
func (p *OpaqueValue) IsVoid() bool {
	return p == nil
}

// EqualTo returns true only if both are the same object (identity equality).
func (p *OpaqueValue) EqualTo(v Value) bool {
	other, ok := v.(*OpaqueValue)
	if !ok {
		return false
	}
	return p == other
}
