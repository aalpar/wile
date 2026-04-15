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

	"github.com/aalpar/wile/werr"
)

var _ Value = (*RecordType)(nil)

// RecordType represents a record type descriptor as defined by R7RS define-record-type.
// It holds the type name, the ordered list of field names, and an optional parent type
// for record inheritance.
type RecordType struct {
	name       *Symbol
	fieldNames []*Symbol
	parent     *RecordType
	opaque     bool
}

// NewRecordType creates a new RecordType with the given name and field names.
// The parent defaults to nil (no inheritance).
func NewRecordType(name *Symbol, fieldNames []*Symbol) *RecordType {
	return &RecordType{
		name:       name,
		fieldNames: fieldNames,
	}
}

// NewOpaqueRecordType creates a new RecordType that is opaque to generic inspection.
// Instances of opaque record types are not recognized by record? and cannot be
// inspected via record-type. Type-specific predicates and accessors still work.
// Panics if name is nil.
func NewOpaqueRecordType(name *Symbol, fieldNames []*Symbol) *RecordType {
	if name == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"NewOpaqueRecordType: name must not be nil",
		))
	}
	return &RecordType{
		name:       name,
		fieldNames: fieldNames,
		opaque:     true,
	}
}

// NewDerivedRecordType creates a new RecordType that inherits from the given parent.
// If the parent is opaque, the derived type is also opaque.
func NewDerivedRecordType(name *Symbol, parent *RecordType, fieldNames []*Symbol) *RecordType {
	return &RecordType{
		name:       name,
		parent:     parent,
		fieldNames: fieldNames,
		opaque:     parent != nil && parent.opaque,
	}
}

// IsOpaque returns true if this record type is opaque to generic inspection.
func (p *RecordType) IsOpaque() bool {
	return p != nil && p.opaque
}

// Parent returns the parent record type, or nil if this is a base type.
func (p *RecordType) Parent() *RecordType {
	return p.parent
}

// Name returns the record type's name symbol.
func (p *RecordType) Name() *Symbol {
	return p.name
}

// FieldNames returns the ordered list of field name symbols.
func (p *RecordType) FieldNames() []*Symbol {
	return p.fieldNames
}

// FieldCount returns the number of fields in this record type.
func (p *RecordType) FieldCount() int {
	return len(p.fieldNames)
}

// FieldIndex returns the index of the field with the given name, or -1 if not found.
func (p *RecordType) FieldIndex(name *Symbol) int {
	for i, fn := range p.fieldNames {
		if fn.Key == name.Key {
			return i
		}
	}
	return -1
}

// IsVoid returns true if the record type is nil.
func (p *RecordType) IsVoid() bool {
	return p == nil
}

// EqualTo implements identity-based equality for record types.
// Two record types are equal only if they are the same object.
func (p *RecordType) EqualTo(v Value) bool {
	other, ok := v.(*RecordType)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme external representation of the record type.
// Opaque record types use #<type:N> to avoid revealing the record nature.
func (p *RecordType) SchemeString() string {
	if p == nil {
		return "#<record-type>"
	}
	if p.opaque {
		return fmt.Sprintf("#<type:%s>", p.name.SchemeString())
	}
	return fmt.Sprintf("#<record-type:%s>", p.name.SchemeString())
}
