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

import "fmt"

var _ Value = (*RecordType)(nil)

// RecordType represents a record type descriptor as defined by R7RS define-record-type.
// It holds the type name and the ordered list of field names.
type RecordType struct {
	name       *Symbol
	fieldNames []*Symbol
}

// NewRecordType creates a new RecordType with the given name and field names.
func NewRecordType(name *Symbol, fieldNames []*Symbol) *RecordType {
	return &RecordType{
		name:       name,
		fieldNames: fieldNames,
	}
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
		if fn == name {
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
func (p *RecordType) SchemeString() string {
	if p == nil {
		return "#<record-type>"
	}
	return fmt.Sprintf("#<record-type:%s>", p.name.SchemeString())
}
