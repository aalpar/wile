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

var _ Value = (*Record)(nil)

// Record represents a record instance as defined by R7RS define-record-type.
// Each record has a type descriptor and a slice of field values.
type Record struct {
	recordType *RecordType
	fields     []Value
}

// NewRecord creates a new Record with the given type and field values.
// Returns an error if rt is nil or len(fields) does not match rt.FieldCount().
func NewRecord(rt *RecordType, fields []Value) (*Record, error) {
	if rt == nil {
		return nil, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"NewRecord: record type must not be nil",
		)
	}
	if len(fields) != rt.FieldCount() {
		return nil, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"NewRecord: got %d fields, record type %s requires %d",
			len(fields), rt.Name().SchemeString(), rt.FieldCount(),
		)
	}
	return &Record{
		recordType: rt,
		fields:     fields,
	}, nil
}

// RecordType returns the record's type descriptor.
func (p *Record) RecordType() *RecordType {
	return p.recordType
}

// Field returns the value at the given field index.
func (p *Record) Field(index int) Value {
	if index < 0 || index >= len(p.fields) {
		return nil
	}
	return p.fields[index]
}

// SetField sets the value at the given field index.
func (p *Record) SetField(index int, value Value) {
	if index >= 0 && index < len(p.fields) {
		p.fields[index] = value
	}
}

// FieldByName returns the value of the field with the given name.
// Returns nil if the field is not found.
func (p *Record) FieldByName(name *Symbol) Value {
	index := p.recordType.FieldIndex(name)
	if index < 0 {
		return nil
	}
	return p.fields[index]
}

// SetFieldByName sets the value of the field with the given name.
// Does nothing if the field is not found.
func (p *Record) SetFieldByName(name *Symbol, value Value) {
	index := p.recordType.FieldIndex(name)
	if index >= 0 {
		p.fields[index] = value
	}
}

// IsVoid returns true if the record is nil.
func (p *Record) IsVoid() bool {
	return p == nil
}

// EqualTo implements structural equality for records.
// Two records are equal if they have the same type and all fields are equal.
func (p *Record) EqualTo(v Value) bool {
	other, ok := v.(*Record)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	if p.recordType != other.recordType {
		return false
	}
	if len(p.fields) != len(other.fields) {
		return false
	}
	for i := range p.fields {
		if !EqualTo(p.fields[i], other.fields[i]) {
			return false
		}
	}
	return true
}

// SchemeString returns the Scheme external representation of the record.
// Opaque records omit the "record:" prefix to avoid revealing their implementation.
func (p *Record) SchemeString() string {
	if p == nil {
		return "#<record>"
	}
	if p.recordType == nil {
		return "#<record>"
	}
	if p.recordType.IsOpaque() {
		return fmt.Sprintf("#<%s>", p.recordType.Name().SchemeString())
	}
	return fmt.Sprintf("#<record:%s>", p.recordType.Name().SchemeString())
}
