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

import "fmt"

var _ Value = (*Record)(nil)

// Record represents a record instance as defined by R7RS define-record-type.
// Each record has a type descriptor and a slice of field values.
type Record struct {
	recordType *RecordType
	fields     []Value
}

// NewRecord creates a new Record with the given type and field values.
// The fields slice should have the same length as the record type's field count.
func NewRecord(rt *RecordType, fields []Value) *Record {
	return &Record{
		recordType: rt,
		fields:     fields,
	}
}

// RecordType returns the record's type descriptor.
func (r *Record) RecordType() *RecordType {
	return r.recordType
}

// Field returns the value at the given field index.
func (r *Record) Field(index int) Value {
	if index < 0 || index >= len(r.fields) {
		return nil
	}
	return r.fields[index]
}

// SetField sets the value at the given field index.
func (r *Record) SetField(index int, value Value) {
	if index >= 0 && index < len(r.fields) {
		r.fields[index] = value
	}
}

// FieldByName returns the value of the field with the given name.
// Returns nil if the field is not found.
func (r *Record) FieldByName(name *Symbol) Value {
	index := r.recordType.FieldIndex(name)
	if index < 0 {
		return nil
	}
	return r.fields[index]
}

// SetFieldByName sets the value of the field with the given name.
// Does nothing if the field is not found.
func (r *Record) SetFieldByName(name *Symbol, value Value) {
	index := r.recordType.FieldIndex(name)
	if index >= 0 {
		r.fields[index] = value
	}
}

// IsVoid returns true if the record is nil.
func (r *Record) IsVoid() bool {
	return r == nil
}

// EqualTo implements structural equality for records.
// Two records are equal if they have the same type and all fields are equal.
func (r *Record) EqualTo(v Value) bool {
	other, ok := v.(*Record)
	if !ok {
		return false
	}
	if r == nil || other == nil {
		return r == other
	}
	if r.recordType != other.recordType {
		return false
	}
	if len(r.fields) != len(other.fields) {
		return false
	}
	for i := range r.fields {
		if !EqualTo(r.fields[i], other.fields[i]) {
			return false
		}
	}
	return true
}

// SchemeString returns the Scheme external representation of the record.
func (r *Record) SchemeString() string {
	if r == nil {
		return "#<record>"
	}
	if r.recordType == nil {
		return "#<record>"
	}
	return fmt.Sprintf("#<record:%s>", r.recordType.Name().SchemeString())
}
