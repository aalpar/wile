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

package machine

import "slices"

// sameType is a helper for EqualTo methods on zero-field operations.
// The caller must perform the type assertion and pass the result.
//
// Usage:
//
//	func (p *OperationType) EqualTo(o values.wrt) bool {
//	    v, ok := o.(*OperationType)
//	    return sameType(p, v, ok)
//	}
func sameType[T any](p, v *T, ok bool) bool {
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}

// fieldMatches is a helper for EqualTo methods on single-field operations.
// The caller must perform the type assertion and pass the result.
//
// Usage:
//
//	func (p *OperationType) EqualTo(o values.wrt) bool {
//	    v, ok := o.(*OperationType)
//	    return fieldMatches(p, v, ok, func(op *OperationType) int { return op.Field })
//	}
func fieldMatches[T comparable, Op any](p, v *Op, ok bool, getField func(*Op) T) bool {
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return getField(p) == getField(v)
}

// fieldMethodMatches is a helper for EqualTo methods on single-field operations
// where the field is compared via a method rather than ==.
//
// Usage:
//
//	func (p *OperationType) EqualTo(o values.Value) bool {
//	    v, ok := o.(*OperationType)
//	    return fieldMethodMatches(p, v, ok,
//	        func(op *OperationType) *FieldType { return op.Field },
//	        func(a, b *FieldType) bool { return a.EqualTo(b) })
//	}
func fieldMethodMatches[T any, Op any](p, v *Op, ok bool, getField func(*Op) T, eq func(T, T) bool) bool {
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return eq(getField(p), getField(v))
}

// sliceMatches is a helper for EqualTo methods on operations with a single
// comparable-element slice field.
//
// Usage:
//
//	func (p *OperationType) EqualTo(o values.Value) bool {
//	    v, ok := o.(*OperationType)
//	    return sliceMatches(p, v, ok, func(op *OperationType) []string { return op.Items })
//	}
func sliceMatches[T comparable, Op any](p, v *Op, ok bool, getField func(*Op) []T) bool {
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return slices.Equal(getField(p), getField(v))
}

// OperationBase provides default SchemeString, IsVoid, and String methods
// for VM operations. Embed as the first field in operation structs.
// EqualTo is intentionally not provided: Go requires per-type assertions.
type OperationBase struct {
	opName string
	goName string
}

// NewOperationBase creates an OperationBase with the given Scheme name.
// The name is used as: "#<" + opName + ">".
func NewOperationBase(opName string) OperationBase {
	return OperationBase{
		opName: opName,
	}
}

// NewOperationBaseWithGoName creates an OperationBase with both a Scheme name
// and a separate Go fmt.Stringer name.
func NewOperationBaseWithGoName(opName, goName string) OperationBase {
	return OperationBase{
		opName: opName,
		goName: goName,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p OperationBase) SchemeString() string {
	return "#<" + p.opName + ">"
}

// IsVoid returns false; operations are never void.
func (p OperationBase) IsVoid() bool {
	return false
}

// String returns the Go string representation of the operation.
func (p OperationBase) String() string {
	if p.goName != "" {
		return p.goName
	}
	return p.opName
}
