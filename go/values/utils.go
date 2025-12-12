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
	"context"
	"encoding/base32"
	"fmt"
	"math/rand"
	"time"
)

// byteCnt is the number of bytes used for generating temporary variable names.
const (
	byteCnt = 128 / 8
)

// List constructs a proper list from the given values.
// Returns EmptyList if no arguments are provided.
// The resulting list has the values in the same order as the arguments.
func List(os ...Value) *Pair {
	l := len(os)
	switch l {
	case 0:
		return EmptyList
	case 1:
		return &Pair{os[0], EmptyList}
	}
	q := &Pair{os[0], &Pair{}}
	curr := q
	for _, v := range os[1:] {
		curr = curr[1].(*Pair)
		curr[0] = v
		curr[1] = &Pair{}
	}
	curr[1] = EmptyList
	return q
}

// ForEach iterates over a Tuple value, calling fn for each element.
// If the value is not a Tuple, returns the value unchanged with no error.
// The callback receives the element index, whether more elements follow, and the element value.
// Returns the tail of the tuple (EmptyList for proper lists) and any error from the callback.
func ForEach(ctx context.Context, o Value, fn ForEachFunc) (Value, error) {
	pr, ok := o.(Tuple)
	if ok {
		return pr.ForEach(ctx, fn)
	}
	return o, nil
}

// EqualTo compares two values for structural equality.
// Handles nil and void values specially: nil equals nil, void equals void.
// For other values, delegates to the Value.EqualTo method.
func EqualTo(a, b Value) bool {
	if a == nil || b == nil {
		return a == b
	}
	if a.IsVoid() || b.IsVoid() {
		return a.IsVoid() == b.IsVoid()
	}
	return a.EqualTo(b)
}

// NewTemporaryVariableName generates a unique symbol for use as a temporary variable.
// The symbol name has the format "__T_<base32-encoded-random-bytes>".
// Uses 128 bits of randomness to ensure uniqueness.
// Panics if random number generation fails.
func NewTemporaryVariableName() *Symbol {
	bs := make([]byte, byteCnt)
	n, err := rand.New(rand.NewSource(time.Now().UnixNano())).Read(bs)
	if err != nil {
		panic(fmt.Errorf("%w: error reading random stream", err))
	}
	if n != byteCnt {
		panic(fmt.Errorf("short read from random stream"))
	}
	q := NewSymbol(
		fmt.Sprintf("__T_%s", base32.StdEncoding.WithPadding(base32.NoPadding).EncodeToString(bs)),
	)
	return q
}

// IsList returns true if the value is a proper list.
// A proper list is either EmptyList or a chain of pairs ending with EmptyList.
// Returns false for nil, improper lists (dotted pairs), and non-list values.
func IsList(v Value) bool {
	if v == nil {
		return false
	}
	if v == EmptyList {
		return true
	}
	switch pr := v.(type) {
	case *ArrayList:
		return pr.IsList()
	case *Pair:
		return pr.IsList()
	}
	return false
}

// IsVoid returns true if the value represents the absence of a value.
// A value is void if it is nil or its IsVoid() method returns true.
// Note: typed nil pointers (e.g., var p *Pair = nil) are handled by the
// type's IsVoid() method, which checks for nil receiver.
func IsVoid(v Value) bool {
	return v == nil || v.IsVoid()
}

// IsEmptyList returns true if the value is the empty list.
// Returns false for nil values. For Tuple types, delegates to their IsEmptyList method.
func IsEmptyList(v Value) bool {
	if v == nil {
		return false
	}
	pr, ok := v.(Tuple)
	if ok {
		return pr.IsEmptyList()
	}
	return false
}

// VectorToList converts a Vector to a proper list preserving element order.
// Iterates backward through the vector, prepending each element to build the list.
// Returns EmptyList for nil or void vectors.
func VectorToList(vs *Vector) *Pair {
	if IsVoid(vs) {
		return EmptyList
	}
	var q = EmptyList
	for j := len(*vs) - 1; j >= 0; j-- {
		q = NewCons((*vs)[j], q)
	}
	return q
}
