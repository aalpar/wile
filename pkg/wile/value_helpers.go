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

package wile

import (
	"context"

	"github.com/aalpar/wile/pkg/values"
)

// IsList returns true if v is a proper list (including the empty list).
func IsList(v Value) bool {
	return values.IsList(unwrapValue(v))
}

// IsPair returns true if v is a non-empty pair (cons cell).
// EmptyList is not a *Pair (it's a separate type), so the type assertion
// handles the distinction without an explicit IsEmptyList check.
func IsPair(v Value) bool {
	_, ok := unwrapValue(v).(*values.Pair)
	return ok
}

// IsNull returns true if v is the empty list.
func IsNull(v Value) bool {
	return values.IsEmptyList(unwrapValue(v))
}

// IsSymbol returns true if v is a symbol.
func IsSymbol(v Value) bool {
	_, ok := unwrapValue(v).(*values.Symbol)
	return ok
}

// IsString returns true if v is a string.
func IsString(v Value) bool {
	_, ok := unwrapValue(v).(*values.String)
	return ok
}

// IsNumber returns true if v is a number.
func IsNumber(v Value) bool {
	_, ok := unwrapValue(v).(values.Number)
	return ok
}

// IsBoolean returns true if v is a boolean.
func IsBoolean(v Value) bool {
	_, ok := unwrapValue(v).(*values.Boolean)
	return ok
}

// IsProcedure returns true if v is a callable procedure
// (lambda, foreign closure, case-lambda, parameter, or composable continuation).
func IsProcedure(v Value) bool {
	_, ok := unwrapValue(v).(values.Callable)
	return ok
}

// Car returns the car of a pair or other Tuple type. Returns (value, true)
// on success, or (nil, false) if v is not a non-empty Tuple.
func Car(v Value) (Value, bool) {
	t, ok := unwrapValue(v).(values.Tuple)
	if !ok || t.IsEmptyList() {
		return nil, false
	}
	return wrapValue(t.Car()), true
}

// Cdr returns the cdr of a pair or other Tuple type. Returns (value, true)
// on success, or (nil, false) if v is not a non-empty Tuple.
func Cdr(v Value) (Value, bool) {
	t, ok := unwrapValue(v).(values.Tuple)
	if !ok || t.IsEmptyList() {
		return nil, false
	}
	return wrapValue(t.Cdr()), true
}

// ToSlice converts a proper list to a Go slice.
// Returns (slice, true) on success, or (nil, false) if v is not a proper list.
func ToSlice(ctx context.Context, v Value) ([]Value, bool) {
	inner := unwrapValue(v)
	if !values.IsList(inner) {
		return nil, false
	}
	var result []Value
	_, err := values.ForEach(ctx, inner, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		result = append(result, wrapValue(elem))
		return nil
	})
	if err != nil {
		return nil, false
	}
	return result, true
}

// ToGoString extracts the Go string from a Scheme string value.
// Returns ("", false) if v is not a string.
func ToGoString(v Value) (string, bool) {
	s, ok := unwrapValue(v).(*values.String)
	if !ok {
		return "", false
	}
	return s.Value, true
}

// ToGoInt extracts an int64 from an exact integer value.
// Returns (0, false) if v is not an exact integer or does not fit in int64.
func ToGoInt(v Value) (int64, bool) {
	return values.ExactInteger(unwrapValue(v))
}

// ToGoFloat extracts a float64 from an inexact real value.
// Returns (0, false) if v is not a Float.
func ToGoFloat(v Value) (float64, bool) {
	f, ok := unwrapValue(v).(*values.Float)
	if !ok {
		return 0, false
	}
	return f.Value, true
}

// ToGoBool extracts a Go bool from a Scheme boolean value.
// Returns (false, false) if v is not a boolean.
func ToGoBool(v Value) (bool, bool) {
	b, ok := unwrapValue(v).(*values.Boolean)
	if !ok {
		return false, false
	}
	return b.Value, true
}
