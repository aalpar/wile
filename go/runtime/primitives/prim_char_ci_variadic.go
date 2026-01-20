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

package primitives

import (
	"context"
	"unicode"

	"wile/machine"
)

// PrimCharCiEqVariadic implements the variadic char-ci=? primitive.
// Case-insensitive character equality comparison with 2+ arguments.
func PrimCharCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci=?", func(a, b rune) bool {
		return unicode.ToLower(a) == unicode.ToLower(b)
	})
}

// PrimCharCiLtVariadic implements the variadic char-ci<? primitive.
// Case-insensitive character less-than comparison with 2+ arguments.
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
		return unicode.ToLower(a) < unicode.ToLower(b)
	})
}

// PrimCharCiGtVariadic implements the variadic char-ci>? primitive.
// Case-insensitive character greater-than comparison with 2+ arguments.
func PrimCharCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>?", func(a, b rune) bool {
		return unicode.ToLower(a) > unicode.ToLower(b)
	})
}

// PrimCharCiLeVariadic implements the variadic char-ci<=? primitive.
// Case-insensitive character less-than-or-equal comparison with 2+ arguments.
func PrimCharCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<=?", func(a, b rune) bool {
		return unicode.ToLower(a) <= unicode.ToLower(b)
	})
}

// PrimCharCiGeVariadic implements the variadic char-ci>=? primitive.
// Case-insensitive character greater-than-or-equal comparison with 2+ arguments.
func PrimCharCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>=?", func(a, b rune) bool {
		return unicode.ToLower(a) >= unicode.ToLower(b)
	})
}
