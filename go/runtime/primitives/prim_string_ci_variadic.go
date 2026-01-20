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
	"strings"

	"wile/machine"
)

// PrimStringCiEqVariadic implements the variadic string-ci=? primitive.
// Case-insensitive string equality comparison with 2+ arguments.
func PrimStringCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci=?", strings.EqualFold)
}

// PrimStringCiLtVariadic implements the variadic string-ci<? primitive.
// Case-insensitive string less-than comparison with 2+ arguments.
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
		return strings.ToLower(a) < strings.ToLower(b)
	})
}

// PrimStringCiGtVariadic implements the variadic string-ci>? primitive.
// Case-insensitive string greater-than comparison with 2+ arguments.
func PrimStringCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci>?", func(a, b string) bool {
		return strings.ToLower(a) > strings.ToLower(b)
	})
}

// PrimStringCiLeVariadic implements the variadic string-ci<=? primitive.
// Case-insensitive string less-than-or-equal comparison with 2+ arguments.
func PrimStringCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci<=?", func(a, b string) bool {
		return strings.ToLower(a) <= strings.ToLower(b)
	})
}

// PrimStringCiGeVariadic implements the variadic string-ci>=? primitive.
// Case-insensitive string greater-than-or-equal comparison with 2+ arguments.
func PrimStringCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci>=?", func(a, b string) bool {
		return strings.ToLower(a) >= strings.ToLower(b)
	})
}
