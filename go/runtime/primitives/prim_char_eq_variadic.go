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

	"wile/machine"
)

// PrimCharEqVariadic implements the variadic char=? primitive.
func PrimCharEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char=?", func(a, b rune) bool { return a == b })
}

// PrimCharLtVariadic implements the variadic char<? primitive.
func PrimCharLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char<?", func(a, b rune) bool { return a < b })
}

// PrimCharGtVariadic implements the variadic char>? primitive.
func PrimCharGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char>?", func(a, b rune) bool { return a > b })
}

// PrimCharLeVariadic implements the variadic char<=? primitive.
func PrimCharLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char<=?", func(a, b rune) bool { return a <= b })
}

// PrimCharGeVariadic implements the variadic char>=? primitive.
func PrimCharGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char>=?", func(a, b rune) bool { return a >= b })
}
