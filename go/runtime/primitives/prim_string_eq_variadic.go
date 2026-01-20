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

// PrimStringEqVariadic implements the variadic string=? primitive.
func PrimStringEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string=?", func(a, b string) bool { return a == b })
}

// PrimStringLtVariadic implements the variadic string<? primitive.
func PrimStringLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string<?", func(a, b string) bool { return a < b })
}

// PrimStringGtVariadic implements the variadic string>? primitive.
func PrimStringGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string>?", func(a, b string) bool { return a > b })
}

// PrimStringLeVariadic implements the variadic string<=? primitive.
func PrimStringLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string<=?", func(a, b string) bool { return a <= b })
}

// PrimStringGeVariadic implements the variadic string>=? primitive.
func PrimStringGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string>=?", func(a, b string) bool { return a >= b })
}
