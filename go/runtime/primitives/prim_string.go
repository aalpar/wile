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
	"wile/values"
)

// PrimString implements the string primitive.
// (string char ...) returns a newly allocated string composed of the given characters.
func PrimString(_ context.Context, mc *machine.MachineContext) error {
	args := mc.Arg(0)

	var sb strings.Builder
	for args != values.EmptyList {
		pair, ok := args.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string: expected a list of characters")
		}
		ch, ok := pair.Car().(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "string: expected a character but got %T", pair.Car())
		}
		sb.WriteRune(ch.Value)
		args = pair.Cdr()
	}

	mc.SetValue(values.NewString(sb.String()))
	return nil
}
