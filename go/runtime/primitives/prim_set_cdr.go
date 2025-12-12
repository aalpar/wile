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
	"wile/values"
)

func PrimSetCdr(_ context.Context, mc *machine.MachineContext) error {
	pair := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	p, ok := pair.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "set-cdr!: expected a pair but got %T", pair)
	}
	if p.IsEmptyList() {
		return values.NewForeignError("set-cdr!: cannot modify empty list")
	}
	p.SetCdr(val)
	mc.SetValue(values.Void)
	return nil
}
