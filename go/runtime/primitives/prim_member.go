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

// PrimMember implements the member primitive.
// Finds an element in a list using equal? for comparison.
func PrimMember(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	lst := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "member: expected a list but got %T", lst)
		}
		if values.EqualTo(pr.Car(), obj) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}
