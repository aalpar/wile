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

// PrimStringAppend implements the (string-append) primitive.
// Concatenates strings.
func PrimStringAppend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewString(""))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "string-append: expected a list but got %T", o)
	}
	var sb strings.Builder
	v, err := pr.ForEach(nil, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		s, ok := v.(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "string-append: expected a string but got %T", v)
		}
		sb.WriteString(s.Value)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "string-append: not a proper list")
	}
	mc.SetValue(values.NewString(sb.String()))
	return nil
}
