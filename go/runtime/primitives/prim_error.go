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

// PrimError implements the error primitive.
// (error message irritant ...)
// Creates an error object with the given message and irritants, then raises it.
func PrimError(_ context.Context, mc *machine.MachineContext) error {
	message := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	irritantsList := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	msgStr, ok := message.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString,
			"error: message must be a string but got %T", message)
	}

	// Convert irritants list to slice
	var irritants []values.Value
	if _, err := values.ForEach(nil, irritantsList, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		irritants = append(irritants, v)
		return nil
	}); err != nil {
		return values.WrapForeignErrorf(err, "error: invalid irritants list")
	}

	// Create error object and raise it
	errObj := values.NewErrorObject(msgStr.Datum(), irritants...)

	return &machine.ErrExceptionEscape{
		Condition:   errObj,
		Continuable: false,
		Handled:     false,
	}
}
