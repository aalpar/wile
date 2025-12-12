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

// PrimErrorObjectMessage implements the error-object-message accessor.
// Returns the message string from an error object.
func PrimErrorObjectMessage(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	errObj, ok := obj.(*values.ErrorObject)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnErrorObject,
			"error-object-message: expected error object but got %T", obj)
	}
	mc.SetValue(errObj.Message())
	return nil
}
