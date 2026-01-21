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

// PrimRaiseContinuable implements the raise-continuable primitive.
// (raise-continuable obj)
// Raises a continuable exception with obj as the condition.
// If the handler returns, its return value becomes the value of raise-continuable,
// and execution continues from the call site per R7RS §6.11.
func PrimRaiseContinuable(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)

	// Copy continuation to prevent mutation issues during handler execution.
	// This follows the pattern established by call/cc.
	cont := mc.Parent()
	if cont != nil {
		cont = cont.Copy()
	}

	return &machine.ErrExceptionEscape{
		Condition:    obj,
		Continuable:  true,
		Continuation: cont,
		Handled:      false,
	}
}
