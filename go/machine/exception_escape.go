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

package machine

import (
	"fmt"

	"wile/values"
)

// ErrExceptionEscape signals an exception being raised through the call stack.
// It is used by raise and raise-continuable to propagate exceptions to handlers.
type ErrExceptionEscape struct {
	Condition    values.Value         // The raised condition/object
	Continuable  bool                 // Whether handler can return
	Continuation *MachineContinuation // Return point for continuable exceptions
	Handled      bool                 // Set true after handler processes it
}

// Error implements the error interface.
func (e *ErrExceptionEscape) Error() string {
	if e.Condition == nil {
		return "exception: <nil>"
	}
	return fmt.Sprintf("exception: %s", e.Condition.SchemeString())
}
