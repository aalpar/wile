// Copyright 2026 Aaron Alpar
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

package helpers

import (
	"context"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// MakeTypePredicate creates a type predicate primitive function.
// The check function should return true if the value matches the expected type.
func MakeTypePredicate(check func(values.Value) bool) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		o := mc.Arg(0)
		mc.SetValue(schemeutil.BoolToBoolean(check(o)))
		return nil
	}
}
