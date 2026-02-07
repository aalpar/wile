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

package core

import (
	"context"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimBox implements the box primitive.
// Creates a new box containing the given value.
func PrimBox(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewBox(mc.Arg(0)))
	return nil
}

// PrimBoxQ implements the box? predicate.
// Returns #t if the argument is a box, #f otherwise.
func PrimBoxQ(_ context.Context, mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(*values.Box)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimUnbox implements the unbox primitive.
// Returns the value contained in a box.
func PrimUnbox(_ context.Context, mc *machine.MachineContext) error {
	b, err := helpers.RequireArg[*values.Box](mc, 0, values.ErrNotABox, "unbox")
	if err != nil {
		return err
	}
	mc.SetValue(b.Unbox())
	return nil
}

// PrimSetBox implements the set-box! primitive.
// Sets the value contained in a box.
func PrimSetBox(_ context.Context, mc *machine.MachineContext) error {
	b, err := helpers.RequireArg[*values.Box](mc, 0, values.ErrNotABox, "set-box!")
	if err != nil {
		return err
	}
	b.Value = mc.Arg(1)
	mc.SetValue(values.Void)
	return nil
}
