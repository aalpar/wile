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
	"wile/utils"
	"wile/values"
)

func makeTypePredicate(check func(values.Value) bool) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
		o := mc.Arg(0)
		mc.SetValue(utils.BoolToBoolean(check(o)))
		return nil
	}
}

var PrimBooleanQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Boolean)
	return ok
})

var PrimStringQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.String)
	return ok
})

var PrimSymbolQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Symbol)
	return ok
})

var PrimVectorQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Vector)
	return ok
})

var PrimCharQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Character)
	return ok
})

var PrimPromiseQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Promise)
	return ok
})

var PrimParameterQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.Parameter)
	return ok
})

var PrimNumberQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Number)
	return ok
})

var PrimComplexQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(values.Number)
	return ok
})

var PrimBytevectorQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.ByteVector)
	return ok
})

var PrimProcedureQ = makeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.MachineClosure)
	return ok
})
