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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimCons implements the cons primitive.
// Creates a new pair from the car and cdr arguments.
func PrimCons(mc machine.CallContext) error {
	car := mc.Arg(0)
	cdr := mc.Arg(1)
	mc.SetValue(values.NewCons(car, cdr))
	return nil
}

// PrimCar implements the car primitive.
// Returns the first element of a pair.
//
// R7RS §6.4: It is an error to take the car of the empty list.
func PrimCar(mc machine.CallContext) error {
	v, err := helpers.RequireArg[values.Tuple](mc, 0, werr.ErrNotAPair, "a pair", "car")
	if err != nil {
		return err
	}
	if v.IsEmptyList() {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "car: cannot take car of empty list")
	}
	mc.SetValue(v.Car())
	return nil
}

// PrimCdr implements the cdr primitive.
// Returns the second element of a pair.
//
// R7RS §6.4: It is an error to take the cdr of the empty list.
func PrimCdr(mc machine.CallContext) error {
	v, err := helpers.RequireArg[values.Tuple](mc, 0, werr.ErrNotAPair, "a pair", "cdr")
	if err != nil {
		return err
	}
	if v.IsEmptyList() {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "cdr: cannot take cdr of empty list")
	}
	mc.SetValue(v.Cdr())
	return nil
}

// PrimSetCar implements the set-car! primitive.
func PrimSetCar(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.Pair](mc, 0, werr.ErrNotAPair, "a pair", "set-car!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)
	p.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimSetCdr implements the set-cdr! primitive.
func PrimSetCdr(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.Pair](mc, 0, werr.ErrNotAPair, "a pair", "set-cdr!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)
	p.SetCdr(val)
	mc.SetValue(values.Void)
	return nil
}
