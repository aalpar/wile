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
func PrimCons(mc *machine.MachineContext) error {
	car := mc.Arg(0)
	cdr := mc.Arg(1)
	mc.SetValue(values.NewCons(car, cdr))
	return nil
}

// PrimCar implements the car primitive.
// Returns the first element of a pair.
//
// R7RS §6.4: It is an error to take the car of the empty list.
func PrimCar(mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[values.Tuple](mc, 0, werr.ErrNotAPair, "car")
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
func PrimCdr(mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[values.Tuple](mc, 0, werr.ErrNotAPair, "cdr")
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
func PrimSetCar(mc *machine.MachineContext) error {
	p, err := helpers.RequireArg[*values.Pair](mc, 0, werr.ErrNotAPair, "set-car!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)
	p.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimSetCdr implements the set-cdr! primitive.
func PrimSetCdr(mc *machine.MachineContext) error {
	p, err := helpers.RequireArg[*values.Pair](mc, 0, werr.ErrNotAPair, "set-cdr!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)
	p.SetCdr(val)
	mc.SetValue(values.Void)
	return nil
}

// cxrHelper applies a sequence of car/cdr operations to a value.
// The ops string contains 'a' for car and 'd' for cdr, applied right-to-left.
// For example, "ad" means (car (cdr x)), i.e., cadr.
//
// R7RS §6.4: It is an error to take car/cdr of the empty list.
func cxrHelper(name string, ops string, o values.Value) (values.Value, error) {
	v := o
	// Apply operations right-to-left (innermost first)
	for i := len(ops) - 1; i >= 0; i-- {
		p, ok := v.(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAPair, "%s: expected a pair but got %T", name, v)
		}
		if p.IsEmptyList() {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAPair, "%s: cannot take car/cdr of empty list", name)
		}
		if ops[i] == 'a' {
			v = p.Car()
		} else {
			v = p.Cdr()
		}
	}
	return v, nil
}

// cxrSpecs defines all 28 CxR accessor combinations (2/3/4-level).
// Each entry maps a Scheme name to the car/cdr operation sequence used by cxrHelper.
//
// R7RS §6.4: These procedures are compositions of car and cdr.
var cxrSpecs = []struct{ name, ops string }{
	// 2-level
	{"caar", "aa"}, {"cadr", "ad"}, {"cdar", "da"}, {"cddr", "dd"},
	// 3-level
	{"caaar", "aaa"}, {"caadr", "aad"}, {"cadar", "ada"}, {"caddr", "add"},
	{"cdaar", "daa"}, {"cdadr", "dad"}, {"cddar", "dda"}, {"cdddr", "ddd"},
	// 4-level
	{"caaaar", "aaaa"}, {"caaadr", "aaad"}, {"caadar", "aada"}, {"caaddr", "aadd"},
	{"cadaar", "adaa"}, {"cadadr", "adad"}, {"caddar", "adda"}, {"cadddr", "addd"},
	{"cdaaar", "daaa"}, {"cdaadr", "daad"}, {"cdadar", "dada"}, {"cdaddr", "dadd"},
	{"cddaar", "ddaa"}, {"cddadr", "ddad"}, {"cdddar", "ddda"}, {"cddddr", "dddd"},
}

// makeCxrPrimitive returns a ForeignFunction that applies the given car/cdr
// operation sequence to its argument.
func makeCxrPrimitive(name, ops string) machine.ForeignFunction {
	return func(mc *machine.MachineContext) error {
		v, err := cxrHelper(name, ops, mc.Arg(0))
		if err != nil {
			return err
		}
		mc.SetValue(v)
		return nil
	}
}
