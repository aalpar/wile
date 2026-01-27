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

	"wile/machine"
	"wile/values"
)

// PrimCons implements the cons primitive.
// Creates a new pair from the car and cdr arguments.
func PrimCons(_ context.Context, mc *machine.MachineContext) error {
	car := mc.Arg(0)
	cdr := mc.Arg(1)
	mc.SetValue(values.NewCons(car, cdr))
	return nil
}

// PrimCar implements the car primitive.
// Returns the first element of a pair.
//
// R7RS §6.4: It is an error to take the car of the empty list.
func PrimCar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "car: expected a pair but got %T", o)
	}
	if v.IsEmptyList() {
		return values.NewForeignError("car: cannot take car of empty list")
	}
	mc.SetValue(v.Car())
	return nil
}

// PrimCdr implements the cdr primitive.
// Returns the second element of a pair.
//
// R7RS §6.4: It is an error to take the cdr of the empty list.
func PrimCdr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cdr: expected a pair but got %T", o)
	}
	if v.IsEmptyList() {
		return values.NewForeignError("cdr: cannot take cdr of empty list")
	}
	mc.SetValue(v.Cdr())
	return nil
}

// PrimSetCar implements the set-car! primitive.
func PrimSetCar(_ context.Context, mc *machine.MachineContext) error {
	pair := mc.Arg(0)
	val := mc.Arg(1)
	p, ok := pair.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "set-car!: expected a pair but got %T", pair)
	}
	if p.IsEmptyList() {
		return values.NewForeignError("set-car!: cannot modify empty list")
	}
	p.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimSetCdr implements the set-cdr! primitive.
func PrimSetCdr(_ context.Context, mc *machine.MachineContext) error {
	pair := mc.Arg(0)
	val := mc.Arg(1)
	p, ok := pair.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "set-cdr!: expected a pair but got %T", pair)
	}
	if p.IsEmptyList() {
		return values.NewForeignError("set-cdr!: cannot modify empty list")
	}
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
		p, ok := v.(*values.Pair)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, v)
		}
		if p.IsEmptyList() {
			return nil, values.NewForeignError(name + ": cannot take car/cdr of empty list")
		}
		if ops[i] == 'a' {
			v = p.Car()
		} else {
			v = p.Cdr()
		}
	}
	return v, nil
}

// 2-level CxR accessors (caar through cddr)

// PrimCaar implements the caar primitive.
func PrimCaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caar", "aa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCadr implements the cadr primitive.
func PrimCadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cadr", "ad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdar implements the cdar primitive.
func PrimCdar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdar", "da", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCddr implements the cddr primitive.
func PrimCddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cddr", "dd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// 3-level CxR accessors (caaar through cdddr)

// PrimCaaar implements the caaar primitive.
func PrimCaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caaar", "aaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaadr implements the caadr primitive.
func PrimCaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caadr", "aad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCadar implements the cadar primitive.
func PrimCadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cadar", "ada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaddr implements the caddr primitive.
func PrimCaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caddr", "add", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdaar implements the cdaar primitive.
func PrimCdaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdaar", "daa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdadr implements the cdadr primitive.
func PrimCdadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdadr", "dad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCddar implements the cddar primitive.
func PrimCddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cddar", "dda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdddr implements the cdddr primitive.
func PrimCdddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdddr", "ddd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// 4-level CxR accessors (caaaar through cddddr)

// PrimCaaaar implements the caaaar primitive.
func PrimCaaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caaaar", "aaaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaaadr implements the caaadr primitive.
func PrimCaaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caaadr", "aaad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaadar implements the caadar primitive.
func PrimCaadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caadar", "aada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaaddr implements the caaddr primitive.
func PrimCaaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caaddr", "aadd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCadaar implements the cadaar primitive.
func PrimCadaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cadaar", "adaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCadadr implements the cadadr primitive.
func PrimCadadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cadadr", "adad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCaddar implements the caddar primitive.
func PrimCaddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("caddar", "adda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCadddr implements the cadddr primitive.
func PrimCadddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cadddr", "addd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdaaar implements the cdaaar primitive.
func PrimCdaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdaaar", "daaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdaadr implements the cdaadr primitive.
func PrimCdaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdaadr", "daad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdadar implements the cdadar primitive.
func PrimCdadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdadar", "dada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdaddr implements the cdaddr primitive.
func PrimCdaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdaddr", "dadd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCddaar implements the cddaar primitive.
func PrimCddaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cddaar", "ddaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCddadr implements the cddadr primitive.
func PrimCddadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cddadr", "ddad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCdddar implements the cdddar primitive.
func PrimCdddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cdddar", "ddda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// PrimCddddr implements the cddddr primitive.
func PrimCddddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, err := cxrHelper("cddddr", "dddd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}
