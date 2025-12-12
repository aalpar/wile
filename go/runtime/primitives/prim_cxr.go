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

// cxrHelper applies a sequence of car/cdr operations to a value.
// The ops string contains 'a' for car and 'd' for cdr, applied right-to-left.
// For example, "ad" means (car (cdr x)), i.e., cadr.
func cxrHelper(name string, ops string, o values.Value) (values.Value, error) {
	v := o
	// Apply operations right-to-left (innermost first)
	for i := len(ops) - 1; i >= 0; i-- {
		p, ok := v.(*values.Pair)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, v)
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

func PrimCaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caar", "aa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cadr", "ad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdar", "da", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cddr", "dd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// 3-level CxR accessors (caaar through cdddr)

func PrimCaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caaar", "aaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caadr", "aad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cadar", "ada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caddr", "add", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdaar", "daa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdadr", "dad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cddar", "dda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdddr", "ddd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

// 4-level CxR accessors (caaaar through cddddr)

func PrimCaaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caaaar", "aaaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caaadr", "aaad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caadar", "aada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caaddr", "aadd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCadaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cadaar", "adaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCadadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cadadr", "adad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCaddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("caddar", "adda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCadddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cadddr", "addd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdaaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdaaar", "daaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdaadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdaadr", "daad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdadar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdadar", "dada", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdaddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdaddr", "dadd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCddaar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cddaar", "ddaa", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCddadr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cddadr", "ddad", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCdddar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cdddar", "ddda", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}

func PrimCddddr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, err := cxrHelper("cddddr", "dddd", o)
	if err != nil {
		return err
	}
	mc.SetValue(v)
	return nil
}
