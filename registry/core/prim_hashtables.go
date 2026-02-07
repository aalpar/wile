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
	"github.com/aalpar/wile/values"
)

// PrimMakeHashtable implements the make-hashtable primitive.
// Creates a new empty hash table.
func PrimMakeHashtable(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewEmptyHashtable())
	return nil
}

// PrimHashtableQ implements the hashtable? predicate.
// Returns #t if the argument is a hash table, #f otherwise.
func PrimHashtableQ(_ context.Context, mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(*values.Hashtable)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimHashtableRef implements the hashtable-ref primitive.
// (hashtable-ref ht key) — errors if key is missing.
// (hashtable-ref ht key default) — returns default if key is missing.
func PrimHashtableRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-ref: expected a hashtable but got %s", o.SchemeString())
	}
	key := mc.Arg(1)
	rest := mc.Arg(2)

	val, found, err := ht.Get(key)
	if err != nil {
		return err
	}
	if found {
		mc.SetValue(val)
		return nil
	}

	// Check for optional default value
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrInvalidArgument, "hashtable-ref: improper argument list")
		}
		mc.SetValue(tuple.Car())
		return nil
	}

	return values.NewForeignErrorf("hashtable-ref: key not found: %s", key.SchemeString())
}

// PrimHashtableSet implements the hashtable-set! primitive.
// (hashtable-set! ht key value)
func PrimHashtableSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-set!: expected a hashtable but got %s", o.SchemeString())
	}
	err := ht.Set(mc.Arg(1), mc.Arg(2))
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimHashtableDelete implements the hashtable-delete! primitive.
// (hashtable-delete! ht key)
func PrimHashtableDelete(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-delete!: expected a hashtable but got %s", o.SchemeString())
	}
	err := ht.Delete(mc.Arg(1))
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimHashtableKeys implements the hashtable-keys primitive.
// Returns a list of all keys in the hash table.
func PrimHashtableKeys(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-keys: expected a hashtable but got %s", o.SchemeString())
	}
	mc.SetValue(ht.Keys())
	return nil
}

// PrimHashtableValues implements the hashtable-values primitive.
// Returns a list of all values in the hash table.
func PrimHashtableValues(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-values: expected a hashtable but got %s", o.SchemeString())
	}
	mc.SetValue(ht.Values())
	return nil
}

// PrimHashtableSize implements the hashtable-size primitive.
// Returns the number of entries in the hash table.
func PrimHashtableSize(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-size: expected a hashtable but got %s", o.SchemeString())
	}
	mc.SetValue(values.NewInteger(int64(ht.Size())))
	return nil
}

// PrimHashtableCopy implements the hashtable-copy primitive.
// Returns a shallow copy of the hash table.
func PrimHashtableCopy(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-copy: expected a hashtable but got %s", o.SchemeString())
	}
	mc.SetValue(ht.Copy())
	return nil
}

// PrimHashtableClear implements the hashtable-clear! primitive.
// Removes all entries from the hash table.
func PrimHashtableClear(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ht, ok := o.(*values.Hashtable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAHashtable, "hashtable-clear!: expected a hashtable but got %s", o.SchemeString())
	}
	ht.Clear()
	mc.SetValue(values.Void)
	return nil
}
