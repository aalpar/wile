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

// PrimMakeHashtable implements the make-hashtable primitive.
// Creates a new empty hash table.
func PrimMakeHashtable(mc machine.CallContext) error {
	mc.SetValue(values.NewEmptyHashtable())
	return nil
}

// PrimHashtableQ implements the hashtable? predicate.
// Returns #t if the argument is a hash table, #f otherwise.
var PrimHashtableQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Hashtable)
	return ok
})

// PrimHashtableRef implements the hashtable-ref primitive.
// (hashtable-ref ht key) — errors if key is missing.
// (hashtable-ref ht key default) — returns default if key is missing.
func PrimHashtableRef(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-ref")
	if err != nil {
		return err
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
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "hashtable-ref: improper argument list")
		}
		mc.SetValue(tuple.Car())
		return nil
	}

	return werr.WrapForeignErrorf(werr.ErrHashtableKeyNotFound, "hashtable-ref: key not found: %s", key.SchemeString())
}

// PrimHashtableSet implements the hashtable-set! primitive.
// (hashtable-set! ht key value)
func PrimHashtableSet(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-set!")
	if err != nil {
		return err
	}
	err = ht.Set(mc.Arg(1), mc.Arg(2))
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimHashtableDelete implements the hashtable-delete! primitive.
// (hashtable-delete! ht key)
func PrimHashtableDelete(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-delete!")
	if err != nil {
		return err
	}
	err = ht.Delete(mc.Arg(1))
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimHashtableKeys implements the hashtable-keys primitive.
// Returns a list of all keys in the hash table.
var PrimHashtableKeys = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-keys", func(ht *values.Hashtable) values.Value {
	return ht.Keys()
})

// PrimHashtableValues implements the hashtable-values primitive.
// Returns a list of all values in the hash table.
var PrimHashtableValues = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-values", func(ht *values.Hashtable) values.Value {
	return ht.Values()
})

// PrimHashtableSize implements the hashtable-size primitive.
// Returns the number of entries in the hash table.
var PrimHashtableSize = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-size", func(ht *values.Hashtable) values.Value {
	return values.NewInteger(int64(ht.Size()))
})

// PrimHashtableCopy implements the hashtable-copy primitive.
// Returns a shallow copy of the hash table.
var PrimHashtableCopy = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-copy", func(ht *values.Hashtable) values.Value {
	return ht.Copy()
})

// PrimHashtableClear implements the hashtable-clear! primitive.
// Removes all entries from the hash table.
var PrimHashtableClear = helpers.MakeUnarySideEffect(werr.ErrNotAHashtable, "hashtable-clear!", func(ht *values.Hashtable) {
	ht.Clear()
})
