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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimMakeHashtable implements the make-hashtable primitive.
// Creates a new empty hash table.
func PrimMakeHashtable(mc machine.CallContext) error {
	mc.SetValue(values.NewEmptyHashtable())
	return nil
}

// makeHashtableConstructor returns the primitive for a fixed-kind R6RS
// constructor. All three accept an optional size hint k, which Wile ignores: the
// backing sync.Map has no capacity knob, and R6RS calls k a hint implementations
// are free to ignore.
func makeHashtableConstructor(kind values.HashtableKind) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		mc.SetValue(values.NewHashtable(kind))
		return nil
	}
}

// PrimMakeEqHashtable implements (make-eq-hashtable [k]).
var PrimMakeEqHashtable = makeHashtableConstructor(values.HashtableEq)

// PrimMakeEqvHashtable implements (make-eqv-hashtable [k]).
var PrimMakeEqvHashtable = makeHashtableConstructor(values.HashtableEqv)

// PrimMakeEqualHashtable implements (make-equal-hashtable [k]). NOT R6RS — it is
// the Chez / Larceny / Vicare / Ypsilon extension, kept because it is what
// portable-ish Scheme writes for the common case and because the R6RS spelling
// (make-hashtable equal-hash equal?) is four tokens longer at 15 call sites.
var PrimMakeEqualHashtable = makeHashtableConstructor(values.HashtableEqual)

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

	val, found := ht.Get(key)
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

// PrimHashtableCopy implements R6RS (hashtable-copy ht [mutable]).
//
// THE SILENT SEMANTIC TRAP OF THIS MIGRATION: with the second argument ABSENT
// the copy is IMMUTABLE, per R6RS. Wile's previous one-argument hashtable-copy
// returned a mutable table, so existing code that copies and then mutates must
// pass #t.
//
// It no longer fits MakeUnaryAccessor, which has no optional-argument slot.
func PrimHashtableCopy(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-copy")
	if err != nil {
		return err
	}
	flag, ok, err := helpers.ParseOptionalArg(mc.Arg(1), "hashtable-copy")
	if err != nil {
		return err
	}
	// Scheme truthiness: everything but #f. FalseValue is a singleton, so the
	// interface compare is the house idiom (prim_syntax.go, prim_prompt.go).
	mutable := false
	if ok {
		mutable = flag != values.FalseValue
	}
	mc.SetValue(ht.Copy(mutable))
	return nil
}

// PrimHashtableMutableQ implements R6RS hashtable-mutable?.
var PrimHashtableMutableQ = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-mutable?", func(ht *values.Hashtable) values.Value {
	return values.BoolToBoolean(ht.Mutable())
})

// PrimHashtableClear implements the hashtable-clear! primitive.
//
// No longer a MakeUnarySideEffect: it has to propagate checkMutable's error.
func PrimHashtableClear(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-clear!")
	if err != nil {
		return err
	}
	err = ht.Clear()
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimHashtableContainsQ implements R6RS hashtable-contains?. It wraps HasKey,
// which had no caller at all before this surface existed.
func PrimHashtableContainsQ(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-contains?")
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(ht.HasKey(mc.Arg(1))))
	return nil
}

// PrimHashtableEntries implements R6RS hashtable-entries, which returns TWO
// values: a vector of keys and an index-aligned vector of values.
func PrimHashtableEntries(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-entries")
	if err != nil {
		return err
	}
	ks, vs := ht.EntriesVectors()
	mc.SetValues(ks, vs)
	return nil
}

// equivNames maps a table's kind to the primitive whose procedure object
// hashtable-equivalence-function must hand back. Indexed by HashtableKind, so a
// new kind that forgets a row is an out-of-range panic at the first call rather
// than a silently wrong answer.
var equivNames = [...]string{
	values.HashtableEqual: "equal?",
	values.HashtableEq:    "eq?",
	values.HashtableEqv:   "eqv?",
}

// PrimHashtableEquivalenceFunction implements R6RS hashtable-equivalence-function.
// The returned procedure is THIS namespace's registered closure, so
// (eq? (hashtable-equivalence-function h) equal?) holds — the same identity
// relation make-hashtable recognizes on the way in. The two directions must stay
// in agreement.
func PrimHashtableEquivalenceFunction(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-equivalence-function")
	if err != nil {
		return err
	}
	return setSealedPrimitive(mc, equivNames[ht.Kind()], "hashtable-equivalence-function")
}

// PrimHashtableHashFunction implements R6RS hashtable-hash-function, which returns
// #f — not a procedure — for eq and eqv tables. Those hash by identity, and R6RS
// does not expose that as a procedure. Chibi reaches the same two answers through
// the same discriminant (lib/srfi/69/interface.scm).
func PrimHashtableHashFunction(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-hash-function")
	if err != nil {
		return err
	}
	if ht.Kind() != values.HashtableEqual {
		mc.SetValue(values.FalseValue)
		return nil
	}
	return setSealedPrimitive(mc, "equal-hash", "hashtable-hash-function")
}

// setSealedPrimitive sets name's registered closure as the result, or raises if this
// namespace has no such binding. Raising rather than returning #f or nil is the point:
// a profile that ships these accessors without equal-hash is misconfigured, and a
// silent #f would be indistinguishable from the legitimate eq/eqv answer above.
func setSealedPrimitive(mc machine.CallContext, name, site string) error {
	q := sealedPrimitive(mc.EnvironmentFrame().Namespace(), name)
	if q == nil {
		return werr.WrapForeignErrorf(werr.ErrUnexpectedNil,
			"%s: %s is not registered in this namespace", site, name)
	}
	mc.SetValue(q)
	return nil
}

// sealedPrimitive returns the closure the registry defined for name in ns's sealed
// base, or nil if the name is unbound there. nil means NONE — the caller refuses
// rather than falling back, so a namespace built without the primitive raises
// instead of silently accepting whatever the caller passed.
//
// Empty scopes resolve the ambient (unscoped) registration, the same query
// registerPhasePrimitive uses to address the binding it just wrote. syntax.ScopeSet
// is a type alias for values.ScopeSet, so no conversion is needed.
func sealedPrimitive(ns *environment.Namespace, name string) values.Value {
	if ns == nil {
		return nil
	}
	base := ns.SealedBase()
	if base == nil {
		return nil
	}
	binding := base.GetBinding(values.NewSymbol(name), values.EmptyScopes())
	if binding == nil {
		return nil
	}
	return binding.Value()
}
