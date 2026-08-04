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

// PrimMakeHashtable implements R6RS (make-hashtable hash equiv [k]).
//
// Only the (equal-hash, equal?) pair is recognized; everything else raises. That
// is the documented gap while user-supplied hash/equivalence procedures are
// deferred — see docs/reference/r7rs-differences.md.
//
// The pair is recognized by PRIMITIVE IDENTITY: each argument must be a closure
// the registry built from the equal-hash / equal? spec, whichever environment
// minted it. That is the question worth asking, and a CLOSURE POINTER compare —
// the earlier design — asked a narrower one that happened to coincide only while
// nothing had been imported. A library environment is a flat island, so the
// library env factory mints a second closure per primitive there; (scheme base)
// exports equal?, and after that import the caller held the library's copy while
// the sealed base still held the engine's. Recognition refused the pair, so every
// conforming R7RS program — which opens with that import — could not write
// (make-hashtable equal-hash equal?). See machine.PrimitiveIdentity.
//
// A registered-NAME compare was the original alternative and is still NOT used:
// SetName is called by whoever registers a primitive, so an embedder shipping its
// own equal-hash would match by string and have its procedure silently discarded.
// The identity token keeps that failing closed — it is minted once at package
// scope in core, cannot be constructed from Scheme, and a second token of the same
// name is a different primitive.
//
// This is NOT a Binding Identity violation, though it reads like one. That
// invariant forbids deciding two IDENTIFIERS denote the same variable by comparing
// SPELLINGS. Nothing is resolved by spelling here: two already-evaluated
// first-class procedure objects are compared by identity.
func PrimMakeHashtable(mc machine.CallContext) error {
	// machine.IdentityOf answers nil for a Scheme procedure, a primitive with no
	// declared identity, and a non-procedure alike, so comparing against a non-nil
	// package-scope token refuses all three without a separate type check.
	if machine.IdentityOf(mc.Arg(0)) != identityEqualHash || machine.IdentityOf(mc.Arg(1)) != identityEqualQ {
		return werr.WrapForeignErrorf(werr.ErrUnsupportedHashtableKind,
			"make-hashtable: only (make-hashtable equal-hash equal?) is supported; "+
				"use make-eq-hashtable, make-eqv-hashtable or make-equal-hashtable")
	}
	// R6RS caps make-hashtable at three arguments; the third is the ignored size
	// hint. ParamCount+IsVariadic alone would accept any number beyond it.
	_, _, err := helpers.ParseOptionalArg(mc.Arg(2), "make-hashtable")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewHashtable(values.HashtableEqual))
	return nil
}

// makeHashtableConstructor returns the primitive for a fixed-kind R6RS
// constructor. All three accept an optional size hint k, which Wile ignores: the
// backing sync.Map has no capacity knob, and R6RS calls k a hint implementations
// are free to ignore.
func makeHashtableConstructor(kind values.HashtableKind, name string) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		// k is optional, not unlimited: ParseOptionalArg rejects a second extra
		// argument. Without it ParamCount+IsVariadic means ">= 0" and every
		// surplus argument was swallowed in silence.
		_, _, err := helpers.ParseOptionalArg(mc.Arg(0), name)
		if err != nil {
			return err
		}
		mc.SetValue(values.NewHashtable(kind))
		return nil
	}
}

// PrimMakeEqHashtable implements (make-eq-hashtable [k]).
var PrimMakeEqHashtable = makeHashtableConstructor(values.HashtableEq, "make-eq-hashtable")

// PrimMakeEqvHashtable implements (make-eqv-hashtable [k]).
var PrimMakeEqvHashtable = makeHashtableConstructor(values.HashtableEqv, "make-eqv-hashtable")

// PrimMakeEqualHashtable implements (make-equal-hashtable [k]). NOT R6RS — it is
// the Chez / Larceny / Vicare / Ypsilon extension, kept because it is what
// portable-ish Scheme writes for the common case and because the R6RS spelling
// (make-hashtable equal-hash equal?) is four tokens longer at 15 call sites.
var PrimMakeEqualHashtable = makeHashtableConstructor(values.HashtableEqual, "make-equal-hashtable")

// PrimHashtableQ implements the hashtable? predicate.
// Returns #t if the argument is a hash table, #f otherwise.
var PrimHashtableQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Hashtable)
	return ok
})

// PrimHashtableRef implements R6RS (hashtable-ref ht key default).
//
// DEFAULT IS REQUIRED. R6RS has no two-argument form, so the missing-key raise
// and werr.ErrHashtableKeyNotFound with it are gone: an absent key now returns
// default, always.
func PrimHashtableRef(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-ref")
	if err != nil {
		return err
	}
	val, found := ht.Get(mc.Arg(1))
	if !found {
		val = mc.Arg(2)
	}
	mc.SetValue(val)
	return nil
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

// PrimHashtableKeys implements R6RS hashtable-keys, which returns a VECTOR.
// Wile previously returned a list.
//
// hashtable-values is gone: hashtable-entries subsumes it, returning keys and
// values together and index-aligned, which is the only way to pair them
// reliably.
var PrimHashtableKeys = helpers.MakeUnaryAccessor(werr.ErrNotAHashtable, "hashtable-keys", func(ht *values.Hashtable) values.Value {
	return ht.KeysVector()
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
	_, _, err = helpers.ParseOptionalArg(mc.Arg(1), "hashtable-clear!")
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

// equivIdentity maps a table's kind to the equivalence primitive
// hashtable-equivalence-function must hand back: its identity token, and the name
// under which the caller's own binding of it is looked for.
//
// A switch rather than an array indexed by HashtableKind. The array read
// `equivNames[ht.Kind()]` PANICS on an out-of-range kind, and values.NewHashtable
// is exported and validates nothing, so values.NewHashtable(HashtableKind(7))
// reached it. Recovering at the VM boundary turned that into an opaque
// ErrPanicRecovery. Worse, the panic was inconsistent: hashKey, keyEqual and
// HashtableKind.String all treat an unknown kind as equal via their own default
// arms, so the array was the one site claiming a policy the type did not have.
// Refusing explicitly is the policy that can actually be relied on.
func equivIdentity(kind values.HashtableKind) (*machine.PrimitiveIdentity, string, bool) {
	switch kind {
	case values.HashtableEqual:
		return identityEqualQ, "equal?", true
	case values.HashtableEq:
		return identityEqQ, "eq?", true
	case values.HashtableEqv:
		return identityEqvQ, "eqv?", true
	default:
		return nil, "", false
	}
}

// PrimHashtableEquivalenceFunction implements R6RS hashtable-equivalence-function.
// The returned procedure is whichever copy of the equivalence primitive the CALLER
// holds, so (eq? (hashtable-equivalence-function h) equal?) holds on both sides of
// an import — the same identity relation make-hashtable recognizes on the way in.
// The two directions must stay in agreement.
func PrimHashtableEquivalenceFunction(mc machine.CallContext) error {
	ht, err := helpers.RequireArg[*values.Hashtable](mc, 0, werr.ErrNotAHashtable, "hashtable-equivalence-function")
	if err != nil {
		return err
	}
	identity, name, ok := equivIdentity(ht.Kind())
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrUnsupportedHashtableKind,
			"hashtable-equivalence-function: table has unknown kind %d", uint8(ht.Kind()))
	}
	return setRecognizedPrimitive(mc, identity, name, "hashtable-equivalence-function")
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
	return setRecognizedPrimitive(mc, identityEqualHash, "equal-hash", "hashtable-hash-function")
}

// setRecognizedPrimitive sets the primitive bearing identity as the result,
// preferring the copy visible at the namespace's MUTABLE TOP LEVEL over the
// sealed base's, or raises if neither is that primitive.
//
// The top level, not the calling frame. mc.EnvironmentFrame() is parented at the
// frame that MINTED this closure, which for a primitive reached through an import
// is the exporting library's flat island — so consulting it would answer with that
// library's private copy, not with anything the program can name. MutableRuntime
// resolves through the namespace instead, which every island shares, and is where
// a top-level (import (scheme base)) installs its bindings. That is what makes the
// answer round-trip through eq?: after that import the program's equal? IS the
// (scheme base) copy, and the sealed base's would be an object it cannot recognize
// as the thing it just wrote.
//
// The sealed-base fallback then covers the un-imported (ambient) case, and keeps
// working where the mutable layer holds something else entirely.
//
// name only LOCATES a candidate; identity DECIDES. So a user (define (equal? a b)
// #t) shadowing in the mutable layer is found, fails the token check, and is
// discarded in favour of the sealed base — the same fail-closed direction
// make-hashtable takes, and the reason this is not a Binding Identity violation:
// nothing is concluded from the spelling.
//
// Raising when neither candidate matches rather than returning #f is the point: a
// profile that ships these accessors without equal-hash is misconfigured, and a
// silent #f would be indistinguishable from the legitimate eq/eqv answer above.
func setRecognizedPrimitive(mc machine.CallContext, identity *machine.PrimitiveIdentity, name, site string) error {
	sym := values.NewSymbol(name)
	// One namespace resolution, two frames off it — MutableRuntimeOrNil walks the
	// lexical chain for the namespace, so deriving the sealed base from its result
	// keeps both probes talking about the same namespace. Reading the sealed base
	// off the calling frame instead would answer nil on a frame the walk could
	// still have resolved.
	runtime := mc.EnvironmentFrame().MutableRuntimeOrNil()
	// The mutable runtime PARENTS the sealed base, so this one lookup already
	// covers the un-imported case: with no (import (scheme base)) to shadow it,
	// GetBinding walks through to the sealed base's copy.
	q := recognizedBinding(runtime, sym, identity)
	if q == nil && runtime != nil {
		// Reached only when the mutable layer holds something that is NOT this
		// primitive — a user shadow. Address the sealed base directly so the
		// shadow cannot suppress the real answer.
		q = recognizedBinding(runtime.Namespace().SealedBase(), sym, identity)
	}
	if q == nil {
		return werr.WrapForeignErrorf(werr.ErrUnexpectedNil,
			"%s: %s is not registered in this namespace", site, name)
	}
	mc.SetValue(q)
	return nil
}

// recognizedBinding resolves sym in env and returns its value only if that value
// IS the primitive identity names. nil means NONE: unbound, no such frame, or
// bound to something else — all three are "this environment cannot supply it", and
// the caller falls back or refuses.
//
// Empty scopes resolve the ambient (unscoped) registration, the same query
// registerPhasePrimitive uses to address the binding it just wrote. syntax.ScopeSet
// is a type alias for values.ScopeSet, so no conversion is needed.
func recognizedBinding(env *environment.EnvironmentFrame, sym *values.Symbol, identity *machine.PrimitiveIdentity) values.Value {
	if env == nil {
		return nil
	}
	binding := env.GetBinding(sym, values.EmptyScopes())
	if binding == nil {
		return nil
	}
	q := binding.Value()
	if machine.IdentityOf(q) != identity {
		return nil
	}
	return q
}
