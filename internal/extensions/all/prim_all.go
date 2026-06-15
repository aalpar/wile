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

// Records, promises, and generic variadic comparison helper

package all

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// =============================================================================
// Record Primitives
// =============================================================================

// PrimMakeRecordType implements the (make-record-type name field-names) primitive.
// Creates a new record type descriptor.
func PrimMakeRecordType(mc machine.CallContext) error {
	nameArg := mc.Arg(0)
	fieldNamesArg := mc.Arg(1)

	name, err := helpers.RequireType[*values.Symbol](nameArg, werr.ErrNotASymbol, "make-record-type")
	if err != nil {
		return err
	}

	fieldNames, err := listToSymbols(mc.Context(), fieldNamesArg)
	if err != nil {
		return werr.WrapForeignErrorf(err, "make-record-type: field-names")
	}

	rt := values.NewRecordType(name, fieldNames)
	mc.SetValue(rt)
	return nil
}

// PrimMakeOpaqueRecordType implements the (make-opaque-record-type name field-names) primitive.
// Creates an opaque record type descriptor that is hidden from generic inspection.
func PrimMakeOpaqueRecordType(mc machine.CallContext) error {
	nameArg := mc.Arg(0)
	fieldNamesArg := mc.Arg(1)

	name, err := helpers.RequireType[*values.Symbol](nameArg, werr.ErrNotASymbol, "make-opaque-record-type")
	if err != nil {
		return err
	}

	fieldNames, err := listToSymbols(mc.Context(), fieldNamesArg)
	if err != nil {
		return werr.WrapForeignErrorf(err, "make-opaque-record-type: field-names")
	}

	rt := values.NewOpaqueRecordType(name, fieldNames)
	mc.SetValue(rt)
	return nil
}

// PrimIsRecordType implements the (record-type? obj) primitive.
var PrimIsRecordType = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.RecordType)
	return ok
})

// PrimIsRecord implements the (record? obj) primitive.
// Returns #f for instances of opaque record types.
var PrimIsRecord = helpers.MakeTypePredicate(func(o values.Value) bool {
	rec, ok := o.(*values.Record)
	return ok && !rec.RecordType().IsOpaque()
})

// PrimRecordType implements the (record-type record) primitive.
// Returns the record type of a record instance.
// Errors if the record's type is opaque.
func PrimRecordType(mc machine.CallContext) error {
	rec, err := helpers.RequireArg[*values.Record](mc, 0, werr.ErrNotARecord, "record-type")
	if err != nil {
		return err
	}
	if rec.RecordType().IsOpaque() {
		return werr.WrapForeignErrorf(werr.ErrOpaqueRecord, "record-type: cannot inspect opaque record")
	}
	mc.SetValue(rec.RecordType())
	return nil
}

// PrimRecordConstructor implements the (record-constructor rt field-tags) primitive.
// Returns a constructor procedure for the record type.
func PrimRecordConstructor(mc machine.CallContext) error {
	rtArg := mc.Arg(0)
	fieldTagsArg := mc.Arg(1)

	rt, err := helpers.RequireType[*values.RecordType](rtArg, werr.ErrNotARecordType, "record-constructor")
	if err != nil {
		return err
	}

	constructorFields, err := listToSymbols(mc.Context(), fieldTagsArg)
	if err != nil {
		return werr.WrapForeignErrorf(err, "record-constructor: field-tags")
	}

	// Compute indices mapping constructor args to record fields
	argIndices := make([]int, len(constructorFields))
	for i, sym := range constructorFields {
		idx := rt.FieldIndex(sym)
		if idx < 0 {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "record-constructor: unknown field %s", sym.SchemeString())
		}
		argIndices[i] = idx
	}

	// Create the constructor closure
	closure := newRecordConstructorClosure(mc.EnvironmentFrame().Namespace().Runtime(), rt, argIndices)
	mc.SetValue(closure)
	return nil
}

// PrimRecordPredicate implements the (record-predicate rt) primitive.
// Returns a predicate procedure for the record type.
func PrimRecordPredicate(mc machine.CallContext) error {
	rt, err := helpers.RequireArg[*values.RecordType](mc, 0, werr.ErrNotARecordType, "record-predicate")
	if err != nil {
		return err
	}

	closure := newRecordPredicateClosure(mc.EnvironmentFrame().Namespace().Runtime(), rt)
	mc.SetValue(closure)
	return nil
}

// resolveRecordField extracts a record type and field tag from the arguments,
// validates the field exists, and returns the closure produced by makeClosure.
func resolveRecordField(
	mc machine.CallContext, name string,
	makeClosure func(*environment.EnvironmentFrame, *values.RecordType, int) *machine.ForeignClosure,
) error {
	rt, err := helpers.RequireArg[*values.RecordType](mc, 0, werr.ErrNotARecordType, name)
	if err != nil {
		return err
	}

	fieldTag, err := helpers.RequireType[*values.Symbol](mc.Arg(1), werr.ErrNotASymbol, name)
	if err != nil {
		return err
	}

	idx := rt.FieldIndex(fieldTag)
	if idx < 0 {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "%s: unknown field %s", name, fieldTag.SchemeString())
	}

	mc.SetValue(makeClosure(mc.EnvironmentFrame().Namespace().Runtime(), rt, idx))
	return nil
}

// PrimRecordAccessor implements the (record-accessor rt field-tag) primitive.
// Returns an accessor procedure for the specified field.
func PrimRecordAccessor(mc machine.CallContext) error {
	return resolveRecordField(mc, "record-accessor", newRecordAccessorClosure)
}

// PrimRecordModifier implements the (record-modifier rt field-tag) primitive.
// Returns a modifier procedure for the specified field.
func PrimRecordModifier(mc machine.CallContext) error {
	return resolveRecordField(mc, "record-modifier", newRecordModifierClosure)
}

// Helper: convert a Scheme list to a slice of symbols
func listToSymbols(ctx context.Context, v values.Value) ([]*values.Symbol, error) {
	var result []*values.Symbol
	if values.IsEmptyList(v) {
		return result, nil
	}
	t, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"expected a list but got %T", v)
	}
	err := helpers.ForEachList(ctx, t, "list", func(_ context.Context, _ int, _ bool, elem values.Value) error {
		sym, ok := elem.(*values.Symbol)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotASymbol, "expected a symbol but got %T", elem)
		}
		result = append(result, sym)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return result, nil
}

// newRecordConstructorClosure creates a closure that constructs records.
func newRecordConstructorClosure(env *environment.EnvironmentFrame, rt *values.RecordType, argIndices []int) *machine.ForeignClosure {
	fieldCount := rt.FieldCount()
	fn := func(innerMC machine.CallContext) error {
		// Create field array with unspecified values
		fields := make([]values.Value, fieldCount)
		for i := range fields {
			fields[i] = values.FalseValue // Default unspecified value
		}
		// Fill in constructor arguments
		for i, fieldIdx := range argIndices {
			val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(i).Value()
			fields[fieldIdx] = val
		}
		rec, err := values.NewRecord(rt, fields)
		if err != nil {
			return err
		}
		innerMC.SetValue(rec)
		return nil
	}
	return machine.NewForeignClosure(env, len(argIndices), false, fn)
}

// newRecordPredicateClosure creates a closure that checks if a value is a record of the given type.
func newRecordPredicateClosure(env *environment.EnvironmentFrame, rt *values.RecordType) *machine.ForeignClosure {
	fn := func(innerMC machine.CallContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		rec, ok := obj.(*values.Record)
		innerMC.SetValue(values.BoolToBoolean(ok && rec.RecordType() == rt))
		return nil
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// newRecordAccessorClosure creates a closure that accesses a specific field of a record.
func newRecordAccessorClosure(env *environment.EnvironmentFrame, rt *values.RecordType, fieldIdx int) *machine.ForeignClosure {
	fn := func(innerMC machine.CallContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		rec, ok := obj.(*values.Record)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotARecord, "record accessor: expected a record but got %T", obj)
		}
		if rec.RecordType() != rt {
			return werr.WrapForeignErrorf(werr.ErrTypeConversion, "record accessor: record type mismatch")
		}
		innerMC.SetValue(rec.Field(fieldIdx))
		return nil
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// newRecordModifierClosure creates a closure that modifies a specific field of a record.
func newRecordModifierClosure(env *environment.EnvironmentFrame, rt *values.RecordType, fieldIdx int) *machine.ForeignClosure {
	fn := func(innerMC machine.CallContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
		rec, ok := obj.(*values.Record)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotARecord, "record modifier: expected a record but got %T", obj)
		}
		if rec.RecordType() != rt {
			return werr.WrapForeignErrorf(werr.ErrTypeConversion, "record modifier: record type mismatch")
		}
		rec.SetField(fieldIdx, val)
		innerMC.SetValue(values.Void)
		return nil
	}
	return machine.NewForeignClosure(env, 2, false, fn)
}

// =============================================================================
// Promise Primitives
// =============================================================================

// PrimPromiseQ tests if an object is a promise.
var PrimPromiseQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Promise)
	return ok
})

// PrimMakePromise implements the (make-promise) primitive.
// Creates a promise from a value, wrapping it if not already a promise.
func PrimMakePromise(mc machine.CallContext) error {
	o := mc.Arg(0)
	// If already a promise, return it unchanged
	p, ok := o.(*values.Promise)
	if ok {
		mc.SetValue(p)
		return nil
	}
	// Otherwise, wrap in an already-forced promise
	mc.SetValue(values.NewForcedPromise(o))
	return nil
}

// executeThunk runs a promise thunk and returns its result.
// The thunk is already validated as Callable when the promise was created.
func executeThunk(mc *machine.MachineContext, thunk values.Callable) (values.Value, error) {
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err := sub.ApplyCallable(thunk)
	if err != nil {
		return nil, err
	}
	err = sub.Run()
	if err != nil {
		return nil, err
	}
	return sub.GetValue(), nil
}

// forcePromise forces a promise and returns its result.
// This is the core recursive implementation of R7RS force semantics.
func forcePromise(mc *machine.MachineContext, promise *values.Promise) (values.Value, error) {
	if promise.IsForced() {
		return promise.CachedResult(), nil
	}

	result, err := executeThunk(mc, promise.Thunk())
	if err != nil {
		return nil, err
	}

	// Nested call may have already forced this promise
	if promise.IsForced() {
		return promise.CachedResult(), nil
	}

	// Recursively force promise results (delay-force semantics)
	rp, ok := result.(*values.Promise)
	if ok && rp != promise {
		result, err = forcePromise(mc, rp)
		if err != nil {
			return nil, err
		}
	}

	promise.Force(result)
	return result, nil
}

// PrimForce implements the (force) primitive.
// Forces evaluation of a promise with proper memoization.
//
// R7RS §4.2.5: The first time a promise is forced, its body is evaluated
// and the result is memoized; on subsequent forces, the memoized result
// is returned.
func PrimForce(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	o := mc.Arg(0)
	promise, ok := o.(*values.Promise)
	if !ok {
		mc.SetValue(o)
		return nil
	}

	result, err := forcePromise(mc, promise)
	if err != nil {
		return err
	}
	mc.SetValue(result)
	return nil
}

// PrimMakeLazyPromise implements the (delay-force) primitive.
// Creates a lazy promise that delays evaluation of a thunk.
func PrimMakeLazyPromise(mc machine.CallContext) error {
	thunk, ok := mc.Arg(0).(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"%%make-lazy-promise: thunk must be a procedure")
	}
	mc.SetValue(values.NewPromise(thunk))
	return nil
}
