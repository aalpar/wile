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
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// =============================================================================
// Record Primitives
// =============================================================================

// PrimMakeRecordType implements the (make-record-type name field-names) primitive.
// Creates a new record type descriptor.
func PrimMakeRecordType(ctx context.Context, mc *machine.MachineContext) error {
	nameArg := mc.Arg(0)
	fieldNamesArg := mc.Arg(1)

	name, err := helpers.RequireType[*values.Symbol](nameArg, values.ErrNotASymbol, "make-record-type")
	if err != nil {
		return err
	}

	fieldNames, err := listToSymbols(ctx, fieldNamesArg)
	if err != nil {
		return values.WrapForeignErrorf(err, "make-record-type: field-names")
	}

	rt := values.NewRecordType(name, fieldNames)
	mc.SetValue(rt)
	return nil
}

// PrimIsRecordType implements the (record-type? obj) primitive.
func PrimIsRecordType(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	_, ok := obj.(*values.RecordType)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimIsRecord implements the (record? obj) primitive.
func PrimIsRecord(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	_, ok := obj.(*values.Record)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimRecordType implements the (record-type record) primitive.
// Returns the record type of a record instance.
func PrimRecordType(_ context.Context, mc *machine.MachineContext) error {
	rec, err := helpers.RequireArg[*values.Record](mc, 0, values.ErrNotARecord, "record-type")
	if err != nil {
		return err
	}
	mc.SetValue(rec.RecordType())
	return nil
}

// PrimRecordConstructor implements the (record-constructor rt field-tags) primitive.
// Returns a constructor procedure for the record type.
func PrimRecordConstructor(ctx context.Context, mc *machine.MachineContext) error {
	rtArg := mc.Arg(0)
	fieldTagsArg := mc.Arg(1)

	rt, err := helpers.RequireType[*values.RecordType](rtArg, values.ErrNotARecordType, "record-constructor")
	if err != nil {
		return err
	}

	constructorFields, err := listToSymbols(ctx, fieldTagsArg)
	if err != nil {
		return values.WrapForeignErrorf(err, "record-constructor: field-tags")
	}

	// Compute indices mapping constructor args to record fields
	argIndices := make([]int, len(constructorFields))
	for i, sym := range constructorFields {
		idx := rt.FieldIndex(sym)
		if idx < 0 {
			return values.WrapForeignErrorf(values.ErrNoSuchBinding, "record-constructor: unknown field %s", sym.SchemeString())
		}
		argIndices[i] = idx
	}

	// Create the constructor closure
	closure := newRecordConstructorClosure(mc.EnvironmentFrame().TopLevel(), rt, argIndices)
	mc.SetValue(closure)
	return nil
}

// PrimRecordPredicate implements the (record-predicate rt) primitive.
// Returns a predicate procedure for the record type.
func PrimRecordPredicate(_ context.Context, mc *machine.MachineContext) error {
	rt, err := helpers.RequireArg[*values.RecordType](mc, 0, values.ErrNotARecordType, "record-predicate")
	if err != nil {
		return err
	}

	closure := newRecordPredicateClosure(mc.EnvironmentFrame().TopLevel(), rt)
	mc.SetValue(closure)
	return nil
}

// PrimRecordAccessor implements the (record-accessor rt field-tag) primitive.
// Returns an accessor procedure for the specified field.
func PrimRecordAccessor(_ context.Context, mc *machine.MachineContext) error {
	fieldTagArg := mc.Arg(1)

	rt, err := helpers.RequireArg[*values.RecordType](mc, 0, values.ErrNotARecordType, "record-accessor")
	if err != nil {
		return err
	}

	fieldTag, err := helpers.RequireType[*values.Symbol](fieldTagArg, values.ErrNotASymbol, "record-accessor")
	if err != nil {
		return err
	}

	idx := rt.FieldIndex(fieldTag)
	if idx < 0 {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "record-accessor: unknown field %s", fieldTag.SchemeString())
	}

	closure := newRecordAccessorClosure(mc.EnvironmentFrame().TopLevel(), rt, idx)
	mc.SetValue(closure)
	return nil
}

// PrimRecordModifier implements the (record-modifier rt field-tag) primitive.
// Returns a modifier procedure for the specified field.
func PrimRecordModifier(_ context.Context, mc *machine.MachineContext) error {
	fieldTagArg := mc.Arg(1)

	rt, err := helpers.RequireArg[*values.RecordType](mc, 0, values.ErrNotARecordType, "record-modifier")
	if err != nil {
		return err
	}

	fieldTag, err := helpers.RequireType[*values.Symbol](fieldTagArg, values.ErrNotASymbol, "record-modifier")
	if err != nil {
		return err
	}

	idx := rt.FieldIndex(fieldTag)
	if idx < 0 {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "record-modifier: unknown field %s", fieldTag.SchemeString())
	}

	closure := newRecordModifierClosure(mc.EnvironmentFrame().TopLevel(), rt, idx)
	mc.SetValue(closure)
	return nil
}

// Helper: convert a Scheme list to a slice of symbols
func listToSymbols(ctx context.Context, v values.Value) ([]*values.Symbol, error) {
	var result []*values.Symbol
	_, err := values.ForEach(ctx, v, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		sym, ok := elem.(*values.Symbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASymbol, "expected a symbol but got %T", elem)
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
func newRecordConstructorClosure(env *environment.EnvironmentFrame, rt *values.RecordType, argIndices []int) *machine.MachineClosure {
	fieldCount := rt.FieldCount()
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
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
		rec := values.NewRecord(rt, fields)
		innerMC.SetValue(rec)
		return nil
	}
	return machine.NewForeignClosure(env, len(argIndices), false, fn)
}

// newRecordPredicateClosure creates a closure that checks if a value is a record of the given type.
func newRecordPredicateClosure(env *environment.EnvironmentFrame, rt *values.RecordType) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		rec, ok := obj.(*values.Record)
		innerMC.SetValue(schemeutil.BoolToBoolean(ok && rec.RecordType() == rt))
		return nil
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// newRecordAccessorClosure creates a closure that accesses a specific field of a record.
func newRecordAccessorClosure(env *environment.EnvironmentFrame, rt *values.RecordType, fieldIdx int) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		rec, ok := obj.(*values.Record)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotARecord, "record accessor: expected a record but got %T", obj)
		}
		if rec.RecordType() != rt {
			return values.WrapForeignErrorf(values.ErrTypeConversion, "record accessor: record type mismatch")
		}
		innerMC.SetValue(rec.Field(fieldIdx))
		return nil
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// newRecordModifierClosure creates a closure that modifies a specific field of a record.
func newRecordModifierClosure(env *environment.EnvironmentFrame, rt *values.RecordType, fieldIdx int) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		obj := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
		rec, ok := obj.(*values.Record)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotARecord, "record modifier: expected a record but got %T", obj)
		}
		if rec.RecordType() != rt {
			return values.WrapForeignErrorf(values.ErrTypeConversion, "record modifier: record type mismatch")
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
func PrimPromiseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Promise)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimMakePromise implements the (make-promise) primitive.
// Creates a promise from a value, wrapping it if not already a promise.
func PrimMakePromise(_ context.Context, mc *machine.MachineContext) error {
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
func executeThunk(mc *machine.MachineContext, thunk values.Value) (values.Value, error) {
	mcls, ok := thunk.(*machine.MachineClosure)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAProcedure,
			"force: promise thunk is not a procedure: %T", thunk)
	}
	sub := mc.NewSubContext()
	_, err := sub.Apply(mcls)
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
	if promise.Forced {
		return promise.Result, nil
	}

	result, err := executeThunk(mc, promise.Thunk)
	if err != nil {
		return nil, err
	}

	// Nested call may have already forced this promise
	if promise.Forced {
		return promise.Result, nil
	}

	// Recursively force promise results (delay-force semantics)
	rp, ok := result.(*values.Promise)
	if ok && rp != promise {
		result, err = forcePromise(mc, rp)
		if err != nil {
			return nil, err
		}
	}

	promise.Result = result
	promise.Forced = true
	promise.Thunk = nil
	return result, nil
}

// PrimForce implements the (force) primitive.
// Forces evaluation of a promise with proper memoization.
//
// R7RS §4.2.5: The first time a promise is forced, its body is evaluated
// and the result is memoized; on subsequent forces, the memoized result
// is returned.
func PrimForce(_ context.Context, mc *machine.MachineContext) error {
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
func PrimMakeLazyPromise(_ context.Context, mc *machine.MachineContext) error {
	thunk := mc.Arg(0)
	mc.SetValue(values.NewPromise(thunk))
	return nil
}
