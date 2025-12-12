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

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimMakeRecordType implements the (make-record-type name field-names) primitive.
// Creates a new record type descriptor.
func PrimMakeRecordType(_ context.Context, mc *machine.MachineContext) error {
	nameArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	fieldNamesArg := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	name, ok := nameArg.(*values.Symbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "make-record-type: expected a symbol for name but got %T", nameArg)
	}

	fieldNames, err := listToSymbols(fieldNamesArg)
	if err != nil {
		return values.WrapForeignErrorf(err, "make-record-type: field-names")
	}

	rt := values.NewRecordType(name, fieldNames)
	mc.SetValue(rt)
	return nil
}

// PrimIsRecordType implements the (record-type? obj) primitive.
func PrimIsRecordType(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := obj.(*values.RecordType)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimIsRecord implements the (record? obj) primitive.
func PrimIsRecord(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := obj.(*values.Record)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimRecordType implements the (record-type record) primitive.
// Returns the record type of a record instance.
func PrimRecordType(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rec, ok := obj.(*values.Record)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARecord, "record-type: expected a record but got %T", obj)
	}
	mc.SetValue(rec.RecordType())
	return nil
}

// PrimRecordConstructor implements the (record-constructor rt field-tags) primitive.
// Returns a constructor procedure for the record type.
func PrimRecordConstructor(_ context.Context, mc *machine.MachineContext) error {
	rtArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	fieldTagsArg := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	rt, ok := rtArg.(*values.RecordType)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARecordType, "record-constructor: expected a record type but got %T", rtArg)
	}

	constructorFields, err := listToSymbols(fieldTagsArg)
	if err != nil {
		return values.WrapForeignErrorf(err, "record-constructor: field-tags")
	}

	// Compute indices mapping constructor args to record fields
	argIndices := make([]int, len(constructorFields))
	for i, sym := range constructorFields {
		idx := rt.FieldIndex(sym)
		if idx < 0 {
			return values.NewForeignError("record-constructor: unknown field " + sym.SchemeString())
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
	rtArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	rt, ok := rtArg.(*values.RecordType)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARecordType, "record-predicate: expected a record type but got %T", rtArg)
	}

	closure := newRecordPredicateClosure(mc.EnvironmentFrame().TopLevel(), rt)
	mc.SetValue(closure)
	return nil
}

// PrimRecordAccessor implements the (record-accessor rt field-tag) primitive.
// Returns an accessor procedure for the specified field.
func PrimRecordAccessor(_ context.Context, mc *machine.MachineContext) error {
	rtArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	fieldTagArg := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	rt, ok := rtArg.(*values.RecordType)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARecordType, "record-accessor: expected a record type but got %T", rtArg)
	}

	fieldTag, ok := fieldTagArg.(*values.Symbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "record-accessor: expected a symbol for field-tag but got %T", fieldTagArg)
	}

	idx := rt.FieldIndex(fieldTag)
	if idx < 0 {
		return values.NewForeignError("record-accessor: unknown field " + fieldTag.SchemeString())
	}

	closure := newRecordAccessorClosure(mc.EnvironmentFrame().TopLevel(), rt, idx)
	mc.SetValue(closure)
	return nil
}

// PrimRecordModifier implements the (record-modifier rt field-tag) primitive.
// Returns a modifier procedure for the specified field.
func PrimRecordModifier(_ context.Context, mc *machine.MachineContext) error {
	rtArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	fieldTagArg := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	rt, ok := rtArg.(*values.RecordType)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotARecordType, "record-modifier: expected a record type but got %T", rtArg)
	}

	fieldTag, ok := fieldTagArg.(*values.Symbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "record-modifier: expected a symbol for field-tag but got %T", fieldTagArg)
	}

	idx := rt.FieldIndex(fieldTag)
	if idx < 0 {
		return values.NewForeignError("record-modifier: unknown field " + fieldTag.SchemeString())
	}

	closure := newRecordModifierClosure(mc.EnvironmentFrame().TopLevel(), rt, idx)
	mc.SetValue(closure)
	return nil
}

// Helper: convert a Scheme list to a slice of symbols
func listToSymbols(v values.Value) ([]*values.Symbol, error) {
	var result []*values.Symbol
	_, err := values.ForEach(nil, v, func(_ int, _ bool, elem values.Value) error {
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
		if ok && rec.RecordType() == rt {
			innerMC.SetValue(values.TrueValue)
		} else {
			innerMC.SetValue(values.FalseValue)
		}
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
			return values.NewForeignError("record accessor: record type mismatch")
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
			return values.NewForeignError("record modifier: record type mismatch")
		}
		rec.SetField(fieldIdx, val)
		innerMC.SetValue(values.Void)
		return nil
	}
	return machine.NewForeignClosure(env, 2, false, fn)
}
