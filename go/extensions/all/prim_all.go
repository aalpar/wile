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

// Additional R7RS primitives for the all extension:
// Records, promises, string operations, character operations

package all

import (
	"context"
	"errors"
	"strings"
	"unicode"

	"wile/environment"
	"wile/machine"
	"wile/utils"
	"wile/values"
)

// =============================================================================
// Record Primitives
// =============================================================================

// PrimMakeRecordType implements the (make-record-type name field-names) primitive.
// Creates a new record type descriptor.
func PrimMakeRecordType(_ context.Context, mc *machine.MachineContext) error {
	nameArg := mc.Arg(0)
	fieldNamesArg := mc.Arg(1)

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
	obj := mc.Arg(0)
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
	obj := mc.Arg(0)
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
	obj := mc.Arg(0)
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
	rtArg := mc.Arg(0)
	fieldTagsArg := mc.Arg(1)

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
	rtArg := mc.Arg(0)

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
	rtArg := mc.Arg(0)
	fieldTagArg := mc.Arg(1)

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
	rtArg := mc.Arg(0)
	fieldTagArg := mc.Arg(1)

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
	_, err := values.ForEach(context.TODO(), v, func(_ context.Context, _ int, _ bool, elem values.Value) error {
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

// =============================================================================
// Promise Primitives
// =============================================================================

// PrimPromiseQ tests if an object is a promise.
func PrimPromiseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Promise)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}

// PrimMakePromise implements the (make-promise) primitive.
// Creates a promise from a value, wrapping it if not already a promise.
func PrimMakePromise(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// If already a promise, return it unchanged
	if p, ok := o.(*values.Promise); ok {
		mc.SetValue(p)
		return nil
	}
	// Otherwise, wrap in an already-forced promise
	mc.SetValue(values.NewForcedPromise(o))
	return nil
}

// PrimForce implements the (force) primitive.
// Forces evaluation of a promise.
func PrimForce(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	promise, ok := o.(*values.Promise)
	if !ok {
		// R7RS says: if not a promise, return the value unchanged
		mc.SetValue(o)
		return nil
	}

	// If already forced, return cached result
	if promise.Forced {
		mc.SetValue(promise.Result)
		return nil
	}

	// Force the promise by invoking the thunk
	mcls, ok := promise.Thunk.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "force: promise thunk is not a procedure: %T", promise.Thunk)
	}

	sub := mc.NewSubContext()
	_, err := sub.Apply(mcls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	result := sub.GetValue()

	// R7RS iterative forcing: if result is a promise, force it too
	if resultPromise, ok := result.(*values.Promise); ok {
		// Update our promise to point to the result promise's contents
		if resultPromise.Forced {
			promise.Result = resultPromise.Result
		} else {
			promise.Thunk = resultPromise.Thunk
			// Recursively force
			mc.SetValue(promise)
			return PrimForce(ctx, mc)
		}
	} else {
		promise.Result = result
	}

	promise.Forced = true
	promise.Thunk = nil
	mc.SetValue(promise.Result)
	return nil
}

// PrimMakeLazyPromise implements the (delay-force) primitive.
// Creates a lazy promise that delays evaluation of a thunk.
func PrimMakeLazyPromise(_ context.Context, mc *machine.MachineContext) error {
	thunk := mc.Arg(0)
	mc.SetValue(values.NewPromise(thunk))
	return nil
}

// =============================================================================
// String Primitives
// =============================================================================

// stringCompareVariadic is a helper for variadic string comparison primitives.
func stringCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b string) bool) error {
	s1 := mc.Arg(0)
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, s1)
	}

	rest := mc.Arg(1)
	prev := str1.Value

	for rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list", name)
		}
		str, ok := pair.Car().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, pair.Car())
		}
		if !cmp(prev, str.Value) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		prev = str.Value
		rest = pair.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}

// PrimStringCopyTo implements the string-copy! primitive.
// R7RS §6.7: (string-copy! to at from [start [end]])
func PrimStringCopyTo(_ context.Context, mc *machine.MachineContext) error {
	toArg := mc.Arg(0)
	rest := mc.Arg(1)

	to, ok := toArg.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy!: expected a string for 'to' but got %T", toArg)
	}

	// Parse variadic arguments: at from [start [end]]
	var args []values.Value
	current := rest
	for current != values.EmptyList {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-copy!: improper argument list")
		}
		args = append(args, pair.Car())
		current = pair.Cdr()
	}

	if len(args) < 2 {
		return values.NewForeignError("string-copy!: expected at least 3 arguments")
	}

	atVal, ok := args[0].(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for 'at' but got %T", args[0])
	}
	at := int(atVal.Value)

	from, ok := args[1].(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-copy!: expected a string for 'from' but got %T", args[1])
	}

	fromLen := from.Len()
	start := 0
	end := fromLen

	if len(args) >= 3 {
		startVal, ok := args[2].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for start but got %T", args[2])
		}
		start = int(startVal.Value)
	}

	if len(args) >= 4 {
		endVal, ok := args[3].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-copy!: expected an integer for end but got %T", args[3])
		}
		end = int(endVal.Value)
	}

	// Validate indices
	if start < 0 || end > fromLen || start > end {
		return values.NewForeignError("string-copy!: invalid source indices")
	}

	toLen := to.Len()
	copyLen := end - start
	if at < 0 || at+copyLen > toLen {
		return values.NewForeignError("string-copy!: destination index out of bounds")
	}

	// Perform the copy
	toRunes := to.Runes()
	fromRunes := from.Runes()
	copy(toRunes[at:], fromRunes[start:end])
	to.SetValue(string(toRunes))

	mc.SetValue(values.Void)
	return nil
}

// PrimStringFill implements the string-fill! primitive.
// R7RS §6.7: (string-fill! string fill [start [end]])
func PrimStringFill(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-fill!: expected a string but got %T", o)
	}

	// Parse variadic arguments: fill [start [end]]
	var args []values.Value
	current := rest
	for current != values.EmptyList {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-fill!: improper argument list")
		}
		args = append(args, pair.Car())
		current = pair.Cdr()
	}

	if len(args) < 1 {
		return values.NewForeignError("string-fill!: expected at least 2 arguments")
	}

	char, ok := args[0].(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "string-fill!: expected a character but got %T", args[0])
	}

	length := s.Len()
	start := 0
	end := length

	if len(args) >= 2 {
		startVal, ok := args[1].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-fill!: expected an integer for start but got %T", args[1])
		}
		start = int(startVal.Value)
	}

	if len(args) >= 3 {
		endVal, ok := args[2].(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "string-fill!: expected an integer for end but got %T", args[2])
		}
		end = int(endVal.Value)
	}

	if start < 0 || end > length || start > end {
		return values.NewForeignError("string-fill!: invalid indices")
	}

	s.Fill(char.Value, start, end)
	mc.SetValue(values.Void)
	return nil
}

// PrimStringMap implements the string-map primitive.
// R7RS §6.7: (string-map proc string1 string2 ...)
func PrimStringMap(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "string-map: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	// Collect all strings into a slice
	var strs []*values.String
	current := stringsVal
	for !values.IsEmptyList(current) {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-map: improper argument list")
		}
		s, ok := pair.Car().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "string-map: expected a string but got %T", pair.Car())
		}
		strs = append(strs, s)
		current = pair.Cdr()
	}

	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-map: expected at least one string")
	}

	// Convert all strings to rune slices and find minimum length
	runeSlices := make([][]rune, len(strs))
	minLen := -1
	for i, s := range strs {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}

	// Apply proc to each position
	result := make([]rune, minLen)
	sub := mc.NewSubContext()

	for i := 0; i < minLen; i++ {
		// Collect one character from each string
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return err
			}
		}

		// Get the result character
		resultVal := sub.GetValue()
		char, ok := resultVal.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "string-map: procedure must return a character but got %T", resultVal)
		}
		result[i] = char.Value
	}

	mc.SetValue(values.NewString(string(result)))
	return nil
}

// PrimStringForEach implements the string-for-each primitive.
// R7RS §6.7: (string-for-each proc string1 string2 ...)
func PrimStringForEach(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	stringsVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "string-for-each: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(stringsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Collect all strings into a slice
	var strs []*values.String
	current := stringsVal
	for !values.IsEmptyList(current) {
		pair, ok := current.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "string-for-each: improper argument list")
		}
		s, ok := pair.Car().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "string-for-each: expected a string but got %T", pair.Car())
		}
		strs = append(strs, s)
		current = pair.Cdr()
	}

	if len(strs) == 0 {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "string-for-each: expected at least one string")
	}

	// Convert all strings to rune slices and find minimum length
	runeSlices := make([][]rune, len(strs))
	minLen := -1
	for i, s := range strs {
		runeSlices[i] = s.Runes()
		if minLen < 0 || len(runeSlices[i]) < minLen {
			minLen = len(runeSlices[i])
		}
	}

	// Apply proc to each position
	sub := mc.NewSubContext()

	for i := 0; i < minLen; i++ {
		// Collect one character from each string
		args := make(values.Vector, len(strs))
		for j := range strs {
			args[j] = values.NewCharacter(runeSlices[j][i])
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return err
			}
		}
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimStringCiEqVariadic implements the variadic string-ci=? primitive.
func PrimStringCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci=?", strings.EqualFold)
}

// PrimStringCiLtVariadic implements the variadic string-ci<? primitive.
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
		return strings.ToLower(a) < strings.ToLower(b)
	})
}

// PrimStringCiGtVariadic implements the variadic string-ci>? primitive.
func PrimStringCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci>?", func(a, b string) bool {
		return strings.ToLower(a) > strings.ToLower(b)
	})
}

// PrimStringCiLeVariadic implements the variadic string-ci<=? primitive.
func PrimStringCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci<=?", func(a, b string) bool {
		return strings.ToLower(a) <= strings.ToLower(b)
	})
}

// PrimStringCiGeVariadic implements the variadic string-ci>=? primitive.
func PrimStringCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return stringCompareVariadic(mc, "string-ci>=?", func(a, b string) bool {
		return strings.ToLower(a) >= strings.ToLower(b)
	})
}

// PrimStringUpcase implements the string-upcase primitive.
func PrimStringUpcase(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-upcase: expected a string but got %T", s)
	}
	mc.SetValue(values.NewString(strings.ToUpper(str.Value)))
	return nil
}

// PrimStringDowncase implements the string-downcase primitive.
func PrimStringDowncase(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-downcase: expected a string but got %T", s)
	}
	mc.SetValue(values.NewString(strings.ToLower(str.Value)))
	return nil
}

// PrimStringFoldcase implements the string-foldcase primitive.
func PrimStringFoldcase(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-foldcase: expected a string but got %T", s)
	}
	mc.SetValue(values.NewString(strings.ToLower(str.Value)))
	return nil
}

// =============================================================================
// Character Primitives
// =============================================================================

// charCompareVariadic is a helper for variadic character comparison primitives.
func charCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b rune) bool) error {
	c1 := mc.Arg(0)
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, c1)
	}

	rest := mc.Arg(1)
	prev := ch1.Value

	for rest != values.EmptyList {
		pair, ok := rest.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list", name)
		}
		ch, ok := pair.Car().(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, pair.Car())
		}
		if !cmp(prev, ch.Value) {
			mc.SetValue(values.FalseValue)
			return nil
		}
		prev = ch.Value
		rest = pair.Cdr()
	}

	mc.SetValue(values.TrueValue)
	return nil
}

// PrimCharCiEqVariadic implements the variadic char-ci=? primitive.
func PrimCharCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci=?", func(a, b rune) bool {
		return unicode.ToLower(a) == unicode.ToLower(b)
	})
}

// PrimCharCiLtVariadic implements the variadic char-ci<? primitive.
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
		return unicode.ToLower(a) < unicode.ToLower(b)
	})
}

// PrimCharCiGtVariadic implements the variadic char-ci>? primitive.
func PrimCharCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>?", func(a, b rune) bool {
		return unicode.ToLower(a) > unicode.ToLower(b)
	})
}

// PrimCharCiLeVariadic implements the variadic char-ci<=? primitive.
func PrimCharCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci<=?", func(a, b rune) bool {
		return unicode.ToLower(a) <= unicode.ToLower(b)
	})
}

// PrimCharCiGeVariadic implements the variadic char-ci>=? primitive.
func PrimCharCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
	return charCompareVariadic(mc, "char-ci>=?", func(a, b rune) bool {
		return unicode.ToLower(a) >= unicode.ToLower(b)
	})
}

// PrimCharAlphabeticQ tests if a character is alphabetic.
func PrimCharAlphabeticQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-alphabetic?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsLetter(ch.Value)))
	return nil
}

// PrimCharNumericQ tests if a character is numeric.
func PrimCharNumericQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-numeric?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsDigit(ch.Value)))
	return nil
}

// PrimCharWhitespaceQ tests if a character is whitespace.
func PrimCharWhitespaceQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-whitespace?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsSpace(ch.Value)))
	return nil
}

// PrimCharUpperCaseQ tests if a character is uppercase.
func PrimCharUpperCaseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-upper-case?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsUpper(ch.Value)))
	return nil
}

// PrimCharLowerCaseQ tests if a character is lowercase.
func PrimCharLowerCaseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-lower-case?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsLower(ch.Value)))
	return nil
}

// PrimCharUpcase returns the uppercase version of a character.
func PrimCharUpcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-upcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToUpper(ch.Value)))
	return nil
}

// PrimCharDowncase returns the lowercase version of a character.
func PrimCharDowncase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-downcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}

// PrimCharFoldcase returns the case-folded version of a character.
func PrimCharFoldcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-foldcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}

// PrimDigitValue implements the (digit-value) primitive.
// Returns the numeric value of a digit character, or #f if not a digit.
func PrimDigitValue(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "digit-value: expected a character but got %T", o)
	}
	if ch.Value >= '0' && ch.Value <= '9' {
		mc.SetValue(values.NewInteger(int64(ch.Value - '0')))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
