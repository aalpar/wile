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

package helpers

import (
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func TestRequireType_Success_ConcretePointer(t *testing.T) {
	c := qt.New(t)
	v := values.NewVector(values.NewInteger(1), values.NewInteger(2))
	result, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Length(), qt.Equals, 2)
}

func TestRequireType_Success_String(t *testing.T) {
	c := qt.New(t)
	v := values.NewString("hello")
	result, err := RequireType[*values.String](v, werr.ErrNotAString, "string-length")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Value, qt.Equals, "hello")
}

func TestRequireType_Success_Integer(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	result, err := RequireType[*values.Integer](v, werr.ErrNotAnInteger, "exact")
	c.Assert(err, qt.IsNil)
	c.Assert(result.Value, qt.Equals, int64(42))
}

func TestRequireType_Success_Interface(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	result, err := RequireType[values.Number](v, werr.ErrNotANumber, "add")
	c.Assert(err, qt.IsNil)
	c.Assert(result.IsExact(), qt.IsTrue)
}

func TestRequireType_Failure_WrongType(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
}

func TestRequireType_Failure_ErrorMessage_Vector(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
}

func TestRequireType_Failure_ErrorMessage_Integer(t *testing.T) {
	c := qt.New(t)
	v := values.NewString("hello")
	_, err := RequireType[*values.Integer](v, werr.ErrNotAnInteger, "exact")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAnInteger), qt.IsTrue)
}

func TestRequireType_Failure_ErrorMessage_ByteVector(t *testing.T) {
	c := qt.New(t)
	v := values.NewInteger(1)
	_, err := RequireType[*values.ByteVector](v, werr.ErrNotAByteVector, "bytevector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAByteVector), qt.IsTrue)
}

func TestRequireType_Failure_NilValue(t *testing.T) {
	c := qt.New(t)
	_, err := RequireType[*values.Vector](nil, werr.ErrNotAVector, "vector-length")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
}

// TestRequireType_ErrorMessageContainsTypeName pins the end-to-end plumbing
// from sentinel.TypeName() through typeNameFromSentinel into the formatted
// error string. Without this test, a future refactor that breaks the
// errors.As cast in typeNameFromSentinel (e.g., changing *werr.StaticError
// to a value type, or switching the lookup mechanism) would silently
// degrade every primitive's type-mismatch message to "expected  but got
// *Foo" — with TestTypeSentinelsCarryTypeName still passing because the
// sentinel itself carries TypeName; only the helpers' use of it would
// have broken.
func TestRequireType_ErrorMessageContainsTypeName(t *testing.T) {
	tcs := []struct {
		name       string
		sentinel   *werr.StaticError
		wantPhrase string
	}{
		{"vector", werr.ErrNotAVector, "expected a vector"},
		{"integer", werr.ErrNotAnInteger, "expected an integer"},
		{"real number", werr.ErrNotAReal, "expected a real number"},
		{"char-set", werr.ErrNotACharSet, "expected a char-set"},
		{"namespace", werr.ErrNotANamespace, "expected a namespace"},
		{"list", werr.ErrNotAList, "expected a list"},
		{"symbol", werr.ErrNotASymbol, "expected a symbol"},
		{"once (pass-through article)", werr.ErrNotAOnce, "expected a once"},
		{"opaque value (vowel)", werr.ErrNotAnOpaqueValue, "expected an opaque value"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := values.NewString("not-the-expected-type")
			_, err := RequireType[*values.Vector](v, tc.sentinel, "test-prim")
			c.Assert(err, qt.IsNotNil)
			c.Assert(strings.Contains(err.Error(), tc.wantPhrase), qt.IsTrue,
				qt.Commentf("error %q missing phrase %q", err.Error(), tc.wantPhrase))
		})
	}
}

// TestRequireArg_ErrorMessageContainsTypeName covers the same plumbing for
// RequireArg, which has its own format string. RequireArg and RequireType
// share typeNameFromSentinel but produce subtly different messages
// ("argument N: expected X" vs "expected X").
func TestRequireArg_ErrorMessageContainsTypeName(t *testing.T) {
	tcs := []struct {
		name       string
		sentinel   *werr.StaticError
		wantPhrase string
	}{
		{"string", werr.ErrNotAString, "expected a string"},
		{"integer", werr.ErrNotAnInteger, "expected an integer"},
		{"input port (vowel)", werr.ErrNotAnInputPort, "expected an input port"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			args := []values.Value{values.NewInteger(42)}
			mc := &stubCallContext{args: args}
			_, err := RequireArg[*values.Vector](mc, 0, tc.sentinel, "test-prim")
			c.Assert(err, qt.IsNotNil)
			c.Assert(strings.Contains(err.Error(), tc.wantPhrase), qt.IsTrue,
				qt.Commentf("error %q missing phrase %q", err.Error(), tc.wantPhrase))
		})
	}
}

// TestRequireType_NonTypeSentinel_DegradedMessage pins the documented
// silent-degradation behavior: passing a non-type sentinel (constructed
// via NewStaticError, no TypeName) produces "expected  but got ..." with
// a double space. This is a misuse path; the test makes the contract
// explicit so a future refactor that turns this into a louder failure
// (e.g., a runtime guard) doesn't accidentally break callers.
//
// ErrInvalidArgument is the most representative misuse — it's the
// catch-all that the new type-sentinel API is replacing across the
// codebase, so a contributor reaching for it before noticing the
// type-sentinel pattern is the realistic failure mode.
func TestRequireType_NonTypeSentinel_DegradedMessage(t *testing.T) {
	c := qt.New(t)
	nonTypeSentinel := werr.ErrInvalidArgument // NewStaticError, no TypeName
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, nonTypeSentinel, "test")
	c.Assert(err, qt.IsNotNil)
	// Double space between "expected" and "but" because TypeName is empty.
	c.Assert(strings.Contains(err.Error(), "expected  but got"), qt.IsTrue,
		qt.Commentf("non-type sentinel should produce empty type phrase: %q", err.Error()))
}

// TestTypeNameFromSentinel_WrappedSentinel pins that typeNameFromSentinel
// uses errors.As, so a sentinel wrapped one or more layers deep still
// surfaces its TypeName. Documents the contract that helpers tolerate
// pre-wrapped error inputs.
func TestTypeNameFromSentinel_WrappedSentinel(t *testing.T) {
	c := qt.New(t)
	// Wrap the sentinel one layer.
	wrapped := werr.WrapForeignErrorf(werr.ErrNotAVector, "outer context")
	v := values.NewInteger(42)
	_, err := RequireType[*values.Vector](v, wrapped, "test")
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "expected a vector"), qt.IsTrue,
		qt.Commentf("wrapped sentinel should still surface TypeName: %q", err.Error()))
}

// TestOptionalArg_ErrorMessageContainsTypeName pins the same plumbing for
// OptionalArg's "wrong type" path. OptionalArg delegates to RequireType
// internally; this test guards against a future refactor that breaks the
// delegation seam.
func TestOptionalArg_ErrorMessageContainsTypeName(t *testing.T) {
	c := qt.New(t)
	rest := &values.Pair{values.NewString("not-an-int"), values.EmptyList}
	defaultInt := values.NewInteger(0)
	_, err := OptionalArg[*values.Integer](rest, defaultInt, werr.ErrNotAnInteger, "test")
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "expected an integer"), qt.IsTrue,
		qt.Commentf("error %q missing phrase 'expected an integer'", err.Error()))
}

// TestVariadicArgs_ErrorMessageContainsTypeName pins the rest-loop error
// format in VariadicArgs. VariadicArgs has its own format string for the
// rest-loop type-mismatch (separate from RequireArg's path); both must
// independently produce the right phrase.
func TestVariadicArgs_ErrorMessageContainsTypeName(t *testing.T) {
	c := qt.New(t)
	// fixed=2: arg(0) is fixed (integer), arg(1) is rest (list of integers).
	// Put a string in the rest list to trigger the rest-loop error path.
	args := []values.Value{
		values.NewInteger(1),
		&values.Pair{values.NewString("bad"), values.EmptyList},
	}
	mc := &stubCallContext{args: args}
	_, err := VariadicArgs[*values.Integer](mc, 2, werr.ErrNotAnInteger, "test-prim")
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "expected an integer"), qt.IsTrue,
		qt.Commentf("error %q missing phrase 'expected an integer'", err.Error()))
}

// TestVariadicArgs_NonTypeSentinel_DegradedMessage pins the silent-degradation
// contract for VariadicArgs (same as RequireType's, but for VariadicArgs's
// independent format string). A future refactor that breaks
// typeNameFromSentinel for VariadicArgs would surface here.
func TestVariadicArgs_NonTypeSentinel_DegradedMessage(t *testing.T) {
	c := qt.New(t)
	args := []values.Value{
		values.NewInteger(1),
		&values.Pair{values.NewString("bad"), values.EmptyList},
	}
	mc := &stubCallContext{args: args}
	// werr.ErrFileNotFound is a NewStaticError sentinel, no TypeName.
	_, err := VariadicArgs[*values.Integer](mc, 2, werr.ErrFileNotFound, "test-prim")
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "expected  but got"), qt.IsTrue,
		qt.Commentf("non-type sentinel should produce empty type phrase: %q", err.Error()))
}

// TestRequireTuple_ErrorMessageContainsTypeName pins that RequireTuple's
// error message reads the type phrase from werr.ErrNotAList rather than
// hardcoding "a list". If ErrNotAList's TypeName changes (e.g., to
// "proper list"), the message updates automatically.
func TestRequireTuple_ErrorMessageContainsTypeName(t *testing.T) {
	c := qt.New(t)
	args := []values.Value{values.NewInteger(42)}
	mc := &stubCallContext{args: args}
	_, err := RequireTuple(mc, 0, "test-prim")
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "expected a list"), qt.IsTrue,
		qt.Commentf("error %q missing phrase 'expected a list'", err.Error()))
}

// TestRequireArg_PositionInIndex pins the 1-indexed argument-position format
// in RequireArg's wrapped error message. Future drift (e.g., re-collapsing
// RequireArg into RequireType for DRY) would silently lose the position.
func TestRequireArg_PositionInError(t *testing.T) {
	tcs := []struct {
		name       string
		index      int
		wantMarker string
	}{
		{"first arg", 0, "argument 1:"},
		{"second arg", 1, "argument 2:"},
		{"third arg", 2, "argument 3:"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			args := []values.Value{values.NewString("x"), values.NewString("y"), values.NewString("z")}
			mc := &stubCallContext{args: args}
			_, err := RequireArg[*values.Vector](mc, tc.index, werr.ErrNotAVector, "test-prim")
			c.Assert(err, qt.IsNotNil)
			c.Assert(strings.Contains(err.Error(), tc.wantMarker), qt.IsTrue,
				qt.Commentf("expected %q in %q", tc.wantMarker, err.Error()))
		})
	}
}

func TestOptionalArg(t *testing.T) {
	c := qt.New(t)
	defaultInt := values.NewInteger(0)

	tcs := []struct {
		name    string
		rest    values.Value
		wantVal int64
		wantErr error
	}{
		{
			name:    "no argument returns default",
			rest:    values.EmptyList,
			wantVal: 0,
		},
		{
			name:    "present and correct type",
			rest:    &values.Pair{values.NewInteger(42), values.EmptyList},
			wantVal: 42,
		},
		{
			name:    "wrong type returns sentinel error",
			rest:    &values.Pair{values.NewString("bad"), values.EmptyList},
			wantErr: werr.ErrNotAnInteger,
		},
		{
			name: "extra args rejected",
			rest: &values.Pair{
				values.NewInteger(1),
				&values.Pair{values.NewInteger(2), values.EmptyList},
			},
			wantErr: werr.ErrWrongNumberOfArguments,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := OptionalArg[*values.Integer](tc.rest, defaultInt, werr.ErrNotAnInteger, "test")
			if tc.wantErr != nil {
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.Value, qt.Equals, tc.wantVal)
		})
	}
}

func TestParseOptionalStartEnd_ExtraArgsRejected(t *testing.T) {
	c := qt.New(t)
	// (start end extra) — three ints in the rest list, one too many.
	rest := &values.Pair{
		values.NewInteger(1),
		&values.Pair{
			values.NewInteger(2),
			&values.Pair{values.NewInteger(99), values.EmptyList},
		},
	}
	_, _, err := ParseOptionalStartEnd(rest, 10, "test")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

func TestParseOptionalStartEnd_ImproperTailRejected(t *testing.T) {
	c := qt.New(t)
	// (start end . improper) — dotted pair after end is not a proper list.
	rest := &values.Pair{
		values.NewInteger(1),
		&values.Pair{
			values.NewInteger(2),
			values.NewInteger(99), // non-Tuple, non-empty tail
		},
	}
	_, _, err := ParseOptionalStartEnd(rest, 10, "test")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAList), qt.IsTrue,
		qt.Commentf("improper-list tail should produce ErrNotAList, not ErrWrongNumberOfArguments"))
}

func TestParseOptionalArg_ImproperTailRejected(t *testing.T) {
	c := qt.New(t)
	// (arg . improper) — dotted pair tail.
	rest := &values.Pair{
		values.NewInteger(1),
		values.NewInteger(2), // non-Tuple, non-empty tail
	}
	_, _, err := ParseOptionalArg(rest, "test")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAList), qt.IsTrue,
		qt.Commentf("improper-list tail should produce ErrNotAList, not ErrWrongNumberOfArguments"))
}

func TestRequireType_Failure_SentinelPreserved(t *testing.T) {
	c := qt.New(t)
	sentinels := []error{
		werr.ErrNotAVector,
		werr.ErrNotAString,
		werr.ErrNotAnInteger,
		werr.ErrNotACharacter,
		werr.ErrNotAPair,
		werr.ErrNotANumber,
		werr.ErrNotAByteVector,
		werr.ErrNotAHashtable,
		werr.ErrNotAProcedure,
		werr.ErrNotABox,
	}
	v := values.TrueValue
	for _, sentinel := range sentinels {
		_, err := RequireType[*values.Vector](v, sentinel, "test")
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, sentinel), qt.IsTrue, qt.Commentf("sentinel: %v", sentinel))
	}
}
