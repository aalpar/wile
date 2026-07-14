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

package werr_test

import (
	"errors"
	"fmt"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

func TestForeignError_EqualTo(t *testing.T) {
}

func TestForeignFileError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
	c.Assert(fileErr.Op, qt.Equals, "open-input-file")
}

func TestForeignFileError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	outer := fmt.Errorf("wrapped: %w", inner)
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(outer, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
}

func TestForeignFileError_NotDetectedAsReadError(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsFalse)
}

func TestForeignReadError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
}

func TestForeignReadError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	outer := fmt.Errorf("wrapped: %w", inner)
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(outer, &readErr), qt.IsTrue)
}

func TestForeignReadError_NotDetectedAsFileError(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsFalse)
}

func TestNewForeignReadErrorf(t *testing.T) {
	c := qt.New(t)
	err := werr.NewForeignReadErrorf("parse error at %d", 42)
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
	c.Assert(err.Error(), qt.Matches, ".*parse error at 42.*")
}

func TestForeignFileError_ErrorsIs_Cause(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("permission denied")
	err := werr.WrapForeignFileError(cause, "open-input-file", "/etc/shadow")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignReadError_ErrorsIs_Cause(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("unexpected )")
	err := werr.WrapForeignReadErrorf(cause, "read error")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignError_Is(t *testing.T) {
	c := qt.New(t)
	sentinel := werr.NewStaticError("test sentinel")
	cause := fmt.Errorf("root cause")

	tcs := []struct {
		name   string
		err    *werr.ForeignError
		target error
		want   bool
	}{
		{"sentinel match", werr.WrapForeignErrorf(sentinel, "msg"), sentinel, true},
		{"cause match", werr.WrapForeignErrorWithCause(sentinel, cause, "msg"), cause, true},
		{"sentinel via cause constructor", werr.WrapForeignErrorWithCause(sentinel, cause, "msg"), sentinel, true},
		{"no match", werr.WrapForeignErrorf(sentinel, "msg"), fmt.Errorf("other"), false},
		{"nil sentinel and cause", werr.NewForeignErrorf("msg"), sentinel, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(errors.Is(tc.err, tc.target), qt.Equals, tc.want)
		})
	}
}

func TestForeignError_As(t *testing.T) {
	c := qt.New(t)

	// ForeignFileError detected through embedded ForeignError
	fileErr := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open", "/tmp/x")
	wrapped := fmt.Errorf("outer: %w", fileErr)
	var target *werr.ForeignFileError
	c.Assert(errors.As(wrapped, &target), qt.IsTrue)
	c.Assert(target.Filename, qt.Equals, "/tmp/x")

	// ForeignReadError not detected through ForeignFileError
	var readTarget *werr.ForeignReadError
	c.Assert(errors.As(wrapped, &readTarget), qt.IsFalse)
}

func TestForeignError_Cause(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		err  *werr.ForeignError
		want error
	}{
		{"with cause", werr.WrapForeignErrorWithCause(werr.ErrNotANumber, fmt.Errorf("root"), "msg"), fmt.Errorf("root")},
		{"without cause", werr.WrapForeignErrorf(werr.ErrNotANumber, "msg"), nil},
		{"nil receiver", nil, nil},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.err.Cause()
			if tc.want == nil {
				c.Assert(got, qt.IsNil)
			} else {
				c.Assert(got.Error(), qt.Equals, tc.want.Error())
			}
		})
	}
}

func TestForeignProcessError(t *testing.T) {
	c := qt.New(t)

	t.Run("wraps with command context", func(t *testing.T) {
		cause := fmt.Errorf("exec: not found")
		err := werr.WrapForeignProcessError(cause, "process-spawn", "nonexistent-cmd")
		c.Assert(err.Op, qt.Equals, "process-spawn")
		c.Assert(err.Command, qt.Equals, "nonexistent-cmd")
		c.Assert(err.Error(), qt.Matches, `.*process-spawn.*nonexistent-cmd.*`)
	})

	t.Run("unwraps to cause", func(t *testing.T) {
		cause := fmt.Errorf("signal: killed")
		err := werr.WrapForeignProcessError(cause, "process-wait", "sleep")
		c.Assert(errors.Is(err, cause), qt.IsTrue)
	})
}

// TestTypeSentinelsCarryTypeName pins the type-sentinel inventory: every
// sentinel in this list must report a non-empty TypeName. If a sentinel is
// constructed via NewStaticError instead of NewTypeSentinel, registry/helpers
// argument-extraction errors would emit "expected  but got *Foo" — silent
// degradation, not a compile error. This test catches that mechanically.
//
// When adding a new type sentinel, append it here. When changing an existing
// non-type sentinel into a type sentinel, append it here too.
func TestTypeSentinelsCarryTypeName(t *testing.T) {
	typeSentinels := map[string]*werr.StaticError{
		"ErrNotABoolean":               werr.ErrNotABoolean,
		"ErrNotAnInputPort":            werr.ErrNotAnInputPort,
		"ErrNotAnOutputPort":           werr.ErrNotAnOutputPort,
		"ErrNotABox":                   werr.ErrNotABox,
		"ErrNotAnOpaqueValue":          werr.ErrNotAnOpaqueValue,
		"ErrNotAByte":                  werr.ErrNotAByte,
		"ErrNotAByteInputPort":         werr.ErrNotAByteInputPort,
		"ErrNotAByteOutputPort":        werr.ErrNotAByteOutputPort,
		"ErrNotATextualPort":           werr.ErrNotATextualPort,
		"ErrNotAPrimitive":             werr.ErrNotAPrimitive,
		"ErrNotANumber":                werr.ErrNotANumber,
		"ErrNotAReal":                  werr.ErrNotAReal,
		"ErrNotAList":                  werr.ErrNotAList,
		"ErrNotAMachineContext":        werr.ErrNotAMachineContext,
		"ErrNotAPair":                  werr.ErrNotAPair,
		"ErrNotACons":                  werr.ErrNotACons,
		"ErrNotACharacter":             werr.ErrNotACharacter,
		"ErrNotACharSet":               werr.ErrNotACharSet,
		"ErrNotASyntaxValue":           werr.ErrNotASyntaxValue,
		"ErrNotASyntaxPair":            werr.ErrNotASyntaxPair,
		"ErrNotASyntaxSymbol":          werr.ErrNotASyntaxSymbol,
		"ErrNotASyntaxList":            werr.ErrNotASyntaxList,
		"ErrNotASyntaxObject":          werr.ErrNotASyntaxObject,
		"ErrNotASymbol":                werr.ErrNotASymbol,
		"ErrNotAClosure":               werr.ErrNotAClosure,
		"ErrNotAnInteger":              werr.ErrNotAnInteger,
		"ErrNotALocalEnvironmentFrame": werr.ErrNotALocalEnvironmentFrame,
		"ErrNotAMachineTemplate":       werr.ErrNotAMachineTemplate,
		"ErrNotAString":                werr.ErrNotAString,
		"ErrNotANamespace":             werr.ErrNotANamespace,
		"ErrNotAVector":                werr.ErrNotAVector,
		"ErrNotAByteVector":            werr.ErrNotAByteVector,
		"ErrNotAProcedure":             werr.ErrNotAProcedure,
		"ErrNotAParameter":             werr.ErrNotAParameter,
		"ErrNotAStringOutputPort":      werr.ErrNotAStringOutputPort,
		"ErrNotABytevectorOutputPort":  werr.ErrNotABytevectorOutputPort,
		"ErrNotANativeError":           werr.ErrNotANativeError,
		"ErrNotARecord":                werr.ErrNotARecord,
		"ErrNotARecordType":            werr.ErrNotARecordType,
		"ErrNotAThread":                werr.ErrNotAThread,
		"ErrNotAMutex":                 werr.ErrNotAMutex,
		"ErrNotAConditionVariable":     werr.ErrNotAConditionVariable,
		"ErrNotATime":                  werr.ErrNotATime,
		"ErrNotAChannel":               werr.ErrNotAChannel,
		"ErrNotAWaitGroup":             werr.ErrNotAWaitGroup,
		"ErrNotARWMutex":               werr.ErrNotARWMutex,
		"ErrNotAOnce":                  werr.ErrNotAOnce,
		"ErrNotAnAtomic":               werr.ErrNotAnAtomic,
		"ErrNotAHashtable":             werr.ErrNotAHashtable,
		"ErrNotAMatch":                 werr.ErrNotAMatch,
		"ErrNotAPromptTag":             werr.ErrNotAPromptTag,
		"ErrNotAContinuationMarkSet":   werr.ErrNotAContinuationMarkSet,
		"ErrNotAContinuation":          werr.ErrNotAContinuation,
		"ErrNotAnErrorContext":         werr.ErrNotAnErrorContext,
		"ErrNotAProcess":               werr.ErrNotAProcess,
	}
	for name, s := range typeSentinels {
		t.Run(name, func(t *testing.T) {
			if s.TypeName() == "" {
				t.Errorf("%s missing TypeName — was NewTypeSentinel skipped? Error()=%q",
					name, s.Error())
			}
		})
	}
}

// TestNewTypeSentinel_AutoArticle pins the article-selection rule:
// "an" before vowel letters, "a" before everything else, with
// pass-through for inputs already starting with "a "/"an ".
func TestNewTypeSentinel_AutoArticle(t *testing.T) {
	tcs := []struct {
		name           string
		noun           string
		wantTypeName   string
		wantErrMessage string
	}{
		{"consonant initial", "string", "a string", "not a string"},
		{"vowel initial i", "integer", "an integer", "not an integer"},
		{"vowel initial e", "error object", "an error object", "not an error object"},
		{"vowel initial o", "output port", "an output port", "not an output port"},
		{"vowel initial a", "atomic", "an atomic", "not an atomic"},
		{"hyphenated consonant", "char-set", "a char-set", "not a char-set"},
		{"capitalized vowel", "Integer", "an Integer", "not an Integer"},
		{"capitalized consonant", "String", "a String", "not a String"},
		{"pass-through a", "a once", "a once", "not a once"},
		{"pass-through an", "an apple", "an apple", "not an apple"},
		// Phonetic exceptions the letter rule does NOT handle.
		// These cases produce grammatically wrong but unambiguous output;
		// callers needing the correct article must use the pass-through
		// form (e.g. NewTypeSentinel("an hour"), NewTypeSentinel("a user")).
		{"phonetic exception hour (silent h, letter rule wrong)", "hour", "a hour", "not a hour"},
		{"phonetic exception user (yu sound, letter rule wrong)", "user", "an user", "not an user"},
		// Non-letter leading characters: the byte-indexed switch returns "a"
		// for anything that isn't a vowel letter. These cases pin the
		// behavior for digit and punctuation initials.
		{"digit leading", "8-byte value", "a 8-byte value", "not a 8-byte value"},
		{"hyphen leading", "-prefixed type", "a -prefixed type", "not a -prefixed type"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			s := werr.NewTypeSentinel(tc.noun)
			if s.TypeName() != tc.wantTypeName {
				t.Errorf("TypeName(): got %q want %q", s.TypeName(), tc.wantTypeName)
			}
			if s.Error() != tc.wantErrMessage {
				t.Errorf("Error(): got %q want %q", s.Error(), tc.wantErrMessage)
			}
		})
	}
}

// TestNewTypeSentinel_EmptyNounPanics pins that NewTypeSentinel rejects
// empty nouns at construction time. A type sentinel with empty noun
// would produce "expected " in error messages — the same shape as the
// documented silent-degradation path for non-type sentinels — but with
// a different cause. Panicking surfaces the misuse loudly at init time.
func TestNewTypeSentinel_EmptyNounPanics(t *testing.T) {
	defer func() {
		r := recover()
		if r == nil {
			t.Errorf("NewTypeSentinel(\"\") should panic, but did not")
		}
	}()
	werr.NewTypeSentinel("")
}

// TestNewStaticError_NoTypeName pins the contract that NewStaticError
// produces sentinels without a TypeName, distinguishing them from
// NewTypeSentinel for purposes of the registry/helpers type-extraction logic.
func TestNewStaticError_NoTypeName(t *testing.T) {
	s := werr.NewStaticError("some condition")
	if s.TypeName() != "" {
		t.Errorf("NewStaticError sentinel should have empty TypeName, got %q", s.TypeName())
	}
}

func TestWrapForeignErrorWithCause(t *testing.T) {
	c := qt.New(t)
	sentinel := werr.NewStaticError("test sentinel")
	cause := fmt.Errorf("disk full")

	err := werr.WrapForeignErrorWithCause(sentinel, cause, "write failed: %s", "/tmp/out")
	c.Assert(errors.Is(err, sentinel), qt.IsTrue)
	c.Assert(errors.Is(err, cause), qt.IsTrue)
	c.Assert(err.Cause(), qt.Equals, cause)
	c.Assert(err.Error(), qt.Matches, ".*write failed.*test sentinel.*")
}

// TestWrapForeign_PercentLiteralNoVararg pins that the wrap constructors treat
// the message as a literal when no varargs are supplied, mirroring the guard
// NewForeignErrorf already has. The probe uses a literal "%%": stored verbatim
// (guard active) it stays "100%% done"; run through fmt.Sprintf (guard absent)
// it collapses to "100% done". Asserting the doubled percent survives therefore
// proves Sprintf was skipped. Using "%%" — a valid, constant format directive —
// keeps go vet's printf analyzer satisfied where a lone '%' would not.
func TestWrapForeign_PercentLiteralNoVararg(t *testing.T) {
	sentinel := werr.NewStaticError("boom")
	cause := werr.NewStaticError("root cause")

	tcs := []struct {
		name string
		err  error
		want string // literal substring that must survive intact
	}{
		{"WrapForeignErrorf no vararg", werr.WrapForeignErrorf(sentinel, "100%% done"), "100%% done"},
		{"WrapForeignErrorf with vararg still formats", werr.WrapForeignErrorf(sentinel, "value %d", 42), "value 42"},
		{"WrapForeignErrorWithCause no vararg", werr.WrapForeignErrorWithCause(sentinel, cause, "100%% done"), "100%% done"},
		{"nil sentinel delegates to NewForeignErrorf", werr.WrapForeignErrorf(nil, "100%% done"), "100%% done"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.err.Error(), qt.Contains, tc.want)
		})
	}
}

// TestRecoverAsError_NilIsNil pins that a no-panic recover() yields no error,
// so callers can assign the result unconditionally in a deferred closure.
func TestRecoverAsError_NilIsNil(t *testing.T) {
	qt.Assert(t, werr.RecoverAsError(nil, werr.ErrInternal, "site"), qt.IsNil)
}

// TestRecoverAsError_ErrorAcquiresSentinelAndKeepsIdentity pins the two halves of
// the error-panic contract at once, because they pull against each other: the
// recovered error must acquire the site's sentinel (so a Go runtime fault in
// foreign code is identifiable as one), AND its own identity must stay matchable
// through the wrap (so the VM's foreign-call bridge still routes signal types).
// Chaining as a cause is what satisfies both; returning the error unchanged, as
// this function once did, satisfies only the second.
func TestRecoverAsError_ErrorAcquiresSentinelAndKeepsIdentity(t *testing.T) {
	sentinel := werr.NewStaticError("original")
	original := werr.WrapForeignErrorf(sentinel, "boom")

	got := werr.RecoverAsError(original, werr.ErrPanicRecovery, "site")

	qt.Assert(t, errors.Is(got, werr.ErrPanicRecovery), qt.IsTrue,
		qt.Commentf("an error panic value must acquire the site's sentinel"))
	qt.Assert(t, errors.Is(got, sentinel), qt.IsTrue,
		qt.Commentf("the recovered error's own identity must survive the wrap"))
	qt.Assert(t, got.Error(), qt.Contains, "site")
	qt.Assert(t, got.Error(), qt.Contains, "boom",
		qt.Commentf("the recovered error's text must survive into the message"))
}

// TestRecoverAsError_RuntimeErrorCarriesSentinel is the regression this whole
// contract exists for. A nil deref / index-out-of-range inside foreign code arrives
// at recover() as a runtime.Error, which satisfies error — so under the old
// pass-through rule it kept no sentinel at all, and errors.Is(err, ErrPanicRecovery)
// could not distinguish host memory corruption from a deliberate (error "...").
// The old test suite never saw this: every case it covered panicked with a non-error.
func TestRecoverAsError_RuntimeErrorCarriesSentinel(t *testing.T) {
	var runtimeErr error
	func() {
		defer func() {
			runtimeErr = werr.RecoverAsError(recover(), werr.ErrPanicRecovery, "FFI \"boom\"")
		}()
		// Index computed at runtime: a constant index into a statically-known
		// slice is diagnosed by the nilness/bounds analyzers before it can fault.
		vs := make([]int, 1)
		_ = vs[len(vs)+2]
	}()

	qt.Assert(t, runtimeErr, qt.IsNotNil)
	qt.Assert(t, errors.Is(runtimeErr, werr.ErrPanicRecovery), qt.IsTrue,
		qt.Commentf("a Go runtime fault must be identifiable as a recovered panic"))

	var re runtime.Error
	qt.Assert(t, errors.As(runtimeErr, &re), qt.IsTrue,
		qt.Commentf("errors.As must still reach the runtime.Error through the wrap — "+
			"this is the traversal the VM signal types rely on"))
	qt.Assert(t, runtimeErr.Error(), qt.Contains, "FFI \"boom\"",
		qt.Commentf("the faulting foreign function must be named"))
	qt.Assert(t, runtimeErr.Error(), qt.Contains, "out of range")
}

// TestRecoverAsError_NonErrorWrapsUnderCallerSentinel pins that the fallback
// sentinel is the caller's, not a hardcoded one. The three recover sites use three
// different sentinels (ErrInternal, ErrThreadPanic, ErrPanicRecovery) and each is
// part of that site's contract.
func TestRecoverAsError_NonErrorWrapsUnderCallerSentinel(t *testing.T) {
	got := werr.RecoverAsError(42, werr.ErrPanicRecovery, "foreign function call")

	qt.Assert(t, got, qt.IsNotNil)
	qt.Assert(t, errors.Is(got, werr.ErrPanicRecovery), qt.IsTrue)
	qt.Assert(t, errors.Is(got, werr.ErrInternal), qt.IsFalse)
	qt.Assert(t, got.Error(), qt.Contains, "foreign function call")
	qt.Assert(t, got.Error(), qt.Contains, "42",
		qt.Commentf("the panic value's text must survive into the message"))
}

// TestRecoverAsError_NonErrorStringValue covers the panic("...") shape specifically,
// which is what the FFI re-raise threw before it was deleted.
func TestRecoverAsError_NonErrorStringValue(t *testing.T) {
	got := werr.RecoverAsError("kaboom", werr.ErrThreadPanic, "thread \"w\"")

	qt.Assert(t, errors.Is(got, werr.ErrThreadPanic), qt.IsTrue)
	qt.Assert(t, got.Error(), qt.Contains, "kaboom")
	qt.Assert(t, got.Error(), qt.Contains, "thread \"w\"")
}

// TestRecoverAsError_EmptyContextOmitsPrefix pins the escape hatch for callers that
// wrap the result again with their own site prefix (RunResumable does), so the site
// name does not appear twice in the rendered message.
func TestRecoverAsError_EmptyContextOmitsPrefix(t *testing.T) {
	got := werr.RecoverAsError(42, werr.ErrInternal, "")

	qt.Assert(t, errors.Is(got, werr.ErrInternal), qt.IsTrue)
	qt.Assert(t, got.Error(), qt.Contains, "non-error panic value: 42")
	qt.Assert(t, got.Error(), qt.Not(qt.Contains), ": non-error panic value: 42: ")
}
