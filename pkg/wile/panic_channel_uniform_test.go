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

package wile_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The gate for wave 4 item 5+7 half C: a Go-level bug escapes guard from EITHER
// registration route, and both report it the same way.
//
// Half C got there by TIGHTENING. The 2026-06-23 directive — a panic stays
// within the VM boundary, and a Go bug is not a Scheme condition — is unamended;
// what changed is that ffiSpec.makeWrapper's recover stopped being a second
// answer to it. It now converts only the deliberate callback protocol, which
// panics because the host's Go signature has no error slot to return through,
// and re-raises everything else.
//
// The two routes disagreed before that, and the disagreement was never decided:
// it fell out of where the recover happened to sit. RegisterPrimitive takes the
// ForeignFunction as given, so its panics always reached the VM boundary;
// RegisterFunc wrapped the function, so its panics were caught one frame in and
// came back as guard-catchable Scheme conditions.

// hostBug trips a genuine runtime.Error rather than panicking with a string, so
// the fault is the shape an embedder actually ships by accident.
func hostBug() int64 {
	empty := []int64{}
	return empty[1]
}

// uniformPanicEngine registers ONE faulting Go body under both routes.
//
// Registering the same body twice is what makes the pair non-vacuous: the two
// rows below cannot diverge because of what the function does, only because of
// how it was registered.
func uniformPanicEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})

	err = eng.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "prim-host-bug",
		ParamCount: 0,
		Impl: func(cc wile.CallContext) error {
			cc.SetValue(values.NewInteger(hostBug()))
			return nil
		},
	})
	if err != nil {
		t.Fatalf("RegisterPrimitive: %v", err)
	}

	err = eng.RegisterFunc("ffi-host-bug", hostBug)
	if err != nil {
		t.Fatalf("RegisterFunc: %v", err)
	}
	return eng
}

// TestHostBugEscapesGuardFromEitherRoute is G8.3, and it asserts at the
// EMBEDDER BOUNDARY — the *wile.RuntimeError an Engine.EvalMultiple caller gets
// back — not at any internal layer. Which layer a gate speaks for is the thing
// that made this item hard to schedule, so it is stated rather than implied.
//
// Four properties, all four of which the FFI row failed before half C: it was
// caught by guard, and when it did reach the embedder it arrived as a Scheme
// exception (Condition non-nil, because the wrapper's returned error routed
// through bridgeForeignError) with empty structured fields.
func TestHostBugEscapesGuardFromEitherRoute(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := uniformPanicEngine(t)

	tcs := []struct {
		name string
		call string
		// want is the registered name the diagnostic must carry. An embedder
		// reading it has no other way to learn which of its primitives faulted:
		// the panic unwind destroys the Go frame, and the source location names
		// the call site, not the callee.
		want string
	}{
		{name: "RegisterPrimitive", call: "(prim-host-bug)", want: "prim-host-bug"},
		{name: "RegisterFunc", call: "(ffi-host-bug)", want: "ffi-host-bug"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := eng.EvalMultiple(ctx, `(guard (e (#t 'caught)) `+tc.call+`)`)
			c.Assert(err, qt.IsNotNil,
				qt.Commentf("a host bug became guard-catchable via %s", tc.name))

			var re *wile.RuntimeError
			c.Assert(errors.As(err, &re), qt.IsTrue)

			c.Assert(re.IsSchemeException(), qt.IsFalse,
				qt.Commentf("a host bug presented itself as a Scheme condition"))
			c.Assert(strings.Contains(re.Message, tc.want), qt.IsTrue,
				qt.Commentf("diagnostic does not name the primitive: %q", re.Message))
			c.Assert(re.Source, qt.Not(qt.Equals), "",
				qt.Commentf("no Scheme source location on the returned error"))
		})
	}
}

// TestCallbackProtocolStillConverts is G8.4: the stop that keeps half C from
// over-tightening into "the FFI wrapper has no recover at all".
//
// The callback protocol panics deliberately — callbackErrorResult has no error
// slot in the host's Go signature to return through — so that panic IS a return
// path and must still become an ordinary error. Scheme guard must still catch
// it. Without this row, deleting the recover outright would pass G8.3.
func TestCallbackProtocolStillConverts(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := uniformPanicEngine(t)

	err := eng.RegisterFunc("apply-callback", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	c.Assert(err, qt.IsNil)

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t 'caught)) (apply-callback (lambda (x) (/ x 0)) 5))`)
	c.Assert(err, qt.IsNil,
		qt.Commentf("a callback fault stopped being catchable: the recover over-tightened"))
	c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewSymbol("caught"))
}

// TestSchemeConditionsStillClassifyAsExceptions guards the ORDERING half C
// introduced in Engine.wrapRuntimeError.
//
// Populating Source/StackTrace on the recovered-panic path meant unpacking a
// *machine.SchemeError, and that arm now runs BEFORE the ErrExceptionEscape arm.
// It is safe only because the escape carrier never travels inside a
// *machine.SchemeError — measured, not assumed. If it ever did, every row here
// would flip to IsSchemeException() == false and a raise would start reporting
// itself as a host fault, with Condition silently dropped.
//
// Nothing else can see that: the rows below are all ordinary catchable errors,
// so a Scheme-level guard around them would still catch, and only the Go-side
// classification changes.
func TestSchemeConditionsStillClassifyAsExceptions(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := uniformPanicEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "raise a symbol", code: `(raise 'boom)`},
		{name: "raise a list", code: `(raise (list 1 2))`},
		{name: "error with irritants", code: `(error "explicit" 1 2)`},
		{name: "primitive domain error", code: `(car 5)`},
		{name: "index out of range", code: `(vector-ref (vector 1 2) 9)`},
		{name: "division by zero", code: `(/ 1 0)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNotNil)

			var re *wile.RuntimeError
			c.Assert(errors.As(err, &re), qt.IsTrue)
			c.Assert(re.IsSchemeException(), qt.IsTrue,
				qt.Commentf("a Scheme condition was classified as a host fault: "+
					"wrapRuntimeError's *machine.SchemeError arm shadowed the escape arm"))
		})
	}
}

// cfgStruct is a RegisterFunc parameter converted from a Scheme alist.
type cfgStruct struct {
	Alpha int64
}

// TestStructConverterRejectsEmptyAlistEntry covers the one live instance the
// half-C prerequisite sweep turned up: makeStructArgConverter asserted
// values.Tuple on each alist element and then called Car() on it, and the empty
// list satisfies values.Tuple while panicking on Car.
//
// It is in-scope for half C specifically because narrowing the wrapper's recover
// changes what this fault costs. It was a caller mistake reported as a
// guard-catchable condition by accident, via the blanket recover; without the
// explicit rejection it would now escape guard as if it were a host bug. Same
// shape and same fix as helpers.AssocLookup, which Phase 6 closed.
func TestStructConverterRejectsEmptyAlistEntry(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := uniformPanicEngine(t)

	err := eng.RegisterFunc("take-cfg", func(v cfgStruct) int64 {
		return v.Alpha
	})
	c.Assert(err, qt.IsNil)

	t.Run("caught by guard", func(t *testing.T) {
		v, evalErr := eng.EvalMultiple(ctx, `(guard (e (#t 'caught)) (take-cfg '(())))`)
		c.Assert(evalErr, qt.IsNil)
		c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewSymbol("caught"))
	})

	t.Run("well-formed alist still converts (control)", func(t *testing.T) {
		v, evalErr := eng.EvalMultiple(ctx, `(take-cfg '((Alpha . 7)))`)
		c.Assert(evalErr, qt.IsNil)
		c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewInteger(7))
	})
}
