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
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Gates for the condition message split, the residual R7RS §4.3.3's delegation to
// error (§6.11) left open once S2 gave the irritants half: error-object-message
// answered the whole Go wrap chain, which fuses three different kinds of text —
// the route to the failure, the failure, and the sentinel's category.
//
// The rule under test: the message is the innermost werr.ForeignError's own
// message plus any non-sentinel error beneath it. Enclosing wraps and the
// sentinel's category are stripped; a root cause is not. The last gate here is
// the one that refutes the simpler "innermost message" rule on its own.

// TestSyntaxErrorMessageMatchesErrorForm is the conformance gate. §4.3.3 says
// syntax-error "behaves similarly to error (6.11)", so both of §6.11's accessors
// must answer the same pair the equivalent error call answers.
func TestSyntaxErrorMessageMatchesErrorForm(t *testing.T) {
	ctx := context.Background()
	eng, path := syntaxErrorEngine(t)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "syntax-error names the message alone",
			code: `(guard (e (#t (error-object-message e))) (load "` + path + `"))`,
			want: "expected a pair",
		},
		{
			// The baseline the row above must match. Same message, same irritants,
			// produced the way that always worked.
			name: "error is the baseline (control)",
			code: `(guard (e (#t (error-object-message e))) (error "expected a pair" 'hello 42))`,
			want: "expected a pair",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			s, ok := v.Internal().(*values.String)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
			c.Assert(s.Value, qt.Equals, tc.want)
		})
	}

	t.Run("the irritants still agree", func(t *testing.T) {
		c := qt.New(t)
		v, err := eng.EvalMultiple(ctx,
			`(guard (e (#t (error-object-irritants e))) (load "`+path+`"))`)
		c.Assert(err, qt.IsNil)
		c.Assert(v.Internal().SchemeString(), qt.Equals, "(hello 42)")
	})
}

// TestCompileConditionMessageIsTheFailureAlone: the compile funnel's condition
// keeps the failure and drops the route to it. The location is not deleted — it
// rides on the condition's own SourceLocation, which is the whole reason S3 stamps
// it there — so repeating it inside the message is the same duplication V3 closed
// for the Go envelope.
func TestCompileConditionMessageIsTheFailureAlone(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	expr, err := eng.ParseWithSource(ctx, `(car)`, "<eval>")
	c.Assert(err, qt.IsNil)
	_, err = eng.Compile(ctx, expr)
	c.Assert(err, qt.IsNotNil)

	var ce *wile.CompilationError
	c.Assert(errors.As(err, &ce), qt.IsTrue, qt.Commentf("got %T", err))
	ne, ok := ce.Condition.Internal().(*values.NativeError)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", ce.Condition.Internal()))

	msg := ne.Message().Value
	c.Assert(msg, qt.Contains, "expected 1 argument(s), got 0")
	c.Assert(msg, qt.Not(qt.Contains), "compilation:",
		qt.Commentf("the route to the failure, not the failure: %q", msg))
	c.Assert(msg, qt.Not(qt.Contains), ce.Source,
		qt.Commentf("the location rides on SourceLocation: %q", msg))
	c.Assert(msg, qt.Not(qt.Contains), "wrong number of arguments",
		qt.Commentf("the sentinel's category, not the message: %q", msg))

	// Narrowing the accessor must not narrow the chain: the envelope's rendering
	// and its sentinel matching are both untouched.
	c.Assert(ce.Error(), qt.Contains, "wrong number of arguments")
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

// TestFileErrorMessageKeepsItsCause is the row that refutes "the innermost wrap's
// message" as a rule on its own. WrapForeignFileError puts the OS error in the
// same slot a sentinel would occupy, so a walk that stops at the innermost wrap
// deletes the only substantive fact in the diagnostic.
func TestFileErrorMessageKeepsItsCause(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t (error-object-message e))) (open-input-file "/nope/nope/nope"))`)
	c.Assert(err, qt.IsNil)
	s, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	c.Assert(s.Value, qt.Contains, "no such file or directory",
		qt.Commentf("the OS error is the root cause, not a category label: %q", s.Value))
	c.Assert(s.Value, qt.Contains, "/nope/nope/nope")
}

// TestRuntimeConditionMessageDropsTheCategory covers the shape the sentinel
// occupies the err slot: the category text is what errors.Is is for, and it is not
// part of the message R7RS specifies.
func TestRuntimeConditionMessageDropsTheCategory(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	tcs := []struct {
		name     string
		code     string
		want     string
		unwanted string
	}{
		{
			name:     "sentinel in the err slot",
			code:     `(car 5)`,
			want:     "car: expected pair, got 5",
			unwanted: "not a pair",
		},
		{
			// Two wraps deep, and BOTH survive: the outer one names the primitive
			// the program called, the inner one the operation that failed inside
			// it. Only the sentinel comes off. A rule that reduced to the innermost
			// wrap would read better here and would delete "file-exists?" from the
			// contract validator's message — see TestContractEnforcement_EndToEnd.
			name:     "nested wraps both survive; only the category comes off",
			code:     `(/ 1 0)`,
			want:     "/: division error: Integer.Divide: division by exact zero",
			unwanted: ": division by zero",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v, err := eng.EvalMultiple(ctx,
				`(guard (e (#t (error-object-message e))) `+tc.code+`)`)
			c.Assert(err, qt.IsNil)
			s, ok := v.Internal().(*values.String)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
			c.Assert(s.Value, qt.Equals, tc.want)
			c.Assert(strings.Contains(s.Value, tc.unwanted), qt.IsFalse,
				qt.Commentf("got %q", s.Value))
		})
	}
}
