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
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The gate for N-1 slice S1: a compile failure that crosses a primitive frame
// reaches Scheme carrying the location it failed AT, not the location of the
// call that triggered the compile.
//
// Measured before the fix: error-object-source on a load-caught compile error
// answered the load call site, while the real location existed only inside the
// message string. One accessor, two meanings, decided by whether the failure had
// a location before or after it crossed the primitive frame — the sharpest
// argument for unifying the two error representations, and the reason S1 ships
// ahead of the rest of N-1.
//
// enrichNativeError stamps the raise site only when the condition has no
// location yet, so pre-stamping here is idempotent by construction: the compile
// site wins and the load site is never written.

// badCompileEngine writes a file whose fourth line references an unbound
// variable and returns an engine plus that file's path. The error is a compile
// error, not a parse error, so it travels the SourcedError chain.
func badCompileEngine(t *testing.T) (*wile.Engine, string) {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "bad.scm")
	src := "(define (a b)\n" +
		"  b)\n" +
		"(define (c d)\n" +
		"  (nope d))\n"
	err := os.WriteFile(path, []byte(src), 0o600)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng, path
}

func TestLoadCompileErrorReportsCompileSite(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, path := badCompileEngine(t)

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t (error-object-source e))) (load "`+path+`"))`)
	c.Assert(err, qt.IsNil)
	src, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	c.Assert(src.Value, qt.Equals, path+":4:3",
		qt.Commentf("error-object-source must name the unbound reference, not the load call"))
}

// The Go half of the same gate: the condition a Scheme guard catches still
// reaches the innermost SourcedError by errors.As, so an embedder can recover
// the structured location rather than parsing it back out of the message.
func TestLoadCompileErrorConditionReachesSourcedError(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, path := badCompileEngine(t)

	_, err := eng.EvalMultiple(ctx, `(load "`+path+`")`)
	c.Assert(err, qt.IsNotNil)

	var re *wile.RuntimeError
	c.Assert(errors.As(err, &re), qt.IsTrue, qt.Commentf("got %T", err))
	c.Assert(re.Condition, qt.IsNotNil)

	ne, ok := re.Condition.Internal().(*values.NativeError)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", re.Condition.Internal()))
	c.Assert(ne.SourceLocation(), qt.Equals, path+":4:3")

	var se *compilation.SourcedError
	c.Assert(errors.As(error(ne), &se), qt.IsTrue,
		qt.Commentf("the condition no longer unwraps to the compiler's own error"))
	c.Assert(se.Source.Location(), qt.Not(qt.Equals), "")
}

// badParseEngine writes a file whose last form is an unterminated list and
// returns an engine plus that file's path. The failure is a READ error, so it
// carries its position on a *parser.ParserError token and never acquires a
// *compilation.SourcedError — the parser sits below the compiler and stamps
// nothing. That is the whole difference from badCompileEngine.
//
// The complete form ahead of it binds nothing on purpose. Both arms of the
// agreement test evaluate this file on one engine, and the top level is
// immutable by default, so a define here would make the SECOND arm fail on
// "cannot redefine immutable top-level binding" — at the define's own location,
// which is a plausible-looking answer to the question the test is asking.
func badParseEngine(t *testing.T) (*wile.Engine, string) {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "bad_parse.scm")
	src := "(+ 1 2)\n" +
		"(display (list 1 2\n"
	err := os.WriteFile(path, []byte(src), 0o600)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng, path
}

// The parse half of the same rule, and it is deliberately an AGREEMENT test:
// both arms report the same failure in the same file, so an answer that differs
// between them is wrong on one of them whatever it says. Both are pinned to the
// literal position as well, so a regression that breaks both the same way cannot
// pass by staying symmetric.
//
// Measured before the fix, and red on exactly one arm: the Go embedder answered
// bad_parse.scm:2:9 while (guard … (load …)) answered the location of the LOAD
// call. innermostCompileLocation keyed on the sourcedError interface alone, so
// the parser's position was invisible to it and the condition reached
// enrichNativeError unstamped, taking the raise site.
//
// The Go arm gets it right only because wrapCompilationError carries an explicit
// *parser.ParserError fallback, and nothing on the Scheme path reaches that
// function: extensions/eval sits below pkg/wile, so PrimLoad calls
// compilation.ExpandAndCompile directly.
func TestLoadParseErrorReportsReadSiteOnBothArms(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, path := badParseEngine(t)

	// Read the fixture back rather than restating it, so the Go arm is handed the
	// exact bytes load will read.
	src, err := os.ReadFile(path)
	c.Assert(err, qt.IsNil)
	_, err = eng.EvalMultipleWithSource(ctx, string(src), path)
	c.Assert(err, qt.IsNotNil)
	var ce *wile.CompilationError
	c.Assert(errors.As(err, &ce), qt.IsTrue, qt.Commentf("got %T", err))
	// Column 9 on line 2 is the '(' of (list — the innermost list still open at
	// end of input, not the outer (display.
	c.Assert(ce.Source, qt.Equals, path+":2:9")

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t (error-object-source e))) (load "`+path+`"))`)
	c.Assert(err, qt.IsNil)
	got, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	c.Assert(got.Value, qt.Equals, ce.Source,
		qt.Commentf("error-object-source must name the unterminated list, not the load call"))
}

// The same fallback reaches read, because ConditionFromError is the one
// converter for every primitive error — and that knock-on is deliberate, so it
// is pinned rather than left to be rediscovered as a surprise. A read failure
// names the position IN THE PORT; before the fallback it named the (read …)
// call, which is one location for every datum a loop reads and therefore no
// location at all.
//
// The answer has no file, and that is a SEPARATE gap, not this rule misfiring:
// PrimRead builds its parser with parser.NewParser rather than
// NewParserWithFile, because values.PortObject carries no name to hand it. A
// successful read from a file port already yields syntax with no file for the
// same reason. Filed in TODO.md under Tier 2.
func TestReadErrorReportsPortPositionNotTheReadCall(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t (error-object-source e))) (read (open-input-string "(1 2")))`)
	c.Assert(err, qt.IsNil)
	got, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	// Line 1, column 0 of the port: the '(' of the list left open at end of
	// input. No leading colon — ParserError.Location omits it for unnamed input,
	// unlike SourceContext.Location.
	c.Assert(got.Value, qt.Equals, "1:0")
}
