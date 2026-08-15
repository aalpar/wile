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
