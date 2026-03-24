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

package process_test

import (
	"context"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	extprocess "github.com/aalpar/wile/extensions/process"
	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/values"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

func engineEval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

func engineEvalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestSystem(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("system uses /bin/sh")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns zero for success", func(t *testing.T) {
		result := engineEval(t, engine, `(system "true")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("returns nonzero for failure", func(t *testing.T) {
		result := engineEval(t, engine, `(system "false")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})

	t.Run("wrong type", func(t *testing.T) {
		engineEvalExpectError(t, engine, `(system 42)`)
	})
}

func TestProcessSpawn(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a process", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((p (process-spawn "echo" "hello")))
			  (process-wait p)
			  (process? p))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("can read stdout", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "echo" "hello")))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "hello")
	})

	t.Run("can write stdin and read stdout", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "cat")))
			  (display "ping" (process-stdin proc))
			  (close-output-port (process-stdin proc))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "ping")
	})

	t.Run("process-wait returns exit code", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("process-wait returns nonzero on failure", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "false")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})
}

func TestProcessKill(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix signals")
	}
	engine := newEngine(t)

	t.Run("kill terminates process", func(t *testing.T) {
		engineEval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'kill)
			  (process-wait proc))
		`)
	})

	t.Run("term terminates process", func(t *testing.T) {
		engineEval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'term)
			  (process-wait proc))
		`)
	})

	t.Run("invalid signal", func(t *testing.T) {
		engineEvalExpectError(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'bogus))
		`)
	})
}

func TestProcessPredicate(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("process? true for process", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc)
			  (process? proc))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("process? false for non-process", func(t *testing.T) {
		result := engineEval(t, engine, `(process? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})
}
