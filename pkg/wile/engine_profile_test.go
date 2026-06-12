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

package wile

import (
	"context"
	"fmt"
	"os"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestProfile_Tiny_CoreOnly(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Tiny))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	_, err = eng.EvalMultiple(ctx, "(display 42)")
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_Console_IOWorks(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Console))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	result, err = eng.EvalMultiple(ctx,
		`(let ((p (open-output-string))) (write "hello" p) (get-output-string p))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Contains, "hello")
}

func TestProfile_Console_FileSandbox(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Console))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(file-exists? "/tmp")`)
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(file-exists? "/etc/passwd")`)
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_ConsoleWithLoad_EvalAndLoad(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
	c.Assert(err, qt.IsNil)

	// (eval ...) works — dispatch through a fresh profile environment to
	// avoid depending on the namespace extension (not in ConsoleWithLoad).
	code := fmt.Sprintf("(%s '(+ 1 2) (environment '(wile tiny)))", "eval")
	result, err := eng.EvalMultiple(ctx, code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	// (load ...) of a /tmp file works
	tmpFile, err := os.CreateTemp("/tmp", "wile-cwl-*.scm")
	c.Assert(err, qt.IsNil)
	defer os.Remove(tmpFile.Name())
	_, err = tmpFile.WriteString("(define cwl-loaded-value 42)\n")
	c.Assert(err, qt.IsNil)
	c.Assert(tmpFile.Close(), qt.IsNil)

	_, err = eng.EvalMultiple(ctx, fmt.Sprintf(`(load %q)`, tmpFile.Name()))
	c.Assert(err, qt.IsNil)
	result, err = eng.EvalMultiple(ctx, `cwl-loaded-value`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

func TestProfile_ConsoleWithLoad_DeniesLoadOutsideTmp(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
	c.Assert(err, qt.IsNil)

	// load outside /tmp is denied even though eval is enabled
	_, err = eng.EvalMultiple(ctx, `(load "/etc/hosts")`)
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_Small_R7RS(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Small))
	c.Assert(err, qt.IsNil)

	code := fmt.Sprintf("(%s '(+ 1 2) (environment '(wile tiny)))", "eval")
	result, err := eng.EvalMultiple(ctx, code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestProfile_KitchenSink_Threads(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(KitchenSink))
	c.Assert(err, qt.IsNil)

	// make-thread returns a real thread value; current-thread in the
	// primordial goroutine returns the symbol 'primordial, so exercise
	// thread construction instead.
	result, err := eng.EvalMultiple(ctx, "(thread? (make-thread (lambda () 42)))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

func TestProfile_Superset_Invariant(t *testing.T) {
	c := qt.New(t)

	tinyExts := Tiny.extensions()
	consoleExts := Console.extensions()
	cwlExts := ConsoleWithLoad.extensions()
	smallExts := Small.extensions()
	kitchenExts := KitchenSink.extensions()

	c.Assert(len(tinyExts), qt.Equals, 0)
	c.Assert(len(consoleExts) > len(tinyExts), qt.IsTrue)
	c.Assert(len(cwlExts) > len(consoleExts), qt.IsTrue)
	c.Assert(len(smallExts) > len(cwlExts), qt.IsTrue)
	c.Assert(len(kitchenExts) > len(smallExts), qt.IsTrue)
}

func TestProfile_NoProfile_BareEngine(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestProfile_WithSandbox_Composition(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Small),
		WithSandbox(),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}
