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

package repl

import (
	"bytes"
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// TestRunSimple_WithInput exercises the simple (non-readline) loop driven by a
// caller-supplied reader via WithInput. It verifies that input is read from the
// configured reader (not the hard-wired os.Stdin), that definitions accumulate
// across lines in one engine, and that EOF exits cleanly.
func TestRunSimple_WithInput(t *testing.T) {
	c := qt.New(t)
	eng, err := wile.NewEngine(context.Background(), wile.WithMutableTopLevel())
	c.Assert(err, qt.IsNil)

	var out bytes.Buffer
	r := New(eng,
		WithInput(strings.NewReader("(define x 21)\n(* x 2)\n")),
		WithOutput(&out),
		WithErrorOutput(&out),
	)

	err = r.RunSimple(context.Background())
	c.Assert(err, qt.IsNil)

	// (* x 2) prints 42 only if x = 21 carried over from the prior line and the
	// configured reader (not os.Stdin) was the input source.
	c.Assert(strings.Contains(out.String(), "42"), qt.IsTrue,
		qt.Commentf("out=%q", out.String()))
}

// TestRunSimple_ErrorRecovery verifies an error in one form does not abort the
// loop: a bad form is reported and the following good form still evaluates.
func TestRunSimple_ErrorRecovery(t *testing.T) {
	c := qt.New(t)
	eng, err := wile.NewEngine(context.Background(), wile.WithMutableTopLevel())
	c.Assert(err, qt.IsNil)

	var out bytes.Buffer
	r := New(eng,
		WithInput(strings.NewReader("(car 5)\n(+ 2 3)\n")),
		WithOutput(&out),
		WithErrorOutput(&out),
	)

	err = r.RunSimple(context.Background())
	c.Assert(err, qt.IsNil)

	full := out.String()
	c.Assert(strings.Contains(full, "Exception:"), qt.IsTrue, qt.Commentf("out=%q", full))
	c.Assert(strings.Contains(full, "5"), qt.IsTrue, qt.Commentf("out=%q", full))
	// The error renders one stack trace, not two (double-render regression).
	c.Assert(strings.Count(full, "Stack trace:"), qt.Equals, 1, qt.Commentf("out=%q", full))
}
