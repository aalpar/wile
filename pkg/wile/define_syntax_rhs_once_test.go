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

// A define-syntax right-hand side is evaluated exactly once (I138).
//
// RED on a23c4543 on every row but "separate units": each printed the RHS
// twice. The expander's body scan (ExpandBodyWithDefineSyntax) evaluated it so
// later forms in the unit could use the macro, and then the compiler
// (CompileDefineSyntax) evaluated it again when it reached the form. A lone
// top-level form was spared only because the expander returned it unchanged.
// The expander now evaluates it when it reaches the form, and the compiler only
// re-checks the shape, as it already does for begin-for-syntax.
//
// Oracles: racket 9.2 and petite both print once for a program. For a library
// body the oracle is Chez's and R7RS's alone: Racket prints twice for a
// required module, which is visit-then-instantiate, once per instantiation, not
// a double evaluation of one definition.
//
// The included-file row asserts the print count only. That path carries a
// second, unrelated defect: the included define-syntax is not visible to the
// forms after the include (see the "A macro defined in an include'd file" row
// in TODO.md, closed by P5.7), so the program's value is not asserted.

import (
	"bytes"
	"context"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	ioext "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

const rhsOnceImports = `(import (scheme base) (scheme write) (for-syntax (scheme base) (scheme write)))`

// rhsOnceDefine is a define-syntax whose right-hand side prints tag.
func rhsOnceDefine(name, tag string) string {
	return `(define-syntax ` + name + ` (begin (display "` + tag + `") (newline) (syntax-rules () ((_) 1))))`
}

func TestDefineSyntaxRHSRunsOnce(t *testing.T) {
	ctx := context.Background()
	fsys := fstest.MapFS{
		"once/lib.sld": &fstest.MapFile{Data: []byte(`(define-library (once lib)
  (import (scheme base) (scheme write) (for-syntax (scheme base) (scheme write)))
  (export ml)
  (begin
    ` + rhsOnceDefine("ml", "RHS-lib") + `))
`)},
		"inc.scm": &fstest.MapFile{Data: []byte(rhsOnceDefine("mi", "RHS-inc") + "\n")},
	}

	// newEngine returns an engine whose current-output-port is buf. Phase-1
	// display writes to the same port, so the RHS's effect is observable here.
	newEngine := func(t *testing.T) (*wile.Engine, *bytes.Buffer) {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		st, ok := eng.Namespace().IOState().(*ioext.State)
		qt.Assert(t, ok, qt.IsTrue)
		buf := &bytes.Buffer{}
		st.SetOutputPort(values.NewCharacterOutputPortFromWriter(buf))
		return eng, buf
	}

	tests := []struct {
		name string
		// program runs as one unit (EvalProgram: a file, or the CLI's -e
		// expressions, which it joins into one program).
		program string
		// multiple runs form by form (EvalMultiple) when program is empty.
		multiple string
		tag      string
		// value is the program's printed value; "" skips the check.
		value string
	}{
		{
			name:    "program unit",
			program: rhsOnceImports + rhsOnceDefine("m", "RHS") + `(m)`,
			tag:     "RHS",
			value:   "1",
		},
		{
			// A pin, green on a23c4543: a lone top-level form was returned
			// unchanged by the expander, so only the compiler ran it.
			name:     "separate units",
			multiple: rhsOnceImports + rhsOnceDefine("m", "RHS") + `(m)`,
			tag:      "RHS",
			value:    "1",
		},
		{
			name:    "lambda body",
			program: rhsOnceImports + `(let () ` + rhsOnceDefine("m", "RHS-body") + ` (m))`,
			tag:     "RHS-body",
			value:   "1",
		},
		{
			name:     "library body",
			multiple: `(import (scheme base) (once lib)) (ml)`,
			tag:      "RHS-lib",
			value:    "1",
		},
		{
			name:    "included file",
			program: rhsOnceImports + `(include "inc.scm")`,
			tag:     "RHS-inc",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng, buf := newEngine(t)
			var v wile.Value
			var err error
			if tt.program != "" {
				v, err = eng.EvalProgram(ctx, tt.program, "")
			} else {
				v, err = eng.EvalMultiple(ctx, tt.multiple)
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, strings.Count(buf.String(), tt.tag+"\n"), qt.Equals, 1,
				qt.Commentf("output %q", buf.String()))
			if tt.value != "" {
				qt.Assert(t, v.SchemeString(), qt.Equals, tt.value)
			}
		})
	}
}
