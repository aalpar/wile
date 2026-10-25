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
	"errors"
	"strings"
	"testing"
)

// TestRuntimeErrorSourceProvenance verifies that a runtime error carries
// :line:col provenance in RuntimeError.Source whether or not the program has a
// file name. A nameless EvalMultiple program (File == "") previously got an empty
// Source even though the position survived in the stack trace; a named program
// must keep its "file:line:col" form unchanged.
func TestRuntimeErrorSourceProvenance(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name       string
		source     string // "" => EvalMultiple (nameless); else EvalMultipleWithSource
		wantPrefix string
	}{
		{
			name:       "nameless eval emits :line:col",
			source:     "",
			wantPrefix: ":1:",
		},
		{
			name:       "named source keeps file:line:col",
			source:     "prog.scm",
			wantPrefix: "prog.scm:1:",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()

			// car of a non-pair compiles fine but raises at runtime.
			const code = "(car 5)"
			if tc.source == "" {
				_, err = eng.EvalMultiple(ctx, code)
			} else {
				_, err = eng.EvalMultipleWithSource(ctx, code, tc.source)
			}
			if err == nil {
				t.Fatal("expected runtime error")
			}
			var re *RuntimeError
			if !errors.As(err, &re) {
				t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
			}
			if !strings.HasPrefix(re.Source, tc.wantPrefix) {
				t.Errorf("RuntimeError.Source = %q, want prefix %q", re.Source, tc.wantPrefix)
			}
		})
	}
}

// TestEmptyListOperatorSourceLocation verifies that a form whose operator is the
// empty list — (()) — localizes its runtime error to its own line, not the
// enclosing (begin ...) wrapper. The empty-list singleton carries no source
// context, so the expander must fall back to the enclosing form's context when
// rebuilding the call node. Regression test for finding 5.3.
func TestEmptyListOperatorSourceLocation(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng, err := NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	defer eng.Close()

	// (()) sits on line 3; the two filler forms push it down so a line-1
	// mislocation is distinguishable from the correct line-3 report.
	const code = "(+ 1 2)\n(+ 3 4)\n(())"
	_, err = eng.EvalMultipleWithSource(ctx, code, "prog.scm")
	if err == nil {
		t.Fatal("expected runtime error")
	}
	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
	}
	const wantPrefix = "prog.scm:3:"
	if !strings.HasPrefix(re.Source, wantPrefix) {
		t.Errorf("RuntimeError.Source = %q, want prefix %q", re.Source, wantPrefix)
	}
}

// TestUnboundBindingCompileErrorLocation covers VERDICTS 92, a CONFIRMED
// provenance defect that belonged to no wave and no plan.
//
// The two ErrNoSuchBinding throws in the compiler carried no source of their
// own, so the error reached CompileExpression's wrapCompilationError with no
// SourcedError anywhere in the chain and the only location available was the
// enclosing top-level form's. An unbound reference on line 3 reported line 1,
// and line 3 appeared nowhere — not in the message text, and not reachable
// through errors.As either. That is the "Error Chain Losslessness" class the
// root CLAUDE.local.md names.
//
// The anchor is the IDENTIFIER, not the enclosing form: engine.go's extractor
// documents exactly that intent — "the innermost source location ... at the
// actual error site (e.g., the undefined variable), not the enclosing form".
// Two compile-time diagnostics on the same shape used to disagree about it,
// because arityError stamps at its node and these did not.
func TestUnboundBindingCompileErrorLocation(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// ONE top-level form spanning three lines. The wrapper matters:
			// EvalMultipleWithSource compiles each top-level form separately,
			// so with three separate forms the enclosing form IS the offending
			// one and the coarse location is accidentally right. File
			// execution (begin ...)-wraps, which is where the defect showed.
			name: "unbound reference",
			code: "(begin\n  (+ 1 2)\n  (nope 1))",
			want: "prog.scm:3:",
		},
		{
			name: "set! on unbound",
			code: "(begin\n  (+ 1 2)\n  (set! nope 1))",
			want: "prog.scm:3:",
		},
		{
			// The scoped arm of the same defect: inside a lambda body the
			// reference carries scopes, so it leaves through the other throw.
			name: "unbound reference inside a lambda body",
			code: "(begin\n  (+ 1 2)\n  (define (f x) (nope x)))",
			want: "prog.scm:3:",
		},
		{
			// Column, not only line: the identifier's own column, not the
			// enclosing form's open paren.
			name: "unbound reference column is the identifier",
			code: "(+ 1      (nope 2))",
			want: "prog.scm:1:11",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()

			_, err = eng.EvalMultipleWithSource(ctx, tc.code, "prog.scm")
			if err == nil {
				t.Fatal("expected compilation error")
			}
			var ce *CompilationError
			if !errors.As(err, &ce) {
				t.Fatalf("expected *CompilationError, got %T: %v", err, err)
			}
			if !strings.HasPrefix(ce.Source, tc.want) {
				t.Errorf("CompilationError.Source = %q, want prefix %q", ce.Source, tc.want)
			}
			// The text must agree with the field. Before the fix the message
			// carried the same wrong location the field did, so a text-only
			// assertion would have passed for the wrong reason.
			if !strings.Contains(err.Error(), tc.want) {
				t.Errorf("error text %q does not mention %q", err.Error(), tc.want)
			}
		})
	}
}

// TestParseErrorLocationSurvivesSourcedErrorStamping is the regression guard
// the unified-error-representation design calls for in its section 4.3: a
// parse error's location comes from a DIFFERENT path — wrapCompilationError's
// ParserError fallback, which runs only when the SourcedError walk yields
// nothing. Stamping more compiler errors must not shadow that fallback.
func TestParseErrorLocationSurvivesSourcedErrorStamping(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng, err := NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	defer eng.Close()

	// #b19 is a radix-prefixed numeral with a digit outside base 2: a
	// tokenizer fault, located at the offending rune on line 2.
	_, err = eng.EvalMultipleWithSource(ctx, "(+ 1 2)\n(+ 1 #b19)", "prog.scm")
	if err == nil {
		t.Fatal("expected parse error")
	}
	assertParseErrorLocatedOnce(t, err, "prog.scm:2:")
}

// assertParseErrorLocatedOnce checks that a parse error's location survived to
// CompilationError.Source and renders exactly once: the ParserError's own text
// renders it for named input, so the prefix yields.
func assertParseErrorLocatedOnce(t *testing.T, err error, wantSource string) {
	t.Helper()
	var ce *CompilationError
	if !errors.As(err, &ce) {
		t.Fatalf("parse error %v is %T, want *CompilationError", err, err)
	}
	if !strings.HasPrefix(ce.Source, wantSource) {
		t.Errorf("parse error Source %q, want prefix %q", ce.Source, wantSource)
	}
	n := strings.Count(err.Error(), wantSource)
	if n != 1 {
		t.Errorf("parse error %q renders %q %d times, want once", err.Error(), wantSource, n)
	}
}

// TestBigNumDelimiterErrorReachesEngine pins the #z/#m delimiter rule through
// the Engine: the read error must survive the compile path with its location
// and rule name. The tokenizer and parser suites pin the check itself (both go
// red with it deleted); this row pins that nothing downstream swallows it.
func TestBigNumDelimiterErrorReachesEngine(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng, err := NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	defer eng.Close()

	_, err = eng.EvalMultipleWithSource(ctx, "(+ 1 2)\n(list #z1/2)", "prog.scm")
	if err == nil {
		t.Fatal("expected parse error for #z1/2")
	}
	assertParseErrorLocatedOnce(t, err, "prog.scm:2:")
	if !strings.Contains(err.Error(), "#z or #m") {
		t.Errorf("parse error %q does not name the #z/#m rule", err.Error())
	}
}

// TestTopLevelParseErrorRendersItsLocationOnce: a read error from a named
// input renders its own location as file:line:col, a parser-level and a
// lexical fault alike, so the CompilationError's prefix yields to it. Unnamed
// input renders "line N, column N", and keeps the prefix.
func TestTopLevelParseErrorRendersItsLocationOnce(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name, src, file string
		wantSource      string
		wantOnce        []string
		wantPrefix      string
	}{
		{name: "parser-level fault", src: "(+ 1 2)\n(foo ]", file: "top.scm",
			wantSource: "top.scm:2:5", wantOnce: []string{"top.scm:2:5", "top.scm"}},
		{name: "lexical fault", src: "(+ 1 2)\n#b19", file: "top.scm",
			wantSource: "top.scm:2:3", wantOnce: []string{"top.scm:2:3", "top.scm"}},
		// The token starts at 2:1 but the text renders the tokenizer's 2:13:
		// a Source that is only a prefix of the rendered position is not in
		// the text, so the prefix stays.
		{name: "token start that prefixes the rendered position", src: "(define a 1)\n \"abcdefghijk", file: "sub.scm",
			wantSource: "sub.scm:2:1", wantPrefix: "sub.scm:2:1: "},
		{name: "unnamed input keeps the prefix", src: "(+ 1 2)\n(foo ]", file: "",
			wantSource: "2:5", wantPrefix: "2:5: "},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()
			_, err = eng.EvalMultipleWithSource(ctx, tc.src, tc.file)
			var ce *CompilationError
			if !errors.As(err, &ce) {
				t.Fatalf("got %T %v, want *CompilationError", err, err)
			}
			if ce.Source != tc.wantSource {
				t.Errorf("Source %q, want %q", ce.Source, tc.wantSource)
			}
			for _, want := range tc.wantOnce {
				n := strings.Count(err.Error(), want)
				if n != 1 {
					t.Errorf("%q renders %q %d times, want once", err.Error(), want, n)
				}
			}
			if !strings.HasPrefix(err.Error(), tc.wantPrefix) {
				t.Errorf("%q, want prefix %q", err.Error(), tc.wantPrefix)
			}
		})
	}
}
