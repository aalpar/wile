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

package compilation

import (
	"context"
	"errors"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestIncludeFileNames(t *testing.T) {
	str := func(s string) values.Value {
		return values.NewString(s)
	}
	tcs := []struct {
		name   string
		args   values.Value
		want   []string
		wantOK bool
	}{
		{name: "one file", args: values.List(str("a.scm")), want: []string{"a.scm"}, wantOK: true},
		{name: "several files keep their order", args: values.List(str("a.scm"), str("b.scm")), want: []string{"a.scm", "b.scm"}, wantOK: true},
		{name: "no files", args: values.EmptyList},
		{name: "a non-string operand", args: values.List(str("a.scm"), values.NewInteger(5))},
		{name: "an improper list", args: values.NewCons(str("a.scm"), values.NewInteger(5))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			args := mustDatumToSyntax(syntax.NewZeroValueSourceContext(), tc.args)

			got, ok := includeFileNames(args)

			qt.Assert(t, ok, qt.Equals, tc.wantOK)
			qt.Assert(t, got, qt.DeepEquals, tc.want)
		})
	}
}

// expandIncludeFor expands (include <args>...) with the head's reading of the
// given virtual FS installed as the env's resolver (none when fsys is nil).
func expandIncludeFor(t *testing.T, fsys fstest.MapFS, args ...values.Value) (*ExpanderTimeContinuation, syntax.SyntaxValue, error) {
	t.Helper()
	env := newNamespace(environment.NewNamespace().Runtime())
	if fsys != nil {
		env.SetFileResolver(NewFSFileResolver(fsys, env))
	}
	p := NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator())
	sctx := syntax.NewZeroValueSourceContext()
	sym := syntax.NewSyntaxSymbol("include", sctx)
	rest := mustDatumToSyntax(sctx, values.List(args...))
	got, err := p.expandIncludeForm(sym, rest)
	return p, got, err
}

// Pin: the splice's result is a (begin ...) whose head is NOT the includer's
// identifier, and whose operands are the files' forms in order.
func TestExpandIncludeForm_SplicesFilesInOrder(t *testing.T) {
	fsys := fstest.MapFS{
		"a.scm": &fstest.MapFile{Data: []byte("(define a 1)")},
		"b.scm": &fstest.MapFile{Data: []byte("(define b 2) (define c 3)")},
	}
	_, got, err := expandIncludeFor(t, fsys, values.NewString("a.scm"), values.NewString("b.scm"))
	qt.Assert(t, err, qt.IsNil)

	datum := syntax.UnwrapAllShared(got, map[syntax.SyntaxValue]values.Value{})
	qt.Assert(t, datum.SchemeString(), qt.Equals, "(begin (define a 1) (define b 2) (define c 3))")

	pair, ok := got.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	head, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, head.Scopes().Len(), qt.Equals, 1, qt.Commentf("the head carries exactly its own fresh scope"))
}

// Pin: the load stack the splice derives for a top-level include lives on the
// child expander's ctx only; the parent's ctx is untouched afterwards.
func TestExpandIncludeForm_LoadStackDoesNotEscape(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": &fstest.MapFile{Data: []byte("(define a 1)")}}

	p, _, err := expandIncludeFor(t, fsys, values.NewString("a.scm"))

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, resolver.SelectLoadStack(p.ctx), qt.IsNil)
}

// Pin: every shape the splice cannot read is returned UNCHANGED for the validator
// and compiler to report, and so is an env with no resolver.
func TestExpandIncludeForm_BailsToTheValidator(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": &fstest.MapFile{Data: []byte("(define a 1)")}}
	tcs := []struct {
		name string
		fsys fstest.MapFS
		args []values.Value
	}{
		{name: "no resolver", args: []values.Value{values.NewString("a.scm")}},
		{name: "no operands", fsys: fsys},
		{name: "a non-string operand", fsys: fsys, args: []values.Value{values.NewInteger(5)}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, got, err := expandIncludeFor(t, tc.fsys, tc.args...)

			qt.Assert(t, err, qt.IsNil)
			pair, ok := got.(*syntax.SyntaxPair)
			qt.Assert(t, ok, qt.IsTrue)
			head, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, head.Scopes().Len(), qt.Equals, 0)
		})
	}
}

// Pin: a missing file is an error from the expander, wrapped with its cause.
func TestExpandIncludeForm_MissingFile(t *testing.T) {
	_, _, err := expandIncludeFor(t, fstest.MapFS{}, values.NewString("absent.scm"))

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue, qt.Commentf("%v", err))
}

// Pin: a form of the file that fails to expand names the file in the chain.
func TestExpandIncludeForm_ExpansionFailureNamesTheFile(t *testing.T) {
	fsys := fstest.MapFS{"bad.scm": &fstest.MapFile{Data: []byte("(syntax-error \"boom\")")}}

	_, _, err := expandIncludeFor(t, fsys, values.NewString("bad.scm"))

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "include bad.scm: error expanding forms")
}
