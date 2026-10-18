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
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// Pin: includeAuthorizerContext installs the compiling namespace's authorizer
// only when ctx carries none. Overwriting an in-flight one would weaken the
// policy LoadLibrary installed for the chain, so the "ctx wins" row is the
// fail-open guard.
func TestIncludeAuthorizerContext(t *testing.T) {
	allow := security.AuthorizerFunc(func(security.AccessRequest) error {
		return nil
	})
	deny := security.DenyAll()
	tcs := []struct {
		name      string
		ctxAuth   security.Authorizer
		envAuth   security.Authorizer
		wantAuth  bool
		wantAllow bool
	}{
		{name: "ctx authorizer wins over the env's", ctxAuth: allow, envAuth: deny, wantAuth: true, wantAllow: true},
		{name: "env authorizer installed when ctx has none", envAuth: deny, wantAuth: true, wantAllow: false},
		{name: "neither: ctx unchanged", wantAuth: false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			env.Namespace().SetAuthorizer(tc.envAuth)
			ctx := context.Background()
			if tc.ctxAuth != nil {
				ctx = security.WithAuthorizer(ctx, tc.ctxAuth)
			}

			got := security.FromContext(includeAuthorizerContext(ctx, env))

			if !tc.wantAuth {
				qt.Assert(t, got, qt.IsNil)
				return
			}
			qt.Assert(t, got, qt.IsNotNil)
			err := got.Authorize(security.AccessRequest{})
			qt.Assert(t, err == nil, qt.Equals, tc.wantAllow)
		})
	}
}

func TestReadIncludedForms(t *testing.T) {
	fsys := fstest.MapFS{
		"ok.scm":     &fstest.MapFile{Data: []byte("(define A 1) (define B 2)")},
		"broken.scm": &fstest.MapFile{Data: []byte("(define x")},
	}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	t.Run("reads every form and returns the resolved path", func(t *testing.T) {
		forms, path, err := readIncludedForms(context.Background(), env, r, "ok.scm", false)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, forms, qt.HasLen, 2)
		qt.Assert(t, path, qt.Contains, "ok.scm")
	})

	t.Run("a read error names the file and keeps its cause", func(t *testing.T) {
		_, _, err := readIncludedForms(context.Background(), env, r, "broken.scm", false)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, `include: error reading "broken.scm"`)
		var parseErr *parser.ParserError
		qt.Assert(t, errors.As(err, &parseErr), qt.IsTrue, qt.Commentf("%v", err))
	})

	t.Run("a missing file is wrapped as include", func(t *testing.T) {
		_, _, err := readIncludedForms(context.Background(), env, r, "absent.scm", false)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "include")
		qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue, qt.Commentf("%v", err))
	})
}

// Pin: the compiler resolves include through the resolver ExpandAndCompile was
// handed, not through env.FileResolver(). The env here has none and the file
// exists only in the virtual FS, so reading it proves p.fileResolver is used.
func TestExpandAndCompile_IncludeUsesTheGivenResolver(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)
	r := NewFSFileResolver(fstest.MapFS{"only-here.scm": &fstest.MapFile{Data: []byte("(define zz 1)")}}, env)
	stx := mustDatumToSyntax(syntax.NewZeroValueSourceContext(), values.List(
		values.NewSymbol("include"),
		values.NewString("only-here.scm")))

	_, err = ExpandAndCompile(context.Background(), env, stx, r, 0, 0)

	qt.Assert(t, err, qt.IsNil)
}

// Pin: every refusal from the lifted resolveCondExpandClause is a *SourcedError
// at the source the caller passed. The callers' own wraps would hide a dropped
// source, so this calls it directly with a distinguishable one.
func TestResolveCondExpandClause_RefusalsCarryTheSource(t *testing.T) {
	sym := values.NewSymbol
	tcs := []struct {
		name string
		args values.Value
		want error
	}{
		{
			name: "no matching clause",
			args: values.List(values.List(sym("nonexistent"), values.NewInteger(1))),
			want: werr.ErrNoMatchingClause,
		},
		{
			name: "else not last",
			args: values.List(values.List(sym("else"), values.NewInteger(1)), values.List(sym("r7rs"), values.NewInteger(2))),
			want: werr.ErrInvalidSyntax,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := newNamespace(environment.NewNamespace().Runtime())
			src := syntax.NewZeroValueSourceContext()
			args := mustDatumToSyntax(src, tc.args)

			_, err := resolveCondExpandClause(context.Background(), env, nil, args, src)

			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue, qt.Commentf("%v", err))
			var sourced *SourcedError
			qt.Assert(t, errors.As(err, &sourced), qt.IsTrue)
			qt.Assert(t, sourced.Source, qt.Equals, src)
		})
	}
}
