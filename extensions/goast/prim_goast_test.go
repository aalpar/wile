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

package goast_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile"
	extgoast "github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the goast extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoast.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// runScheme runs Scheme code and returns the result.
func runScheme(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// runSchemeExpectError runs Scheme code and asserts that it produces an error.
func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestGoParseExpr(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "integer literal",
			code: `(go-node-type (go-parse-expr "42"))`,
			want: values.NewSymbol("lit"),
		},
		{
			name: "identifier",
			code: `(go-node-type (go-parse-expr "x"))`,
			want: values.NewSymbol("ident"),
		},
		{
			name: "binary expression",
			code: `(go-node-type (go-parse-expr "1 + 2"))`,
			want: values.NewSymbol("binary-expr"),
		},
		{
			name: "call expression",
			code: `(go-node-type (go-parse-expr "fmt.Println(42)"))`,
			want: values.NewSymbol("call-expr"),
		},
		{
			name: "unary expression",
			code: `(go-node-type (go-parse-expr "-x"))`,
			want: values.NewSymbol("unary-expr"),
		},
		{
			name: "selector expression",
			code: `(go-node-type (go-parse-expr "pkg.Name"))`,
			want: values.NewSymbol("selector-expr"),
		},
		{
			name: "index expression",
			code: `(go-node-type (go-parse-expr "a[0]"))`,
			want: values.NewSymbol("index-expr"),
		},
		{
			name: "star expression",
			code: `(go-node-type (go-parse-expr "*p"))`,
			want: values.NewSymbol("star-expr"),
		},
		{
			name: "paren expression",
			code: `(go-node-type (go-parse-expr "(x)"))`,
			want: values.NewSymbol("paren-expr"),
		},
		{
			name: "composite literal",
			code: `(go-node-type (go-parse-expr "[]int{1, 2}"))`,
			want: values.NewSymbol("composite-lit"),
		},
		{
			name: "func literal",
			code: `(go-node-type (go-parse-expr "func() {}"))`,
			want: values.NewSymbol("func-lit"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runScheme(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestGoParseString(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "parse minimal file",
			code: `(go-node-type (go-parse-string "package main"))`,
			want: values.NewSymbol("file"),
		},
		{
			name: "package name extracted",
			code: `(cdr (assoc 'name (cdr (go-parse-string "package main"))))`,
			want: values.NewString("main"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runScheme(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestGoParseFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Write a temp Go file.
	dir := t.TempDir()
	path := filepath.Join(dir, "test.go")
	err := os.WriteFile(path, []byte("package test\n\nfunc Hello() {}\n"), 0o644)
	c.Assert(err, qt.IsNil)

	code := `(go-node-type (go-parse-file "` + path + `"))`
	result := runScheme(t, engine, code)
	c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewSymbol("file"))
}

func TestGoFormatRoundTrip(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name   string
		source string
	}{
		{
			name:   "expression",
			source: "1 + 2",
		},
		{
			name:   "identifier",
			source: "x",
		},
		{
			name:   "call",
			source: "f(x, y)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Parse, format, compare.
			code := `(go-format (go-parse-expr "` + tc.source + `"))`
			result := runScheme(t, engine, code)
			s, ok := result.Internal().(*values.String)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected string, got %T", result.Internal()))
			c.Assert(s.Value, qt.Equals, tc.source)
		})
	}
}

func TestGoFormatFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	source := `package main

func Add(a, b int) int {
	return a + b
}
`
	// Use Scheme string escaping — the Go source has tabs and newlines.
	code := `(go-format (go-parse-string ` + schemeStringLiteral(source) + `))`
	result := runScheme(t, engine, code)
	s, ok := result.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected string, got %T", result.Internal()))
	c.Assert(s.Value, qt.Equals, source)
}

func TestGoNodeType(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			name: "ident",
			code: `(go-node-type (go-parse-expr "x"))`,
			want: values.NewSymbol("ident"),
		},
		{
			name: "file",
			code: `(go-node-type (go-parse-string "package main"))`,
			want: values.NewSymbol("file"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runScheme(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestGoASTErrors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "parse-expr invalid", code: `(go-parse-expr "if {")`},
		{name: "parse-expr wrong type", code: `(go-parse-expr 42)`},
		{name: "parse-string wrong type", code: `(go-parse-string 42)`},
		{name: "parse-file wrong type", code: `(go-parse-file 42)`},
		{name: "parse-file nonexistent", code: `(go-parse-file "/nonexistent/file.go")`},
		{name: "format wrong type", code: `(go-format 42)`},
		{name: "format malformed", code: `(go-format '(unknown-xyz))`},
		{name: "node-type wrong type", code: `(go-node-type 42)`},
		{name: "node-type no symbol tag", code: `(go-node-type '(42 x))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}

func TestGoTypecheckPackage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	const pkgPath = "github.com/aalpar/wile/extensions/goast"

	// Load the package once and cache via define — avoids three separate go list calls.
	runScheme(t, engine, `(define typechecked (go-typecheck-package "`+pkgPath+`"))`)

	t.Run("returns package node", func(t *testing.T) {
		result := runScheme(t, engine, `(go-node-type (car typechecked))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewSymbol("package"))
	})

	t.Run("package name", func(t *testing.T) {
		result := runScheme(t, engine, `(cdr (assoc 'name (cdr (car typechecked))))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString("goast"))
	})

	t.Run("package path", func(t *testing.T) {
		result := runScheme(t, engine, `(cdr (assoc 'path (cdr (car typechecked))))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewString(pkgPath))
	})

	t.Run("has file nodes", func(t *testing.T) {
		result := runScheme(t, engine, `(go-node-type (car (cdr (assoc 'files (cdr (car typechecked))))))`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewSymbol("file"))
	})
}

func TestGoTypecheckPackageErrors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong arg type", code: `(go-typecheck-package 42)`},
		{name: "nonexistent package", code: `(go-typecheck-package "github.com/aalpar/wile/does-not-exist-xyz")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}

// schemeStringLiteral wraps a Go string as a Scheme string literal,
// escaping backslashes, double quotes, and newlines.
func schemeStringLiteral(s string) string {
	var b []byte
	b = append(b, '"')
	for _, r := range s {
		switch r {
		case '\\':
			b = append(b, '\\', '\\')
		case '"':
			b = append(b, '\\', '"')
		case '\n':
			b = append(b, '\\', 'n')
		case '\t':
			b = append(b, '\\', 't')
		default:
			b = append(b, byte(r))
		}
	}
	b = append(b, '"')
	return string(b)
}
