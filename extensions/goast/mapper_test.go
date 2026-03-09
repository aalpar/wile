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

package goast

import (
	"go/ast"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"strings"
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// roundTripExpr parses a Go expression, maps to s-expression, unmaps back,
// and verifies the printed output of the round-tripped AST matches the
// printed output of the original AST. This accounts for go/printer
// normalization (e.g., func literal body formatting).
func roundTripExpr(t *testing.T, source string) {
	t.Helper()
	c := qt.New(t)

	expr, err := parser.ParseExpr(source)
	c.Assert(err, qt.IsNil)

	// Print the original AST for a normalized baseline.
	fset := token.NewFileSet()
	var origBuf strings.Builder
	err = printer.Fprint(&origBuf, fset, expr)
	c.Assert(err, qt.IsNil)

	opts := &mapperOpts{}
	sexpr := mapNode(expr, opts)

	n, err := unmapNode(sexpr)
	c.Assert(err, qt.IsNil)

	var rtBuf strings.Builder
	err = printer.Fprint(&rtBuf, fset, n)
	c.Assert(err, qt.IsNil)
	c.Assert(rtBuf.String(), qt.Equals, origBuf.String())
}

// roundTripFile parses Go source as a file, maps to s-expression, unmaps back,
// formats, and compares.
func roundTripFile(t *testing.T, source string) {
	t.Helper()
	c := qt.New(t)

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "test.go", source, 0)
	c.Assert(err, qt.IsNil)

	opts := &mapperOpts{fset: fset}
	sexpr := mapNode(f, opts)

	n, err := unmapNode(sexpr)
	c.Assert(err, qt.IsNil)

	outFset := token.NewFileSet()
	var buf strings.Builder
	err = printer.Fprint(&buf, outFset, n)
	c.Assert(err, qt.IsNil)

	formatted, err := format.Source([]byte(buf.String()))
	c.Assert(err, qt.IsNil)

	expectedFormatted, err := format.Source([]byte(source))
	c.Assert(err, qt.IsNil)

	c.Assert(string(formatted), qt.Equals, string(expectedFormatted))
}

func TestRoundTripExpressions(t *testing.T) {
	tcs := []struct {
		name   string
		source string
	}{
		{name: "ident", source: "x"},
		{name: "int literal", source: "42"},
		{name: "string literal", source: `"hello"`},
		{name: "float literal", source: "3.14"},
		{name: "binary add", source: "1 + 2"},
		{name: "binary mul", source: "x * y"},
		{name: "binary compare", source: "a < b"},
		{name: "unary neg", source: "-x"},
		{name: "unary not", source: "!ok"},
		{name: "call no args", source: "f()"},
		{name: "call with args", source: "f(x, y)"},
		{name: "selector", source: "pkg.Name"},
		{name: "index", source: "a[0]"},
		{name: "star", source: "*p"},
		{name: "paren", source: "(x)"},
		{name: "composite lit", source: "[]int{1, 2, 3}"},
		{name: "key-value in composite", source: `map[string]int{"a": 1}`},
		{name: "func literal", source: "func() {}"},
		{name: "func literal with params", source: "func(x int) int {\n\treturn x\n}"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			roundTripExpr(t, tc.source)
		})
	}
}

func TestRoundTripFiles(t *testing.T) {
	tcs := []struct {
		name   string
		source string
	}{
		{
			name:   "empty file",
			source: "package main\n",
		},
		{
			name: "import",
			source: `package main

import "fmt"
`,
		},
		{
			name: "function",
			source: `package main

func Add(a, b int) int {
	return a + b
}
`,
		},
		{
			name: "variable declaration",
			source: `package main

var x int
`,
		},
		{
			name: "const declaration",
			source: `package main

const Pi = 3.14
`,
		},
		{
			name: "type declaration",
			source: `package main

type Point struct {
	X int
	Y int
}
`,
		},
		{
			name: "if statement",
			source: `package main

func f() {
	if x > 0 {
		return
	}
}
`,
		},
		{
			name: "if-else",
			source: `package main

func f() {
	if x > 0 {
		return
	} else {
		x++
	}
}
`,
		},
		{
			name: "for loop",
			source: `package main

func f() {
	for i := 0; i < 10; i++ {
	}
}
`,
		},
		{
			name: "range loop",
			source: `package main

func f() {
	for i, v := range items {
		_ = i
		_ = v
	}
}
`,
		},
		{
			name: "assign",
			source: `package main

func f() {
	x := 1
	_ = x
}
`,
		},
		{
			name: "method with receiver",
			source: `package main

func (p *Point) String() string {
	return ""
}
`,
		},
		{
			name: "interface type",
			source: `package main

type Reader interface {
	Read(p []byte) (int, error)
}
`,
		},
		{
			name: "map type",
			source: `package main

var m map[string]int
`,
		},
		{
			name: "branch statements",
			source: `package main

func f() {
	for {
		break
	}
}
`,
		},
		{
			name: "expression statement",
			source: `package main

func f() {
	println()
}
`,
		},
		{
			name: "decl statement",
			source: `package main

func f() {
	var x int
	_ = x
}
`,
		},
		{
			name: "named import",
			source: `package main

import foo "fmt"
`,
		},
		{
			name:   "struct tag",
			source: "package main\n\ntype Foo struct {\n\tX int " + "`json:\"x\"`" + "\n}\n",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			roundTripFile(t, tc.source)
		})
	}
}

func TestMapNodeNil(t *testing.T) {
	c := qt.New(t)
	opts := &mapperOpts{}
	result := mapNode(nil, opts)
	_, ok := result.(*values.Boolean)
	c.Assert(ok, qt.IsTrue, qt.Commentf("nil node should map to #f"))
}

func TestUnmapNodeErrors(t *testing.T) {
	tcs := []struct {
		name  string
		input values.Value
	}{
		{
			name:  "not a pair",
			input: values.NewString("not a node"),
		},
		{
			name:  "no symbol tag",
			input: values.NewCons(values.NewString("bad"), values.EmptyList),
		},
		{
			name:  "unknown tag",
			input: node("nonexistent-node-type"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := unmapNode(tc.input)
			qt.New(t).Assert(err, qt.IsNotNil)
		})
	}
}

func TestParseOpts(t *testing.T) {
	c := qt.New(t)
	fset := token.NewFileSet()

	tcs := []struct {
		name      string
		input     values.Value
		positions bool
		comments  bool
		wantMode  parser.Mode
	}{
		{
			name:      "no options",
			input:     values.EmptyList,
			positions: false,
			comments:  false,
			wantMode:  0,
		},
		{
			name:      "positions option",
			input:     values.List(values.NewSymbol("positions")),
			positions: true,
			comments:  false,
			wantMode:  0,
		},
		{
			name:      "comments option",
			input:     values.List(values.NewSymbol("comments")),
			positions: false,
			comments:  true,
			wantMode:  parser.ParseComments,
		},
		{
			name:      "both options",
			input:     values.List(values.NewSymbol("positions"), values.NewSymbol("comments")),
			positions: true,
			comments:  true,
			wantMode:  parser.ParseComments,
		},
		{
			name:      "not a tuple",
			input:     values.NewString("not-a-list"),
			positions: false,
			comments:  false,
			wantMode:  0,
		},
		{
			name:      "unknown option ignored",
			input:     values.List(values.NewSymbol("unknown"), values.NewSymbol("positions")),
			positions: true,
			comments:  false,
			wantMode:  0,
		},
		{
			name:      "non-symbol element ignored",
			input:     values.List(values.NewString("not-a-symbol")),
			positions: false,
			comments:  false,
			wantMode:  0,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			opts, mode := parseOpts(tc.input, fset)
			c.Assert(opts.positions, qt.Equals, tc.positions)
			c.Assert(opts.comments, qt.Equals, tc.comments)
			c.Assert(mode, qt.Equals, tc.wantMode)
		})
	}
}

func TestUnmapNodeMalformedFields(t *testing.T) {
	tcs := []struct {
		name  string
		input values.Value
	}{
		{
			name:  "ident missing name field",
			input: node("ident"),
		},
		{
			name:  "ident name wrong type",
			input: node("ident", field("name", values.NewInteger(42))),
		},
		{
			name:  "binary-expr missing op",
			input: node("binary-expr"),
		},
		{
			name:  "lit bad kind symbol",
			input: node("lit", field("kind", sym("NONEXISTENT")), field("value", str("42"))),
		},
		{
			name:  "call-expr missing fun",
			input: node("call-expr"),
		},
		{
			name:  "func-type missing params",
			input: node("func-type"),
		},
		{
			name:  "assign-stmt missing lhs",
			input: node("assign-stmt"),
		},
		{
			name:  "if-stmt missing cond",
			input: node("if-stmt", field("init", values.FalseValue)),
		},
		{
			name:  "for-stmt missing init",
			input: node("for-stmt"),
		},
		{
			name:  "range-stmt missing key",
			input: node("range-stmt"),
		},
		{
			name:  "branch-stmt missing tok",
			input: node("branch-stmt"),
		},
		{
			name:  "inc-dec-stmt missing x",
			input: node("inc-dec-stmt"),
		},
		{
			name:  "selector-expr missing x",
			input: node("selector-expr"),
		},
		{
			name:  "index-expr missing x",
			input: node("index-expr"),
		},
		{
			name:  "star-expr missing x",
			input: node("star-expr"),
		},
		{
			name:  "paren-expr missing x",
			input: node("paren-expr"),
		},
		{
			name:  "composite-lit missing type",
			input: node("composite-lit"),
		},
		{
			name:  "kv-expr missing key",
			input: node("kv-expr"),
		},
		{
			name:  "func-lit missing type",
			input: node("func-lit"),
		},
		{
			name:  "array-type missing len",
			input: node("array-type"),
		},
		{
			name:  "map-type missing key",
			input: node("map-type"),
		},
		{
			name:  "struct-type missing fields",
			input: node("struct-type"),
		},
		{
			name:  "interface-type missing methods",
			input: node("interface-type"),
		},
		{
			name:  "file missing name",
			input: node("file"),
		},
		{
			name:  "gen-decl missing tok",
			input: node("gen-decl"),
		},
		{
			name:  "import-spec missing name",
			input: node("import-spec"),
		},
		{
			name:  "value-spec missing names",
			input: node("value-spec"),
		},
		{
			name:  "type-spec missing name",
			input: node("type-spec"),
		},
		{
			name:  "block missing list",
			input: node("block"),
		},
		{
			name:  "return-stmt missing results",
			input: node("return-stmt"),
		},
		{
			name:  "expr-stmt missing x",
			input: node("expr-stmt"),
		},
		{
			name:  "decl-stmt missing decl",
			input: node("decl-stmt"),
		},
		{
			name:  "unary-expr missing op",
			input: node("unary-expr"),
		},
		{
			name:  "field missing names",
			input: node("field"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := unmapNode(tc.input)
			qt.New(t).Assert(err, qt.IsNotNil)
		})
	}
}

func TestUnmapExprTypeMismatch(t *testing.T) {
	// Pass a statement node where an expression is expected.
	stmtNode := node("block", field("list", values.EmptyList))
	_, err := unmapExpr(stmtNode)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestUnmapStmtTypeMismatch(t *testing.T) {
	// Pass an expression node where a statement is expected.
	exprNode := node("ident", field("name", str("x")))
	_, err := unmapStmt(exprNode)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestMapFieldListOrFalse(t *testing.T) {
	c := qt.New(t)
	opts := &mapperOpts{}

	// nil FieldList maps to #f
	result := mapFieldListOrFalse(nil, opts)
	b, ok := result.(*values.Boolean)
	c.Assert(ok, qt.IsTrue)
	c.Assert(b.Value, qt.IsFalse)

	// empty FieldList maps to empty list
	result = mapFieldListOrFalse(&ast.FieldList{}, opts)
	c.Assert(values.IsEmptyList(result), qt.IsTrue)
}
