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
		{
			name:   "type assert",
			source: "x.(int)",
		},
		{
			name:   "slice 2-index",
			source: "s[1:3]",
		},
		{
			name:   "slice 3-index",
			source: "s[1:3:5]",
		},
		{
			name:   "slice no low",
			source: "s[:3]",
		},
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
		{
			name: "go statement",
			source: `package p

func f(g func()) {
	go g()
}
`,
		},
		{
			name: "defer statement",
			source: `package p

func f(g func()) {
	defer g()
}
`,
		},
		{
			name: "labeled statement",
			source: `package p

func f() {
outer:
	for {
		break outer
	}
}
`,
		},
		{
			name: "switch statement",
			source: `package p

func f(x int) int {
	switch x {
	case 1:
		return 10
	case 2, 3:
		return 20
	default:
		return 0
	}
}
`,
		},
		{
			name: "bare switch",
			source: `package p

func f(x int) {
	switch {
	case x > 0:
		return
	}
}
`,
		},
		{
			name: "type switch",
			source: `package p

func f(x Any) {
	switch v := x.(type) {
	case int:
		_ = v
	case string:
		_ = v
	default:
		_ = v
	}
}
`,
		},
		{
			name: "type assertion",
			source: `package p

func f(x Any) int {
	return x.(int)
}
`,
		},
		{
			name: "send statement",
			source: `package p

func f(ch chan int) {
	ch <- 42
}
`,
		},
		{
			name: "select statement",
			source: `package p

func f(c1, c2 chan int) int {
	select {
	case v := <-c1:
		return v
	case c2 <- 42:
		return 0
	default:
		return -1
	}
}
`,
		},
		{
			name: "slice expr",
			source: `package p

func f(s []int) []int {
	return s[1:3]
}
`,
		},
		{
			name: "3-index slice",
			source: `package p

func f(s []int) []int {
	return s[1:3:5]
}
`,
		},
		{
			name: "channel types",
			source: `package p

var (
	a chan int
	b chan<- int
	c <-chan int
)
`,
		},
		{
			name: "variadic function",
			source: `package p

func f(args ...int) {
}
`,
		},
		{
			name: "array ellipsis",
			source: `package p

var a = [...]int{1, 2, 3}
`,
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
			input: Node("nonexistent-node-type"),
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
			input: Node("ident"),
		},
		{
			name:  "ident name wrong type",
			input: Node("ident", Field("name", values.NewInteger(42))),
		},
		{
			name:  "binary-expr missing op",
			input: Node("binary-expr"),
		},
		{
			name:  "lit bad kind symbol",
			input: Node("lit", Field("kind", Sym("NONEXISTENT")), Field("value", Str("42"))),
		},
		{
			name:  "call-expr missing fun",
			input: Node("call-expr"),
		},
		{
			name:  "func-type missing params",
			input: Node("func-type"),
		},
		{
			name:  "assign-stmt missing lhs",
			input: Node("assign-stmt"),
		},
		{
			name:  "if-stmt missing cond",
			input: Node("if-stmt", Field("init", values.FalseValue)),
		},
		{
			name:  "for-stmt missing init",
			input: Node("for-stmt"),
		},
		{
			name:  "range-stmt missing key",
			input: Node("range-stmt"),
		},
		{
			name:  "branch-stmt missing tok",
			input: Node("branch-stmt"),
		},
		{
			name:  "inc-dec-stmt missing x",
			input: Node("inc-dec-stmt"),
		},
		{
			name:  "selector-expr missing x",
			input: Node("selector-expr"),
		},
		{
			name:  "index-expr missing x",
			input: Node("index-expr"),
		},
		{
			name:  "star-expr missing x",
			input: Node("star-expr"),
		},
		{
			name:  "paren-expr missing x",
			input: Node("paren-expr"),
		},
		{
			name:  "composite-lit missing type",
			input: Node("composite-lit"),
		},
		{
			name:  "kv-expr missing key",
			input: Node("kv-expr"),
		},
		{
			name:  "func-lit missing type",
			input: Node("func-lit"),
		},
		{
			name:  "array-type missing len",
			input: Node("array-type"),
		},
		{
			name:  "map-type missing key",
			input: Node("map-type"),
		},
		{
			name:  "struct-type missing fields",
			input: Node("struct-type"),
		},
		{
			name:  "interface-type missing methods",
			input: Node("interface-type"),
		},
		{
			name:  "file missing name",
			input: Node("file"),
		},
		{
			name:  "gen-decl missing tok",
			input: Node("gen-decl"),
		},
		{
			name:  "import-spec missing name",
			input: Node("import-spec"),
		},
		{
			name:  "value-spec missing names",
			input: Node("value-spec"),
		},
		{
			name:  "type-spec missing name",
			input: Node("type-spec"),
		},
		{
			name:  "block missing list",
			input: Node("block"),
		},
		{
			name:  "return-stmt missing results",
			input: Node("return-stmt"),
		},
		{
			name:  "expr-stmt missing x",
			input: Node("expr-stmt"),
		},
		{
			name:  "decl-stmt missing decl",
			input: Node("decl-stmt"),
		},
		{
			name:  "unary-expr missing op",
			input: Node("unary-expr"),
		},
		{
			name:  "field missing names",
			input: Node("field"),
		},
		{
			name:  "go-stmt missing call",
			input: Node("go-stmt"),
		},
		{
			name:  "defer-stmt missing call",
			input: Node("defer-stmt"),
		},
		{
			name:  "send-stmt missing chan",
			input: Node("send-stmt"),
		},
		{
			name:  "labeled-stmt missing label",
			input: Node("labeled-stmt"),
		},
		{
			name:  "switch-stmt missing init",
			input: Node("switch-stmt"),
		},
		{
			name:  "type-switch-stmt missing init",
			input: Node("type-switch-stmt"),
		},
		{
			name:  "case-clause missing list",
			input: Node("case-clause"),
		},
		{
			name:  "select-stmt missing body",
			input: Node("select-stmt"),
		},
		{
			name:  "comm-clause missing comm",
			input: Node("comm-clause"),
		},
		{
			name:  "type-assert-expr missing x",
			input: Node("type-assert-expr"),
		},
		{
			name:  "slice-expr missing x",
			input: Node("slice-expr"),
		},
		{
			name:  "ellipsis missing elt",
			input: Node("ellipsis"),
		},
		{
			name:  "chan-type missing dir",
			input: Node("chan-type"),
		},
		{
			name:  "go-stmt call not call-expr",
			input: Node("go-stmt", Field("call", Node("ident", Field("name", Str("x"))))),
		},
		{
			name:  "slice-expr slice3 not boolean",
			input: Node("slice-expr", Field("x", Node("ident", Field("name", Str("s")))), Field("low", values.FalseValue), Field("high", values.FalseValue), Field("max", values.FalseValue), Field("slice3", Str("yes"))),
		},
		{
			name:  "chan-type invalid dir symbol",
			input: Node("chan-type", Field("dir", Sym("invalid")), Field("value", Node("ident", Field("name", Str("int"))))),
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
	stmtNode := Node("block", Field("list", values.EmptyList))
	_, err := unmapExpr(stmtNode)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestUnmapStmtTypeMismatch(t *testing.T) {
	// Pass an expression node where a statement is expected.
	exprNode := Node("ident", Field("name", Str("x")))
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
