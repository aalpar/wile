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

package parser

import (
	"context"
	"io"
	"math"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/syntax/syntaxtest"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// getComplexParts extracts real and imaginary parts as float64 from
// a complex number. Also handles real numbers (Float, Integer) which
// are complex with zero imaginary part per R7RS numeric tower.
func getComplexParts(n values.Number) (rel, iam float64) {
	switch v := n.(type) {
	case *values.Complex:
		return v.Real(), v.Imag()
	case *values.BigComplex:
		return v.RealAsBigFloat().Float64Truncated(), v.ImagAsBigFloat().Float64Truncated()
	case *values.Float:
		return v.Value, 0
	case *values.Integer:
		return float64(v.Value), 0
	}
	panic("not a complex number")
}

func TestParser_Read(t *testing.T) {
	tcs := []struct {
		in      string
		expect  values.Value
		sexpect syntax.SyntaxValue
		err     error
	}{
		{
			in:     "10",
			expect: values.NewInteger(10),
			sexpect: syntax.NewSyntaxObject(values.NewInteger(10),
				syntax.NewSourceContext("10", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in:     "begin",
			expect: values.NewSymbol("begin"),
			sexpect: syntax.NewSyntaxSymbol(
				"begin",
				syntax.NewSourceContext("begin", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(5, 5, 0),
				),
			),
		},
		{
			in:     "#t",
			expect: values.TrueValue,
			sexpect: syntax.NewSyntaxObject(
				values.TrueValue,
				syntax.NewSourceContext("#t", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in:     "#f",
			expect: values.FalseValue,
			sexpect: syntax.NewSyntaxObject(
				values.FalseValue,
				syntax.NewSourceContext("#f", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in:     "#false",
			expect: values.FalseValue,
			sexpect: syntax.NewSyntaxObject(
				values.FalseValue,
				syntax.NewSourceContext("#false", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(6, 6, 0),
				),
			),
		},
		{
			in:     "#true",
			expect: values.TrueValue,
			sexpect: syntax.NewSyntaxObject(
				values.TrueValue,
				syntax.NewSourceContext("#true", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(5, 5, 0),
				),
			),
		},
		{
			in: "'begin",
			expect: values.List(
				values.NewSymbol("quote"),
				values.NewSymbol("begin"),
			),
			sexpect: syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol(
					"quote",
					syntax.NewSourceContext(
						"'", "",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(1, 1, 0),
					),
				),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol(
						"begin",
						syntax.NewSourceContext(
							"begin", "",
							syntax.NewSourceIndexes(1, 1, 0),
							syntax.NewSourceIndexes(6, 6, 0),
						),
					),
					syntax.SyntaxEmptyList,
					syntax.NewSourceContext(
						"'", "",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(1, 1, 0),
					),
				),
				syntax.NewSourceContext(
					"'", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		{
			in: "( 'begin )",
			expect: values.List(
				values.List(
					values.NewSymbol("quote"),
					values.NewSymbol("begin"),
				),
			),
			sexpect: syntax.NewSyntaxCons(
				syntax.NewSyntaxCons(
					syntax.NewSyntaxSymbol(
						"quote",
						syntax.NewSourceContext(
							"'", "",
							syntax.NewSourceIndexes(2, 2, 0),
							syntax.NewSourceIndexes(3, 3, 0),
						),
					),
					syntax.NewSyntaxCons(
						syntax.NewSyntaxSymbol(
							"begin",
							syntax.NewSourceContext(
								"begin", "",
								syntax.NewSourceIndexes(3, 3, 0),
								syntax.NewSourceIndexes(8, 8, 0),
							),
						),
						syntax.SyntaxEmptyList,
						syntax.NewSourceContext(
							"'", "",
							syntax.NewSourceIndexes(2, 2, 0),
							syntax.NewSourceIndexes(3, 3, 0),
						),
					),
					syntax.NewSourceContext(
						"'", "",
						syntax.NewSourceIndexes(2, 2, 0),
						syntax.NewSourceIndexes(3, 3, 0),
					),
				),
				syntax.SyntaxEmptyList,
				syntax.NewSourceContext(
					"(", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		{
			in: "'( begin )",
			expect: values.List(
				values.NewSymbol("quote"),
				values.List(
					values.NewSymbol("begin"),
				),
			),
		},
		{
			in: "( quote begin )",
			expect: values.List(
				values.NewSymbol("quote"),
				values.NewSymbol("begin"),
			),
		},
		{
			in: "#'begin",
			expect: values.List(
				values.NewSymbol("syntax"),
				values.NewSymbol("begin"),
			),
		},
		{
			in: "( #'begin )",
			expect: values.List(
				values.List(
					values.NewSymbol("syntax"),
					values.NewSymbol("begin"),
				),
			),
		},
		{
			in: "#'( begin )",
			expect: values.List(
				values.NewSymbol("syntax"),
				values.List(
					values.NewSymbol("begin"),
				),
			),
		},
		{
			in: "( syntax begin )",
			expect: values.List(
				values.NewSymbol("syntax"),
				values.NewSymbol("begin"),
			),
		},
		{
			in:     "( begin )",
			expect: values.List(values.NewSymbol("begin")),
		},
		{
			in: "( begin another )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.NewSymbol("another"),
			),
		},
		{
			in:     "#( 10 20 )",
			expect: values.NewVector(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			in:     "#u8( 10 20 )",
			expect: values.NewByteVector(values.NewByte(10), values.NewByte(20)),
		},
		{
			in:  "#u8( 256 )",
			err: werr.ErrNotAByte,
		},
		{
			in:  "#u8( -1 )",
			err: werr.ErrNotAByte,
		},
		{
			in:  "#u8( 10 300 )",
			err: werr.ErrNotAByte,
		},
		{
			in:     "( 10 . 20 )",
			expect: values.NewCons(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			in:  ". 20",
			err: werr.ErrNotACons,
		},
		{
			in:     ".20",
			expect: values.NewFloat(0.20),
		},
		{
			in:     "()",
			expect: values.EmptyList,
		},
		{
			in: "'()",
			expect: values.List(
				values.NewSymbol("quote"),
				values.EmptyList,
			),
		},
		{
			in: "( begin ( another ) )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.List(
					values.NewSymbol("another"),
				),
			),
		},
		{
			in: "( begin () )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.EmptyList),
		},
		{
			in: "( begin () () )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.EmptyList,
				values.EmptyList,
			),
		},
		{
			in: "( begin ( define ) )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.List(
					values.NewSymbol("define"),
				)),
		},
		{
			in: "( begin ( define foo 10 ) )",
			expect: values.List(
				values.NewSymbol("begin"),
				values.List(
					values.NewSymbol("define"),
					values.NewSymbol("foo"),
					values.NewInteger(10),
				),
			),
		},
		{
			in: "#10=( 10 20 )",
			sexpect: syntax.NewSyntaxDatumLabelAssignment(
				10,
				syntax.NewSyntaxCons(
					syntax.NewSyntaxObject(
						values.NewInteger(10),
						syntax.NewSourceContext("10", "",
							syntax.NewSourceIndexes(6, 6, 0),
							syntax.NewSourceIndexes(8, 8, 0),
						),
					),
					syntax.NewSyntaxCons(
						syntax.NewSyntaxObject(
							values.NewInteger(20),
							syntax.NewSourceContext("20", "",
								syntax.NewSourceIndexes(9, 9, 0),
								syntax.NewSourceIndexes(11, 11, 0),
							),
						),
						syntax.SyntaxEmptyList,
						syntax.NewSourceContext("20", "",
							syntax.NewSourceIndexes(9, 9, 0),
							syntax.NewSourceIndexes(11, 11, 0),
						),
					),
					syntax.NewSourceContext("(", "",
						syntax.NewSourceIndexes(4, 4, 0),
						syntax.NewSourceIndexes(5, 5, 0),
					),
				),
				syntax.NewSourceContext("#10=", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(4, 4, 0),
				),
			),
		},
		{
			in: "#10#",
			sexpect: syntax.NewSyntaxDatumLabel(10,
				syntax.NewSourceContext("#10#", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(4, 4, 0),
				),
			),
		},
		{
			in:     "#\\newline",
			expect: values.NewCharacter('\n'),
		},
		{
			in:     "#\\x0a",
			expect: values.NewCharacter(10),
		},
		{
			in: "#!this-is-a-test",
			sexpect: syntax.NewSyntaxDirective("this-is-a-test",
				syntax.NewSourceContext("#!this-is-a-test", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(16, 16, 0),
				),
			),
		},
		{
			in: "; this is a comment",
			sexpect: syntax.NewSyntaxComment("; this is a comment",
				syntax.NewSourceContext("; this is a comment", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(19, 19, 0),
				),
			),
		},
		{
			in: "#;( this is a comment )",
			sexpect: syntax.NewSyntaxObject(
				syntax.NewSyntaxDatumComment("#;",
					syntax.NewSyntaxCons(
						syntax.NewSyntaxSymbol(
							"this",
							syntax.NewSourceContext("this", "",
								syntax.NewSourceIndexes(4, 4, 0),
								syntax.NewSourceIndexes(8, 8, 0),
							),
						),
						syntax.NewSyntaxCons(
							syntax.NewSyntaxSymbol(
								"is",
								syntax.NewSourceContext("is", "",
									syntax.NewSourceIndexes(9, 9, 0),
									syntax.NewSourceIndexes(11, 11, 0),
								),
							),
							syntax.NewSyntaxCons(
								syntax.NewSyntaxSymbol(
									"a",
									syntax.NewSourceContext("a", "",
										syntax.NewSourceIndexes(12, 12, 0),
										syntax.NewSourceIndexes(13, 13, 0),
									),
								),
								syntax.NewSyntaxCons(
									syntax.NewSyntaxSymbol(
										"comment",
										syntax.NewSourceContext("comment", "",
											syntax.NewSourceIndexes(14, 14, 0),
											syntax.NewSourceIndexes(21, 21, 0),
										),
									),
									syntax.SyntaxEmptyList,
									syntax.NewSourceContext("a", "",
										syntax.NewSourceIndexes(12, 12, 0),
										syntax.NewSourceIndexes(13, 13, 0),
									),
								),
								syntax.NewSourceContext("is", "",
									syntax.NewSourceIndexes(9, 9, 0),
									syntax.NewSourceIndexes(11, 11, 0),
								),
							),
							syntax.NewSourceContext("this", "",
								syntax.NewSourceIndexes(4, 4, 0),
								syntax.NewSourceIndexes(8, 8, 0),
							),
						),
						syntax.NewSourceContext("(", "",
							syntax.NewSourceIndexes(2, 2, 0),
							syntax.NewSourceIndexes(3, 3, 0),
						),
					),
					syntax.NewSourceContext(")", "",
						syntax.NewSourceIndexes(22, 22, 0),
						syntax.NewSourceIndexes(23, 23, 0),
					),
				),
				syntax.NewSourceContext(")", "",
					syntax.NewSourceIndexes(22, 22, 0),
					syntax.NewSourceIndexes(23, 23, 0),
				),
			),
		},
		// Block comment tests
		{
			in: "#| block comment |#", // cant use '#' because of lack of escape at shell level (interpreted as bash comments)
			sexpect: syntax.NewSyntaxComment("#| block comment |#",
				syntax.NewSourceContext("#| block comment |#", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(19, 19, 0),
				),
			),
		},
		{
			in: "#||#",
			sexpect: syntax.NewSyntaxComment("#||#",
				syntax.NewSourceContext("#||#", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(4, 4, 0),
				),
			),
		},
		{
			in: "#| outer #| nested |# outer |#",
			sexpect: syntax.NewSyntaxComment("#| outer #| nested |# outer |#",
				syntax.NewSourceContext("#| outer #| nested |# outer |#", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(30, 30, 0),
				),
			),
		},
		{
			in: "#| multi\nline\ncomment |#",
			sexpect: syntax.NewSyntaxComment("#| multi\nline\ncomment |#",
				syntax.NewSourceContext("#| multi\nline\ncomment |#", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(24, 10, 2), // Fixed: 24 bytes (was 26 due to double-counting newlines)
				),
			),
		},
		// Line comment with newline (not EOF)
		{
			in: "; comment with newline\n",
			sexpect: syntax.NewSyntaxComment("; comment with newline",
				syntax.NewSourceContext("; comment with newline", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(22, 22, 0),
				),
			),
		},
		// Empty line comment
		{
			in: ";\n",
			sexpect: syntax.NewSyntaxComment(";",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		// Empty line comment at EOF
		{
			in: ";",
			sexpect: syntax.NewSyntaxComment(";",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		// Line comment with multiple semicolons
		{
			in: ";;; triple semicolon",
			sexpect: syntax.NewSyntaxComment(";;; triple semicolon",
				syntax.NewSourceContext(";;; triple semicolon", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(20, 20, 0),
				),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, false, strings.NewReader(tc.in))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.ErrorIs, tc.err)
			if err != nil {
				return
			}
			if tc.sexpect != nil {
				c.Assert(syn, syntaxtest.SyntaxEquals, tc.sexpect)
			}
			if tc.expect != nil {
				v := syn.UnwrapAll()
				c.Assert(v, valuestest.SchemeEquals, tc.expect)
			}
		})
	}
}

// TestParse tests the Parse convenience function.
func TestParse(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	syn, err := NewParser(env, true, strings.NewReader("42")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestParser_Close tests the Close function.
func TestParser_Close(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("10 20"))
	p.skipComment = false

	// Read first expression
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(10))

	// Close the parser
	err = p.Close()
	c.Assert(err, qt.IsNil)

	// Closing again should error
	err = p.Close()
	c.Assert(err, qt.ErrorIs, ErrAlreadyClosed)
}

// TestParser_Text tests the Text function.
func TestParser_Text(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("hello"))
	p.skipComment = false

	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	text := p.Text()
	c.Assert(text, qt.Equals, "hello")
}

// TestParser_Quasiquote tests quasiquote, unquote, and unquote-splicing.
func TestParser_Quasiquote(t *testing.T) {
	tcs := []struct {
		in     string
		expect values.Value
	}{
		{
			in: "`foo",
			expect: values.List(
				values.NewSymbol("quasiquote"),
				values.NewSymbol("foo"),
			),
		},
		{
			in: ",foo",
			expect: values.List(
				values.NewSymbol("unquote"),
				values.NewSymbol("foo"),
			),
		},
		{
			in: ",@foo",
			expect: values.List(
				values.NewSymbol("unquote-splicing"),
				values.NewSymbol("foo"),
			),
		},
		{
			in: "`(a ,b ,@c)",
			expect: values.List(
				values.NewSymbol("quasiquote"),
				values.List(
					values.NewSymbol("a"),
					values.List(values.NewSymbol("unquote"), values.NewSymbol("b")),
					values.List(values.NewSymbol("unquote-splicing"), values.NewSymbol("c")),
				),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_Quasisyntax tests quasisyntax, unsyntax, and unsyntax-splicing.
func TestParser_Quasisyntax(t *testing.T) {
	tcs := []struct {
		in     string
		expect values.Value
	}{
		{
			in: "#`foo",
			expect: values.List(
				values.NewSymbol("quasisyntax"),
				values.NewSymbol("foo"),
			),
		},
		{
			in: "#,foo",
			expect: values.List(
				values.NewSymbol("unsyntax"),
				values.NewSymbol("foo"),
			),
		},
		{
			in: "#,@foo",
			expect: values.List(
				values.NewSymbol("unsyntax-splicing"),
				values.NewSymbol("foo"),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_Strings tests string parsing.
func TestParser_Strings(t *testing.T) {
	tcs := []struct {
		in     string
		expect values.Value
	}{
		{
			in:     `"hello"`,
			expect: values.NewString("hello"),
		},
		{
			in:     `"hello world"`,
			expect: values.NewString("hello world"),
		},
		{
			in:     `""`,
			expect: values.NewString(""),
		},
		// Escape sequence tests
		{
			in:     `"hello\nworld"`,
			expect: values.NewString("hello\nworld"),
		},
		{
			in:     `"tab\there"`,
			expect: values.NewString("tab\there"),
		},
		{
			in:     `"quote\"here"`,
			expect: values.NewString("quote\"here"),
		},
		{
			in:     `"back\\slash"`,
			expect: values.NewString("back\\slash"),
		},
		{
			in:     `"return\rhere"`,
			expect: values.NewString("return\rhere"),
		},
		{
			in:     `"alarm\ahere"`,
			expect: values.NewString("alarm\ahere"),
		},
		{
			in:     `"backspace\bhere"`,
			expect: values.NewString("backspace\bhere"),
		},
		{
			in:     `"hex\x41;here"`,
			expect: values.NewString("hexAhere"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_MoreCharacters tests additional character literals.
func TestParser_MoreCharacters(t *testing.T) {
	tcs := []struct {
		in     string
		expect values.Value
	}{
		{in: "#\\a", expect: values.NewCharacter('a')},
		{in: "#\\Z", expect: values.NewCharacter('Z')},
		{in: "#\\space", expect: values.NewCharacter(' ')},
		{in: "#\\tab", expect: values.NewCharacter('\t')},
		{in: "#\\return", expect: values.NewCharacter('\r')},
		{in: "#\\null", expect: values.NewCharacter(0)},
		{in: "#\\alarm", expect: values.NewCharacter('\a')},
		{in: "#\\backspace", expect: values.NewCharacter('\b')},
		{in: "#\\escape", expect: values.NewCharacter(27)},
		{in: "#\\delete", expect: values.NewCharacter(127)},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_CommentsFollowedByCode tests that comments are properly parsed
// and the parser can continue to read subsequent expressions.
func TestParser_CommentsFollowedByCode(t *testing.T) {
	tcs := []struct {
		name         string
		in           string
		skipComments bool
		expects      []values.Value
	}{
		{
			name:         "line comment followed by integer",
			in:           "; comment\n42",
			skipComments: false,
			expects: []values.Value{
				syntax.NewSyntaxComment(
					"; comment",
					syntax.NewSourceContext(
						"; comment",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				values.NewInteger(42),
			},
		},
		{
			name:         "line comment followed by integer",
			in:           "; comment\n42",
			skipComments: true,
			expects: []values.Value{
				values.NewInteger(42),
			},
		},
		{
			name:         "block comment followed by symbol",
			in:           "#| comment |# foo",
			skipComments: false,
			expects: []values.Value{
				syntax.NewSyntaxComment(
					"#| comment |#",
					syntax.NewSourceContext(
						"#| comment |#",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				values.NewSymbol("foo"),
			},
		},
		{
			// Datum comments produce the commented datum as a value
			name:         "datum comment followed by integer",
			in:           "#;(ignored) 42",
			skipComments: false,
			expects: []values.Value{
				syntax.NewSyntaxDatumComment(
					"#;",
					syntax.NewSyntaxCons(
						syntax.NewSyntaxSymbol(
							"ignored",
							syntax.NewSourceContext(
								"",
								"",
								syntax.NewSourceIndexes(0, 0, 0),
								syntax.NewSourceIndexes(0, 0, 0))),
						syntax.SyntaxEmptyList,
						syntax.NewSourceContext(
							"",
							"",
							syntax.NewSourceIndexes(0, 0, 0),
							syntax.NewSourceIndexes(0, 0, 0))),
					syntax.NewSourceContext(
						"ignored",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				values.NewInteger(42),
			},
		},
		{
			name:         "multiple line comments followed by code",
			in:           "; first\n; second\n10",
			skipComments: false,
			expects: []values.Value{
				syntax.NewSyntaxComment(
					"; first",
					syntax.NewSourceContext(
						"; first",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				syntax.NewSyntaxComment(
					"; second",
					syntax.NewSourceContext(
						"; second",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				values.NewInteger(10),
			},
		},
		{
			name:         "block comment between expressions",
			in:           "10 #| middle |# 20",
			skipComments: false,
			expects: []values.Value{
				values.NewInteger(10),
				syntax.NewSyntaxComment(
					"#| middle |#",
					syntax.NewSourceContext(
						"#| middle |#",
						"",
						syntax.NewSourceIndexes(0, 0, 0),
						syntax.NewSourceIndexes(0, 0, 0))),
				values.NewInteger(20),
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, tc.skipComments, strings.NewReader(tc.in))

			for i, expect := range tc.expects {
				syn, err := p.ReadSyntax(context.TODO())
				if err == io.EOF {
					c.Fatalf("unexpected EOF at index %d, expected %v", i, expect)
				}
				c.Assert(err, qt.IsNil, qt.Commentf("error at index %d", i))

				var got values.Value
				got = syn
				if !schemeutil.IsSyntaxComment(syn) {
					got = syn.UnwrapAll()
				}
				// For datum comments, just check the type since the inner form varies
				c.Assert(got, valuestest.SchemeEquals, expect, qt.Commentf("mismatch at index %d", i))
			}

			// Verify we've consumed everything
			_, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.Equals, io.EOF)
		})
	}
}

// TestParser_CommentsInsideLists tests that comments inside lists are properly
// skipped when skipComments is true. This is important for macro definitions
// where comments between clauses should be ignored.
func TestParser_CommentsInsideLists(t *testing.T) {
	tcs := []struct {
		name   string
		in     string
		expect values.Value
	}{
		{
			name:   "line comment inside list",
			in:     "(1 ;; comment\n2 3)",
			expect: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:   "block comment inside list",
			in:     "(a #| comment |# b c)",
			expect: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name:   "datum comment inside list",
			in:     "(1 #;ignored 2 3)",
			expect: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:   "multiple comments inside list",
			in:     "(a ;; first\n#| second |# b #;c d)",
			expect: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("d")),
		},
		{
			name:   "comment at start of list",
			in:     "(;; comment\n1 2)",
			expect: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name:   "comment at end of list",
			in:     "(1 2 ;; comment\n)",
			expect: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name:   "nested list with comment",
			in:     "((a ;; inner comment\nb) c)",
			expect: values.List(values.List(values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("c")),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.in)) // skipComments = true

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)

			// Verify we've consumed everything
			_, err = p.ReadSyntax(context.TODO())
			c.Assert(err, qt.Equals, io.EOF)
		})
	}
}

// TestParserError tests the ParserError type.
func TestParserError(t *testing.T) {
	c := qt.New(t)

	// Test NewTokenizerError
	err1 := NewParserError(nil, "test error")
	c.Assert(err1.Error(), qt.Equals, "test error")
	c.Assert(err1.Unwrap(), qt.IsNil)

	// Test NewTokenizerErrorWithWrap
	innerErr := werr.NewForeignErrorf("inner error")
	err2 := NewParserErrorWithWrap(innerErr, nil, "wrapped error")
	c.Assert(err2.Error(), qt.Equals, "wrapped error")
	c.Assert(err2.Unwrap(), qt.Equals, innerErr)

	// Test Is
	c.Assert(err1.Is(err2), qt.IsTrue)
	c.Assert(err1.Is(innerErr), qt.IsFalse)
}

// ============================================================================
// Rational Number Parsing Tests
// ============================================================================

func TestParseRational(t *testing.T) {
	// Cases that remain Rational after Simplify
	rationalCases := []struct {
		input string
		num   int64
		denom int64
	}{
		{"1/2", 1, 2},
		{"3/4", 3, 4},
		{"-1/2", -1, 2},
		{"+3/4", 3, 4},
		{"10/3", 10, 3},
		{"-7/8", -7, 8},
		{"100/200", 1, 2}, // big.Rat normalizes to 1/2
	}
	for _, tc := range rationalCases {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			r, err := p.parseRational(tc.input)
			c.Assert(err, qt.IsNil)
			rat, ok := r.(*values.Rational)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Rational, got %T: %v", r, r))
			c.Assert(rat.Num().Int64(), qt.Equals, tc.num)
			c.Assert(rat.Denom().Int64(), qt.Equals, tc.denom)
		})
	}

	// Cases that reduce to Integer after Simplify
	integerCases := []struct {
		input  string
		expect int64
	}{
		{"0/1", 0},
		{"10/2", 5},
		{"6/3", 2},
		{"-9/3", -3},
		{"0/10", 0},
	}
	for _, tc := range integerCases {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			r, err := p.parseRational(tc.input)
			c.Assert(err, qt.IsNil)
			intVal, ok := r.(*values.Integer)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Integer, got %T: %v", r, r))
			c.Assert(intVal.Value, qt.Equals, tc.expect)
		})
	}
}

func TestParseRationalErrors(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{"abc"},
		{"1/0/2"},
		{""},
		{"1.5/2"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			_, err := p.parseRational(tc.input)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// ============================================================================
// Pure Imaginary Number Parsing Tests
// ============================================================================

func TestParseImaginary(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
		exact bool // true if result should be exact BigComplex
	}{
		{"+i", 0, 1, true},
		{"-i", 0, -1, true},
		{"+3i", 0, 3, true},
		{"-3i", 0, -3, true},
		{"+2.5i", 0, 2.5, false},
		{"-2.5i", 0, -2.5, false},
		{"+0.5i", 0, 0.5, false},
		{"-0.5i", 0, -0.5, false},
		{"+100i", 0, 100, true},
		{"-100i", 0, -100, true},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			z, err := p.parseImaginary(tc.input)
			c.Assert(err, qt.IsNil)
			if tc.exact {
				// Should be exact BigComplex
				bc, ok := z.(*values.BigComplex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigComplex, got %T", z))
				c.Assert(bc.IsExact(), qt.IsTrue)
				// For exact complex, real and imag are BigInteger
				realBi, realOk := bc.Real().(*values.BigInteger)
				c.Assert(realOk, qt.IsTrue)
				c.Assert(realBi.Int64(), qt.Equals, int64(tc.real))
				imagBi, imagOk := bc.Imag().(*values.BigInteger)
				c.Assert(imagOk, qt.IsTrue)
				c.Assert(imagBi.Int64(), qt.Equals, int64(tc.imag))
			} else {
				// Should be inexact Complex
				cplx, ok := z.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected Complex, got %T", z))
				c.Assert(cplx.Real(), qt.Equals, tc.real)
				c.Assert(cplx.Imag(), qt.Equals, tc.imag)
			}
		})
	}
}

func TestParseImaginaryErrors(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{"abc"},
		{"+abci"}, // invalid coefficient
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			_, err := p.parseImaginary(tc.input)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// ============================================================================
// Complex Number Parsing Tests
// ============================================================================

func TestParseComplex(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		// Integer real and imaginary parts
		{"1+2i", 1, 2},
		{"1-2i", 1, -2},
		{"3+4i", 3, 4},
		{"3-4i", 3, -4},
		{"0+1i", 0, 1},
		{"0-1i", 0, -1},
		{"10+20i", 10, 20},
		{"10-20i", 10, -20},

		// Decimal real and imaginary parts
		{"1.5+2.5i", 1.5, 2.5},
		{"1.5-2.5i", 1.5, -2.5},
		{"0.5+0.5i", 0.5, 0.5},
		{"0.5-0.5i", 0.5, -0.5},
		{"3.14+2.71i", 3.14, 2.71},

		// Unit imaginary coefficient (1+i, 5-i)
		{"1+i", 1, 1},
		{"1-i", 1, -1},
		{"5+i", 5, 1},
		{"5-i", 5, -1},
		{"0+i", 0, 1},
		{"0-i", 0, -1},

		// Negative real parts
		{"-1+2i", -1, 2},
		{"-1-2i", -1, -2},
		{"-3.5+4.5i", -3.5, 4.5},
		{"-3.5-4.5i", -3.5, -4.5},

		// Zero components
		{"0+0i", 0, 0},
		{"1+0i", 1, 0},
		{"0+5i", 0, 5},

		// Scientific notation in real part
		{"1e2+3i", 100, 3},
		{"1.5e2+3i", 150, 3},
		{"1e-2+3i", 0.01, 3},

		// Scientific notation in imaginary part
		{"1+3e2i", 1, 300},
		{"1+3.5e2i", 1, 350},
		{"1+3e-2i", 1, 0.03},

		// Scientific notation in both parts
		{"1e2+3e2i", 100, 300},
		{"1.5e2+2.5e2i", 150, 250},

		// Rational real parts
		{"3/2+i", 1.5, 1},
		{"3/2-i", 1.5, -1},
		{"1/2+2i", 0.5, 2},
		{"-3/2+i", -1.5, 1},
		{"-3/2-i", -1.5, -1},
		{"5/4+3i", 1.25, 3},

		// Rational imaginary parts
		{"1+3/4i", 1, 0.75},
		{"1-3/4i", 1, -0.75},
		{"2+1/2i", 2, 0.5},
		{"2-1/2i", 2, -0.5},

		// Rational real and imaginary parts
		{"1/2+3/4i", 0.5, 0.75},
		{"1/2-3/4i", 0.5, -0.75},
		{"-1/2+3/4i", -0.5, 0.75},
		{"-1/2-3/4i", -0.5, -0.75},
		{"3/4+5/8i", 0.75, 0.625},
		{"7/2+9/4i", 3.5, 2.25},

		// Uppercase I (R7RS §7.1.1: case-insensitive numeric literals)
		{"1+2I", 1, 2},
		{"3-4I", 3, -4},
		{"1+I", 1, 1},
		{"1-I", 1, -1},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(floatEquals(rel, tc.real, 1e-10), qt.IsTrue)
			c.Assert(floatEquals(iam, tc.imag, 1e-10), qt.IsTrue)
		})
	}
}

func TestParseComplexErrors(t *testing.T) {
	tcs := []struct {
		input string
		desc  string
	}{
		{"1", "no imaginary part"},
		{"abc", "not a number"},
		{"+2i", "pure imaginary, not complex"},
		{"i", "just i"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.desc, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			_, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// floatEquals compares floats within tolerance
func floatEquals(a, b, tolerance float64) bool {
	diff := a - b
	if diff < 0 {
		diff = -diff
	}
	return diff <= tolerance
}

// ============================================================================
// Integration Tests: Full ReadSyntax Path for Complex Numbers
// ============================================================================

func TestReadSyntaxRational(t *testing.T) {
	tcs := []struct {
		input string
		num   int64
		denom int64
	}{
		{"3/4", 3, 4},
		{"-1/2", -1, 2},
		{"+7/8", 7, 8},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			r, ok := syn.UnwrapAll().(*values.Rational)
			c.Assert(ok, qt.IsTrue)
			c.Assert(r.Num().Int64(), qt.Equals, tc.num)
			c.Assert(r.Denom().Int64(), qt.Equals, tc.denom)
		})
	}
}

func TestReadSyntaxImaginary(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
		exact bool // true if result should be exact BigComplex
	}{
		{"+i", 0, 1, true},
		{"-i", 0, -1, true},
		{"+3i", 0, 3, true},
		{"-2.5i", 0, -2.5, false},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			val := syn.UnwrapAll()
			rel, iam := getComplexParts(val.(values.Number))
			c.Assert(rel, qt.Equals, tc.real)
			c.Assert(iam, qt.Equals, tc.imag)
			if tc.exact {
				bc, ok := val.(*values.BigComplex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigComplex, got %T", val))
				c.Assert(bc.IsExact(), qt.IsTrue)
			} else {
				_, ok := val.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected Complex, got %T", val))
			}
		})
	}
}

func TestReadSyntaxComplex(t *testing.T) {
	// Note: Tokenizer only handles complex numbers starting with unsigned real parts.
	// Numbers like -1+2i tokenize as two tokens (-1 and +2i), not as a single complex.
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		{"1+2i", 1, 2},
		{"3-4i", 3, -4},
		{"1.5+2.5i", 1.5, 2.5},
		{"1+i", 1, 1},
		{"5-i", 5, -1},
		{"1e2+3i", 100, 3},
		// Rational complex numbers
		{"3/2+i", 1.5, 1},
		{"1/2+3/4i", 0.5, 0.75},
		{"3/4-1/2i", 0.75, -0.5},
		{"1+3/4i", 1, 0.75},
		{"1-3/4i", 1, -0.75},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			// Complex numbers can be either *values.Complex (inexact) or *values.BigComplex (exact)
			z, ok := syn.UnwrapAll().(values.Number)
			c.Assert(ok, qt.IsTrue)
			rel, iam := getComplexParts(z)
			c.Assert(floatEquals(rel, tc.real, 1e-10), qt.IsTrue)
			c.Assert(floatEquals(iam, tc.imag, 1e-10), qt.IsTrue)
		})
	}
}

// ============================================================================
// Radix Prefix Tests (#b, #o, #d, #x)
// ============================================================================

func TestReadSyntaxRadixBinary(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#b0", 0},
		{"#b1", 1},
		{"#b10", 2},
		{"#b11", 3},
		{"#b101", 5},
		{"#b1111", 15},
		{"#b10000000", 128},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

func TestReadSyntaxRadixOctal(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#o0", 0},
		{"#o7", 7},
		{"#o10", 8},
		{"#o77", 63},
		{"#o100", 64},
		{"#o777", 511},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

func TestReadSyntaxRadixDecimal(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#d0", 0},
		{"#d42", 42},
		{"#d100", 100},
		{"#d999", 999},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

func TestReadSyntaxRadixHex(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#x0", 0},
		{"#xA", 10},
		{"#xa", 10},
		{"#xF", 15},
		{"#xff", 255},
		{"#xFF", 255},
		{"#x10", 16},
		{"#x100", 256},
		{"#xDEAD", 0xDEAD},
		{"#xBEEF", 0xBEEF},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

// ============================================================================
// Exactness Marker Tests (#e, #i)
// ============================================================================

// TestReadSyntaxExactMarker tests the #e (exact) prefix which preserves integer type.
func TestReadSyntaxExactMarker(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#e42", 42},
		{"#e100", 100},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

// TestReadSyntaxInexactMarker tests the #i (inexact) prefix which converts to Float.
//
// R7RS §6.2.5: The #i prefix requests an inexact representation.
func TestReadSyntaxInexactMarker(t *testing.T) {
	tcs := []struct {
		input  string
		expect float64
	}{
		{"#i42", 42.0},
		{"#i100", 100.0},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			f, ok := syn.UnwrapAll().(*values.Float)
			c.Assert(ok, qt.IsTrue)
			c.Assert(f.Value, qt.Equals, tc.expect)
		})
	}
}

// ============================================================================
// Special Float Values Tests (+inf.0, -inf.0, +nan.0, -nan.0)
// ============================================================================

func TestReadSyntaxRealInf(t *testing.T) {
	c := qt.New(t)

	// Test +inf.0
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("+inf.0"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.UnwrapAll().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsInf(f.Value, 1), qt.IsTrue) // positive infinity

	// Test -inf.0
	env2 := environment.NewNamespace().Runtime()
	p2 := NewParser(env2, true, strings.NewReader("-inf.0"))
	syn2, err := p2.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f2, ok := syn2.UnwrapAll().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsInf(f2.Value, -1), qt.IsTrue) // negative infinity
}

func TestReadSyntaxRealNan(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{"+nan.0"},
		{"-nan.0"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			f, ok := syn.UnwrapAll().(*values.Float)
			c.Assert(ok, qt.IsTrue)
			c.Assert(math.IsNaN(f.Value), qt.IsTrue) // NaN check
		})
	}
}

func TestReadSyntaxImaginaryInf(t *testing.T) {
	c := qt.New(t)

	// Test +inf.0i
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("+inf.0i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	z, ok := syn.UnwrapAll().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(z.Real(), qt.Equals, 0.0)
	c.Assert(math.IsInf(z.Imag(), 1), qt.IsTrue) // positive infinity

	// Test -inf.0i
	env2 := environment.NewNamespace().Runtime()
	p2 := NewParser(env2, true, strings.NewReader("-inf.0i"))
	syn2, err := p2.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	z2, ok := syn2.UnwrapAll().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(z2.Real(), qt.Equals, 0.0)
	c.Assert(math.IsInf(z2.Imag(), -1), qt.IsTrue) // negative infinity
}

func TestReadSyntaxImaginaryNan(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{"+nan.0i"},
		{"-nan.0i"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			z, ok := syn.UnwrapAll().(*values.Complex)
			c.Assert(ok, qt.IsTrue)
			c.Assert(z.Real(), qt.Equals, 0.0)
			c.Assert(z.Imag() != z.Imag(), qt.IsTrue) // NaN check: NaN != NaN
		})
	}
}

// ============================================================================
// Complex Numbers in List Context
// ============================================================================

func TestReadSyntaxComplexInList(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(1+2i 3-4i)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	// Get the list - UnwrapAll returns the underlying Pair
	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// First element: 1+2i (can be Complex or BigComplex)
	first, ok := pair.Car().(values.Number)
	c.Assert(ok, qt.IsTrue)
	firstReal, firstImag := getComplexParts(first)
	c.Assert(firstReal, qt.Equals, 1.0)
	c.Assert(firstImag, qt.Equals, 2.0)

	// Second element: 3-4i (can be Complex or BigComplex)
	rest := pair.Cdr().(*values.Pair)
	second, ok := rest.Car().(values.Number)
	c.Assert(ok, qt.IsTrue)
	secondReal, secondImag := getComplexParts(second)
	c.Assert(secondReal, qt.Equals, 3.0)
	c.Assert(secondImag, qt.Equals, -4.0)
}

func TestReadSyntaxMixedNumericTypes(t *testing.T) {
	c := qt.New(t)

	// List with integer, rational, imaginary, and complex
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(42 3/4 +2i 1+2i)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	pair := syn.UnwrapAll().(*values.Pair)

	// First: 42 (integer)
	first, ok := pair.Car().(*values.Integer)
	c.Assert(ok, qt.IsTrue)
	c.Assert(first.Value, qt.Equals, int64(42))

	// Second: 3/4 (rational)
	pair = pair.Cdr().(*values.Pair)
	second, ok := pair.Car().(*values.Rational)
	c.Assert(ok, qt.IsTrue)
	c.Assert(second.Num().Int64(), qt.Equals, int64(3))
	c.Assert(second.Denom().Int64(), qt.Equals, int64(4))

	// Third: +2i (pure imaginary - now exact BigComplex since parseImaginary returns exact for integers)
	pair = pair.Cdr().(*values.Pair)
	third, ok := pair.Car().(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigComplex, got %T", pair.Car()))
	c.Assert(third.IsExact(), qt.IsTrue)
	thirdReal, thirdImag := getComplexParts(third)
	c.Assert(thirdReal, qt.Equals, 0.0)
	c.Assert(thirdImag, qt.Equals, 2.0)

	// Fourth: 1+2i (complex - now exact BigComplex since both parts are integers)
	pair = pair.Cdr().(*values.Pair)
	fourth, ok := pair.Car().(values.Number)
	c.Assert(ok, qt.IsTrue)
	fourthReal, fourthImag := getComplexParts(fourth)
	c.Assert(fourthReal, qt.Equals, 1.0)
	c.Assert(fourthImag, qt.Equals, 2.0)
}

// ============================================================================
// Extended parseComplex Tests - Edge Cases per R7RS/R6RS
// ============================================================================

func TestParseComplexInfNan(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		wantReal float64
		wantImag float64
		realInf  int  // 0 for not inf, 1 for +inf, -1 for -inf
		imagInf  int  // 0 for not inf, 1 for +inf, -1 for -inf
		realNaN  bool // true if real part is NaN
		imagNaN  bool // true if imag part is NaN
	}{
		// Infinity in real part
		{name: "+inf real +int imag", input: "+inf.0+2i", wantReal: 0, wantImag: 2, realInf: 1},
		{name: "-inf real +int imag", input: "-inf.0+2i", wantReal: 0, wantImag: 2, realInf: -1},
		{name: "+inf real -int imag", input: "+inf.0-2i", wantReal: 0, wantImag: -2, realInf: 1},
		{name: "-inf real -int imag", input: "-inf.0-2i", wantReal: 0, wantImag: -2, realInf: -1},
		{name: "+inf real +float imag", input: "+inf.0+2.5i", wantReal: 0, wantImag: 2.5, realInf: 1},

		// Infinity in imaginary part
		{name: "int real +inf imag", input: "1+inf.0i", wantReal: 1, wantImag: 0, imagInf: 1},
		{name: "int real -inf imag", input: "1-inf.0i", wantReal: 1, wantImag: 0, imagInf: -1},
		{name: "float real +inf imag", input: "1.5+inf.0i", wantReal: 1.5, wantImag: 0, imagInf: 1},
		{name: "float real -inf imag", input: "1.5-inf.0i", wantReal: 1.5, wantImag: 0, imagInf: -1},
		{name: "zero real +inf imag", input: "0+inf.0i", wantReal: 0, wantImag: 0, imagInf: 1},
		{name: "zero real -inf imag", input: "0-inf.0i", wantReal: 0, wantImag: 0, imagInf: -1},

		// Infinity in both parts
		{name: "+inf +inf", input: "+inf.0+inf.0i", wantReal: 0, wantImag: 0, realInf: 1, imagInf: 1},
		{name: "+inf -inf", input: "+inf.0-inf.0i", wantReal: 0, wantImag: 0, realInf: 1, imagInf: -1},
		{name: "-inf +inf", input: "-inf.0+inf.0i", wantReal: 0, wantImag: 0, realInf: -1, imagInf: 1},
		{name: "-inf -inf", input: "-inf.0-inf.0i", wantReal: 0, wantImag: 0, realInf: -1, imagInf: -1},

		// NaN in real part
		{name: "+nan real +int imag", input: "+nan.0+2i", wantReal: 0, wantImag: 2, realNaN: true},
		{name: "-nan real +int imag", input: "-nan.0+2i", wantReal: 0, wantImag: 2, realNaN: true},
		{name: "+nan real -int imag", input: "+nan.0-2i", wantReal: 0, wantImag: -2, realNaN: true},
		{name: "+nan real +float imag", input: "+nan.0+2.5i", wantReal: 0, wantImag: 2.5, realNaN: true},

		// NaN in imaginary part
		{name: "int real +nan imag", input: "1+nan.0i", wantReal: 1, wantImag: 0, imagNaN: true},
		{name: "int real -nan imag", input: "1-nan.0i", wantReal: 1, wantImag: 0, imagNaN: true},
		{name: "float real +nan imag", input: "1.5+nan.0i", wantReal: 1.5, wantImag: 0, imagNaN: true},
		{name: "zero real +nan imag", input: "0+nan.0i", wantReal: 0, wantImag: 0, imagNaN: true},

		// NaN in both parts
		{name: "+nan +nan", input: "+nan.0+nan.0i", realNaN: true, imagNaN: true},
		{name: "+nan -nan", input: "+nan.0-nan.0i", realNaN: true, imagNaN: true},
		{name: "-nan +nan", input: "-nan.0+nan.0i", realNaN: true, imagNaN: true},

		// Mixed inf and nan
		{name: "+inf +nan", input: "+inf.0+nan.0i", realInf: 1, imagNaN: true},
		{name: "+nan +inf", input: "+nan.0+inf.0i", realNaN: true, imagInf: 1},
		{name: "-inf +nan", input: "-inf.0+nan.0i", realInf: -1, imagNaN: true},
		{name: "+nan -inf", input: "+nan.0-inf.0i", realNaN: true, imagInf: -1},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)

			rel, iam := getComplexParts(z)

			// Check real part
			switch {
			case tc.realNaN:
				c.Assert(math.IsNaN(rel), qt.IsTrue, qt.Commentf("real should be NaN"))
			case tc.realInf != 0:
				c.Assert(math.IsInf(rel, tc.realInf), qt.IsTrue, qt.Commentf("real should be inf(%d)", tc.realInf))
			default:
				c.Assert(rel, qt.Equals, tc.wantReal)
			}

			// Check imaginary part
			switch {
			case tc.imagNaN:
				c.Assert(math.IsNaN(iam), qt.IsTrue, qt.Commentf("imag should be NaN"))
			case tc.imagInf != 0:
				c.Assert(math.IsInf(iam, tc.imagInf), qt.IsTrue, qt.Commentf("imag should be inf(%d)", tc.imagInf))
			default:
				c.Assert(iam, qt.Equals, tc.wantImag)
			}
		})
	}
}

func TestParseComplexUnitImaginary(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		// Unit imaginary with various real parts
		{"1+i", 1, 1},
		{"1-i", 1, -1},
		{"0+i", 0, 1},
		{"0-i", 0, -1},
		{"5+i", 5, 1},
		{"5-i", 5, -1},
		{"100+i", 100, 1},
		{"100-i", 100, -1},
		{"1.5+i", 1.5, 1},
		{"1.5-i", 1.5, -1},
		{"0.5+i", 0.5, 1},
		{"0.5-i", 0.5, -1},

		// Negative real with unit imaginary
		{"-1+i", -1, 1},
		{"-1-i", -1, -1},
		{"-5+i", -5, 1},
		{"-5-i", -5, -1},
		{"-1.5+i", -1.5, 1},
		{"-1.5-i", -1.5, -1},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(rel, qt.Equals, tc.real)
			c.Assert(iam, qt.Equals, tc.imag)
		})
	}
}

func TestParseComplexScientificNotation(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		// Scientific notation in real part only
		{"1e2+3i", 100, 3},
		{"1e-2+3i", 0.01, 3},
		{"1e+2+3i", 100, 3},
		{"1.5e2+3i", 150, 3},
		{"1.5e-2+3i", 0.015, 3},
		{"-1e2+3i", -100, 3},
		{"-1e-2+3i", -0.01, 3},

		// Scientific notation in imaginary part only
		{"1+3e2i", 1, 300},
		{"1+3e-2i", 1, 0.03},
		{"1+3e+2i", 1, 300},
		{"1+3.5e2i", 1, 350},
		{"1+3.5e-2i", 1, 0.035},
		{"1-3e2i", 1, -300},
		{"1-3e-2i", 1, -0.03},

		// Scientific notation in both parts
		{"1e2+3e2i", 100, 300},
		{"1e-2+3e-2i", 0.01, 0.03},
		{"1.5e2+2.5e2i", 150, 250},
		{"1e2-3e2i", 100, -300},

		// Uppercase E (R7RS allows this)
		{"1E2+3i", 100, 3},
		{"1+3E2i", 1, 300},
		{"1E2+3E2i", 100, 300},

		// Very large/small exponents
		{"1e10+2i", 1e10, 2},
		{"1+2e10i", 1, 2e10},
		{"1e-10+2i", 1e-10, 2},
		{"1+2e-10i", 1, 2e-10},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(floatEquals(rel, tc.real, 1e-15), qt.IsTrue, qt.Commentf("real: got %v want %v", rel, tc.real))
			c.Assert(floatEquals(iam, tc.imag, 1e-15), qt.IsTrue, qt.Commentf("imag: got %v want %v", iam, tc.imag))
		})
	}
}

// TestParser_ExtendedExponentMarkers tests R7RS s/f/d/l exponent markers (R7RS §7.1.1).
// All markers are treated as equivalent to 'e' — they produce the same numeric values.
func TestParser_ExtendedExponentMarkers(t *testing.T) {
	tcs := []struct {
		input  string
		expect values.Value
	}{
		// All markers produce Float (inexact) per R7RS §7.1.1
		{"1s10", values.NewFloat(1e10)},
		{"1f10", values.NewFloat(1e10)},
		{"1d10", values.NewFloat(1e10)},
		{"1l10", values.NewFloat(1e10)},
		// Uppercase
		{"1S10", values.NewFloat(1e10)},
		{"1F10", values.NewFloat(1e10)},
		{"1D10", values.NewFloat(1e10)},
		{"1L10", values.NewFloat(1e10)},
		// Signed
		{"+1s10", values.NewFloat(1e10)},
		{"-1f10", values.NewFloat(-1e10)},
		// Negative exponent produces float
		{"1s-2", values.NewFloat(0.01)},
		{"1f-2", values.NewFloat(0.01)},
		// With decimal mantissa
		{"1.5s3", values.NewFloat(1500)},
		{"1.5f3", values.NewFloat(1500)},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

func TestParseComplexZeroParts(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		// Zero real part
		{"0+1i", 0, 1},
		{"0-1i", 0, -1},
		{"0+2.5i", 0, 2.5},
		{"0-2.5i", 0, -2.5},
		{"0.0+1i", 0, 1},
		{"0.0-1i", 0, -1},

		// Zero imaginary part
		{"1+0i", 1, 0},
		{"1-0i", 1, 0},
		{"2.5+0i", 2.5, 0},
		{"2.5-0i", 2.5, 0},
		{"1+0.0i", 1, 0},
		{"1-0.0i", 1, 0},

		// Both zero
		{"0+0i", 0, 0},
		{"0-0i", 0, 0},
		{"0.0+0.0i", 0, 0},
		{"0.0-0.0i", 0, 0},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(rel, qt.Equals, tc.real)
			c.Assert(iam, qt.Equals, tc.imag)
		})
	}
}

func TestParseComplexDecimalForms(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		// Decimal without leading digit
		{".5+.5i", 0.5, 0.5},
		{".5+2i", 0.5, 2},
		{"1+.5i", 1, 0.5},
		{".5-.5i", 0.5, -0.5},

		// Many decimal places
		{"1.123456789+2.987654321i", 1.123456789, 2.987654321},
		{"3.141592653589793+2.718281828459045i", 3.141592653589793, 2.718281828459045},

		// Trailing zeros
		{"1.00+2.00i", 1, 2},
		{"1.000000+2.000000i", 1, 2},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(floatEquals(rel, tc.real, 1e-15), qt.IsTrue, qt.Commentf("real: got %v want %v", rel, tc.real))
			c.Assert(floatEquals(iam, tc.imag, 1e-15), qt.IsTrue, qt.Commentf("imag: got %v want %v", iam, tc.imag))
		})
	}
}

func TestParseComplexLargeNumbers(t *testing.T) {
	tcs := []struct {
		input string
		real  float64
		imag  float64
	}{
		{"123456789+987654321i", 123456789, 987654321},
		{"1.23456789e10+9.87654321e10i", 1.23456789e10, 9.87654321e10},
		{"1e100+2e100i", 1e100, 2e100},
		{"1e-100+2e-100i", 1e-100, 2e-100},
		{"1e200+2e200i", 1e200, 2e200},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			rel, iam := getComplexParts(z)
			c.Assert(floatEquals(rel, tc.real, tc.real*1e-10), qt.IsTrue, qt.Commentf("real: got %v want %v", rel, tc.real))
			c.Assert(floatEquals(iam, tc.imag, tc.imag*1e-10), qt.IsTrue, qt.Commentf("imag: got %v want %v", iam, tc.imag))
		})
	}
}

// ============================================================================
// Polar Complex Number Parsing Tests (parsePolarComplex)
// ============================================================================

func TestParsePolarComplex(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		wantReal float64
		wantImag float64
	}{
		// Basic polar forms (magnitude@angle)
		{name: "unit at zero", input: "1@0", wantReal: 1, wantImag: 0},
		{name: "unit at pi/2", input: "1@1.5707963267948966", wantReal: 0, wantImag: 1},
		{name: "unit at pi", input: "1@3.141592653589793", wantReal: -1, wantImag: 0},
		{name: "unit at 3pi/2", input: "1@4.71238898038469", wantReal: 0, wantImag: -1},
		{name: "unit at 2pi", input: "1@6.283185307179586", wantReal: 1, wantImag: 0},

		// Various magnitudes
		{name: "mag 2 at zero", input: "2@0", wantReal: 2, wantImag: 0},
		{name: "mag 0.5 at zero", input: "0.5@0", wantReal: 0.5, wantImag: 0},
		{name: "mag 10 at pi/4", input: "10@0.7853981633974483", wantReal: 7.0710678118654755, wantImag: 7.071067811865475},
		{name: "mag 100 at zero", input: "100@0", wantReal: 100, wantImag: 0},

		// Signed magnitude
		{name: "+mag at zero", input: "+1@0", wantReal: 1, wantImag: 0},
		{name: "+mag at pi/2", input: "+1@1.5707963267948966", wantReal: 0, wantImag: 1},
		{name: "-mag at zero", input: "-1@0", wantReal: -1, wantImag: 0},
		{name: "-mag at pi", input: "-1@3.141592653589793", wantReal: 1, wantImag: 0},
		{name: "-mag at pi/2", input: "-1@1.5707963267948966", wantReal: 0, wantImag: -1},

		// Negative angles
		{name: "unit at -pi/2", input: "1@-1.5707963267948966", wantReal: 0, wantImag: -1},
		{name: "unit at -pi", input: "1@-3.141592653589793", wantReal: -1, wantImag: 0},

		// Zero magnitude
		{name: "zero mag at zero", input: "0@0", wantReal: 0, wantImag: 0},
		{name: "zero mag at pi", input: "0@3.141592653589793", wantReal: 0, wantImag: 0},
		{name: "zero mag at any", input: "0@1.234", wantReal: 0, wantImag: 0},

		// Decimal values (computed as r*cos(θ), r*sin(θ))
		{name: "float mag float angle", input: "2.5@0.5", wantReal: 2.5 * 0.8775825618903728, wantImag: 2.5 * 0.479425538604203},
		{name: "small mag small angle", input: "0.1@0.1", wantReal: 0.09950041652780258, wantImag: 0.009983341664682815},

		// Scientific notation in magnitude
		{name: "sci mag at zero", input: "1e2@0", wantReal: 100, wantImag: 0},
		{name: "sci mag at pi/2", input: "1e2@1.5707963267948966", wantReal: 0, wantImag: 100},
		{name: "neg sci mag at zero", input: "1e-2@0", wantReal: 0.01, wantImag: 0},

		// Scientific notation in angle
		{name: "mag at sci angle", input: "1@1e-1", wantReal: 0.9950041652780258, wantImag: 0.09983341664682815},
		{name: "mag at neg sci angle", input: "1@-1e-1", wantReal: 0.9950041652780258, wantImag: -0.09983341664682815},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parsePolarComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(floatEquals(z.Real(), tc.wantReal, 1e-10), qt.IsTrue,
				qt.Commentf("real: got %v want %v", z.Real(), tc.wantReal))
			c.Assert(floatEquals(z.Imag(), tc.wantImag, 1e-10), qt.IsTrue,
				qt.Commentf("imag: got %v want %v", z.Imag(), tc.wantImag))
		})
	}
}

func TestParsePolarComplexInfNan(t *testing.T) {
	tcs := []struct {
		name    string
		input   string
		realInf int
		imagInf int
		realNaN bool
		imagNaN bool
	}{
		// Infinity in magnitude
		{name: "+inf mag at zero", input: "+inf.0@0", realInf: 1, imagInf: 0},
		{name: "-inf mag at zero", input: "-inf.0@0", realInf: -1, imagInf: 0},
		{name: "+inf mag at pi/2", input: "+inf.0@1.5707963267948966", realInf: 0, imagInf: 1},
		{name: "+inf mag at pi", input: "+inf.0@3.141592653589793", realInf: -1, imagInf: 0},

		// NaN in magnitude (produces NaN in both parts)
		{name: "+nan mag at zero", input: "+nan.0@0", realNaN: true, imagNaN: true},
		{name: "-nan mag at zero", input: "-nan.0@0", realNaN: true, imagNaN: true},

		// Infinity in angle (produces NaN due to cos/sin of inf)
		{name: "unit at +inf", input: "1@+inf.0", realNaN: true, imagNaN: true},
		{name: "unit at -inf", input: "1@-inf.0", realNaN: true, imagNaN: true},

		// NaN in angle
		{name: "unit at +nan", input: "1@+nan.0", realNaN: true, imagNaN: true},
		{name: "unit at -nan", input: "1@-nan.0", realNaN: true, imagNaN: true},

		// Both inf
		{name: "+inf at +inf", input: "+inf.0@+inf.0", realNaN: true, imagNaN: true},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			z, err := p.parsePolarComplex(tc.input)
			c.Assert(err, qt.IsNil)

			if tc.realNaN {
				c.Assert(math.IsNaN(z.Real()), qt.IsTrue, qt.Commentf("real should be NaN"))
			} else if tc.realInf != 0 {
				c.Assert(math.IsInf(z.Real(), tc.realInf), qt.IsTrue, qt.Commentf("real should be inf(%d)", tc.realInf))
			}

			if tc.imagNaN {
				c.Assert(math.IsNaN(z.Imag()), qt.IsTrue, qt.Commentf("imag should be NaN"))
			} else if tc.imagInf != 0 {
				c.Assert(math.IsInf(z.Imag(), tc.imagInf), qt.IsTrue, qt.Commentf("imag should be inf(%d)", tc.imagInf))
			}
		})
	}
}

func TestParsePolarComplexErrors(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{name: "no at sign", input: "1 2"},
		{name: "empty string", input: ""},
		{name: "just at sign", input: "@"},
		{name: "missing magnitude", input: "@1"},
		{name: "missing angle", input: "1@"},
		{name: "letters in magnitude", input: "abc@1"},
		{name: "letters in angle", input: "1@abc"},
		{name: "double at", input: "1@@2"},
		{name: "plus instead of at", input: "1+2"},
		{name: "minus instead of at", input: "1-2"},
		{name: "i suffix", input: "1@2i"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			_, err := p.parsePolarComplex(tc.input)
			c.Assert(err, qt.IsNotNil, qt.Commentf("expected error for input %q", tc.input))
		})
	}
}

// ============================================================================
// parseRealPart and parseImagPart Unit Tests
// ============================================================================

func TestParseRealPart(t *testing.T) {
	tcs := []struct {
		input   string
		want    float64
		wantInf int
		wantNaN bool
	}{
		// Regular floats
		{input: "0", want: 0},
		{input: "1", want: 1},
		{input: "-1", want: -1},
		{input: "1.5", want: 1.5},
		{input: "-1.5", want: -1.5},
		{input: "0.5", want: 0.5},
		{input: "-0.5", want: -0.5},
		{input: "123.456", want: 123.456},

		// Scientific notation
		{input: "1e2", want: 100},
		{input: "1e-2", want: 0.01},
		{input: "1e+2", want: 100},
		{input: "1.5e2", want: 150},
		{input: "-1e2", want: -100},

		// Infnan
		{input: "+inf.0", wantInf: 1},
		{input: "-inf.0", wantInf: -1},
		{input: "+nan.0", wantNaN: true},
		{input: "-nan.0", wantNaN: true},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			got, err := p.parseRealPart(tc.input)
			c.Assert(err, qt.IsNil)

			switch {
			case tc.wantNaN:
				c.Assert(math.IsNaN(got), qt.IsTrue)
			case tc.wantInf != 0:
				c.Assert(math.IsInf(got, tc.wantInf), qt.IsTrue)
			default:
				c.Assert(got, qt.Equals, tc.want)
			}
		})
	}
}

func TestParseImagPart(t *testing.T) {
	tcs := []struct {
		input   string
		want    float64
		wantInf int
		wantNaN bool
	}{
		// Just sign (unit imaginary coefficient)
		{input: "+", want: 1},
		{input: "-", want: -1},

		// Regular floats with sign
		{input: "+0", want: 0},
		{input: "-0", want: 0},
		{input: "+1", want: 1},
		{input: "-1", want: -1},
		{input: "+1.5", want: 1.5},
		{input: "-1.5", want: -1.5},
		{input: "+0.5", want: 0.5},
		{input: "-0.5", want: -0.5},
		{input: "+123.456", want: 123.456},
		{input: "-123.456", want: -123.456},

		// Scientific notation
		{input: "+1e2", want: 100},
		{input: "-1e2", want: -100},
		{input: "+1e-2", want: 0.01},
		{input: "-1e-2", want: -0.01},

		// Infnan
		{input: "+inf.0", wantInf: 1},
		{input: "-inf.0", wantInf: -1},
		{input: "+nan.0", wantNaN: true},
		{input: "-nan.0", wantNaN: true},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(""))
			got, err := p.parseImagPart(tc.input)
			c.Assert(err, qt.IsNil)

			switch {
			case tc.wantNaN:
				c.Assert(math.IsNaN(got), qt.IsTrue)
			case tc.wantInf != 0:
				c.Assert(math.IsInf(got, tc.wantInf), qt.IsTrue)
			default:
				c.Assert(got, qt.Equals, tc.want)
			}
		})
	}
}

// ============================================================================
// Integration Tests: Polar Complex Through ReadSyntax
// ============================================================================

func TestReadSyntaxPolarComplex(t *testing.T) {
	tcs := []struct {
		input    string
		wantReal float64
		wantImag float64
	}{
		{"1@0", 1, 0},
		{"2@0", 2, 0},
		{"+1@0", 1, 0},
		{"-1@0", -1, 0},
		{"1@1.5707963267948966", 0, 1},
		{"1@3.141592653589793", -1, 0},
		{"2@0.7853981633974483", 1.4142135623730951, 1.414213562373095},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			z, ok := syn.UnwrapAll().(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Complex, got %T", syn.UnwrapAll()))
			c.Assert(floatEquals(z.Real(), tc.wantReal, 1e-10), qt.IsTrue,
				qt.Commentf("real: got %v want %v", z.Real(), tc.wantReal))
			c.Assert(floatEquals(z.Imag(), tc.wantImag, 1e-10), qt.IsTrue,
				qt.Commentf("imag: got %v want %v", z.Imag(), tc.wantImag))
		})
	}
}

func TestReadSyntaxComplexInfNan(t *testing.T) {
	// Note: Only forms that tokenize as a single complex number are tested here.
	// Forms like "+nan.0+2i" tokenize as two separate tokens (+nan.0 and +2i)
	// and are tested in the direct parseComplex tests instead.
	tcs := []struct {
		name    string
		input   string
		realInf int
		imagInf int
		realNaN bool
		imagNaN bool
	}{
		// Unsigned real with infnan imaginary (tokenizes as single UnsignedComplex)
		{name: "imag inf", input: "1+inf.0i", imagInf: 1},
		{name: "imag neg inf", input: "1-inf.0i", imagInf: -1},
		{name: "imag nan", input: "1+nan.0i", imagNaN: true},
		{name: "imag neg nan", input: "1-nan.0i", imagNaN: true},
		// Float real with infnan imaginary
		{name: "float imag inf", input: "1.5+inf.0i", imagInf: 1},
		{name: "float imag nan", input: "1.5+nan.0i", imagNaN: true},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			z, ok := syn.UnwrapAll().(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Complex, got %T", syn.UnwrapAll()))

			if tc.realNaN {
				c.Assert(math.IsNaN(z.Real()), qt.IsTrue)
			} else if tc.realInf != 0 {
				c.Assert(math.IsInf(z.Real(), tc.realInf), qt.IsTrue)
			}

			if tc.imagNaN {
				c.Assert(math.IsNaN(z.Imag()), qt.IsTrue)
			} else if tc.imagInf != 0 {
				c.Assert(math.IsInf(z.Imag(), tc.imagInf), qt.IsTrue)
			}
		})
	}
}

// ============================================================================
// Coverage-Boosting Tests: listSyntax, vectors, multiple reads
// ============================================================================

// TestParser_MultipleReads tests that the tokenizer is preserved between reads
func TestParser_MultipleReads(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	input := "10 20 30"
	p := NewParser(env, true, strings.NewReader(input))

	// First read
	syn1, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn1.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(10))

	// Second read - tokenizer should be preserved
	syn2, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn2.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(20))

	// Third read
	syn3, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn3.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(30))

	// Fourth read should hit EOF
	_, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.Equals, io.EOF)
}

// TestParser_EmptyVector tests parsing empty vectors
func TestParser_EmptyVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("#()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 0)
}

// TestParser_EmptyByteVector tests parsing empty byte vectors
func TestParser_EmptyByteVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Empty byte vector - tests the close paren path in byte vector parsing
	p := NewParser(env, true, strings.NewReader("#u8(10 20)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	bv, ok := syn.UnwrapAll().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*bv), qt.Equals, 2)
}

// TestParser_SingleElementVector tests vectors with one element (tests wrapSyntaxVector)
func TestParser_SingleElementVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("#(42)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 1)
	c.Assert((*vec)[0], valuestest.SchemeEquals, values.NewInteger(42))
}

// TestParser_NestedLists tests lists within lists (tests listSyntax with multiple elements)
func TestParser_NestedLists(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("((a b) (c d e))"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	outerList, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(outerList.Length(), qt.Equals, 2)

	// Check first inner list
	innerList1 := outerList.Car().(*values.Pair)
	c.Assert(innerList1.Length(), qt.Equals, 2)

	// Check second inner list
	innerList2 := outerList.Cdr().(*values.Pair).Car().(*values.Pair)
	c.Assert(innerList2.Length(), qt.Equals, 3)
}

// TestParser_VectorWithMixedTypes tests vectors with different value types
func TestParser_VectorWithMixedTypes(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader(`#(42 "hello" #t foo)`))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 4)

	c.Assert((*vec)[0], valuestest.SchemeEquals, values.NewInteger(42))
	c.Assert((*vec)[1], valuestest.SchemeEquals, values.NewString("hello"))
	c.Assert((*vec)[2], valuestest.SchemeEquals, values.TrueValue)
	c.Assert((*vec)[3], valuestest.SchemeEquals, values.NewSymbol("foo"))
}

// TestParser_ListSyntaxMultipleElements tests listSyntax with more than 2 elements
func TestParser_ListSyntaxMultipleElements(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test quote with multiple elements in a list
	p := NewParser(env, true, strings.NewReader("'(a b c d)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	// Should be (quote (a b c d))
	pair := syn.UnwrapAll().(*values.Pair)
	c.Assert(pair.Car(), valuestest.SchemeEquals, values.NewSymbol("quote"))

	// The cdr should be the list (a b c d)
	quotedList := pair.Cdr().(*values.Pair).Car().(*values.Pair)
	c.Assert(quotedList.Length(), qt.Equals, 4)
}

// TestParser_CharacterMnemonics verifies all R7RS §6.6 character mnemonics
// parse to the correct rune value. The parser uses tokenizer.CharMnemonics
// as the single source of truth.
func TestParser_CharacterMnemonics(t *testing.T) {
	tcs := []struct {
		input    string
		expected rune
	}{
		{`#\alarm`, '\a'},
		{`#\backspace`, '\b'},
		{`#\delete`, '\x7F'},
		{`#\escape`, '\x1B'},
		{`#\newline`, '\n'},
		{`#\null`, '\x00'},
		{`#\return`, '\r'},
		{`#\space`, ' '},
		{`#\tab`, '\t'},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			ch := syn.UnwrapAll().(*values.Character)
			c.Assert(ch.Value, qt.Equals, tc.expected)
		})
	}
}

// TestParser_ReadSyntaxErrorPropagation tests error propagation in ReadSyntax
func TestParser_ReadSyntaxErrorPropagation(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test with unclosed list - should propagate EOF error
	p := NewParser(env, true, strings.NewReader("("))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)

	// After error, tokenizer should be nil
	c.Assert(p.toks, qt.IsNil)
}

// TestParser_ComplexWithScientificNotation tests parseComplex edge case
func TestParser_ComplexWithScientificNotation_EdgeCase(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(""))

	// Test with uppercase E which is allowed by R7RS
	z, err := p.parseComplex("1E2+3E2i")
	c.Assert(err, qt.IsNil)
	rel, iam := getComplexParts(z)
	c.Assert(floatEquals(rel, 100, 1e-10), qt.IsTrue)
	c.Assert(floatEquals(iam, 300, 1e-10), qt.IsTrue)
}

// TestParser_ByteVectorWithMultipleValues tests byte vector parsing
func TestParser_ByteVectorWithMultipleValues(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("#u8(255 128 0 64)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	bv, ok := syn.UnwrapAll().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*bv), qt.Equals, 4)
	c.Assert((*bv)[0].Value, qt.Equals, uint8(255))
	c.Assert((*bv)[1].Value, qt.Equals, uint8(128))
	c.Assert((*bv)[2].Value, qt.Equals, uint8(0))
	c.Assert((*bv)[3].Value, qt.Equals, uint8(64))
}

// TestParser_ReadSyntaxPreservesTokenizer tests that ReadSyntax preserves tokenizer
func TestParser_ReadSyntaxPreservesTokenizer(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("(a b) (c d)"))

	// First read
	syn1, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	list1 := syn1.UnwrapAll().(*values.Pair)
	c.Assert(list1.Length(), qt.Equals, 2)

	// Tokenizer should still exist
	c.Assert(p.toks, qt.Not(qt.IsNil))

	// Second read should work
	syn2, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	list2 := syn2.UnwrapAll().(*values.Pair)
	c.Assert(list2.Length(), qt.Equals, 2)
}

// TestParser_ComplexNumberSignSeparatorEdgeCases tests edge cases in sign detection
func TestParser_ComplexNumberSignSeparatorEdgeCases(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(""))

	// Test number that has no valid sign separator (should error)
	_, err := p.parseComplex("123.456")
	c.Assert(err, qt.IsNotNil)
}

// TestParser_ReadSyntaxEOFHandling tests EOF handling in ReadSyntax
func TestParser_ReadSyntaxEOFHandling(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("42"))

	// First read succeeds
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, values.NewInteger(42))

	// Second read hits EOF (but this is OK, tokenizer advances)
	_, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.Equals, io.EOF)
}

// TestParser_VectorLoop tests the vector parsing loop
func TestParser_VectorLoop(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test vector with multiple elements to exercise the loop
	p := NewParser(env, true, strings.NewReader("#(1 2 3 4 5)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 5)
}

// TestParser_ListWithMultipleElements tests list parsing loop
func TestParser_ListWithMultipleElements(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// List with many elements
	p := NewParser(env, true, strings.NewReader("(a b c d e f g h)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	list, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(list.Length(), qt.Equals, 8)
}

// TestParser_ByteVectorLoop tests byte vector parsing loop
func TestParser_ByteVectorLoop(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("#u8(1 2 3 4 5 6 7 8)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	bv, ok := syn.UnwrapAll().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*bv), qt.Equals, 8)
}

// TestParser_EmptyList tests empty list parsing
func TestParser_EmptyList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, values.EmptyList)
}

// TestParser_ImproperList tests improper list (dotted pair) parsing
func TestParser_ImproperList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("(a b . c)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	// Should be a pair with a and a pair with b and c
	pair := syn.UnwrapAll().(*values.Pair)
	c.Assert(pair.Car(), valuestest.SchemeEquals, values.NewSymbol("a"))
}

// TestParser_QuasiquoteSingleElement tests listSyntax with 1 element
func TestParser_QuasiquoteSingleElement(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader("`x"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	// Should be (quasiquote x)
	list := syn.UnwrapAll().(*values.Pair)
	c.Assert(list.Car(), valuestest.SchemeEquals, values.NewSymbol("quasiquote"))
	c.Assert(list.Length(), qt.Equals, 2) // (quasiquote x) is length 2
}

// TestParser_UnquoteSplicing tests listSyntax with 2 elements
func TestParser_UnquoteSplicing(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	p := NewParser(env, true, strings.NewReader(",@foo"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	// Should be (unquote-splicing foo)
	list := syn.UnwrapAll().(*values.Pair)
	c.Assert(list.Car(), valuestest.SchemeEquals, values.NewSymbol("unquote-splicing"))
	c.Assert(list.Length(), qt.Equals, 2)
}

// TestParser_SignedNumbers tests signed integer and float parsing
func TestParser_SignedNumbers(t *testing.T) {
	tcs := []struct {
		input  string
		expect values.Value
	}{
		{"-42", values.NewInteger(-42)},
		{"+42", values.NewInteger(42)},
		{"-3.14", values.NewFloat(-3.14)},
		{"+3.14", values.NewFloat(3.14)},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_RationalNumbers tests rational number parsing
func TestParser_RationalNumbers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test both signed and unsigned rational fractions
	p1 := NewParser(env, true, strings.NewReader("1/2"))
	syn1, err := p1.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	r1 := syn1.UnwrapAll().(*values.Rational)
	c.Assert(r1.Num().Int64(), qt.Equals, int64(1))
	c.Assert(r1.Denom().Int64(), qt.Equals, int64(2))

	// Test signed rational
	p2 := NewParser(env, true, strings.NewReader("-3/4"))
	syn2, err := p2.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	r2 := syn2.UnwrapAll().(*values.Rational)
	c.Assert(r2.Num().Int64(), qt.Equals, int64(-3))
	c.Assert(r2.Denom().Int64(), qt.Equals, int64(4))
}

func TestNewParserWithFile(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test that NewParserWithFile stores the filename in source context
	filename := "test-file.scm"
	p := NewParserWithFile(env, true, strings.NewReader("(define x 42)"), filename)

	stx, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(stx, qt.IsNotNil)

	// Check that the source context has the correct file
	sc := stx.SourceContext()
	c.Assert(sc, qt.IsNotNil)
	c.Assert(sc.File, qt.Equals, filename)

	// Verify nested elements also have the file
	pair, ok := stx.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)

	car := pair.Car()
	carStx, ok := car.(syntax.SyntaxValue)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carStx.SourceContext().File, qt.Equals, filename)
}

func TestNewParserWithFile_EmptyFilename(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Test backward compatibility: NewParser should set empty filename
	p := NewParser(env, true, strings.NewReader("hello"))

	stx, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	sc := stx.SourceContext()
	c.Assert(sc, qt.IsNotNil)
	c.Assert(sc.File, qt.Equals, "")
}

// TestParser_FoldCase tests R7RS §2.1 fold-case directive handling.
func TestParser_FoldCase(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	tcs := []struct {
		name   string
		input  string
		expect string // expected symbol name after fold-case processing
	}{
		{
			name:   "fold-case converts uppercase to lowercase",
			input:  "#!fold-case FOO",
			expect: "foo",
		},
		{
			name:   "fold-case converts mixed case to lowercase",
			input:  "#!fold-case FoObAr",
			expect: "foobar",
		},
		{
			name:   "no-fold-case preserves case",
			input:  "#!fold-case #!no-fold-case FOO",
			expect: "FOO",
		},
		{
			name:   "fold-case directive is case-insensitive",
			input:  "#!FOLD-CASE FOO",
			expect: "foo",
		},
		{
			name:   "no-fold-case directive is case-insensitive",
			input:  "#!fold-case #!NO-FOLD-CASE FOO",
			expect: "FOO",
		},
		{
			name:   "lowercase preserved without fold-case",
			input:  "foo",
			expect: "foo",
		},
		{
			name:   "uppercase preserved without fold-case",
			input:  "FOO",
			expect: "FOO",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			sv, err := p.ReadSyntax(ctx)
			c.Assert(err, qt.IsNil)

			sym, ok := sv.(*syntax.SyntaxSymbol)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected SyntaxSymbol, got %T", sv))
			c.Assert(sym.Key(), qt.Equals, tc.expect)
		})
	}
}

// TestRadixExactnessPrefix verifies R7RS §7.1.1: prefix ordering (#x#e10 and #e#x10)
// must both work. Previously, #x#e10 was failing because the MarkerBase16 case
// did not handle a following exactness marker.
func TestRadixExactnessPrefix(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tcs := []struct {
		name string
		in   string
		want values.Value
	}{
		// #e#x order (always worked)
		{"#e#x10 exact hex 16", "#e#x10", values.NewInteger(16)},
		{"#i#x10 inexact hex 16.0", "#i#x10", values.NewFloat(16.0)},
		{"#e#b101 exact binary 5", "#e#b101", values.NewInteger(5)},
		{"#i#b101 inexact binary 5.0", "#i#b101", values.NewFloat(5.0)},
		{"#e#o17 exact octal 15", "#e#o17", values.NewInteger(15)},
		{"#i#o17 inexact octal 15.0", "#i#o17", values.NewFloat(15.0)},
		// #x#e order (was broken before this fix)
		{"#x#e10 exact hex 16", "#x#e10", values.NewInteger(16)},
		{"#x#i10 inexact hex 16.0", "#x#i10", values.NewFloat(16.0)},
		{"#b#e101 exact binary 5", "#b#e101", values.NewInteger(5)},
		{"#b#i101 inexact binary 5.0", "#b#i101", values.NewFloat(5.0)},
		{"#o#e17 exact octal 15", "#o#e17", values.NewInteger(15)},
		{"#o#i17 inexact octal 15.0", "#o#i17", values.NewFloat(15.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			p := NewParser(env, true, strings.NewReader(tc.in))
			sv, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("input: %q", tc.in))
			c.Assert(sv.Unwrap(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestParseSpecialFloat verifies the public ParseSpecialFloat function.
func TestParseSpecialFloat(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		in     string
		wantOK bool
		isInf  bool
		isNaN  bool
		posInf bool
	}{
		{"+inf.0", true, true, false, true},
		{"-inf.0", true, true, false, false},
		{"+nan.0", true, false, true, false},
		{"-nan.0", true, false, true, false},
		{"3.14", false, false, false, false},
		{"", false, false, false, false},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			f, ok := ParseSpecialFloat(tc.in)
			c.Assert(ok, qt.Equals, tc.wantOK, qt.Commentf("input: %q", tc.in))
			if !ok {
				return
			}
			if tc.isInf {
				c.Assert(f.IsFinite(), qt.IsFalse)
				c.Assert(f.IsNaN(), qt.IsFalse)
				if tc.posInf {
					c.Assert(f.IsPositive(), qt.IsTrue)
				} else {
					c.Assert(f.IsNegative(), qt.IsTrue)
				}
			}
			if tc.isNaN {
				c.Assert(f.IsNaN(), qt.IsTrue)
			}
		})
	}
}

// TestParseImaginaryStringNumber verifies ParseImaginaryStringNumber.
func TestParseImaginaryStringNumber(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		in     string
		wantOK bool
	}{
		{"+i", true},
		{"-i", true},
		{"+3i", true},
		{"-2.5i", true},
		{"+inf.0i", true},
		{"-nan.0i", true},
		{"3+4i", false}, // full complex, not pure imaginary
		{"abc", false},
		{"", false},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			_, ok := ParseImaginaryStringNumber(tc.in)
			c.Assert(ok, qt.Equals, tc.wantOK, qt.Commentf("input: %q", tc.in))
		})
	}
}

// TestParseComplexStringNumber verifies ParseComplexStringNumber.
func TestParseComplexStringNumber(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		in     string
		wantOK bool
	}{
		{"3+4i", true},
		{"-1.5+2.5i", true},
		{"1+inf.0i", true},
		{"+i", false}, // pure imaginary, no real separator
		{"abc", false},
		{"", false},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			_, ok := ParseComplexStringNumber(tc.in)
			c.Assert(ok, qt.Equals, tc.wantOK, qt.Commentf("input: %q", tc.in))
		})
	}
}
