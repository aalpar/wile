// Copyright 2025 Aaron Alpar
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
	"io"
	"math"
	"wile/environment"
	"wile/syntax"
	"wile/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

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
			expect: values.NewBoolean(true),
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
			expect: values.NewBoolean(false),
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
			expect: values.NewBoolean(false),
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
			expect: values.NewBoolean(true),
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
					syntax.NewSyntaxEmptyList(
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
						syntax.NewSyntaxEmptyList(
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
					syntax.NewSourceContext(
						"'", "",
						syntax.NewSourceIndexes(2, 2, 0),
						syntax.NewSourceIndexes(3, 3, 0),
					),
				),
				syntax.NewSyntaxEmptyList(
					syntax.NewSourceContext(
						")", "",
						syntax.NewSourceIndexes(9, 9, 0),
						syntax.NewSourceIndexes(10, 10, 0),
					),
				),
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
			expect: values.NewByteVector(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			in:     "( 10 . 20 )",
			expect: values.NewCons(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			in:  ". 20",
			err: values.ErrNotACons,
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
			sexpect: syntax.NewSyntaxObject(
				syntax.NewSyntaxDatumLabelAssignment(
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
							syntax.NewSyntaxEmptyList(
								syntax.NewSourceContext(")", "",
									syntax.NewSourceIndexes(12, 12, 0),
									syntax.NewSourceIndexes(13, 13, 0),
								),
							),
							syntax.NewSourceContext("10", "",
								syntax.NewSourceIndexes(6, 6, 0),
								syntax.NewSourceIndexes(8, 8, 0),
							),
						),
						syntax.NewSourceContext("(", "",
							syntax.NewSourceIndexes(4, 4, 0),
							syntax.NewSourceIndexes(8, 8, 0),
						),
					),
					syntax.NewSourceContext(")", "",
						syntax.NewSourceIndexes(12, 12, 0),
						syntax.NewSourceIndexes(13, 13, 0),
					),
				),
				syntax.NewSourceContext(")", "",
					syntax.NewSourceIndexes(12, 12, 0),
					syntax.NewSourceIndexes(13, 13, 0),
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
			sexpect: syntax.NewSyntaxComment(" this is a comment",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
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
									syntax.NewSyntaxEmptyList(
										syntax.NewSourceContext(")", "",
											syntax.NewSourceIndexes(22, 22, 0),
											syntax.NewSourceIndexes(23, 23, 0),
										),
									),
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
			in: "#| block comment |#",
			sexpect: syntax.NewSyntaxComment(" block comment ",
				syntax.NewSourceContext("#|", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in: "#||#",
			sexpect: syntax.NewSyntaxComment("",
				syntax.NewSourceContext("#|", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in: "#| outer #| nested |# outer |#",
			sexpect: syntax.NewSyntaxComment(" outer #| nested |# outer ",
				syntax.NewSourceContext("#|", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		{
			in: "#| multi\nline\ncomment |#",
			sexpect: syntax.NewSyntaxComment(" multi\nline\ncomment ",
				syntax.NewSourceContext("#|", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(2, 2, 0),
				),
			),
		},
		// Line comment with newline (not EOF)
		{
			in: "; comment with newline\n",
			sexpect: syntax.NewSyntaxComment(" comment with newline",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		// Empty line comment
		{
			in: ";\n",
			sexpect: syntax.NewSyntaxComment("",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		// Empty line comment at EOF
		{
			in: ";",
			sexpect: syntax.NewSyntaxComment("",
				syntax.NewSourceContext(";", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(1, 1, 0),
				),
			),
		},
		// Line comment with multiple semicolons
		{
			in: ";;; triple semicolon",
			sexpect: syntax.NewSyntaxComment(" triple semicolon",
				syntax.NewSourceContext(";;;", "",
					syntax.NewSourceIndexes(0, 0, 0),
					syntax.NewSourceIndexes(3, 3, 0),
				),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(nil)
			qt.Assert(t, err, qt.Equals, tc.err)
			if err != nil {
				return
			}
			if tc.sexpect != nil {
				qt.Assert(t, syn, syntax.SyntaxEquals, tc.sexpect)
			}
			if tc.expect != nil {
				v := syn.UnwrapAll()
				qt.Assert(t, v, values.SchemeEquals, tc.expect)
			}
		})
	}
}

// TestParse tests the Parse convenience function.
func TestParse(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	syn, err := Parse(env, strings.NewReader("42"))
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), values.SchemeEquals, values.NewInteger(42))
}

// TestParser_Close tests the Close function.
func TestParser_Close(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("10 20"))
	p.skipComment = false

	// Read first expression
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), values.SchemeEquals, values.NewInteger(10))

	// Close the parser
	err = p.Close()
	c.Assert(err, qt.IsNil)

	// Closing again should error
	err = p.Close()
	c.Assert(err, qt.Equals, ErrAlreadyClosed)
}

// TestParser_Text tests the Text function.
func TestParser_Text(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("hello"))
	p.skipComment = false

	_, err := p.ReadSyntax(nil)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), values.SchemeEquals, tc.expect)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), values.SchemeEquals, tc.expect)
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
			in:     `"hex\x41here"`,
			expect: values.NewString("hexAhere"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), values.SchemeEquals, tc.expect)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), values.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_CommentsFollowedByCode tests that comments are properly parsed
// and the parser can continue to read subsequent expressions.
func TestParser_CommentsFollowedByCode(t *testing.T) {
	tcs := []struct {
		name    string
		in      string
		expects []values.Value
	}{
		{
			name: "line comment followed by integer",
			in:   "; comment\n42",
			expects: []values.Value{
				values.NewString(" comment"),
				values.NewInteger(42),
			},
		},
		{
			name: "block comment followed by symbol",
			in:   "#| comment |# foo",
			expects: []values.Value{
				values.NewString(" comment "),
				values.NewSymbol("foo"),
			},
		},
		{
			// Datum comments produce the commented datum as a value
			name: "datum comment followed by integer",
			in:   "#;(ignored) 42",
			expects: []values.Value{
				values.List(values.NewSymbol("ignored")),
				values.NewInteger(42),
			},
		},
		{
			name: "multiple line comments followed by code",
			in:   "; first\n; second\n10",
			expects: []values.Value{
				values.NewString(" first"),
				values.NewString(" second"),
				values.NewInteger(10),
			},
		},
		{
			name: "block comment between expressions",
			in:   "10 #| middle |# 20",
			expects: []values.Value{
				values.NewInteger(10),
				values.NewString(" middle "),
				values.NewInteger(20),
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.in))
			p.skipComment = false

			for i, expect := range tc.expects {
				syn, err := p.ReadSyntax(nil)
				if err == io.EOF {
					c.Fatalf("unexpected EOF at index %d, expected %v", i, expect)
				}
				c.Assert(err, qt.IsNil, qt.Commentf("error at index %d", i))

				got := syn.UnwrapAll()
				// For datum comments, just check the type since the inner form varies
				c.Assert(got, values.SchemeEquals, expect, qt.Commentf("mismatch at index %d", i))
			}

			// Verify we've consumed everything
			_, err := p.ReadSyntax(nil)
			c.Assert(err, qt.Equals, io.EOF)
		})
	}
}

// TestParserError tests the ParserError type.
func TestParserError(t *testing.T) {
	c := qt.New(t)

	// Test NewTokenizerError
	err1 := NewTokenizerError(nil, "test error")
	c.Assert(err1.Error(), qt.Equals, "test error")
	c.Assert(err1.Unwrap(), qt.IsNil)

	// Test NewTokenizerErrorWithWrap
	innerErr := values.NewForeignError("inner error")
	err2 := NewTokenizerErrorWithWrap(innerErr, nil, "wrapped error")
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
	tcs := []struct {
		input string
		num   int64
		denom int64
	}{
		{"1/2", 1, 2},
		{"3/4", 3, 4},
		{"-1/2", -1, 2},
		{"+3/4", 3, 4},
		{"0/1", 0, 1},
		{"10/3", 10, 3},
		{"-7/8", -7, 8},
		{"100/200", 1, 2}, // Should be normalized
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			r, err := p.parseRational(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(r.Num().Int64(), qt.Equals, tc.num)
			c.Assert(r.Denom().Int64(), qt.Equals, tc.denom)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
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
	}{
		{"+i", 0, 1},
		{"-i", 0, -1},
		{"+3i", 0, 3},
		{"-3i", 0, -3},
		{"+2.5i", 0, 2.5},
		{"-2.5i", 0, -2.5},
		{"+0.5i", 0, 0.5},
		{"-0.5i", 0, -0.5},
		{"+100i", 0, 100},
		{"-100i", 0, -100},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			z, err := p.parseImaginary(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(z.Real(), qt.Equals, tc.real)
			c.Assert(z.Imag(), qt.Equals, tc.imag)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
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
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(floatEquals(z.Real(), tc.real, 1e-10), qt.IsTrue)
			c.Assert(floatEquals(z.Imag(), tc.imag, 1e-10), qt.IsTrue)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
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
	}{
		{"+i", 0, 1},
		{"-i", 0, -1},
		{"+3i", 0, 3},
		{"-2.5i", 0, -2.5},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			z, ok := syn.UnwrapAll().(*values.Complex)
			c.Assert(ok, qt.IsTrue)
			c.Assert(z.Real(), qt.Equals, tc.real)
			c.Assert(z.Imag(), qt.Equals, tc.imag)
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
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			z, ok := syn.UnwrapAll().(*values.Complex)
			c.Assert(ok, qt.IsTrue)
			c.Assert(floatEquals(z.Real(), tc.real, 1e-10), qt.IsTrue)
			c.Assert(floatEquals(z.Imag(), tc.imag, 1e-10), qt.IsTrue)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Datum(), qt.Equals, tc.expect)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Datum(), qt.Equals, tc.expect)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Datum(), qt.Equals, tc.expect)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Datum(), qt.Equals, tc.expect)
		})
	}
}

// ============================================================================
// Exactness Marker Tests (#e, #i)
// ============================================================================

func TestReadSyntaxExactnessMarkers(t *testing.T) {
	// Note: Exactness markers are parsed but don't change the value type
	// (as noted in parser.go: "For now, we don't track exactness in the value types")
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#e42", 42},
		{"#i42", 42},
		{"#e100", 100},
		{"#i100", 100},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.input, func(c *qt.C) {
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			i, ok := syn.UnwrapAll().(*values.Integer)
			c.Assert(ok, qt.IsTrue)
			c.Assert(i.Datum(), qt.Equals, tc.expect)
		})
	}
}

// ============================================================================
// Special Float Values Tests (+inf.0, -inf.0, +nan.0, -nan.0)
// ============================================================================

func TestReadSyntaxRealInf(t *testing.T) {
	c := qt.New(t)

	// Test +inf.0
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("+inf.0"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	f, ok := syn.UnwrapAll().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsInf(f.Datum(), 1), qt.IsTrue) // positive infinity

	// Test -inf.0
	env2 := environment.NewTopLevelEnvironmentFrame()
	p2 := NewParser(env2, strings.NewReader("-inf.0"))
	syn2, err := p2.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	f2, ok := syn2.UnwrapAll().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsInf(f2.Datum(), -1), qt.IsTrue) // negative infinity
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)

			f, ok := syn.UnwrapAll().(*values.Float)
			c.Assert(ok, qt.IsTrue)
			c.Assert(math.IsNaN(f.Datum()), qt.IsTrue) // NaN check
		})
	}
}

func TestReadSyntaxImaginaryInf(t *testing.T) {
	c := qt.New(t)

	// Test +inf.0i
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("+inf.0i"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	z, ok := syn.UnwrapAll().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(z.Real(), qt.Equals, 0.0)
	c.Assert(math.IsInf(z.Imag(), 1), qt.IsTrue) // positive infinity

	// Test -inf.0i
	env2 := environment.NewTopLevelEnvironmentFrame()
	p2 := NewParser(env2, strings.NewReader("-inf.0i"))
	syn2, err := p2.ReadSyntax(nil)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
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

	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("(1+2i 3-4i)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	// Get the list - UnwrapAll returns the underlying Pair
	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// First element: 1+2i
	first, ok := pair.Car().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(first.Real(), qt.Equals, 1.0)
	c.Assert(first.Imag(), qt.Equals, 2.0)

	// Second element: 3-4i
	rest := pair.Cdr().(*values.Pair)
	second, ok := rest.Car().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(second.Real(), qt.Equals, 3.0)
	c.Assert(second.Imag(), qt.Equals, -4.0)
}

func TestReadSyntaxMixedNumericTypes(t *testing.T) {
	c := qt.New(t)

	// List with integer, rational, imaginary, and complex
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader("(42 3/4 +2i 1+2i)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	pair := syn.UnwrapAll().(*values.Pair)

	// First: 42 (integer)
	first, ok := pair.Car().(*values.Integer)
	c.Assert(ok, qt.IsTrue)
	c.Assert(first.Datum(), qt.Equals, int64(42))

	// Second: 3/4 (rational)
	pair = pair.Cdr().(*values.Pair)
	second, ok := pair.Car().(*values.Rational)
	c.Assert(ok, qt.IsTrue)
	c.Assert(second.Num().Int64(), qt.Equals, int64(3))
	c.Assert(second.Denom().Int64(), qt.Equals, int64(4))

	// Third: +2i (pure imaginary)
	pair = pair.Cdr().(*values.Pair)
	third, ok := pair.Car().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(third.Real(), qt.Equals, 0.0)
	c.Assert(third.Imag(), qt.Equals, 2.0)

	// Fourth: 1+2i (complex)
	pair = pair.Cdr().(*values.Pair)
	fourth, ok := pair.Car().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(fourth.Real(), qt.Equals, 1.0)
	c.Assert(fourth.Imag(), qt.Equals, 2.0)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)

			// Check real part
			if tc.realNaN {
				c.Assert(math.IsNaN(z.Real()), qt.IsTrue, qt.Commentf("real should be NaN"))
			} else if tc.realInf != 0 {
				c.Assert(math.IsInf(z.Real(), tc.realInf), qt.IsTrue, qt.Commentf("real should be inf(%d)", tc.realInf))
			} else {
				c.Assert(z.Real(), qt.Equals, tc.wantReal)
			}

			// Check imaginary part
			if tc.imagNaN {
				c.Assert(math.IsNaN(z.Imag()), qt.IsTrue, qt.Commentf("imag should be NaN"))
			} else if tc.imagInf != 0 {
				c.Assert(math.IsInf(z.Imag(), tc.imagInf), qt.IsTrue, qt.Commentf("imag should be inf(%d)", tc.imagInf))
			} else {
				c.Assert(z.Imag(), qt.Equals, tc.wantImag)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(z.Real(), qt.Equals, tc.real)
			c.Assert(z.Imag(), qt.Equals, tc.imag)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(floatEquals(z.Real(), tc.real, 1e-15), qt.IsTrue, qt.Commentf("real: got %v want %v", z.Real(), tc.real))
			c.Assert(floatEquals(z.Imag(), tc.imag, 1e-15), qt.IsTrue, qt.Commentf("imag: got %v want %v", z.Imag(), tc.imag))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(z.Real(), qt.Equals, tc.real)
			c.Assert(z.Imag(), qt.Equals, tc.imag)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(floatEquals(z.Real(), tc.real, 1e-15), qt.IsTrue, qt.Commentf("real: got %v want %v", z.Real(), tc.real))
			c.Assert(floatEquals(z.Imag(), tc.imag, 1e-15), qt.IsTrue, qt.Commentf("imag: got %v want %v", z.Imag(), tc.imag))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			z, err := p.parseComplex(tc.input)
			c.Assert(err, qt.IsNil)
			c.Assert(floatEquals(z.Real(), tc.real, tc.real*1e-10), qt.IsTrue, qt.Commentf("real: got %v want %v", z.Real(), tc.real))
			c.Assert(floatEquals(z.Imag(), tc.imag, tc.imag*1e-10), qt.IsTrue, qt.Commentf("imag: got %v want %v", z.Imag(), tc.imag))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			got, err := p.parseRealPart(tc.input)
			c.Assert(err, qt.IsNil)

			if tc.wantNaN {
				c.Assert(math.IsNaN(got), qt.IsTrue)
			} else if tc.wantInf != 0 {
				c.Assert(math.IsInf(got, tc.wantInf), qt.IsTrue)
			} else {
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(""))
			got, err := p.parseImagPart(tc.input)
			c.Assert(err, qt.IsNil)

			if tc.wantNaN {
				c.Assert(math.IsNaN(got), qt.IsTrue)
			} else if tc.wantInf != 0 {
				c.Assert(math.IsInf(got, tc.wantInf), qt.IsTrue)
			} else {
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
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
	env := environment.NewTopLevelEnvironmentFrame()

	input := "10 20 30"
	p := NewParser(env, strings.NewReader(input))

	// First read
	syn1, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn1.UnwrapAll(), values.SchemeEquals, values.NewInteger(10))

	// Second read - tokenizer should be preserved
	syn2, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn2.UnwrapAll(), values.SchemeEquals, values.NewInteger(20))

	// Third read
	syn3, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn3.UnwrapAll(), values.SchemeEquals, values.NewInteger(30))

	// Fourth read should hit EOF
	_, err = p.ReadSyntax(nil)
	c.Assert(err, qt.Equals, io.EOF)
}

// TestParser_EmptyVector tests parsing empty vectors
func TestParser_EmptyVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("#()"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 0)
}

// TestParser_EmptyByteVector tests parsing empty byte vectors
func TestParser_EmptyByteVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Empty byte vector - tests the close paren path in byte vector parsing
	p := NewParser(env, strings.NewReader("#u8(10 20)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	bv, ok := syn.UnwrapAll().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*bv), qt.Equals, 2)
}

// TestParser_SingleElementVector tests vectors with one element (tests wrapSyntaxVector)
func TestParser_SingleElementVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("#(42)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 1)
	c.Assert((*vec)[0], values.SchemeEquals, values.NewInteger(42))
}

// TestParser_NestedLists tests lists within lists (tests listSyntax with multiple elements)
func TestParser_NestedLists(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("((a b) (c d e))"))
	syn, err := p.ReadSyntax(nil)
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
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader(`#(42 "hello" #t foo)`))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 4)

	c.Assert((*vec)[0], values.SchemeEquals, values.NewInteger(42))
	c.Assert((*vec)[1], values.SchemeEquals, values.NewString("hello"))
	c.Assert((*vec)[2], values.SchemeEquals, values.TrueValue)
	c.Assert((*vec)[3], values.SchemeEquals, values.NewSymbol("foo"))
}

// TestParser_ListSyntaxMultipleElements tests listSyntax with more than 2 elements
func TestParser_ListSyntaxMultipleElements(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Test quote with multiple elements in a list
	p := NewParser(env, strings.NewReader("'(a b c d)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	// Should be (quote (a b c d))
	pair := syn.UnwrapAll().(*values.Pair)
	c.Assert(pair.Car(), values.SchemeEquals, values.NewSymbol("quote"))

	// The cdr should be the list (a b c d)
	quotedList := pair.Cdr().(*values.Pair).Car().(*values.Pair)
	c.Assert(quotedList.Length(), qt.Equals, 4)
}

// TestParser_CharacterMnemonicCoverage tests all character mnemonics
func TestParser_CharacterMnemonicCoverage(t *testing.T) {
	tcs := []struct {
		input    string
		expected rune
	}{
		{"#\\form-feed", RuneFormFeed},
		{"#\\vertical-tab", RuneVerticalTab},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			ch := syn.UnwrapAll().(*values.Character)
			c.Assert(ch.Value, qt.Equals, tc.expected)
		})
	}
}

// TestParser_ReadSyntaxErrorPropagation tests error propagation in ReadSyntax
func TestParser_ReadSyntaxErrorPropagation(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Test with unclosed list - should propagate EOF error
	p := NewParser(env, strings.NewReader("("))
	_, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNotNil)

	// After error, tokenizer should be nil
	c.Assert(p.toks, qt.IsNil)
}

// TestParser_ComplexWithScientificNotation tests parseComplex edge case
func TestParser_ComplexWithScientificNotation_EdgeCase(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader(""))

	// Test with uppercase E which is allowed by R7RS
	z, err := p.parseComplex("1E2+3E2i")
	c.Assert(err, qt.IsNil)
	c.Assert(floatEquals(z.Real(), 100, 1e-10), qt.IsTrue)
	c.Assert(floatEquals(z.Imag(), 300, 1e-10), qt.IsTrue)
}

// TestParser_ByteVectorWithMultipleValues tests byte vector parsing
func TestParser_ByteVectorWithMultipleValues(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("#u8(255 128 0 64)"))
	syn, err := p.ReadSyntax(nil)
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
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("(a b) (c d)"))

	// First read
	syn1, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	list1 := syn1.UnwrapAll().(*values.Pair)
	c.Assert(list1.Length(), qt.Equals, 2)

	// Tokenizer should still exist
	c.Assert(p.toks, qt.Not(qt.IsNil))

	// Second read should work
	syn2, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	list2 := syn2.UnwrapAll().(*values.Pair)
	c.Assert(list2.Length(), qt.Equals, 2)
}

// TestParser_ComplexNumberSignSeparatorEdgeCases tests edge cases in sign detection
func TestParser_ComplexNumberSignSeparatorEdgeCases(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()
	p := NewParser(env, strings.NewReader(""))

	// Test number that has no valid sign separator (should error)
	_, err := p.parseComplex("123.456")
	c.Assert(err, qt.IsNotNil)
}

// TestParser_ReadSyntaxEOFHandling tests EOF handling in ReadSyntax
func TestParser_ReadSyntaxEOFHandling(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("42"))

	// First read succeeds
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), values.SchemeEquals, values.NewInteger(42))

	// Second read hits EOF (but this is OK, tokenizer advances)
	_, err = p.ReadSyntax(nil)
	c.Assert(err, qt.Equals, io.EOF)
}

// TestParser_VectorLoop tests the vector parsing loop
func TestParser_VectorLoop(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Test vector with multiple elements to exercise the loop
	p := NewParser(env, strings.NewReader("#(1 2 3 4 5)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	vec, ok := syn.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*vec), qt.Equals, 5)
}

// TestParser_ListWithMultipleElements tests list parsing loop
func TestParser_ListWithMultipleElements(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// List with many elements
	p := NewParser(env, strings.NewReader("(a b c d e f g h)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	list, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(list.Length(), qt.Equals, 8)
}

// TestParser_ByteVectorLoop tests byte vector parsing loop
func TestParser_ByteVectorLoop(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("#u8(1 2 3 4 5 6 7 8)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	bv, ok := syn.UnwrapAll().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*bv), qt.Equals, 8)
}

// TestParser_EmptyList tests empty list parsing
func TestParser_EmptyList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("()"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(syn.UnwrapAll(), values.SchemeEquals, values.EmptyList)
}

// TestParser_ImproperList tests improper list (dotted pair) parsing
func TestParser_ImproperList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("(a b . c)"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	// Should be a pair with a and a pair with b and c
	pair := syn.UnwrapAll().(*values.Pair)
	c.Assert(pair.Car(), values.SchemeEquals, values.NewSymbol("a"))
}

// TestParser_QuasiquoteSingleElement tests listSyntax with 1 element
func TestParser_QuasiquoteSingleElement(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader("`x"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	// Should be (quasiquote x)
	list := syn.UnwrapAll().(*values.Pair)
	c.Assert(list.Car(), values.SchemeEquals, values.NewSymbol("quasiquote"))
	c.Assert(list.Length(), qt.Equals, 2) // (quasiquote x) is length 2
}

// TestParser_UnquoteSplicing tests listSyntax with 2 elements
func TestParser_UnquoteSplicing(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	p := NewParser(env, strings.NewReader(",@foo"))
	syn, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	// Should be (unquote-splicing foo)
	list := syn.UnwrapAll().(*values.Pair)
	c.Assert(list.Car(), values.SchemeEquals, values.NewSymbol("unquote-splicing"))
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
			env := environment.NewTopLevelEnvironmentFrame()
			p := NewParser(env, strings.NewReader(tc.input))
			syn, err := p.ReadSyntax(nil)
			c.Assert(err, qt.IsNil)
			c.Assert(syn.UnwrapAll(), values.SchemeEquals, tc.expect)
		})
	}
}

// TestParser_RationalNumbers tests rational number parsing
func TestParser_RationalNumbers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Test both signed and unsigned rational fractions
	p1 := NewParser(env, strings.NewReader("1/2"))
	syn1, err := p1.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	r1 := syn1.UnwrapAll().(*values.Rational)
	c.Assert(r1.Num().Int64(), qt.Equals, int64(1))
	c.Assert(r1.Denom().Int64(), qt.Equals, int64(2))

	// Test signed rational
	p2 := NewParser(env, strings.NewReader("-3/4"))
	syn2, err := p2.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)
	r2 := syn2.UnwrapAll().(*values.Rational)
	c.Assert(r2.Num().Int64(), qt.Equals, int64(-3))
	c.Assert(r2.Denom().Int64(), qt.Equals, int64(4))
}

func TestNewParserWithFile(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironmentFrame()

	// Test that NewParserWithFile stores the filename in source context
	filename := "test-file.scm"
	p := NewParserWithFile(env, strings.NewReader("(define x 42)"), filename)

	stx, err := p.ReadSyntax(nil)
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
	env := environment.NewTopLevelEnvironmentFrame()

	// Test backward compatibility: NewParser should set empty filename
	p := NewParser(env, strings.NewReader("hello"))

	stx, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	sc := stx.SourceContext()
	c.Assert(sc, qt.IsNotNil)
	c.Assert(sc.File, qt.Equals, "")
}
