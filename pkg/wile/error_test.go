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
	"fmt"
	"io"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/werr"
)

func TestIsIncompleteInput(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		err  error
		want bool
	}{
		{
			name: "nil error",
			err:  nil,
			want: false,
		},
		{
			name: "plain io.EOF",
			err:  io.EOF,
			want: false,
		},
		{
			name: "wrapped io.EOF via fmt.Errorf",
			err:  fmt.Errorf("parse failed: %w", io.EOF),
			want: true,
		},
		{
			name: "CompilationError wrapping io.EOF",
			err: &CompilationError{
				Message: "parse error",
				Cause:   io.EOF,
			},
			want: true,
		},
		{
			// Mid-parse EOF: source ended inside a form (wrapMidParseEOF wraps
			// io.ErrUnexpectedEOF). Covers unclosed lists/vectors/block comments.
			name: "CompilationError wrapping io.ErrUnexpectedEOF",
			err: &CompilationError{
				Message: "inside list",
				Cause:   io.ErrUnexpectedEOF,
			},
			want: true,
		},
		{
			// Tokenizer hit EOF inside an unterminated string/symbol.
			name: "CompilationError wrapping ErrIncompleteInput",
			err: &CompilationError{
				Message: "parse error",
				Cause:   werr.ErrIncompleteInput,
			},
			want: true,
		},
		{
			// Parser produced a partial token from premature EOF.
			name: "CompilationError wrapping ErrUnknownTokenType",
			err: &CompilationError{
				Message: "parse error",
				Cause:   parser.ErrUnknownTokenType,
			},
			want: true,
		},
		{
			// Detection is structural (R20), not substring-based: an error carrying
			// a non-incomplete sentinel does not match regardless of its message.
			name: "non-incomplete sentinel does not match",
			err:  werr.ErrInvalidArgument,
			want: false,
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			c.Assert(IsIncompleteInput(tc.err), qt.Equals, tc.want)
		})
	}
}

// TestIsIncompleteInput_RealParse proves structural detection wires through the
// real tokenizer → parser → engine chain: genuinely-incomplete inputs are
// reported incomplete, while a complete-but-malformed parse is not.
func TestIsIncompleteInput_RealParse(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	incomplete := []struct {
		name string
		src  string
	}{
		{"unterminated string", `"abc`},
		{"unclosed list", `(foo bar`},
		{"unclosed vector", `#(1 2`},
		{"unclosed block comment", `#| comment`},
	}
	for _, tc := range incomplete {
		c.Run("incomplete/"+tc.name, func(c *qt.C) {
			_, perr := eng.Parse(ctx, tc.src)
			c.Assert(perr, qt.IsNotNil)
			c.Assert(IsIncompleteInput(perr), qt.IsTrue,
				qt.Commentf("err = %v", perr))
		})
	}

	// A stray close paren is a complete (malformed) token, not incomplete input —
	// the REPL should surface it, not wait for more.
	_, perr := eng.Parse(ctx, `)`)
	c.Assert(perr, qt.IsNotNil)
	c.Assert(IsIncompleteInput(perr), qt.IsFalse,
		qt.Commentf("err = %v", perr))
}
