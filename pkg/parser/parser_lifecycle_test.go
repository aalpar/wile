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
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
)

// TestParser_CloseIsTerminal pins that Close actually closes.
//
// Close released the tokenizer and nothing else, and ReadSyntax rebuilds the
// tokenizer from the raw reader whenever p.toks is nil — so a closed parser
// silently reopened itself and kept reading: Close on a parser over "4 5"
// followed by ReadSyntax returned 4 with a nil error.
func TestParser_CloseIsTerminal(t *testing.T) {
	cases := []struct {
		name      string
		readFirst bool
	}{
		{"close before any read", false},
		{"close after a read", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader("4 5"))
			if tc.readFirst {
				q, err := p.ReadSyntax(context.TODO())
				c.Assert(err, qt.IsNil)
				c.Assert(q.UnwrapAll().SchemeString(), qt.Equals, "4")
			}
			// Close's own return is unchanged: ErrAlreadyClosed when the
			// tokenizer was never built, nil when it was.
			_ = p.Close()

			for range 2 {
				q, err := p.ReadSyntax(context.TODO())
				c.Assert(err, qt.IsNotNil, qt.Commentf("closed parser returned %v", q))
				c.Assert(errors.Is(err, ErrAlreadyClosed), qt.IsTrue, qt.Commentf("err = %v", err))
			}
		})
	}
}

// TestParser_ReadErrorIsTerminal pins the other half of the same field, the one
// with no Close involved: after a read error the parser must stay dead.
//
// p.err was never sticky — ReadSyntax overwrites it unconditionally whenever
// p.toks is nil, which is exactly the state an error leaves behind — so
// "(1 2] (3 4) (5 6)" reported the mismatched delimiter once and then handed
// back (3 4) and (5 6) as if nothing had happened. A caller that logs and
// continues silently accepted a file it had already diagnosed as malformed.
func TestParser_ReadErrorIsTerminal(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(1 2] (3 4) (5 6)"))

	_, first := p.ReadSyntax(context.TODO())
	c.Assert(first, qt.IsNotNil)
	c.Assert(first.Error(), qt.Contains, "mismatched delimiters")

	for range 2 {
		q, err := p.ReadSyntax(context.TODO())
		c.Assert(err, qt.IsNotNil, qt.Commentf("dead parser resumed and returned %v", q))
		c.Assert(err.Error(), qt.Contains, "mismatched delimiters",
			qt.Commentf("a dead parser must keep reporting the error that killed it"))
	}
}

// TestParser_CleanEOFIsRepeatableAndNotAnError is the control: reaching the end
// of input is not death. Every caller that drains a parser loops on io.EOF, so
// the terminal-state flag must not turn a clean end into ErrAlreadyClosed.
func TestParser_CleanEOFIsRepeatableAndNotAnError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("1 2"))

	for _, want := range []string{"1", "2"} {
		q, err := p.ReadSyntax(context.TODO())
		c.Assert(err, qt.IsNil)
		c.Assert(q.UnwrapAll().SchemeString(), qt.Equals, want)
	}
	for range 2 {
		_, err := p.ReadSyntax(context.TODO())
		c.Assert(errors.Is(err, ErrAlreadyClosed), qt.IsFalse, qt.Commentf("err = %v", err))
	}
}

// TestParser_ReadSyntaxStopsAtDatumEnd pins that ReadSyntax consumes the datum
// and at most the one rune of tokenizer lookahead, never the next token: it used
// to advance to the following datum before returning, so a reader shared with
// read-char (R7RS §6.13.2) had already lost it, and an interactive read held its
// return until the next datum was typed. ReleaseLookahead returns that one rune.
func TestParser_ReadSyntaxStopsAtDatumEnd(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	r := strings.NewReader("(a b) cd")
	p := NewParser(env, true, r)

	q, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(q.UnwrapAll().SchemeString(), qt.Equals, "(a b)")
	c.Assert(r.Len(), qt.Equals, len(" cd")-1, qt.Commentf("only the lookahead rune past the datum is consumed"))

	c.Assert(p.ReleaseLookahead(r), qt.IsNil)
	c.Assert(r.Len(), qt.Equals, len(" cd"))

	q, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(q.UnwrapAll().SchemeString(), qt.Equals, "cd")
	c.Assert(q.SourceContext().Start.Column(), qt.Equals, 6, qt.Commentf("positions keep counting across a release"))
}
