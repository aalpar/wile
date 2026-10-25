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

package io_test

import (
	"context"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/extensions/files"
	extio "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/internal/extensions/iotest"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngineWithIOTest creates an engine with both the io and iotest
// extensions loaded. iotest provides fault-injection input ports that let
// us drive ReadRune/UnreadRune failure modes from Scheme — necessary
// because the shipped port types succeed deterministically on the call
// sequences peek-char and read-line produce.
func newEngineWithIOTest(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(iotest.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// TestReadErrorClassification verifies that I/O failures inside peek-char
// and read-line surface as read errors per R7RS §6.11 — i.e.
// (read-error? e) returns #t.
//
// These tests guard against regressions of two prior bugs in
// internal/extensions/io/prim_read_write.go:
//   - peek-char's UnreadRune error was wrapped with WrapForeignErrorf
//     (instead of WrapForeignReadErrorf), classifying it as Generic
//     instead of Read.
//   - read-line silently dropped UnreadRune errors via //nolint:errcheck
//     and silently dropped non-EOF errors from the inner ReadRune.
func TestReadErrorClassification(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithIOTest(t)

	tcs := []struct {
		name string
		code string
	}{
		{
			name: "peek-char surfaces UnreadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (peek-char (make-failing-unread-port "abc"))))`,
		},
		{
			name: "read-line surfaces UnreadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read-line (make-failing-unread-port "ab\rxy"))))`,
		},
		{
			// read returns its lookahead rune to the port after each datum; if
			// that fails the port position is wrong, which is a read error.
			name: "read surfaces UnreadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read (make-failing-unread-port "ab cd"))))`,
		},
		{
			name: "read-line surfaces inner ReadRune failure as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read-line (make-failing-read-after-port "ab\rxy" 3))))`,
		},
		{
			// A fault one rune into a symbol used to end the symbol: read
			// returned a, a truncated datum, with no error.
			name: "read surfaces a fault inside a symbol as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read (make-failing-read-after-port "abc" 1))))`,
		},
		{
			name: "read-syntax surfaces a fault inside a symbol as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read-syntax (make-failing-read-after-port "abc" 1))))`,
		},
		{
			name: "read surfaces a fault before the first rune as read-error",
			code: `(read-error?
                     (guard (e (#t e))
                       (read (make-failing-read-after-port "abc" 0))))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestReadLineBareCarriageReturnAtEOF is a positive regression: after the
// read-line fixes, a bare \r at end-of-stream must still produce the
// preceding line, not raise.
func TestReadLineBareCarriageReturnAtEOF(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithIOTest(t)
	// Input "hi\r" — after \r, inner ReadRune sees io.EOF, which must be
	// treated as line-ends-with-bare-\r, not propagated.
	result := eval(t, engine,
		`(equal? (read-line (open-input-string "hi\r")) "hi")`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

// classifyCondition renders the R7RS §6.11 predicates on the condition expr
// raises, or no-raise when it returns.
func classifyCondition(expr string) string {
	return `(guard (e (#t (list (read-error? e) (file-error? e)))) ` + expr + ` 'no-raise)`
}

// TestReadFaultKeepsTheFault pins that a fault from the port's reader reaches
// Scheme as that fault. read used to see it as end of input: a fault inside a
// list reported "unterminated list" and lost the injected cause. read and
// read-char classify the same fault the same way.
func TestReadFaultKeepsTheFault(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name string
		code string
	}{
		{"read inside a list", `(read (make-failing-read-after-port "(a b c)" 4))`},
		{"read-syntax inside a list", `(read-syntax (make-failing-read-after-port "(a b c)" 4))`},
		{"read-char", `(read-char (make-failing-read-after-port "abc" 0))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngineWithIOTest(t)
			_, err := engine.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, iotest.ErrIOTestFault), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "unterminated"), qt.IsFalse, qt.Commentf("got: %v", err))
			c.Check(eval(t, engine, classifyCondition(tc.code)).Internal().SchemeString(), qt.Equals, "(#t #f)")
		})
	}
}

// TestReadOnClosedPortRaises pins that reading a closed port raises, naming
// the port, rather than answering #<eof> (petite: "not permitted on closed
// port"; racket: "input port is closed"). A closed port is not a read error:
// read-error? is for a fault in the data or the stream, and racket raises a
// plain exn:fail, not exn:fail:read. Every read primitive agrees, so the
// answer does not depend on which one met the closed port.
func TestReadOnClosedPortRaises(t *testing.T) {
	ctx := context.Background()
	const textual = `(open-input-string "abc")`
	const binary = `(open-input-bytevector (bytevector 1 2))`
	tcs := []struct {
		name string
		port string
		code string
		want string
	}{
		{"read", textual, `(read p)`, "string-input-port is closed"},
		{"read-syntax", textual, `(read-syntax p)`, "string-input-port is closed"},
		{"read-char", textual, `(read-char p)`, "string-input-port is closed"},
		{"peek-char", textual, `(peek-char p)`, "string-input-port is closed"},
		{"read-line", textual, `(read-line p)`, "string-input-port is closed"},
		{"read-string", textual, `(read-string 2 p)`, "string-input-port is closed"},
		{"read-u8", binary, `(read-u8 p)`, "bytevector-input-port is closed"},
		{"peek-u8", binary, `(peek-u8 p)`, "bytevector-input-port is closed"},
		{"read-bytevector", binary, `(read-bytevector 2 p)`, "bytevector-input-port is closed"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngineWithIOTest(t)
			code := `(let ((p ` + tc.port + `)) (close-port p) ` + tc.code + `)`
			_, err := engine.EvalMultiple(ctx, code)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, werr.ErrPortClosed), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), tc.want), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(eval(t, engine, classifyCondition(code)).Internal().SchemeString(), qt.Equals, "(#f #f)")
		})
	}
	// Control: an open, empty port is end of input, not a fault.
	c := qt.New(t)
	engine := newEngineWithIOTest(t)
	c.Check(eval(t, engine, `(eof-object? (read (open-input-string "")))`).Internal(), qt.Equals, values.TrueValue)
}

// TestReadOnClosedFilePortNamesTheFile pins that a closed file port's refusal
// names the file through read and read-syntax as well as read-char. read
// renders its fault through the parser's file:line:col form, which once
// dropped the cause and printed only "reader failed".
func TestReadOnClosedFilePortNamesTheFile(t *testing.T) {
	c := qt.New(t)
	path := filepath.Join(t.TempDir(), "ok.scm")
	err := os.WriteFile(path, []byte("(a b c)"), 0o600)
	c.Assert(err, qt.IsNil)
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	err = engine.Define("path", wile.WrapValue(values.NewString(path)))
	c.Assert(err, qt.IsNil)
	for _, op := range []string{"read", "read-syntax", "read-char"} {
		t.Run(op, func(t *testing.T) {
			c := qt.New(t)
			code := `(let ((p (open-input-file path))) (close-port p) (` + op + ` p))`
			_, err := engine.EvalMultiple(context.Background(), code)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, werr.ErrPortClosed), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), path+" is closed"), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}

// TestReadAgreesWithReadCharOnInvalidUTF8 pins that read and read-char take
// the same view of a byte that does not decode: both read it as U+FFFD, as
// racket and petite do. read used to raise a rune error on a port read-char
// read cleanly. U+FFFD is an identifier character, so an invalid byte inside
// a symbol joins it, as in racket. Where U+FFFD may not appear (ending a
// radix-prefixed numeral) the read fails as it does for a written U+FFFD,
// with a lexical error that names the character.
func TestReadAgreesWithReadCharOnInvalidUTF8(t *testing.T) {
	tcs := []struct {
		name    string
		content string
		code    string
		want    string
		wantErr string
	}{
		{name: "read-char", content: "a\xffb", code: "(let* ((a (read-char p)) (b (read-char p)) (c (read-char p))) (list a (char->integer b) c (read-char p)))", want: `(#\a 65533 #\b #<eof>)`},
		{name: "string", content: "\"a\xffb\"", code: "(let ((s (read p))) (list (string-length s) (char->integer (string-ref s 1)) (read p)))", want: "(3 65533 #<eof>)"},
		{name: "comment", content: "; a\xffb\n42", code: "(list (read p) (read p))", want: "(42 #<eof>)"},
		{name: "character", content: "#\\\xff", code: "(char->integer (read p))", want: "65533"},
		{name: "symbol", content: "a\xffb", code: "(let ((s (symbol->string (read p)))) (list (string-length s) (char->integer (string-ref s 1)) (read p)))", want: "(3 65533 #<eof>)"},
		{name: "symbol at end of input", content: "a\xff", code: "(let ((s (symbol->string (read p)))) (list (string-length s) (char->integer (string-ref s 1)) (read p)))", want: "(2 65533 #<eof>)"},
		{name: "radix numeral", content: "#b1\xff", code: "(list (read p) (read p))", wantErr: "expecting delimiter"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			path := filepath.Join(t.TempDir(), "bad.txt")
			err := os.WriteFile(path, []byte(tc.content), 0o600)
			c.Assert(err, qt.IsNil)
			engine, err := wile.NewEngine(context.Background(),
				wile.WithExtension(extio.Extension),
				wile.WithExtension(files.Extension),
			)
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = engine.Close()
			})
			err = engine.Define("path", wile.WrapValue(values.NewString(path)))
			c.Assert(err, qt.IsNil)
			got, err := engine.EvalMultiple(context.Background(), "(define p (open-input-file path)) "+tc.code)
			if tc.wantErr != "" {
				c.Assert(err, qt.IsNotNil, qt.Commentf("got %v", got))
				c.Check(err.Error(), qt.Contains, tc.wantErr)
				c.Check(err.Error(), qt.Contains, "U+FFFD")
				c.Check(err.Error(), qt.Not(qt.Contains), "rune error")
				return
			}
			c.Assert(err, qt.IsNil)
			c.Check(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// transientReader yields its chunks in order; a nil chunk is one read failing
// with a deadline timeout, after which the stream continues.
type transientReader struct {
	chunks []*string
}

func (p *transientReader) Read(b []byte) (int, error) {
	if len(p.chunks) == 0 {
		return 0, io.EOF
	}
	chunk := p.chunks[0]
	p.chunks = p.chunks[1:]
	if chunk == nil {
		return 0, os.ErrDeadlineExceeded
	}
	return copy(b, *chunk), nil
}

// TestReadRecoversFromATransientReaderFault pins that a reader fault ends only
// the read it interrupted. The port's cached parser used to survive the fault
// in its terminal state, so every later read on the port repeated it while
// read-char on the same port carried on.
func TestReadRecoversFromATransientReaderFault(t *testing.T) {
	tcs := []struct {
		op, datum string
	}{
		{"read", "d"},
		{"read-syntax", "(syntax->datum d)"},
	}
	for _, tc := range tcs {
		t.Run(tc.op, func(t *testing.T) {
			c := qt.New(t)
			x, y := "x ", "y"
			rdr := &transientReader{chunks: []*string{&x, nil, &y}}
			engine := newEngineWithIOTest(t)
			err := engine.Define("p", wile.WrapValue(values.NewCharacterInputPortFromReader(rdr)))
			c.Assert(err, qt.IsNil)
			step := `(guard (e ((error-object? e) 'fault))
			           (let ((d (` + tc.op + ` p)))
			             (if (eof-object? d) 'eof ` + tc.datum + `)))`
			r := eval(t, engine, `(let* ((a `+step+`) (b `+step+`) (c `+step+`) (d `+step+`)) (list a b c d))`)
			c.Check(r.Internal().SchemeString(), qt.Equals, "(x fault y eof)")
		})
	}
}

// TestReadFaultPastACloseParenIsIntertoken pins the port-level shape of B39's
// gate: a fault just past a ")", here a reader failing once after "(x)". The
// fault is in no token, so it is stamped failed (between datums). The read
// that completed (x) returns it, the next read on the port reports the fault,
// and the port then reads on, as racket does ((x), the fault, y, eof). The
// port's cached parser keeps the fault for that next read rather than being
// rebuilt over the reader, which would have read past the transient failure.
func TestReadFaultPastACloseParenIsIntertoken(t *testing.T) {
	tcs := []struct {
		op, datum string
	}{
		{"read", "d"},
		{"read-syntax", "(syntax->datum d)"},
	}
	for _, tc := range tcs {
		t.Run(tc.op, func(t *testing.T) {
			c := qt.New(t)
			datum, rest := "(x)", "y"
			rdr := &transientReader{chunks: []*string{&datum, nil, &rest}}
			engine := newEngineWithIOTest(t)
			err := engine.Define("p", wile.WrapValue(values.NewCharacterInputPortFromReader(rdr)))
			c.Assert(err, qt.IsNil)
			step := `(let ((d (` + tc.op + ` p))) (if (eof-object? d) 'eof ` + tc.datum + `))`
			first := eval(t, engine, step)
			c.Check(first.Internal().SchemeString(), qt.Equals, "(x)")
			_, err = engine.EvalMultiple(context.Background(), step)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, os.ErrDeadlineExceeded), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "(at index 3, line 1, column 3,"), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "tokenizer state failed)"), qt.IsTrue, qt.Commentf("got: %v", err))
			tail := eval(t, engine, `(let* ((a `+step+`) (b `+step+`)) (list a b))`)
			c.Check(tail.Internal().SchemeString(), qt.Equals, "(y eof)")
		})
	}
}

// TestDeferredReadFaultPrecedesTheNextCharacter pins that a reader fault read
// deferred past (x) is reported by the port's next input operation, whichever
// it is: the fault came before any character past it, so handing out y first
// and raising the fault on a later read (at index 3, behind the characters
// already taken) misordered the stream. racket raises it from read-char too.
// After the report the port reads on.
func TestDeferredReadFaultPrecedesTheNextCharacter(t *testing.T) {
	for _, op := range []string{"(read-char p)", "(peek-char p)", "(read-line p)", "(read-string 1 p)", "(char-ready? p)", "(read p)", "(read-syntax p)"} {
		t.Run(op, func(t *testing.T) {
			c := qt.New(t)
			datum, rest := "(x)", "yz"
			rdr := &transientReader{chunks: []*string{&datum, nil, &rest}}
			engine := newEngineWithIOTest(t)
			err := engine.Define("p", wile.WrapValue(values.NewCharacterInputPortFromReader(rdr)))
			c.Assert(err, qt.IsNil)
			c.Check(eval(t, engine, `(read p)`).SchemeString(), qt.Equals, "(x)")
			_, err = engine.EvalMultiple(context.Background(), op)
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, os.ErrDeadlineExceeded), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "(at index 3, line 1, column 3,"), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(eval(t, engine, `(list (read-char p) (read p))`).SchemeString(), qt.Equals, `(#\y z)`)
		})
	}
}

// TestReplacementCharacterIsAnIdentifierCharacter pins U+FFFD as a symbol
// constituent in every reader entry, as racket and petite read it, and pins
// the trap beside it: end of input arrives as the same utf8.RuneError, so a
// symbol that ends the input must end there, with nothing appended.
func TestReplacementCharacterIsAnIdentifierCharacter(t *testing.T) {
	tcs := []struct {
		name, code, want string
	}{
		{name: "read at end of input", code: `(let ((p (open-input-string "a"))) (list (read p) (read p)))`, want: "(a #<eof>)"},
		{name: "read-syntax at end of input", code: `(let ((p (open-input-string "ab"))) (list (syntax->datum (read-syntax p)) (read-syntax p)))`, want: "(ab #<eof>)"},
		{name: "read-char after a symbol at end of input", code: `(let ((p (open-input-string "a"))) (read p) (read-char p))`, want: "#<eof>"},
		{name: "inside a symbol", code: `(symbol->string (read (open-input-string "a\xFFFD;b")))`, want: "\"a�b\""},
		{name: "ending the input", code: `(string-length (symbol->string (read (open-input-string "a\xFFFD;"))))`, want: "2"},
		{name: "in a list", code: `(let ((d (read (open-input-string "(\xFFFD;)")))) (list (length d) (symbol? (car d))))`, want: "(1 #t)"},
		{name: "in a datum comment", code: `(read (open-input-string "#;a\xFFFD; 42"))`, want: "42"},
		{name: "read-syntax inside a symbol", code: `(string-length (symbol->string (syntax->datum (read-syntax (open-input-string "a\xFFFD;b")))))`, want: "3"},
		{name: "source text", code: "(symbol->string 'a�b)", want: "\"a�b\""},
		{name: "source text at end of input", code: "'a", want: "a"},
		{name: "written and read back", code: `(let* ((s (string->symbol "a\xFFFD;b")) (o (open-output-string))) (write s o) (eq? s (read (open-input-string (get-output-string o)))))`, want: "#t"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngineWithIOTest(t)
			c.Check(eval(t, engine, tc.code).SchemeString(), qt.Equals, tc.want)
			// The one-expression entry reads the same source.
			expr, err := engine.Parse(context.Background(), tc.code)
			c.Assert(err, qt.IsNil)
			got, err := engine.Eval(context.Background(), expr)
			c.Assert(err, qt.IsNil)
			c.Check(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestCharReadyRequiresATextualInputPort pins that char-ready? checks its
// argument: it answered #t for anything, a number, a binary port and an output
// port included (petite refuses all three).
func TestCharReadyRequiresATextualInputPort(t *testing.T) {
	tcs := []struct {
		arg, want string
	}{
		{arg: "5", want: "err"},
		{arg: "(open-input-bytevector (bytevector 1))", want: "err"},
		{arg: "(open-output-string)", want: "err"},
		{arg: `(open-input-string "a")`, want: "#t"},
		{arg: "", want: "#t"},
	}
	for _, tc := range tcs {
		t.Run(tc.arg, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngineWithIOTest(t)
			got := eval(t, engine, `(guard (e (#t 'err)) (char-ready? `+tc.arg+`))`)
			c.Check(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}
