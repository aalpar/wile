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
	"bufio"
	"context"
	"io"
	"strings"

	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// Expression represents a single parsed Scheme expression.
//
// Expression wraps a syntax value produced by the parser, before any
// macro expansion or compilation. It captures the source name (if any)
// for use in error messages.
//
// Expression is not safe for concurrent use.
type Expression struct {
	stx    syntax.SyntaxValue
	source string
}

// Source returns the source name associated with this expression.
// Returns the empty string if no source was specified at parse time.
func (p *Expression) Source() string {
	return p.source
}

// String returns a string representation of the expression.
func (p *Expression) String() string {
	return "#<expression>"
}

// Parse parses a single Scheme expression from code.
//
// Parse returns a CompilationError if the input is empty, malformed,
// or contains more than one expression.
func (p *Engine) Parse(ctx context.Context, code string) (*Expression, error) {
	return p.parse(ctx, code, "")
}

// ParseWithSource parses a single Scheme expression from code.
// The source parameter identifies where the code came from (e.g. a filename)
// and appears in error messages.
func (p *Engine) ParseWithSource(ctx context.Context, code string, source string) (*Expression, error) {
	return p.parse(ctx, code, source)
}

// ReadExpression reads a single complete expression from r.
//
// Unlike [Engine.Parse], ReadExpression does not require the reader to
// contain exactly one expression — it reads the first complete expression
// and stops. Trailing input in the reader is ignored (the reader position
// advances past the consumed expression).
//
// Use [IsIncompleteInput] to check whether a returned error indicates the
// input is a valid prefix of an expression that needs more input to complete.
// This is the intended pattern for REPL implementations:
//
//	expr, err := eng.ReadExpression(ctx, r)
//	if err != nil {
//	    if wile.IsIncompleteInput(err) {
//	        // prompt for more input
//	    }
//	    // real parse error
//	}
func (p *Engine) ReadExpression(ctx context.Context, r io.Reader) (*Expression, error) {
	rr, ok := r.(io.RuneReader)
	if !ok {
		rr = bufio.NewReader(r)
	}
	pr := parser.NewParser(p.env, true, rr)
	pr.SetMaxDepth(p.maxParseDepth)
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, wrapCompilationError("parse error", err)
	}
	return &Expression{stx: stx}, nil
}

// ReadExpressions parses every complete expression available in r, in order,
// reusing a single parser so the tokenizer's one-rune lookahead is preserved
// across forms (a fresh parser per form would drop the inter-form delimiter).
//
// On clean end-of-input it returns the parsed expressions and a nil error. If
// the input ends partway through an expression, it returns the complete
// expressions parsed so far together with an error satisfying IsIncompleteInput
// — the REPL uses that signal to keep accumulating lines. A genuine syntax
// error is returned wrapped, with the expressions parsed before it.
//
// Unlike ReadExpression (exactly one form) this is the multi-form read the REPL
// needs to evaluate every expression on a pasted or piped line rather than
// silently dropping all but the first.
func (p *Engine) ReadExpressions(ctx context.Context, r io.Reader) ([]*Expression, error) {
	rr, ok := r.(io.RuneReader)
	if !ok {
		rr = bufio.NewReader(r)
	}
	pr := parser.NewParser(p.env, true, rr)
	pr.SetMaxDepth(p.maxParseDepth)
	var q []*Expression
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			// Order matters: an expression left unbalanced at end-of-input
			// surfaces as a *wrapped* io.EOF, so it satisfies both
			// IsIncompleteInput and isEOF. Test incompleteness first or the
			// partial trailing form is silently dropped — the very bug this
			// method exists to fix. A bare io.EOF (clean boundary between
			// forms) is incomplete=false and ends the read.
			if IsIncompleteInput(err) {
				return q, wrapCompilationError("parse error", err)
			}
			if isEOF(err) {
				return q, nil
			}
			return q, wrapCompilationError("parse error", err)
		}
		q = append(q, &Expression{stx: stx})
	}
}

// MustParse is like Parse but panics on error.
func (p *Engine) MustParse(ctx context.Context, code string) *Expression {
	return p.mustParse(ctx, code, "")
}

// MustParseWithSource is like ParseWithSource but panics on error.
func (p *Engine) MustParseWithSource(ctx context.Context, code string, source string) *Expression {
	return p.mustParse(ctx, code, source)
}

func (p *Engine) mustParse(ctx context.Context, code string, source string) *Expression {
	expr, err := p.parse(ctx, code, source)
	if err != nil {
		panic(err)
	}
	return expr
}

func (p *Engine) parse(ctx context.Context, code string, source string) (q *Expression, err error) {
	// Containment boundary. A Go host reaches Wile through three verbs — parse,
	// compile, run — and only run was guarded. Parse and ParseWithSource both
	// delegate here, so this is the single parse-side boundary.
	//
	// Placed at the Engine, not in pkg/parser: a panic below this line is a Wile
	// bug, and containing it inside the parser would hide our own defects from our
	// own suite. Guarding here gives the embedder the documented contract while
	// `go test ./pkg/parser/...` stays as loud as it is today.
	//
	// No valid input reaches a panic here today; this is a backstop, not a fix.
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		q = nil
		err = wrapCompilationError("parse error", werr.RecoverAsError(r, werr.ErrInternal, "parse"))
	}()

	reader := strings.NewReader(code)
	pr := parser.NewParserWithFile(p.env, true, reader, source)
	pr.SetMaxDepth(p.maxParseDepth)

	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, wrapCompilationError("parse error", err)
	}

	// Reject trailing expressions — Parse is a single-expression API.
	_, trailing := pr.ReadSyntax(ctx)
	if trailing == nil || !isEOF(trailing) {
		return nil, wrapCompilationError(
			"trailing input after expression (use EvalMultiple for multiple expressions)", nil)
	}

	return &Expression{
		stx:    stx,
		source: source,
	}, nil
}
