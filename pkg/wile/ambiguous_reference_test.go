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

package wile_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/werr"
)

// ambiguousDefM is Flatt's ambiguous-binding program (sets of scopes, 2016),
// with the inner macro's reference left as %s. def-m introduces x under its
// intro scope {a}; (m) defines given-x, which is the user's x, under m's intro
// scope {b}; the reference in m's template carries {a, b}. Both binders are
// subsets of it, neither of the other, and they have one scope each: no binding
// is maximal. Racket reports "x: identifier's binding is ambiguous" at the
// reference; Chez answers 1 (R6RS, not this model).
const ambiguousDefM = `(define-syntax def-m
  (syntax-rules ()
    ((_ m given-x)
     (begin
       (define x 1)
       (define-syntax m
         (syntax-rules ()
           ((_) (begin (define given-x 2) %s))))))))
`

// ambiguousTop and ambiguousLocal place ref in m's template, with def-m and the
// use at top level and inside a procedure body. (m) is on line 10 and 11.
func ambiguousTop(ref string) string {
	return fmt.Sprintf(ambiguousDefM, ref) + "(def-m m x)\n(m)"
}

func ambiguousLocal(ref string) string {
	return "(define (f)\n" + fmt.Sprintf(ambiguousDefM, ref) + "(def-m m x)\n(m))\n(f)"
}

// ambiguousDefMNamed is ambiguousDefM with the tied name spelled name, so the
// tie can be placed at a form head (a binder named if, lambda, set!, ...). The
// line layout is ambiguousDefM's, so (m) stays on line 10 or 11.
const ambiguousDefMNamed = `(define-syntax def-m
  (syntax-rules ()
    ((_ m given-x)
     (begin
       (define %[1]s (lambda args 1))
       (define-syntax m
         (syntax-rules ()
           ((_) (begin (define given-x (lambda args 2)) %[2]s))))))))
`

func tiedHeadTop(name, ref string) string {
	return fmt.Sprintf(ambiguousDefMNamed, name, ref) + "(def-m m " + name + ")\n(m)"
}

func tiedHeadLocal(name, ref string) string {
	return "(define (f)\n" + fmt.Sprintf(ambiguousDefMNamed, name, ref) + "(def-m m " + name + ")\n(m))\n(f)"
}

// TestAmbiguousReferenceIsALocatedCompilationError pins B45 (X21): a reference
// whose scope set ties among incomparable binders is refused with
// ErrAmbiguousBinding located at the identifier. It used to panic in the
// environment's resolver and come back from the engine's recover as ErrInternal
// with no location. The reference is a template identifier, so its location is
// the (m) use that introduced it, as an unbound template identifier's is.
//
// The rows are the positions whose first resolver call is not CompileSymbol's:
// set!, the head of a call (expander macro lookup, validator form head, arity
// check, HOF inlining), a callback argument (capture-safety), a lambda's free
// variable (closure capture), and a let-syntax template identifier
// (definition-site pin). Each takes the conservative answer on a tie, so
// CompileSymbol's located refusal is reported.
//
// The "tied head" rows spell the tied name as a special form. A tie at a head is
// its own answer, never a miss: read as a miss, the spelling decided, and the
// form compiled as the core form ((if #f 'then 'else) printed else). Racket
// refuses each as ambiguous.
func TestAmbiguousReferenceIsALocatedCompilationError(t *testing.T) {
	const top, local = "includer.scm:10:1: ", "includer.scm:11:1: "
	type row struct {
		name string
		code string
		want string
	}
	tcs := []row{
		{name: "global reference", code: ambiguousTop("(display x)"), want: top},
		{name: "local reference", code: ambiguousLocal("x"), want: local},
		{name: "set! target", code: ambiguousTop("(set! x 3)"), want: top},
		{name: "local set! target", code: ambiguousLocal("(set! x 3)"), want: local},
		{name: "global operator", code: ambiguousTop("(x 5)"), want: top},
		{name: "local operator", code: ambiguousLocal("(x)"), want: local},
		{name: "operator in a lambda", code: ambiguousTop("(lambda () (x))"), want: top},
		{name: "callback argument", code: ambiguousTop("(map x '(1))"), want: top},
		{name: "local callback argument", code: ambiguousLocal("(for-each x '(1))"), want: local},
		{name: "captured local", code: ambiguousLocal("(lambda () x)"), want: local},
		{name: "let-syntax template", code: ambiguousTop("(let-syntax ((k (syntax-rules () ((_) x)))) (k))"), want: top},
	}
	heads := []struct{ name, ref string }{
		{"if", "(if #f 'then 'else)"},
		{"lambda", "((lambda (y) y) 7)"},
		{"quote", "(quote q)"},
		{"set!", "(let ((z 1)) (set! z 5) z)"},
		{"let-syntax", "(let-syntax () 9)"},
		{"when", "(when #t 3)"},
		{"begin", "(begin 4)"},
	}
	for _, h := range heads {
		tcs = append(tcs, row{name: "local tied head " + h.name, code: tiedHeadLocal(h.name, h.ref), want: local})
		// No top-level begin row: m's template splices with a begin of its own,
		// and at top level that head reaches def-m's (define begin ...), whose
		// define has already expanded, so it is a call and the tie inside it is
		// never a head (X25). Racket reads it the same way and refuses the
		// spliced define instead: "define: not allowed in an expression context".
		if h.name == "begin" {
			continue
		}
		tcs = append(tcs, row{name: "tied head " + h.name, code: tiedHeadTop(h.name, h.ref), want: top})
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Check(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue, qt.Commentf("%v", err))
			qt.Check(t, errors.Is(err, werr.ErrInternal), qt.IsFalse, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, "resolves ambiguously")
			qt.Check(t, strings.Count(err.Error(), tc.want), qt.Equals, 1, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, "expanded from 'm' at "+strings.TrimSuffix(tc.want, ": "))
			var deepest, se *compilation.SourcedError
			for e := err; errors.As(e, &se); e = se.Cause {
				deepest = se
			}
			qt.Assert(t, deepest != nil, qt.IsTrue, qt.Commentf("no SourcedError in %v", err))
			got := fmt.Sprintf("%s:%d:%d: ", deepest.Source.File, deepest.Source.Start.Line(), deepest.Source.Start.Column())
			qt.Check(t, got, qt.Equals, tc.want)
		})
	}
}

// TestQuoteSyntaxOfAnAmbiguousIdentifierIsSyntax pins the other answer a tie
// gets (X21): quote-syntax and a syntax template reference no binding, so a tie
// is no error and no pin. Racket's (quote-syntax x) returns #<syntax x> for the
// same program. Before, both panicked as ErrInternal.
func TestQuoteSyntaxOfAnAmbiguousIdentifierIsSyntax(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{name: "quote-syntax", code: ambiguousTop("(quote-syntax x)")},
		{name: "local quote-syntax", code: ambiguousLocal("(quote-syntax x)")},
		{name: "syntax template", code: ambiguousLocal("(syntax-case 1 () (_ (syntax x)))")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "#'x")
		})
	}
}
