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
	"context"
	"errors"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestClosureTransformerFailureIsNotLabelledSyntaxRules: every closure
// transformer (a lambda, a syntax-case, and with the Scheme-layer forms an ER or
// a syntax-rules transformer too) is invoked through one wrap, which named
// syntax-rules whatever the transformer was. A syntax-rules transformer's own
// message may still name syntax-rules; the wrap does not.
func TestClosureTransformerFailureIsNotLabelledSyntaxRules(t *testing.T) {
	t.Parallel()

	const (
		lambdaSrc     = `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (car 5))) (m)`
		syntaxCaseSrc = `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (syntax-case s () ((_) (car 5))))) (m)`
	)
	for _, tc := range []struct {
		name, src string
		scheme    bool
	}{
		{"lambda transformer", lambdaSrc, false},
		{"syntax-case transformer", syntaxCaseSrc, false},
		{"scheme-layer lambda transformer", lambdaSrc, true},
		{"scheme-layer syntax-case transformer", syntaxCaseSrc, true},
		{"scheme-layer er transformer", `(import (for-syntax (scheme base))) (define-syntax m (er-macro-transformer (lambda (f r c) (car 5)))) (m)`, true},
		{"scheme-layer syntax-rules transformer", `(define-syntax m (syntax-rules () ((_ x) x))) (m)`, true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			opts := []wile.EngineOption{
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			}
			if tc.scheme {
				opts = append(opts, wile.WithSchemeSyntaxForms())
			}
			eng, err := wile.NewEngine(context.Background(), opts...)
			qt.Assert(t, err, qt.IsNil)
			_, err = eng.EvalMultiple(context.Background(), tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, "expand: transformer invocation failed")
			qt.Assert(t, err.Error(), qt.Not(qt.Contains), "syntax-rules transformer invocation failed")
		})
	}
}

// TestGuardMissReportsTheRaiseSite: a guard whose clauses all miss re-raises
// the condition from inside the guard, a handler may raise it again, a program
// may keep it and raise it later, and the foreign-call bridge re-raises a library
// body's or a transformer's escape at the import or eval; each time the uncaught
// carrier was built at the latest raise. An error object reports its first raise,
// its location and its stack trace together: the trace's top frame is where the
// header points. A condition of another type has no first raise of its own on
// record and reports the latest one.
func TestGuardMissReportsTheRaiseSite(t *testing.T) {
	t.Parallel()

	fsys := fstest.MapFS{
		"probe/bad.sld": &fstest.MapFile{
			Data: []byte("(define-library (probe bad) (import (scheme base)) (export x)\n  (begin (define x 1)\n         (car 5)))"),
		},
	}
	for _, tc := range []struct {
		name, src, want string
	}{
		{"a guard around a call", "(define (f) (car 5))\n(guard (e ((string? e) 'no))\n  (f))", ":1:13"},
		{"a handler that raises it again", "(define (f) (car 5))\n(with-exception-handler\n  (lambda (e) (raise e))\n  (lambda () (f)))", ":1:13"},
		{"an error object kept and raised later", "(define (f) (car 5))\n(define saved (guard (e (#t e)) (f)))\n(raise saved)", ":1:13"},
		{"a guard around an import whose body raised", "(guard (e ((string? e) 'no)) (environment '(probe bad)))", "probe/bad.sld:3:10"},
		{"a guard around an eval whose transformer raised",
			"(import (for-syntax (scheme base)))\n(define-syntax m (lambda (s) (car 5)))\n(guard (e ((string? e) 'no)) (eval '(m) (interaction-environment)))",
			":2:30"},
		{"a condition with no location of its own", "(define (f) (raise 'boom))\n(guard (e ((string? e) 'no))\n  (f))", ":2:1"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithSourceFS(fsys),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			_, err = eng.EvalMultiple(context.Background(), tc.src)
			var re *wile.RuntimeError
			qt.Assert(t, errors.As(err, &re), qt.IsTrue, qt.Commentf("%v", err))
			qt.Assert(t, re.Source, qt.Equals, tc.want)
			lines := strings.Split(re.StackTrace, "\n")
			qt.Assert(t, len(lines) > 1, qt.IsTrue, qt.Commentf("%q", re.StackTrace))
			qt.Assert(t, lines[1], qt.Contains, "("+tc.want+")", qt.Commentf("%q", re.StackTrace))
		})
	}
}

// TestRaiseContinuableOutOfAnExpansionRootNamesItself: a transformer's
// raise-continuable reaches the caller's handlers while the transformer's root is
// live, so a handler that returns resumes it, as in petite ((eval '(m)) is 42). A
// later importer of a library whose body's raise-continuable was uncaught gets
// that raise after the body has ended, so it cannot be resumed, and a handler
// that returns is an error. The error names the raise the program wrote, and
// where it was, rather than "a non-continuable exception" at the import.
func TestRaiseContinuableOutOfAnExpansionRootNamesItself(t *testing.T) {
	t.Parallel()

	v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
		"(import (for-syntax (scheme base)))\n"+
			"(define-syntax m (lambda (s) (datum->syntax s (raise-continuable 'boom))))\n"+
			"(with-exception-handler (lambda (e) 42) (lambda () (eval '(m) (interaction-environment))))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "42")

	eng := newLibraryBodyProbeEngine(t, `(raise-continuable 'boom)`)
	_, err = eng.EvalMultiple(context.Background(), `(import (probe box)) (environment '(probe user))`)
	qt.Assert(t, err, qt.IsNotNil)
	_, err = eng.EvalMultiple(context.Background(),
		`(with-exception-handler (lambda (e) 42) (lambda () (environment '(probe user))))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNonContinuableException), qt.IsTrue, qt.Commentf("%v", err))
	qt.Assert(t, err.Error(), qt.Contains, "returned from raise-continuable at probe/user.sld:5:10")
	qt.Assert(t, err.Error(), qt.Not(qt.Contains), "non-continuable exception")
}

// TestGuardMissInAnExpansionRootKeepsTheRaisesContinuability: a guard whose
// clauses miss re-raises with raise-continuable (R7RS 4.2.7), in the dynamic
// environment of the original raise, where the caller's handlers are below the
// transformer's own. For a raise the re-raise is the program's raise again,
// non-continuable, so a handler around the eval that returns is told it returned
// from a non-continuable exception, not from a raise-continuable the program did
// not write. For a raise-continuable it resumes the transformer with the
// handler's value, as in petite.
func TestGuardMissInAnExpansionRootKeepsTheRaisesContinuability(t *testing.T) {
	t.Parallel()

	const prelude = "(import (for-syntax (scheme base)))\n" +
		"(define-syntax m (lambda (s) (datum->syntax s (guard (e ((string? e) 'no)) (%s 'boom)))))\n" +
		"(with-exception-handler (lambda (e) 42) (lambda () (eval '(m) (interaction-environment))))"
	_, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), strings.Replace(prelude, "%s", "raise", 1))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNonContinuableException), qt.IsTrue, qt.Commentf("%v", err))
	qt.Assert(t, err.Error(), qt.Contains, "exception handler returned from non-continuable exception")

	v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), strings.Replace(prelude, "%s", "raise-continuable", 1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "42")
}
