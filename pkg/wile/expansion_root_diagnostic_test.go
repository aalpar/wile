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
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestClosureTransformerFailureIsNotLabelledSyntaxRules: every closure
// transformer (a lambda, a syntax-case or a Scheme-layer ER transformer) is
// invoked through one wrap, which named syntax-rules whatever the transformer
// was.
func TestClosureTransformerFailureIsNotLabelledSyntaxRules(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, src string
	}{
		{"lambda transformer", `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (car 5))) (m)`},
		{"syntax-case transformer", `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (syntax-case s () ((_) (car 5))))) (m)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, "transformer invocation failed")
			qt.Assert(t, err.Error(), qt.Not(qt.Contains), "syntax-rules")
		})
	}
}

// TestGuardMissReportsTheRaiseSite: a guard whose clauses all miss re-raises
// the condition from inside the guard, and so does the foreign-call bridge for a
// raise out of a library body or a transformer, so the uncaught carrier's own
// source is the guard's (or the import's, or the eval's). A *NativeError keeps
// the location of its first raise, which is what is reported. A condition of
// another type has no location of its own and still reports the latest raise.
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
		})
	}
}

// TestRaiseContinuableOutOfAnExpansionRootNamesItself: a transformer's
// raise-continuable reaches the caller's handlers after the transformer's root
// has ended, so it cannot be resumed, and a handler that returns is an error. The
// error names the raise the program wrote, and where it was, rather than "a
// non-continuable exception" at the eval.
func TestRaiseContinuableOutOfAnExpansionRootNamesItself(t *testing.T) {
	t.Parallel()

	_, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
		"(import (for-syntax (scheme base)))\n"+
			"(define-syntax m (lambda (s) (raise-continuable 'boom)))\n"+
			"(with-exception-handler (lambda (e) 42) (lambda () (eval '(m) (interaction-environment))))")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNonContinuableException), qt.IsTrue, qt.Commentf("%v", err))
	qt.Assert(t, err.Error(), qt.Contains, "returned from raise-continuable at :2:30")
	qt.Assert(t, err.Error(), qt.Not(qt.Contains), "non-continuable exception")
}
