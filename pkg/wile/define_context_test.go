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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestDefinitionInExpressionContextIsRefused pins R7RS §5.3: a definition
// belongs at top level, at the start of a body, or in a begin spliced into one
// of those. Anywhere else it is refused with a located ErrInvalidSyntax; Wile
// used to accept it as a void-valued expression, so (list (define x 1) x) was
// (#<void> 1). Racket ("define: not allowed in an expression context") and Chez
// ("invalid context for definition") refuse every row. want is the location of
// the refused form.
func TestDefinitionInExpressionContextIsRefused(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "call argument", code: `(list (define x 1) x)`, want: "includer.scm:1:7"},
		{name: "if arm", code: `(if #t (define y 2) 0)`, want: "includer.scm:1:8"},
		{name: "argument inside a body", code: `(let () (display (define z 3)) z)`, want: "includer.scm:1:18"},
		{name: "begin in expression position", code: `(display (begin (define w 4) w))`, want: "includer.scm:1:17"},
		{name: "define right-hand side", code: `(define v (define u 1))`, want: "includer.scm:1:11"},
		{name: "procedure body argument", code: `(define (f) (list (define x 1) x)) (f)`, want: "includer.scm:1:19"},
		{name: "when body", code: `(when #t (define q 5))`, want: "includer.scm:1:10"},
		{name: "do command", code: `(do ((i 0 (+ i 1))) ((= i 1)) (define d 1))`, want: "includer.scm:1:31"},
		{name: "define-values", code: `(list (define-values (a b) (values 1 2)))`, want: "includer.scm:1:7"},
		{name: "define-record-type", code: `(list (define-record-type p (mk) p?))`, want: "includer.scm:1:7"},
		{name: "define-syntax", code: `(list (define-syntax m (syntax-rules () ((_) 1))) 2)`, want: "includer.scm:1:7"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Check(t, errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, tc.want+": definition not allowed in an expression context", qt.Commentf("%v", err))
			var ce *wile.CompilationError
			qt.Assert(t, errors.As(err, &ce), qt.IsTrue, qt.Commentf("%T: %v", err, err))
			qt.Check(t, ce.Source, qt.Equals, tc.want)
			var deepest, se *compilation.SourcedError
			for e := err; errors.As(e, &se); e = se.Cause {
				deepest = se
			}
			qt.Assert(t, deepest != nil, qt.IsTrue, qt.Commentf("no SourcedError in %v", err))
			got := fmt.Sprintf("%s:%d:%d", deepest.Source.File, deepest.Source.Start.Line(), deepest.Source.Start.Column())
			qt.Check(t, got, qt.Equals, tc.want)
		})
	}
}

// TestDefinitionInDefinitionContextIsAccepted: the controls for the refusal.
// Each places a definition where R7RS allows one, including the bodies the
// stdlib's derived forms produce (parameterize's had been a bare begin, under
// with-continuation-mark or, with no bindings, alone). A user procedure named
// define is a call, not a definition, so its argument position is not checked.
func TestDefinitionInDefinitionContextIsAccepted(t *testing.T) {
	fsys := fstest.MapFS{
		"lf.scm":   &fstest.MapFile{Data: []byte(`(define lv 2)`)},
		"dlib.sld": &fstest.MapFile{Data: []byte(`(define-library (dlib) (import (scheme base)) (export dv) (begin (define dv 7)))`)},
	}
	tcs := []struct {
		name string
		code string
		want string
		opts []wile.EngineOption
	}{
		{name: "top-level begin", code: `(begin (define w 4) w)`, want: "4"},
		{name: "nested top-level begin", code: `(begin (begin (define a 1))) a`, want: "1"},
		{name: "body begin splice", code: `(let () (begin (define a 1) (define b 2)) (+ a b))`, want: "3"},
		{name: "top-level cond-expand", code: `(cond-expand (else (define ce 1))) ce`, want: "1"},
		{name: "body cond-expand", code: `(let () (cond-expand (else (define ce 1))) ce)`, want: "1"},
		{name: "eval of a define", code: `(eval '(define ev 1) (interaction-environment)) (eval 'ev (interaction-environment))`, want: "1"},
		{name: "load of a file of defines", code: `(load "lf.scm") (eval 'lv (interaction-environment))`, want: "2"},
		{name: "library body", code: `(import (dlib)) dv`, want: "7", opts: withStdlibResolver()},
		{name: "lambda body", code: `((lambda () (define x 1) x))`, want: "1"},
		{name: "named let body", code: `(let loop ((i 0)) (define n 3) (+ i n))`, want: "3"},
		{name: "case-lambda body", code: `((case-lambda ((x) (define c 3) (+ x c))) 1)`, want: "4"},
		{name: "let-values body", code: `(let-values (((a b) (values 1 2))) (define c 3) (+ a b c))`, want: "6"},
		{name: "guard body", code: `(guard (e (#t 0)) (define g 3) g)`, want: "3"},
		{name: "parameterize body", code: `(define p (make-parameter 1)) (parameterize ((p 2)) (define z 3) (+ z (p)))`, want: "5"},
		{name: "empty parameterize body", code: `(parameterize () (define z 3) z)`, want: "3"},
		// A parameterize body is local: z does not escape to top level, as it
		// did while the zero-binding expansion was a bare begin (racket and
		// petite: unbound).
		{name: "parameterize body is local", code: `(define z 'top) (parameterize () (define z 3) z) z`, want: "top"},
		{name: "define-values at top level", code: `(define-values (a b) (values 1 2)) (+ a b)`, want: "3"},
		{name: "user procedure named define", code: `(define (define x) (* x 2)) (list (define 4))`, want: "(8)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code, tc.opts...)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}
