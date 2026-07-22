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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// An internal define-syntax inside a function-shorthand define body must be
// visible to later forms in the same body, exactly as it is inside a
// lambda/let/named-let body (R7RS §5.3). The shorthand
// (define (f) ...) desugars to (define f (lambda () ...)), so its body must
// receive the same expand-time treatment: the body scope plus internal
// define-syntax registration. It previously did not — the shorthand path
// expanded its body as a flat argument list, so internal macros were never
// registered and a use raised "no such local or global binding".
func TestInternalDefineSyntax_InShorthandDefineBody(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "simple internal define-syntax",
			src:  `(define (f) (define-syntax m (syntax-rules () ((_) 42))) (m)) (f)`,
			want: "42",
		},
		{
			name: "macro-generating macro via internal define-syntax",
			src: `(define (f)
			         (define-syntax gen
			           (syntax-rules () ((_ name) (define-syntax name (syntax-rules () ((_) 42))))))
			         (gen m)
			         (m))
			       (f)`,
			want: "42",
		},
		{
			name: "internal define-syntax expanding to a define",
			src:  `(define (f) (define-syntax gen (syntax-rules () ((_ x) (define x 7)))) (gen y) y) (f)`,
			want: "7",
		},
		{
			name: "top-level macro-generating macro used in a shorthand body",
			src: `(define-syntax gen
			         (syntax-rules () ((_ name) (define-syntax name (syntax-rules () ((_) 5))))))
			       (define (f) (gen m) (m))
			       (f)`,
			want: "5",
		},
		{
			name: "two internal define-syntax in one shorthand body",
			src:  `(define (f) (define-syntax a (syntax-rules () ((_) 1))) (define-syntax b (syntax-rules () ((_) 2))) (+ (a) (b))) (f)`,
			want: "3",
		},
		{
			name: "internal define-syntax uses a parameter",
			src:  `(define (f x) (define-syntax dbl (syntax-rules () ((_ v) (+ v v)))) (dbl x)) (f 21)`,
			want: "42",
		},
		{
			// Variadic formals: sig.SyntaxCdr() is a bare symbol, so the shared
			// procedure-body expander must handle a symbol (not a list) of formals.
			name: "variadic shorthand formals with internal define-syntax",
			src:  `(define (f . xs) (define-syntax n (syntax-rules () ((_) (length xs)))) (n)) (f 1 2 3)`,
			want: "3",
		},
		{
			// Improper formals (x . rest): the extractor must walk the improper tail.
			name: "improper shorthand formals with internal define-syntax",
			src:  `(define (f x . rest) (define-syntax r (syntax-rules () ((_) (cons x rest)))) (r)) (f 1 2 3)`,
			want: "(1 2 3)",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			result, err := eng.EvalProgram(ctx, tc.src, "<test>")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// A signature whose name is not a bare symbol (curried or empty) takes the
// fallback branch that preserves the prior flat expansion; it must produce a
// clean diagnostic, not a panic or a silently-wrong expansion. Curried define
// is unsupported in Wile.
func TestShorthandDefine_MalformedSignatureRejectedCleanly(t *testing.T) {
	ctx := context.Background()
	cases := []string{
		`(define ((f a) b) (+ a b))`, // curried
		`(define () 1)`,              // empty signature
	}
	for _, src := range cases {
		t.Run(src, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			_, err = eng.EvalProgram(ctx, src, "<test>")
			qt.Assert(t, err, qt.IsNotNil,
				qt.Commentf("malformed define signature must be rejected, not silently accepted"))
		})
	}
}

// Regression guard: the same internal-define-syntax bodies must keep working
// under the lambda, let, and named-let forms that already handled them, so the
// shorthand-define fix does not diverge from them.
func TestInternalDefineSyntax_LambdaLetParityUnbroken(t *testing.T) {
	ctx := context.Background()
	body := `(define-syntax m (syntax-rules () ((_) 42))) (m)`

	cases := map[string]string{
		"lambda":    `((lambda () ` + body + `))`,
		"let":       `(let () ` + body + `)`,
		"named-let": `(let loop () ` + body + `)`,
	}

	for name, src := range cases {
		t.Run(name, func(t *testing.T) {
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			result, err := eng.EvalProgram(ctx, src, "<test>")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, "42")
		})
	}
}
