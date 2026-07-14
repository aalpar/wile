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

// TestDynamicWindIsAFirstClassProcedure closes reviews/2026-07-13 specialforms.go:176.
//
// R7RS §6.10 specifies dynamic-wind as a PROCEDURE. Wile compiled it as a special
// form only, and the review's CORRECTION is the sharp part: in non-operator position
// it was not (procedure? dynamic-wind) => #f, it was UNBOUND — a compile error, "no
// such binding". So (apply dynamic-wind …), passing it to a higher-order procedure,
// or merely asking procedure? about it all failed to compile.
//
// The fix is a bootstrap definition, and it works because form dispatch precedes
// symbol resolution: a special form is shadowed by a LOCAL binding, never by a
// top-level define. So the procedure's own body compiles (dynamic-wind before thunk
// after) to the wind opcodes rather than calling back into itself, operator position
// keeps the fast path, and value position gets a procedure.
//
// The last case is the one that matters. A first-class dynamic-wind that did not
// unwind on a continuation escape would satisfy procedure? and still be worthless.
func TestDynamicWindIsAFirstClassProcedure(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			// Was: compile error "no such binding".
			name: "procedure? in value position",
			src:  `(procedure? dynamic-wind)`,
			want: "#t",
		},
		{
			// Was: compile error.
			name: "apply",
			src: `(let ((log '()))
			         (apply dynamic-wind
			                (list (lambda () (set! log (cons 'in log)))
			                      (lambda () (set! log (cons 'body log)))
			                      (lambda () (set! log (cons 'out log)))))
			         (reverse log))`,
			want: "(in body out)",
		},
		{
			// Passed to a higher-order procedure — the point of being first class.
			name: "passed as an argument",
			src: `(let ((log '()))
			         ((lambda (f)
			            (f (lambda () (set! log (cons 'in log)))
			               (lambda () (set! log (cons 'body log)))
			               (lambda () (set! log (cons 'out log)))))
			          dynamic-wind)
			         (reverse log))`,
			want: "(in body out)",
		},
		{
			// The operator-position form must be untouched — it is the fast path, and
			// the bootstrap define must not have shadowed it into infinite recursion.
			name: "operator position still uses the form",
			src: `(let ((log '()))
			         (dynamic-wind (lambda () (set! log (cons 'in log)))
			                       (lambda () (set! log (cons 'body log)))
			                       (lambda () (set! log (cons 'out log))))
			         (reverse log))`,
			want: "(in body out)",
		},
		{
			// The assertion that makes the rest worth anything: called as a VALUE, the
			// after-thunk must still run when a continuation escapes the extent.
			name: "escaping continuation still runs after, via the procedure",
			src: `(let ((log '()))
			         (call/cc
			           (lambda (k)
			             ((lambda (f)
			                (f (lambda () (set! log (cons 'in log)))
			                   (lambda () (k 'escaped))
			                   (lambda () (set! log (cons 'out log)))))
			              dynamic-wind)))
			         (reverse log))`,
			want: "(in out)",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()

			v, err := eng.EvalMultiple(ctx, "(begin "+tc.src+")")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}
