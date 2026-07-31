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

package integration_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestBoxLiteralReachesTheVM covers the half of #& that the parser tests cannot:
// a box literal now travels through the expander, the compiler, and the VM for
// the first time. Before #& was readable a *values.Box could only be built at
// runtime by (box x), so no box ever appeared in constant position and no arm
// was written for one.
//
// It works by falling through to ExpandSelfEvaluating, which is the right answer
// — a box literal is self-evaluating — but it is an answer nothing states, so
// the #&sym row is the load-bearing one: it fails loudly if a box's content is
// ever mistaken for an expression to evaluate.
func TestBoxLiteralReachesTheVM(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{"unbox a quoted literal", `(unbox '#&5)`, "5"},
		{"unquoted literal is self-evaluating", `(box? #&5)`, "#t"},
		{"content is not evaluated", `(symbol->string (unbox #&sym))`, `"sym"`},
		{"content keeps its own prefixes", `(unbox #&#x1f)`, "31"},
		{"boxes nest", `(unbox (unbox #&#&5))`, "5"},
		{"list content", `(car (unbox #&(1 2)))`, "1"},
		{"a literal box is still mutable", `(let ((b #&1)) (set-box! b 2) (unbox b))`, "2"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)

			result, err := engine.Eval(ctx, engine.MustParse(ctx, tc.src))
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestBoxWriteReadRoundTripThroughPorts closes the loop #& exists to close, at
// the level a user sees it: whatever `write` emits for a box, `read` accepts.
// The cyclic row is the one that decides a design question — Racket reads
// #0=#&#0# and Chez rejects it — and Wile has to follow Racket, because Wile's
// own writer emits that form for a box reachable from itself.
func TestBoxWriteReadRoundTripThroughPorts(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{"atom", `(box 5)`, `"#&5"`},
		{"list", `(box (list 1 2))`, `"#&(1 2)"`},
		{"nested", `(box (box 5))`, `"#&#&5"`},
		{"cyclic", `(let ((b (box 1))) (set-box! b b) b)`, `"#0=#&#0#"`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.Console))
			c.Assert(err, qt.IsNil)

			// Write it, then read the written text back and write that. A form
			// the reader mangles produces different text the second time round.
			src := `(let* ((v ` + tc.src + `)
			                (render (lambda (x)
			                          (let ((out (open-output-string)))
			                            (write x out)
			                            (get-output-string out))))
			                (once (render v)))
			           (render (read (open-input-string once))))`
			result, err := engine.Eval(ctx, engine.MustParse(ctx, src))
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
