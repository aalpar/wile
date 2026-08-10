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

// TestDynamicWindOperandErrorIsCatchable closes Wave 4 item 3 (review §2.2.24).
//
// The defect lives on ONE arm, not three. `dynamic-wind` checks its `after`
// operand in OperationPushWind.Apply, which the OpComplex dispatch arm returns
// unbridged, so `(dynamic-wind (lambda () 1) (lambda () 1) 2)` escaped `guard`
// with "runtime error: dynamic-wind: after must be a procedure" and exit 1 —
// a host error for what R7RS §6.10 makes an argument-domain fault.
//
// The `before` and `thunk` positions never had the defect: compileDynamicWind
// applies them through OpApply, which already bridges, so a non-procedure there
// was always catchable. Those two rows are regression pins, not fixes — they
// are here so a later change to the operand order cannot quietly re-open the
// hole on an arm that is currently closed for a different reason.
func TestDynamicWindOperandErrorIsCatchable(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			// THE fix. Was: escapes guard, exit 1.
			name: "after/non-procedure caught by guard",
			src:  `(guard (e (#t "caught")) (dynamic-wind (lambda () 1) (lambda () 1) 2))`,
			want: `"caught"`,
		},
		{
			// The same arm through the other catch mechanism. A handler that
			// returns from a non-continuable raise still reports as a condition,
			// so the assertion is that the handler RAN, not what it returned.
			name: "after/non-procedure reaches with-exception-handler",
			src: `(guard (outer (#t "handler-ran"))
			         (with-exception-handler
			           (lambda (e) (raise 'reraised))
			           (lambda () (dynamic-wind (lambda () 1) (lambda () 1) 2))))`,
			want: `"handler-ran"`,
		},
		{
			// Regression pin: already caught before the fix, via OpApply.
			name: "before/non-procedure caught by guard",
			src:  `(guard (e (#t "caught")) (dynamic-wind 42 (lambda () 2) (lambda () 3)))`,
			want: `"caught"`,
		},
		{
			// Regression pin: already caught before the fix, via OpApply.
			name: "thunk/non-procedure caught by guard",
			src:  `(guard (e (#t "caught")) (dynamic-wind (lambda () 1) 42 (lambda () 3)))`,
			want: `"caught"`,
		},
		{
			// The `after` thunk must still run on the happy path — the bridge
			// must not have been bought by skipping the winding frame.
			name: "after/procedure still winds",
			src: `(let ((log '()))
			         (dynamic-wind (lambda () (set! log (cons 'in log)))
			                       (lambda () (set! log (cons 'body log)))
			                       (lambda () (set! log (cons 'out log))))
			         (reverse log))`,
			want: "(in body out)",
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
