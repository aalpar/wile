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

// Value regression for a frame-pool defect: a parameter converter ran with the
// CALLER's activation frame as mc.env, and the ownership transfer that assumed
// otherwise handed out a second pool token for it.

package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestParameterConverterDoesNotReleaseTheCallersFrame is the frame-pool half.
//
// Setting a parameter that HAS A CONVERTER runs the converter on the live chain
// through RunBodyUnderFrame, whose transferEnvOwnership assumed mc.env was the
// primitive's own argument frame. A parameter is not a foreign closure, so no
// argument frame exists and mc.env is the CALLER's activation frame — which the
// caller's own continuation already holds a pool token for. Both tokens were
// then spent, the frame was recycled into the converter's finalizer, and the
// caller read the converted value back out of the slot holding the parameter.
//
// The converter is what arms it: without one the set returns immediately and
// never reaches RunBodyUnderFrame. That is the discriminator, so the no-converter
// arm is a control rather than coverage.
func TestParameterConverterDoesNotReleaseTheCallersFrame(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			name: "converter, parameter passed in",
			code: `(define (g p) (p 5) (p))
			       (g (make-parameter 0 (lambda (x) (* x 2))))`,
			want: "10",
		},
		{
			name: "converter, parameter let-bound in the body",
			code: `(define (g) (let ((p (make-parameter 0 (lambda (x) (* x 2))))) (p 5) (p)))
			       (g)`,
			want: "10",
		},
		{
			name: "converter reached through apply",
			code: `(define (g) (let ((p (make-parameter 0 (lambda (x) (* x 2))))) (apply p '(5)) (p)))
			       (g)`,
			want: "10",
		},
		{
			name: "control: no converter",
			code: `(define (g p) (p 5) (p))
			       (g (make-parameter 0))`,
			want: "5",
		},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}
