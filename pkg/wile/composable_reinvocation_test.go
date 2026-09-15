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
	"context"
	"strings"
	"testing"
)

// A composable continuation re-invoked while an earlier invocation of it is still
// running must leave that invocation's frames intact. The first invocation runs
// the captured frames themselves; a segment of two or more frames still has its
// bottom frame on the live chain when the body re-invokes it, so a re-invocation
// that writes the bottom frame's parent cuts the rest of the program off.
//
// A one-frame segment cannot show it (its only frame is already in the registers),
// which is why "composable continuation re-invoked inside its own invocation" in
// let_reentry_freshness_test.go passed throughout.
//
// Every want is what Racket 9.2 CS prints for the same program.
var composableReinvocationCases = []struct {
	name string
	code string
	want string
}{
	{
		name: "re-invoked inside its own invocation, two-frame segment",
		code: `
(define tag (make-continuation-prompt-tag))
(define (drive)
  (define kc #f)
  (define count 0)
  (define (body v)
    (set! count (+ count 1))
    (if (= count 2) (kc 100) v))
  (define result
    (call-with-continuation-prompt
      (lambda ()
        (* 2 (body (call-with-composable-continuation (lambda (k) (set! kc k) 5) tag))))
      tag (lambda (v) v)))
  (list result (list 'a 'b (kc 1))))
(drive)`,
		want: "(10 (a b 400))",
	},
	{
		// The third invocation copies from the frames the first two ran through.
		name: "invoked again after a nested re-invocation returns",
		code: `
(define tag (make-continuation-prompt-tag))
(define (drive)
  (define kc #f)
  (define count 0)
  (define (body v)
    (set! count (+ count 1))
    (if (= count 2) (kc 100) v))
  (call-with-continuation-prompt
    (lambda ()
      (+ 1 (* 2 (body (call-with-composable-continuation (lambda (k) (set! kc k) 5) tag)))))
    tag (lambda (v) v))
  (let* ((first (kc 1))
         (second (kc 7)))
    (list 'x first second)))
(drive)`,
		want: "(x 403 15)",
	},
	{
		// A call/cc continuation is resumed through the same segment acquisition;
		// the inner prompt keeps the running invocation's frames below the boundary.
		name: "call/cc re-invoked under a prompt inside its own invocation",
		code: `
(define (drive)
  (define k #f)
  (define count 0)
  (define (prompt thunk)
    (call-with-continuation-prompt thunk (default-continuation-prompt-tag) (lambda args args)))
  (define (body v)
    (set! count (+ count 1))
    (if (= count 2) (prompt (lambda () (k 100))) v))
  (define result
    (prompt (lambda () (* 2 (body (call/cc (lambda (c) (set! k c) 5)))))))
  (list result (list 'a 'b (prompt (lambda () (k 1))))))
(drive)`,
		want: "(10 (a b 400))",
	},
}

func TestComposableContinuationReinvocationKeepsLiveSegment(t *testing.T) {
	ctx := context.Background()
	for _, tc := range composableReinvocationCases {
		t.Run(strings.ReplaceAll(tc.name, " ", "_"), func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			got, err := engine.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			if got.SchemeString() != tc.want {
				t.Errorf("= %s, want %s: a re-invocation cut the running "+
					"invocation's segment off the live chain", got.SchemeString(), tc.want)
			}
		})
	}
}
