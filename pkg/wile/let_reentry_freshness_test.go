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

	"github.com/aalpar/wile/pkg/machine"
)

// A `let` binds FRESH locations every time it runs (R7RS §4.2.2), including when
// a continuation re-runs it. A merged let's slot (merged_slots.go) belongs to the
// enclosing frame, not to one execution of the let, so a re-run wrote the slot an
// earlier pass's continuation still read. Merging is therefore refused wherever a
// continuation can be captured while the frame it would merge into is live.
//
// Every want is what Chez 10.4.1 and Racket 9.2 CS print for the same program.
var letReentryFreshnessCases = []struct {
	name string
	code string
	want string
}{
	{
		name: "call/cc in a let init inside a procedure",
		code: `
(define (drive)
  (define k1 #f) (define kb #f) (define log '())
  (define (run)
    (let ((x (call/cc (lambda (k) (set! k1 k) 0))))
      (call/cc (lambda (k) (if (= x 0) (set! kb k))))
      (set! log (cons x log))
      x))
  (let ((r (run)))
    (cond ((= r 0) (k1 1))
          ((= (length log) 2) (kb #f))))
  (reverse log))
(drive)`,
		want: "(0 1 0 1)",
	},
	{
		// The init captures nothing: the capture precedes the let, so a verdict
		// over the let's own inits alone would miss it.
		name: "capture earlier in the procedure body",
		code: `
(define (drive)
  (define k0 #f) (define kb #f) (define counter 0) (define log '())
  (define (next!) (set! counter (+ counter 1)) (- counter 1))
  (define (run)
    (call/cc (lambda (k) (if (not k0) (set! k0 k))))
    (let ((x (next!)))
      (call/cc (lambda (k) (if (= x 0) (set! kb k))))
      (set! log (cons x log))
      x))
  (let ((r (run)))
    (cond ((= r 0) (k0 #f))
          ((= (length log) 2) (kb #f))))
  (reverse log))
(drive)`,
		want: "(0 1 0 2)",
	},
	{
		// No capture operator is spelled in run's body: the capture arrives
		// through a parameter.
		name: "capture through an unknown callee",
		code: `
(define (drive)
  (define k1 #f) (define kb #f) (define log '())
  (define (run f)
    (let ((x (f (lambda (k) (set! k1 k) 0))))
      (f (lambda (k) (if (= x 0) (set! kb k))))
      (set! log (cons x log))
      x))
  (let ((r (run call/cc)))
    (cond ((= r 0) (k1 1))
          ((= (length log) 2) (kb #f))))
  (reverse log))
(drive)`,
		want: "(0 1 0 1)",
	},
	{
		name: "composable continuation re-invoked inside its own invocation",
		code: `
(define tag (make-continuation-prompt-tag))
(define (drive)
  (define kc #f)
  (define hook (lambda () 'none))
  (define out '())
  (define (body)
    (let ((v (call-with-composable-continuation (lambda (k) (set! kc k) 0) tag)))
      (set! out (cons v out))
      (hook)
      v))
  (call-with-continuation-prompt body tag (lambda args args))
  (set! hook (lambda () (set! hook (lambda () 'none)) (kc 2)))
  (set! out (cons (kc 1) out))
  (reverse out))
(drive)`,
		want: "(0 1 2 1)",
	},
	{
		// The frame merged into is a pushing top-level let, not a procedure's.
		name: "nested let under a pushing top-level let",
		code: `
(let ((log '()) (k1 #f) (kb #f))
  (let ((r (let ((x (call/cc (lambda (k) (set! k1 k) 0))))
             (call/cc (lambda (k) (if (= x 0) (set! kb k))))
             (set! log (cons x log))
             x)))
    (cond ((= r 0) (k1 1))
          ((= (length log) 2) (kb #f))))
  (reverse log))`,
		want: "(0 1 0 1)",
	},
}

func TestLetIsFreshWhenAContinuationReentersIt(t *testing.T) {
	ctx := context.Background()
	for _, tc := range letReentryFreshnessCases {
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
				t.Errorf("= %s, want %s: a re-run let wrote a slot an earlier "+
					"pass still reads", got.SchemeString(), tc.want)
			}
		})
	}
}

// TestLetMergeFollowsCaptureSafety is the census arm. Refusing every merge
// passes the value table above, so this pins that a capture-free region still
// merges while a capturing one pushes.
func TestLetMergeFollowsCaptureSafety(t *testing.T) {
	ctx := context.Background()
	tcs := []struct {
		name       string
		code       string
		wantPushes int
	}{
		{
			name:       "capture-free procedure merges",
			code:       `(lambda (n) (let ((a (+ n 1))) (let ((b (* a 2))) (+ a b))))`,
			wantPushes: 0,
		},
		{
			name:       "call/cc in the procedure body pushes",
			code:       `(lambda (n) (let ((a (call/cc (lambda (k) n)))) (+ a 1)))`,
			wantPushes: 1,
		},
		{
			name:       "unknown callee in the procedure body pushes",
			code:       `(lambda (f n) (f n) (let ((a (+ n 1))) a))`,
			wantPushes: 1,
		},
		{
			name:       "capture-free top-level let merges its nested lets",
			code:       `(let ((a 1)) (let ((b (+ a 1))) (let ((c (+ b 1))) (+ a b c))))`,
			wantPushes: 1,
		},
		{
			// A precision loss, pinned so it is visible: an operator that is a
			// lambda expression is an unknown callee to the verdict, though its
			// body is walked in place.
			name:       "lambda applied in place refuses the merge",
			code:       `(let ((a 1)) (let ((b 2)) ((lambda () (+ a b)))))`,
			wantPushes: 2,
		},
		{
			name:       "capturing top-level let pushes its nested lets",
			code:       `(let ((a 1)) (let ((b (call/cc (lambda (k) a)))) (+ a b)))`,
			wantPushes: 2,
		},
	}
	for _, tc := range tcs {
		t.Run(strings.ReplaceAll(tc.name, " ", "_"), func(t *testing.T) {
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatalf("new engine: %v", err)
			}
			expr, err := engine.Parse(ctx, tc.code)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			compiled, err := engine.Compile(ctx, expr)
			if err != nil {
				t.Fatalf("compile: %v", err)
			}
			got := countOpInTemplateTree(compiled.template, machine.OpPushEnv, map[*machine.NativeTemplate]bool{})
			if got != tc.wantPushes {
				t.Errorf("OpPushEnv sites = %d, want %d", got, tc.wantPushes)
			}
		})
	}
}
