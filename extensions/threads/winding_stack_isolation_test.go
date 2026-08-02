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

package threads_test

// Guards one hazard: a spawned thread must not share its parent's winding-stack
// BACKING ARRAY. CaptureSubContextParams used to hand over the bare slice
// header, which carries the parent's spare capacity. WindingStack.Pop retains
// capacity by design, so after any completed dynamic-wind the parent sits at
// len < cap and the child inherited a writable alias; both then pushed into the
// same slot from different goroutines.
//
// Run under -race. Without the fix this reports concurrent writes at
// WindingStack.Push from OperationPushWind.Apply on the thread's goroutine
// against the same address on the parent's. It is invisible to a plain run and
// to every other test in this package, because the collision needs a COMPLETED
// dynamic-wind before thread-start! within one top-level form — that is what
// leaves capacity behind to alias.

import (
	"context"
	"testing"

	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/wile"
)

// windingStackIsolationProgram seeds spare capacity with a completed
// dynamic-wind, then winds concurrently on the parent and two children.
const windingStackIsolationProgram = `
(begin
  (dynamic-wind (lambda () #f) (lambda () 1) (lambda () #f))
  (define (churn n)
    (let loop ((i 0))
      (if (= i n)
          'done
          (begin
            (dynamic-wind (lambda () #f) (lambda () i) (lambda () #f))
            (loop (+ i 1))))))
  (define ts
    (list (thread-start! (make-thread (lambda () (churn 400))))
          (thread-start! (make-thread (lambda () (churn 400))))))
  (churn 400)
  (for-each thread-join! ts)
  'ok)
`

func TestWindingStackNotSharedWithThread(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithExtension(extthreads.Extension))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	_, err = engine.EvalMultiple(ctx, windingStackIsolationProgram)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
}
