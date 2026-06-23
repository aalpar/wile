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

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestEngine_WriteLongFlatList_DoesNotCrash is the regression for the
// motivating bug: (write (make-list 10000000)) overflowed the host stack
// because the writer's shared-structure analysis recursed once per cdr-spine
// element. A flat list nests only one level, so length must not be charged as
// depth; the spine is now walked iteratively and writes complete. If that
// regressed, this test binary would die with a fatal stack overflow rather
// than fail. (1M here keeps the test fast; the reported case was 10M.)
func TestEngine_WriteLongFlatList_DoesNotCrash(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatal(err)
	}
	src := `(let ((p (open-output-string)))
	          (write (make-list 1000000 0) p)
	          (string-length (get-output-string p)))`
	_, err = engine.EvalMultiple(ctx, src)
	if err != nil {
		t.Fatalf("writing a long flat list should succeed, got: %v", err)
	}
}

// TestEngine_WriteDeeplyNested_Raises proves the other half: a value nested
// deeper than the writer's bound (built programmatically — the reader could
// never produce it, since it caps textual nesting at the same depth) raises a
// catchable error instead of overflowing the host. The depth guard trips at
// ~DefaultMaxWriteDepth Go frames, well within what the host tolerates (the
// parser recurses just as deep on bounded input).
func TestEngine_WriteDeeplyNested_Raises(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatal(err)
	}
	// Tail loop builds a list nested ~11000 deep (over the 10000 default),
	// then tries to write it. Source nesting stays shallow, so this exercises
	// the writer's bound, not the parser's.
	src := `(let loop ((n 0) (x 0))
	          (if (= n 11000)
	              (let ((p (open-output-string))) (write x p) 'unreached)
	              (loop (+ n 1) (list x))))`
	_, err = engine.EvalMultiple(ctx, src)
	if err == nil {
		t.Fatal("writing a value nested past the depth bound should raise, got nil")
	}
	// errors.Is, not a substring of "depth" — that would also match
	// ErrParseDepthExceeded/ErrCallDepthExceeded and fail to pin that the
	// writer's bound (not the parser's) tripped. The sentinel survives the
	// engine-level *RuntimeError wrap.
	if !errors.Is(err, werr.ErrWriteDepthExceeded) {
		t.Fatalf("expected ErrWriteDepthExceeded, got: %v", err)
	}
	t.Logf("got expected error: %v", err)
}
