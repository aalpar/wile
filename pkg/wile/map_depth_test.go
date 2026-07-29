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
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// mapDepthProbe maps a CAPTURING callback over a list far longer than the engine's
// call-depth limit. The capture is load-bearing, not decoration: a capture-safe
// callback lets tryInlineHOFCall substitute the inline-HOF template — whose loop is
// tail-recursive for its own reasons — so a non-capturing callback would succeed
// against a structurally-recursive map too, and the probe would discriminate
// nothing. Capturing forces the deoptimized path into map's real definition.
const mapDepthProbe = `(begin
  (define (build n)
    (let loop ((i 0) (acc '()))
      (if (= i n) acc (loop (+ i 1) (cons i acc)))))
  (define (capturing x) (call/cc (lambda (k) x)))
  (length (map capturing (build %d))))`

// mapDepthProbeMulti is the same probe through map's multi-list clause, which has
// its own loop and its own tail-pointer construction.
const mapDepthProbeMulti = `(begin
  (define (build n)
    (let loop ((i 0) (acc '()))
      (if (= i n) acc (loop (+ i 1) (cons i acc)))))
  (define (capturing x) (call/cc (lambda (k) x)))
  (length (map (lambda (a b) (capturing (+ a b))) (build %d) (build %d))))`

// TestMapCallDepthCeiling pins the reason bootstrap_procedures.scm defines map as
// accumulate-and-reverse rather than the structural recursion it is usually written
// as: the loop saves NO continuation per element, so map over a list an order of
// magnitude past maxCallDepth completes instead of raising ErrCallDepthExceeded.
//
// The control is negative rather than a second assertion: reverting map's body to
// (cons (f (car lst)) (loop (cdr lst))) fails every case here. Both clauses are
// covered because each has its own loop.
//
// The NoMutation case is not redundant. This shape needs no mutation primitive, so
// that dialect gets the same tail loop and the same guarantee — which is exactly
// what a set-cdr! tail-pointer implementation could NOT have delivered, since
// NoMutation removes set-cdr! and would have had to fall back to a ceilinged map.
// Pinning it here keeps that property from being traded away silently.
func TestMapCallDepthCeiling(t *testing.T) {
	const limit = 2000
	const elements = 20000

	tcs := []struct {
		name    string
		dialect wile.Dialect
		code    string
	}{
		{
			name: "default dialect, single list",
			code: fmt.Sprintf(mapDepthProbe, elements),
		},
		{
			name: "default dialect, multi list",
			code: fmt.Sprintf(mapDepthProbeMulti, elements, elements),
		},
		{
			name:    "no-mutation dialect, single list",
			dialect: wile.NoMutation,
			code:    fmt.Sprintf(mapDepthProbe, elements),
		},
		{
			name:    "no-mutation dialect, multi list",
			dialect: wile.NoMutation,
			code:    fmt.Sprintf(mapDepthProbeMulti, elements, elements),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			opts := []wile.EngineOption{wile.WithMaxCallDepth(limit)}
			if tc.dialect != nil {
				opts = append(opts, wile.WithDialect(tc.dialect))
			}
			eng, err := wile.NewEngine(ctx, opts...)
			c.Assert(err, qt.IsNil)

			_, err = eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil,
				qt.Commentf("map over %d elements must not consume call depth (limit %d)",
					elements, limit))
		})
	}
}
