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

package values_test

import (
	"context"
	"errors"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// properList builds (0 1 2 ... n-1).
func properList(n int) *values.Pair {
	q := values.EmptyList
	for i := n - 1; i >= 0; i-- {
		q = values.NewCons(values.NewInteger(int64(i)), q)
	}
	return q.(*values.Pair)
}

// circularList builds an n-element list whose last cdr points back at the head.
func circularList(n int) *values.Pair {
	head := properList(n)
	last := head
	for {
		next, ok := last[1].(*values.Pair)
		if !ok {
			break
		}
		last = next
	}
	last.SetCdr(head)
	return head
}

// TestForEach_CircularListIsDetected asserts that walking a circular list
// terminates with ErrCircularList instead of spinning forever.
//
// Citation: machine_context.go:459. Pair.ForEach is *the* list walker —
// ForEachProperList, length, list-copy, append, reverse, and apply's argument
// spread all funnel through it — and it chased cdr pointers unconditionally.
func TestForEach_CircularListIsDetected(t *testing.T) {
	c := circularList(5)

	_, err := c.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, _ values.Value) error {
		return nil
	})

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrCircularList), qt.IsTrue)
}

// TestForEachProperList_CircularListReportsNotAList asserts the mapping at the
// proper-list boundary: R7RS callers already reject improper lists, and a circular
// list is improper, so they must see the error they already handle.
func TestForEachProperList_CircularListReportsNotAList(t *testing.T) {
	c := circularList(3)

	err := values.ForEachProperList(context.Background(), c, "apply",
		func(_ context.Context, _ int, _ bool, _ values.Value) error {
			return nil
		})

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue)
}

// TestForEach_LongProperListHonoursContext asserts that a legal, finite, very long
// list still respects the embedder's deadline. This is independent of cycle
// detection: a proper list of 10^9 elements is legal and must remain preemptible.
func TestForEach_LongProperListHonoursContext(t *testing.T) {
	p := properList(100000)
	ctx, cancel := context.WithCancel(context.Background())

	_, err := p.ForEach(ctx, func(_ context.Context, i int, _ bool, _ values.Value) error {
		if i == 5000 {
			cancel()
		}
		return nil
	})

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, context.Canceled), qt.IsTrue)
}

// TestForEach_ProperListsUnchanged pins the walk itself across the sizes where the
// teleport arithmetic could be off by one. Every element must be visited exactly
// once, in order, and the terminator must still come back as EmptyList.
func TestForEach_ProperListsUnchanged(t *testing.T) {
	for _, n := range []int{1, 2, 3, 4, 5, 7, 8, 9, 16, 17, 100} {
		p := properList(n)

		var seen []int64
		tail, err := p.ForEach(context.Background(),
			func(_ context.Context, _ int, _ bool, v values.Value) error {
				seen = append(seen, v.(*values.Integer).Value)
				return nil
			})

		qt.Assert(t, err, qt.IsNil, qt.Commentf("n=%d", n))
		qt.Assert(t, values.IsEmptyList(tail), qt.IsTrue, qt.Commentf("n=%d", n))
		qt.Assert(t, len(seen), qt.Equals, n, qt.Commentf("n=%d", n))
		for i := range n {
			qt.Assert(t, seen[i], qt.Equals, int64(i), qt.Commentf("n=%d i=%d", n, i))
		}
	}
}

// TestForEach_ImproperTailStillReturned pins that a non-cyclic improper list still
// reports its terminating cdr rather than being mistaken for a cycle.
func TestForEach_ImproperTailStillReturned(t *testing.T) {
	p := values.NewCons(values.NewInteger(1), values.NewInteger(2))

	tail, err := p.ForEach(context.Background(),
		func(_ context.Context, _ int, _ bool, _ values.Value) error {
			return nil
		})

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail.SchemeString(), qt.Equals, "2")
}

// TestForEach_HonoursContextCancellation pins that a ForEach walk stops on a
// cancelled context, for both implementations that accept a ctx.
//
// Pair.ForEach is the load-bearing one: it is *the* list walker, and its ctx was
// unread until a circular list showed that apply ignored cancellation entirely.
// SyntaxVector.ForEach has no caller yet and is not a Tuple, so nothing reaches it
// today — it is pinned here so the ctx in its signature stays honest, rather than
// waiting to hand the same bug to whoever wires it up.
//
// The walk must exceed contextCheckMask (1023) for the amortized poll to fire.
func TestForEach_HonoursContextCancellation(t *testing.T) {
	const n = 4096

	assertStops := func(t *testing.T, walk func(context.Context, values.ForEachFunc) (values.Value, error)) {
		t.Helper()
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()

		seen := 0
		_, err := walk(ctx, func(_ context.Context, _ int, _ bool, _ values.Value) error {
			seen++
			if seen == 1 {
				// Cancel on the first element. The walk may overshoot to the next
				// poll boundary, but must not run to completion.
				cancel()
			}
			return nil
		})

		qt.Assert(t, errors.Is(err, context.Canceled), qt.IsTrue,
			qt.Commentf("ForEach must surface the cancellation, got %v", err))
		qt.Assert(t, seen, qt.Not(qt.Equals), n,
			qt.Commentf("ForEach walked all %d elements despite cancellation", n))
	}

	t.Run("Pair", func(t *testing.T) {
		vs := make([]values.Value, n)
		for i := range vs {
			vs[i] = values.NewInteger(int64(i))
		}
		assertStops(t, values.List(vs...).ForEach)
	})

	t.Run("SyntaxVector", func(t *testing.T) {
		svs := make([]values.SyntaxValue, n)
		for i := range svs {
			svs[i] = syntax.NewSyntaxSymbol(fmt.Sprintf("s%d", i), nil)
		}
		assertStops(t, values.NewSyntaxVector(nil, svs...).ForEach)
	})
}
