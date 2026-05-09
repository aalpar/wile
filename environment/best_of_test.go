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

package environment

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestBestOf_ShouldRecord exercises the bestOf reducer directly,
// independent of EnvironmentFrame, so the contract is pinned even if
// the call sites in GetBinding / GetLocalIndex are restructured.
func TestBestOf_ShouldRecord(t *testing.T) {
	tcs := []struct {
		name       string
		ops        []recordOp
		wantHas    bool
		wantItem   string
		wantWeight int
		wantStopAt int // -1 means walk did not stop
	}{
		{
			name:       "empty walk",
			ops:        nil,
			wantHas:    false,
			wantStopAt: -1,
		},
		{
			name: "single weight=2 target=3",
			ops: []recordOp{
				{item: "a", weight: 2, target: 3},
			},
			wantHas:    true,
			wantItem:   "a",
			wantWeight: 2,
			wantStopAt: -1,
		},
		{
			name: "perfect match short-circuits walk",
			ops: []recordOp{
				{item: "a", weight: 1, target: 3},
				{item: "b", weight: 3, target: 3}, // perfect — must stop here
				{item: "c", weight: 4, target: 3}, // never visited (would pass walk-stops check)
			},
			wantHas:    true,
			wantItem:   "b",
			wantWeight: 3,
			wantStopAt: 1,
		},
		{
			name: "ties keep first-seen (innermost-wins under innermost-first walk)",
			ops: []recordOp{
				{item: "inner", weight: 2, target: 5},
				{item: "outer", weight: 2, target: 5}, // tie — first stays
			},
			wantHas:    true,
			wantItem:   "inner",
			wantWeight: 2,
			wantStopAt: -1,
		},
		{
			name: "later strict-greater overtakes earlier",
			ops: []recordOp{
				{item: "small", weight: 1, target: 5},
				{item: "big", weight: 4, target: 5},
				{item: "medium", weight: 3, target: 5}, // less than current best
			},
			wantHas:    true,
			wantItem:   "big",
			wantWeight: 4,
			wantStopAt: -1,
		},
		{
			name: "weight=0 candidate cannot trigger perfect-match even when target=0",
			ops: []recordOp{
				{item: "zero", weight: 0, target: 0},
			},
			wantHas:    true, // it IS recorded as the only candidate
			wantItem:   "zero",
			wantWeight: 0,
			wantStopAt: -1, // but does not stop the walk
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var best bestOf[string]
			stopAt := -1
			for i, op := range tc.ops {
				rec, done := best.shouldRecord(op.weight, op.target)
				if rec {
					best.record(op.item, op.weight)
				}
				if done {
					stopAt = i
					break
				}
			}
			qt.Assert(t, best.has, qt.Equals, tc.wantHas)
			if tc.wantHas {
				qt.Assert(t, best.item, qt.Equals, tc.wantItem)
				qt.Assert(t, best.scopeCount, qt.Equals, tc.wantWeight)
			}
			qt.Assert(t, stopAt, qt.Equals, tc.wantStopAt)
		})
	}
}

type recordOp struct {
	item   string
	weight int
	target int
}
