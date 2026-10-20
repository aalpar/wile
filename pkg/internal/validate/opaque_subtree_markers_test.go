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

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// quasiquoteHeadDepthBefore is quasiHeadDepth as it stood before the marker set
// became a parameter, kept verbatim as an oracle. The quasiquote side of the
// scan decides which names lose their Stable stamp, and a change there fails
// toward deopt, which no value assertion sees. TestQuasiHeadDepthQuasiquoteSideIsUnchanged
// compares the new function against this one over the whole input space.
func quasiquoteHeadDepthBefore(key string, quasi int) (argDepth int, barrier bool) {
	switch key {
	case "quasiquote":
		return quasi + 1, false
	case "unquote", "unquote-splicing":
		if quasi == 0 {
			return quasi, false
		}
		return quasi - 1, false
	case "quote":
		if quasi == 0 {
			return quasi, true
		}
		return quasi, false
	}
	return quasi, false
}

func TestQuasiHeadDepthQuasiquoteSideIsUnchanged(t *testing.T) {
	c := qt.New(t)
	keys := []string{"quasiquote", "unquote", "unquote-splicing", "quote", "list", "syntax", ""}
	for quasi := 0; quasi <= 4; quasi++ {
		for _, key := range keys {
			wantDepth, wantBarrier := quasiquoteHeadDepthBefore(key, quasi)
			gotDepth, gotMarkers, gotBarrier := quasiHeadDepth(key, quasi, quasiquoteMarkers)
			c.Assert(gotDepth, qt.Equals, wantDepth,
				qt.Commentf("key %q depth %d", key, quasi))
			c.Assert(gotBarrier, qt.Equals, wantBarrier,
				qt.Commentf("key %q depth %d", key, quasi))
			c.Assert(gotMarkers, qt.Equals, quasiquoteMarkers,
				qt.Commentf("key %q depth %d: a quasiquote walk must stay in its dialect", key, quasi))
		}
	}
}

func TestQuasiHeadDepthQuasisyntaxMarkers(t *testing.T) {
	tcs := []struct {
		name        string
		key         string
		quasi       int
		in          quasiMarkers
		wantDepth   int
		wantMarkers quasiMarkers
		wantBarrier bool
	}{
		{"code: quasisyntax opens a quasisyntax template", "quasisyntax", 0, quasiquoteMarkers, 1, quasisyntaxMarkers, false},
		{"code: quasiquote opens a quasiquote template", "quasiquote", 0, quasisyntaxMarkers, 1, quasiquoteMarkers, false},
		{"code: quote is a barrier", "quote", 0, quasisyntaxMarkers, 0, quasisyntaxMarkers, true},
		{"code: unsyntax is an ordinary head", "unsyntax", 0, quasisyntaxMarkers, 0, quasisyntaxMarkers, false},
		{"template: unsyntax escapes", "unsyntax", 1, quasisyntaxMarkers, 0, quasisyntaxMarkers, false},
		{"template: unsyntax-splicing escapes", "unsyntax-splicing", 2, quasisyntaxMarkers, 1, quasisyntaxMarkers, false},
		{"template: quasisyntax nests", "quasisyntax", 1, quasisyntaxMarkers, 2, quasisyntaxMarkers, false},
		{"template: quote is not a barrier", "quote", 1, quasisyntaxMarkers, 1, quasisyntaxMarkers, false},
		{"template: unquote is data", "unquote", 1, quasisyntaxMarkers, 1, quasisyntaxMarkers, false},
		{"template: quasiquote is data", "quasiquote", 1, quasisyntaxMarkers, 1, quasisyntaxMarkers, false},
		{"quasiquote template: unsyntax is data", "unsyntax", 1, quasiquoteMarkers, 1, quasiquoteMarkers, false},
		{"quasiquote template: quasisyntax is data", "quasisyntax", 1, quasiquoteMarkers, 1, quasiquoteMarkers, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			depth, markers, barrier := quasiHeadDepth(tc.key, tc.quasi, tc.in)
			c.Assert(depth, qt.Equals, tc.wantDepth)
			c.Assert(markers, qt.Equals, tc.wantMarkers)
			c.Assert(barrier, qt.Equals, tc.wantBarrier)
		})
	}
}
