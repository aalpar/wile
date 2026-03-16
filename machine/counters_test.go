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

package machine

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestRecordCall_NilMap(t *testing.T) {
	var c VMCounters
	// Should not panic when callCounts map is nil.
	c.RecordCall("+")
}

func TestRecordCall_Counting(t *testing.T) {
	var c VMCounters
	c.callCounts = make(map[string]uint64)

	c.RecordCall("+")
	c.RecordCall("+")
	c.RecordCall("car")

	qt.Assert(t, c.callCounts["+"], qt.Equals, uint64(2))
	qt.Assert(t, c.callCounts["car"], qt.Equals, uint64(1))
}

func TestCallHistogram_NilMap(t *testing.T) {
	var c VMCounters
	qt.Assert(t, c.CallHistogram(), qt.Equals, "")
}

func TestCallHistogram_SortedByFrequency(t *testing.T) {
	var c VMCounters
	c.callCounts = map[string]uint64{
		"+":     100,
		"car":   50,
		"null?": 200,
	}

	hist := c.CallHistogram()

	// Verify all primitives appear.
	qt.Assert(t, strings.Contains(hist, "+"), qt.IsTrue)
	qt.Assert(t, strings.Contains(hist, "car"), qt.IsTrue)
	qt.Assert(t, strings.Contains(hist, "null?"), qt.IsTrue)

	// Verify descending order: null? (200) before + (100) before car (50).
	nullIdx := strings.Index(hist, "null?")
	plusIdx := strings.Index(hist, "+")
	carIdx := strings.Index(hist, "car")
	qt.Assert(t, nullIdx < plusIdx, qt.IsTrue,
		qt.Commentf("null? at %d should appear before + at %d", nullIdx, plusIdx))
	qt.Assert(t, plusIdx < carIdx, qt.IsTrue,
		qt.Commentf("+ at %d should appear before car at %d", plusIdx, carIdx))
}

func TestCallHistogram_EmptyMap(t *testing.T) {
	var c VMCounters
	c.callCounts = make(map[string]uint64)
	qt.Assert(t, c.CallHistogram(), qt.Equals, "")
}

func TestCallHistogram_PercentageTotals(t *testing.T) {
	var c VMCounters
	c.callCounts = map[string]uint64{
		"+": 75,
		"-": 25,
	}

	hist := c.CallHistogram()
	qt.Assert(t, strings.Contains(hist, "75.0%"), qt.IsTrue)
	qt.Assert(t, strings.Contains(hist, "25.0%"), qt.IsTrue)
}
