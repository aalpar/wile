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

package values

import (
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

func TestTime_NewTime(t *testing.T) {
	goTime := time.Date(2025, 6, 15, 12, 0, 0, 0, time.UTC)
	tm := NewTime(goTime)
	qt.Assert(t, tm, qt.Not(qt.IsNil))
	qt.Assert(t, tm.GoTime().Equal(goTime), qt.IsTrue)
}

func TestTime_NewTimeFromSeconds(t *testing.T) {
	tm := NewTimeFromSeconds(1000.5)
	qt.Assert(t, tm, qt.Not(qt.IsNil))

	// Round-trip check
	secs := tm.Seconds()
	qt.Assert(t, secs > 1000.0, qt.IsTrue)
	qt.Assert(t, secs < 1001.0, qt.IsTrue)
}

func TestTime_Seconds_RoundTrip(t *testing.T) {
	goTime := time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC)
	tm := NewTime(goTime)
	secs := tm.Seconds()
	tm2 := NewTimeFromSeconds(secs)

	// Should be within a microsecond
	diff := tm.GoTime().Sub(tm2.GoTime())
	qt.Assert(t, diff.Abs() < time.Microsecond, qt.IsTrue)
}

func TestTime_CurrentTime(t *testing.T) {
	before := time.Now()
	tm := CurrentTime()
	after := time.Now()

	qt.Assert(t, !tm.GoTime().Before(before), qt.IsTrue)
	qt.Assert(t, !tm.GoTime().After(after), qt.IsTrue)
}

func TestTime_Add(t *testing.T) {
	goTime := time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC)
	tm := NewTime(goTime)
	tm2 := tm.Add(time.Hour)

	expected := goTime.Add(time.Hour)
	qt.Assert(t, tm2.GoTime().Equal(expected), qt.IsTrue)
}

func TestTime_Sub(t *testing.T) {
	t1 := NewTime(time.Date(2025, 1, 1, 1, 0, 0, 0, time.UTC))
	t2 := NewTime(time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC))

	d := t1.Sub(t2)
	qt.Assert(t, d, qt.Equals, time.Hour)
}

func TestTime_BeforeAfter(t *testing.T) {
	t1 := NewTime(time.Date(2025, 1, 1, 0, 0, 0, 0, time.UTC))
	t2 := NewTime(time.Date(2025, 1, 2, 0, 0, 0, 0, time.UTC))

	qt.Assert(t, t1.Before(t2), qt.IsTrue)
	qt.Assert(t, t2.Before(t1), qt.IsFalse)
	qt.Assert(t, t2.After(t1), qt.IsTrue)
	qt.Assert(t, t1.After(t2), qt.IsFalse)
}

func TestTime_IsVoid(t *testing.T) {
	tm := CurrentTime()
	qt.Assert(t, tm.IsVoid(), qt.IsFalse)

	var nilTime *Time
	qt.Assert(t, nilTime.IsVoid(), qt.IsTrue)
}

func TestTime_EqualTo(t *testing.T) {
	goTime := time.Date(2025, 6, 15, 12, 0, 0, 0, time.UTC)
	t1 := NewTime(goTime)
	t2 := NewTime(goTime)
	t3 := NewTime(goTime.Add(time.Second))

	qt.Assert(t, t1.EqualTo(t2), qt.IsTrue)
	qt.Assert(t, t1.EqualTo(t3), qt.IsFalse)
	qt.Assert(t, t1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestTime_SchemeString(t *testing.T) {
	tm := CurrentTime()
	s := tm.SchemeString()
	qt.Assert(t, strings.Contains(s, "#<time"), qt.IsTrue)

	var nilTime *Time
	qt.Assert(t, nilTime.SchemeString(), qt.Equals, "#<time:void>")
}
