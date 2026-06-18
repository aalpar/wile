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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// SRFI-18 Time Primitives Tests
// ----------------------------------------------------------------------------

func TestCurrentTime(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, "(current-time)")
	qt.Assert(t, err, qt.IsNil)

	// Should return a time object
	_, ok := result.(*values.Time)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("current-time should return a time, got %T", result))
}

func TestTimeQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "time? on time object",
			code: "(time? (current-time))",
			out:  values.TrueValue,
		},
		{
			name: "time? on integer",
			code: "(time? 42)",
			out:  values.FalseValue,
		},
		{
			name: "time? on string",
			code: `(time? "now")`,
			out:  values.FalseValue,
		},
		{
			name: "time? on float",
			code: "(time? 3.14)",
			out:  values.FalseValue,
		},
		{
			name: "time? on symbol",
			code: "(time? 'time)",
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestTimeToSeconds(t *testing.T) {
	// time->seconds should return a positive number
	code := "(> (time->seconds (current-time)) 0)"
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestTimeToSecondsReturnsFloat(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, "(time->seconds (current-time))")
	qt.Assert(t, err, qt.IsNil)

	// Should return a float
	_, ok := result.(*values.Float)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("time->seconds should return a float, got %T", result))
}

func TestTimeToSecondsError(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "time->seconds with integer", Code: "(time->seconds 42)"},
		{Name: "time->seconds with string", Code: `(time->seconds "now")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestSecondsToTime(t *testing.T) {
	// seconds->time should return a time object
	result, err := testhelpers.RunSchemeCode(t, "(seconds->time 1000000000)")
	qt.Assert(t, err, qt.IsNil)

	_, ok := result.(*values.Time)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("seconds->time should return a time, got %T", result))
}

func TestSecondsToTimeWithFloat(t *testing.T) {
	// Should work with floats
	result, err := testhelpers.RunSchemeCode(t, "(seconds->time 1000000000.5)")
	qt.Assert(t, err, qt.IsNil)

	_, ok := result.(*values.Time)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("seconds->time should return a time, got %T", result))
}

func TestSecondsToTimeRoundTrip(t *testing.T) {
	// Converting to time and back should preserve the value (approximately)
	code := `
		(let ((secs 1500000000.5))
			(< (abs (- (time->seconds (seconds->time secs)) secs)) 0.001))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestSecondsToTimeError(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "seconds->time with string", Code: `(seconds->time "1000")`},
		{Name: "seconds->time with symbol", Code: "(seconds->time 'now)"},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestCurrentTimeIncreasing(t *testing.T) {
	// Two successive calls should return increasing times
	code := `
		(let ((t1 (current-time))
		      (t2 (current-time)))
			(>= (time->seconds t2) (time->seconds t1)))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
