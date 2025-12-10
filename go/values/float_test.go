package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestFloat_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewFloat(1.1),
			out: "1.1",
		},
		{
			in:  NewFloat(1.2),
			out: "1.2",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestFloat_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewFloat(1.1),
			in1: NewFloat(1.1),
			out: true,
		},
		{
			in0: NewFloat(1.0),
			in1: NewFloat(1.1),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
