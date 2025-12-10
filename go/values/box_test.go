package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestBox_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewBox(NewBoolean(true)),
			out: "#&#t",
		},
		{
			in:  NewBox(NewBoolean(false)),
			out: "#&#f",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestBox_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewBox(NewInteger(10)),
			in1: NewBox(NewInteger(20)),
			out: false,
		},
		{
			in0: NewBox(NewInteger(10)),
			in1: NewBox(NewInteger(10)),
			out: true,
		},
		{
			in0: NewBox(nil),
			in1: NewBox(nil),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
