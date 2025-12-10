package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestByte_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewByte(10),
			out: "10",
		},
		{
			in:  NewByte(20),
			out: "20",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestByte_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewByte(1),
			in1: NewByte(1),
			out: true,
		},
		{
			in0: NewByte(1),
			in1: NewByte(0),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
