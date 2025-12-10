package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestByteVector_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewByteVector(NewInteger(10)),
			out: "#u8( 10 )",
		},
		{
			in:  NewByteVector(NewInteger(10), NewInteger(20)),
			out: "#u8( 10 20 )",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestByteVector_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewByteVector(NewInteger(10)),
			in1: NewByteVector(NewInteger(20)),
			out: false,
		},
		{
			in0: NewByteVector(NewInteger(10)),
			in1: NewByteVector(NewInteger(10)),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
