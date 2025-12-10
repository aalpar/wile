package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestCharacter_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewCharacter('='),
			out: `#\=`,
		},
		{
			in:  NewCharacter('>'),
			out: `#\>`,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestCharacter_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewCharacter('='),
			in1: NewCharacter('='),
			out: true,
		},
		{
			in0: NewCharacter('='),
			in1: NewCharacter('>'),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
