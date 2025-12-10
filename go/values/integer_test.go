package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestInteger_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewInteger(10),
			in1: NewInteger(10),
			out: true,
		},
		{
			in0: NewInteger(10),
			in1: NewInteger(11),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
