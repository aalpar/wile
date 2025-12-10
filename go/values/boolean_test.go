package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestBoolean_New(t *testing.T) {
	tcs := []struct {
		in  bool
		out Value
	}{
		{
			in:  true,
			out: NewBoolean(true),
		},
		{
			in:  false,
			out: NewBoolean(false),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			v := NewBoolean(tc.in)
			qt.Assert(t, v, SchemeEquals, tc.out)
		})
	}
}

func TestBoolean_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewBoolean(true),
			out: "#t",
		},
		{
			in:  NewBoolean(false),
			out: "#f",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestBoolean_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewBoolean(true),
			in1: NewBoolean(true),
			out: true,
		},
		{
			in0: NewBoolean(true),
			in1: NewBoolean(false),
			out: false,
		},
		{
			in0: NewBoolean(false),
			in1: NewBoolean(false),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
