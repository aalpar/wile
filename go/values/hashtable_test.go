package values

import (
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestHashtable_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			in1: NewHashtable(map[string]Value{
				"key2": NewInteger(1),
			}),
			out: false,
		},
		{
			in0: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			in1: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
