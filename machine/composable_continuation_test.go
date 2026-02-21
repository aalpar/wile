package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestComposableContinuation_AcceptsArity(t *testing.T) {
	cc := NewComposableContinuation(nil, nil, 0, nil)

	tcs := []struct {
		name string
		n    int
		want bool
	}{
		{"0 args", 0, false},
		{"1 arg", 1, true},
		{"2 args", 2, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, cc.AcceptsArity(tc.n), qt.Equals, tc.want)
		})
	}
}
