package machine

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestParameter_AcceptsArity(t *testing.T) {
	p := NewParameter(values.NewInteger(42), nil)

	tcs := []struct {
		name string
		n    int
		want bool
	}{
		{"0 args (get)", 0, true},
		{"1 arg (set)", 1, true},
		{"2 args", 2, false},
		{"3 args", 3, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, p.AcceptsArity(tc.n), qt.Equals, tc.want)
		})
	}
}

func TestParameter_ImplementsCallable(t *testing.T) {
	p := NewParameter(values.NewInteger(0), nil)
	var c values.Callable = p
	qt.Assert(t, c, qt.IsNotNil)
}
