package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
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

// --- bottomOfChain tests ---

func TestBottomOfChain_Nil(t *testing.T) {
	qt.Assert(t, bottomOfChain(nil), qt.IsNil)
}

func TestBottomOfChain_SingleFrame(t *testing.T) {
	frame := &MachineContinuation{}
	qt.Assert(t, bottomOfChain(frame), qt.Equals, frame)
}

func TestBottomOfChain_MultiFrame(t *testing.T) {
	bottom := &MachineContinuation{}
	mid := &MachineContinuation{parent: bottom}
	top := &MachineContinuation{parent: mid}

	qt.Assert(t, bottomOfChain(top), qt.Equals, bottom)
}

// --- AcquireSegment tests ---

func TestAcquireSegment_NilCont(t *testing.T) {
	cc := NewComposableContinuation(nil, nil, 0, nil)
	qt.Assert(t, cc.AcquireSegment(), qt.IsNil)
}

func TestAcquireSegment_FirstCallReturnsOriginal(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, nil, env)
	cc := NewComposableContinuation(cont, nil, 0, nil)

	segment := cc.AcquireSegment()

	// First call returns the stored segment directly (no copy).
	qt.Assert(t, segment, qt.Equals, cont)
	// Segment is marked shared.
	qt.Assert(t, cont.shared, qt.IsTrue)
	// CC is marked consumed.
	qt.Assert(t, cc.consumed, qt.IsTrue)
}

func TestAcquireSegment_SecondCallDeepCopies(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	bottom := NewMachineContinuation(nil, nil, env)
	top := NewMachineContinuation(bottom, nil, env)
	cc := NewComposableContinuation(top, nil, 0, nil)

	first := cc.AcquireSegment()
	qt.Assert(t, first, qt.Equals, top)

	// Simulate GraftContinuation setting bottom.parent to a non-nil value.
	graftTarget := &MachineContinuation{}
	bottom.parent = graftTarget

	second := cc.AcquireSegment()

	// Second call returns a deep copy, not the original.
	qt.Assert(t, second != top, qt.IsTrue)
	// The bottom frame's parent was reset to nil before copy.
	qt.Assert(t, bottom.parent, qt.IsNil)
	// The deep copy's bottom also has parent == nil.
	qt.Assert(t, bottomOfChain(second).parent, qt.IsNil)
}
