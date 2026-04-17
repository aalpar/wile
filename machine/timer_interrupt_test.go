package machine

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestErrTimerInterrupt_Error(t *testing.T) {
	err := &ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}

func TestErrTimerInterrupt_ErrorsAs(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := error(&ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	})
	qt.Assert(t, errors.As(err, &timerErr), qt.IsTrue)
	qt.Assert(t, timerErr.Handler, qt.IsNotNil)
}

func TestErrTimerInterrupt_NotMatchOtherErrors(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := errors.New("something else")
	qt.Assert(t, errors.As(err, &timerErr), qt.IsFalse)
}

func TestErrTimerInterrupt_NilHandler(t *testing.T) {
	err := &ErrTimerInterrupt{}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}
