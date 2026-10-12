// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
