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

package io

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// TestGetCurrentPortReturnsErrorOnNonPort pins R10: the GetCurrent*Port save/
// restore helpers are called from tests outside any VM recover, so a panic on a
// non-port parameter value crashes the caller. They must return a wrapped
// sentinel error instead. (Production code uses resolveCurrent*Port, which now
// returns the same wrapped sentinel so a wrong-typed current port is a
// guard-catchable Scheme condition.)
func TestGetCurrentPortReturnsErrorOnNonPort(t *testing.T) {
	t.Run("input", func(t *testing.T) {
		st := NewState()
		st.inPort.SetValue(values.NewInteger(42))
		_, err := st.GetInputPort()
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAnInputPort)
	})

	t.Run("output", func(t *testing.T) {
		st := NewState()
		st.outPort.SetValue(values.NewInteger(42))
		_, err := st.GetOutputPort()
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAnOutputPort)
	})
}

// TestStateFrom_NoIOState pins the ErrNoIOState error path: a context whose
// namespace has no installed io.State (no io namespace-init hook ran) must yield
// a wrapped ErrNoIOState from stateFrom, which the read/close primitives return
// as a Scheme error rather than resolving a wrong/default port.
func TestStateFrom_NoIOState(t *testing.T) {
	ns := environment.NewNamespace() // no SetIOState — slot is nil
	mc := machine.AcquireTopLevelContext(context.Background(),
		machine.NewNativeTemplate(0, 0, false), ns.Runtime())
	defer machine.ReleaseTopLevelContext(mc)

	_, err := stateFrom(mc)
	qt.Assert(t, err, qt.ErrorIs, werr.ErrNoIOState)
}
