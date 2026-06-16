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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// TestGetCurrentPortReturnsErrorOnNonPort pins R10: the GetCurrent*Port save/
// restore helpers are called from tests outside any VM recover, so a panic on a
// non-port parameter value crashes the caller. They must return a wrapped
// sentinel error instead. (Production code uses resolveCurrent*Port, which keeps
// its panic-by-design contract because the VM recovers it.)
func TestGetCurrentPortReturnsErrorOnNonPort(t *testing.T) {
	InitState()

	t.Run("input", func(t *testing.T) {
		saved := currentInputPortParam.Value()
		defer currentInputPortParam.SetValue(saved)
		currentInputPortParam.SetValue(values.NewInteger(42))
		_, err := GetCurrentInputPort()
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAnInputPort)
	})

	t.Run("output", func(t *testing.T) {
		saved := currentOutputPortParam.Value()
		defer currentOutputPortParam.SetValue(saved)
		currentOutputPortParam.SetValue(values.NewInteger(42))
		_, err := GetCurrentOutputPort()
		qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAnOutputPort)
	})
}
