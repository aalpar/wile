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

package helpers

import (
	"context"
	"errors"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// stubCallContext is a minimal machine.CallContext for helper unit tests.
// Only Arg and Context are exercised; other methods satisfy the interface.
type stubCallContext struct {
	args []values.Value
}

func (p *stubCallContext) Arg(i int) values.Value {
	return p.args[i]
}
func (p *stubCallContext) SetValue(values.Value) {
}
func (p *stubCallContext) SetValues(...values.Value) {
}
func (p *stubCallContext) Authorizer() security.Authorizer {
	return nil
}
func (p *stubCallContext) Context() context.Context {
	return context.Background()
}
func (p *stubCallContext) EnvironmentFrame() *environment.EnvironmentFrame {
	return nil
}
func (p *stubCallContext) Thread() *values.Thread {
	return nil
}
func (p *stubCallContext) ImmutableLiterals() *environment.ImmutableLiterals {
	return nil
}

var _ machine.CallContext = (*stubCallContext)(nil)

func TestVariadicArgs(t *testing.T) {
	int1 := values.NewInteger(1)
	int2 := values.NewInteger(2)
	int3 := values.NewInteger(3)
	str := values.NewString("not an int")

	tcs := []struct {
		name       string
		fixedCount int
		args       []values.Value
		wantValues []int64 // expected integer values in output (success cases)
		wantErr    error
		wantArgPos int // 1-indexed position the error should report (0 = don't check)
	}{
		{
			name:       "single fixed arg, empty rest",
			fixedCount: 2,
			args:       []values.Value{int1, values.EmptyList},
			wantValues: []int64{1},
		},
		{
			name:       "single fixed arg, two rest elements",
			fixedCount: 2,
			args:       []values.Value{int1, values.List(int2, int3)},
			wantValues: []int64{1, 2, 3},
		},
		{
			name:       "two fixed args, one rest element",
			fixedCount: 3,
			args:       []values.Value{int1, int2, values.List(int3)},
			wantValues: []int64{1, 2, 3},
		},
		{
			name:       "type mismatch on fixed arg position 1",
			fixedCount: 2,
			args:       []values.Value{str, values.EmptyList},
			wantErr:    werr.ErrNotAnInteger,
			wantArgPos: 1,
		},
		{
			name:       "type mismatch on rest element 1 (overall position 2)",
			fixedCount: 2,
			args:       []values.Value{int1, values.List(str)},
			wantErr:    werr.ErrNotAnInteger,
			wantArgPos: 2,
		},
		{
			name:       "type mismatch on rest element 2 (overall position 4) with 2 fixed args",
			fixedCount: 3,
			args:       []values.Value{int1, int2, values.List(int3, str)},
			wantErr:    werr.ErrNotAnInteger,
			wantArgPos: 4,
		},
		{
			name:       "improper rest list",
			fixedCount: 2,
			args:       []values.Value{int1, &values.Pair{int2, int3}}, // (2 . 3) — improper
			wantErr:    werr.ErrNotAList,
		},
		{
			name:       "non-list rest argument",
			fixedCount: 2,
			args:       []values.Value{int1, int2}, // Arg(1) is an Integer, not a list
			wantErr:    werr.ErrNotAList,
		},
		{
			name:       "fixedCount=0 rejected",
			fixedCount: 0,
			args:       []values.Value{values.EmptyList},
			wantErr:    werr.ErrInternal,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			mc := &stubCallContext{args: tc.args}
			out, err := VariadicArgs[*values.Integer](mc, tc.fixedCount, werr.ErrNotAnInteger, "test")

			if tc.wantErr != nil {
				c.Assert(err, qt.IsNotNil)
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue,
					qt.Commentf("expected error chain to include %v, got %v", tc.wantErr, err))
				if tc.wantArgPos > 0 {
					expected := "argument " + strconv.Itoa(tc.wantArgPos) + ":"
					c.Assert(strings.Contains(err.Error(), expected), qt.IsTrue,
						qt.Commentf("expected error to mention %q, got %q", expected, err.Error()))
				}
				return
			}

			c.Assert(err, qt.IsNil)
			c.Assert(len(out), qt.Equals, len(tc.wantValues))
			for i, want := range tc.wantValues {
				c.Assert(out[i].Value, qt.Equals, want)
			}
		})
	}
}
