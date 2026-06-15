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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

func TestCheckArity(t *testing.T) {
	tcs := []struct {
		name       string
		paramCount int
		isVariadic bool
		argCount   int
		wantErr    bool
	}{
		{
			name:       "exact match non-variadic",
			paramCount: 3,
			isVariadic: false,
			argCount:   3,
			wantErr:    false,
		},
		{
			name:       "zero params non-variadic zero args",
			paramCount: 0,
			isVariadic: false,
			argCount:   0,
			wantErr:    false,
		},
		{
			name:       "too few args non-variadic",
			paramCount: 3,
			isVariadic: false,
			argCount:   2,
			wantErr:    true,
		},
		{
			name:       "too many args non-variadic",
			paramCount: 3,
			isVariadic: false,
			argCount:   4,
			wantErr:    true,
		},
		{
			name:       "variadic exact required args",
			paramCount: 3,
			isVariadic: true,
			argCount:   2, // paramCount-1 required args
			wantErr:    false,
		},
		{
			name:       "variadic with extra args",
			paramCount: 3,
			isVariadic: true,
			argCount:   5,
			wantErr:    false,
		},
		{
			name:       "variadic too few args",
			paramCount: 3,
			isVariadic: true,
			argCount:   1,
			wantErr:    true,
		},
		{
			name:       "variadic zero required (rest only)",
			paramCount: 1,
			isVariadic: true,
			argCount:   0,
			wantErr:    false,
		},
		{
			name:       "variadic zero required with args",
			paramCount: 1,
			isVariadic: true,
			argCount:   5,
			wantErr:    false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := checkArity(tc.paramCount, tc.isVariadic, tc.argCount)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
			} else {
				qt.Assert(t, err, qt.IsNil)
			}
		})
	}
}

func TestBindArgs(t *testing.T) {
	tcs := []struct {
		name       string
		paramCount int
		isVariadic bool
		args       []values.Value
		restArgFn  func([]values.Value, int) values.Tuple
		wantValues []values.Value // expected values in binding slots
	}{
		{
			name:       "non-variadic binds all args",
			paramCount: 3,
			isVariadic: false,
			args: []values.Value{
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			},
			wantValues: []values.Value{
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			},
		},
		{
			name:       "variadic with default restArgFn (nil)",
			paramCount: 2,
			isVariadic: true,
			args: []values.Value{
				values.NewInteger(10),
				values.NewInteger(20),
				values.NewInteger(30),
			},
			restArgFn: nil, // uses values.List
			wantValues: []values.Value{
				values.NewInteger(10),
				nil, // checked separately: rest arg is a list
			},
		},
		{
			name:       "variadic with custom restArgFn",
			paramCount: 2,
			isVariadic: true,
			args: []values.Value{
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			},
			restArgFn: func(vs []values.Value, start int) values.Tuple {
				return values.List(vs[start:]...)
			},
			wantValues: []values.Value{
				values.NewInteger(1),
				nil, // checked separately: rest arg is a list
			},
		},
		{
			name:       "variadic with zero rest args",
			paramCount: 2,
			isVariadic: true,
			args: []values.Value{
				values.NewInteger(42),
			},
			restArgFn: nil,
			wantValues: []values.Value{
				values.NewInteger(42),
				nil, // rest arg is empty list
			},
		},
		{
			// The valid zero boundary: a non-variadic 0-param primitive binds
			// nothing. (The dangerous case — variadic with ParamCount:0, which
			// would index bnds[:-1] — is rejected at registration by
			// validateParamTypes, so bindArgs never sees it.)
			name:       "non-variadic zero params binds nothing",
			paramCount: 0,
			isVariadic: false,
			args:       nil,
			wantValues: nil,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			lenv := environment.NewLocalEnvironment(tc.paramCount)
			bnds := lenv.Bindings()

			bindArgs(bnds, tc.args, tc.paramCount, tc.isVariadic, tc.restArgFn)

			if !tc.isVariadic {
				for i, want := range tc.wantValues {
					qt.Assert(t, bnds[i].Value(), valuestest.SchemeEquals, want)
				}
				return
			}
			// For variadic: check required args
			for i := range tc.paramCount - 1 {
				qt.Assert(t, bnds[i].Value(), valuestest.SchemeEquals, tc.wantValues[i])
			}
			// Check that rest arg slot was set (is a Tuple)
			restVal := bnds[tc.paramCount-1].Value()
			qt.Assert(t, restVal, qt.IsNotNil)
			_, isTuple := restVal.(values.Tuple)
			qt.Assert(t, isTuple, qt.IsTrue)
		})
	}
}
