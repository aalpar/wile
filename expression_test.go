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

package wile_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

func TestReadExpression(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name       string
		input      string
		wantErr    bool
		incomplete bool
		wantValue  string // SchemeString of eval result (only checked if !wantErr)
	}{
		{"simple atom", "42", false, false, "42"},
		{"list expression", "(+ 1 2)", false, false, "3"},
		{"string literal", `"hello"`, false, false, `"hello"`},
		{"incomplete paren", "(+ 1", true, true, ""},
		{"incomplete string", `"hello`, true, true, ""},
		{"empty input", "", true, true, ""},
		{"trailing input ignored", "(+ 1 2) (+ 3 4)", false, false, "3"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			r := strings.NewReader(tc.input)
			expr, parseErr := eng.ReadExpression(ctx, r)
			if tc.wantErr {
				qt.Assert(t, parseErr, qt.IsNotNil)
				qt.Assert(t, wile.IsIncompleteInput(parseErr), qt.Equals, tc.incomplete)
				return
			}
			qt.Assert(t, parseErr, qt.IsNil)
			cc, compileErr := eng.Compile(ctx, expr)
			qt.Assert(t, compileErr, qt.IsNil)
			val, runErr := eng.Run(ctx, cc)
			qt.Assert(t, runErr, qt.IsNil)
			qt.Assert(t, val.SchemeString(), qt.Equals, tc.wantValue)
		})
	}
}
