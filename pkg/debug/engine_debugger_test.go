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

package debug_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/debug"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func TestSetDebugger(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	dbg := debug.NewDebugger()
	eng.SetDebugger(dbg)

	// Basic eval still works with debugger attached.
	val, evalErr := eng.Eval(ctx, eng.MustParse(ctx, "(+ 1 2)"))
	qt.Assert(t, evalErr, qt.IsNil)
	qt.Assert(t, val.SchemeString(), qt.Equals, "3")

	// Setting nil clears it.
	eng.SetDebugger(nil)

	// Still works after clearing.
	val, evalErr = eng.Eval(ctx, eng.MustParse(ctx, "(+ 3 4)"))
	qt.Assert(t, evalErr, qt.IsNil)
	qt.Assert(t, val.SchemeString(), qt.Equals, "7")
}
