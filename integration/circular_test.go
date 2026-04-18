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

package integration_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

// TestCircularPair_Write tests that `write` on a runtime-created circular pair
// produces datum-label notation via SchemeWriter (already cycle-aware).
func TestCircularPair_Write(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((x (list 'a))
		      (out (open-output-string)))
		  (set-cdr! x x)
		  (write x out)
		  (get-output-string out))
	`))
	c.Assert(err, qt.IsNil)
	// SchemeWriter uses datum-label notation for circular structures
	got := result.SchemeString()
	// Result is a string value, so it's wrapped in quotes
	c.Assert(strings.Contains(got, "#"), qt.IsTrue,
		qt.Commentf("write on circular pair should produce datum-label notation, got: %s", got))
}

// TestCircularPair_Display tests that `display` on a runtime-created circular pair
// terminates (DisplayValueToString is already cycle-aware).
func TestCircularPair_Display(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.Console))
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(context.Background(), engine.MustParse(context.Background(), `
		(let ((x (list 'a))
		      (out (open-output-string)))
		  (set-cdr! x x)
		  (display x out)
		  (get-output-string out))
	`))
	c.Assert(err, qt.IsNil)
	got := result.SchemeString()
	// display should terminate and produce some output (not hang)
	c.Assert(len(got) > 0, qt.IsTrue,
		qt.Commentf("display on circular pair should terminate with output, got: %s", got))
}
