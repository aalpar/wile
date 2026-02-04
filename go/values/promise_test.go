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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestPromise_NewPromise(t *testing.T) {
	thunk := NewSymbol("thunk-placeholder")
	p := NewPromise(thunk)

	qt.Assert(t, p.Forced, qt.IsFalse)
	qt.Assert(t, p.Thunk, SchemeEquals, thunk)
	qt.Assert(t, p.Result == nil, qt.IsTrue)
}

func TestPromise_NewForcedPromise(t *testing.T) {
	val := NewInteger(42)
	p := NewForcedPromise(val)

	qt.Assert(t, p.Forced, qt.IsTrue)
	qt.Assert(t, p.Thunk == nil, qt.IsTrue)
	qt.Assert(t, p.Result, SchemeEquals, val)
}

func TestPromise_IsVoid(t *testing.T) {
	p := NewPromise(NewSymbol("thunk"))
	qt.Assert(t, p.IsVoid(), qt.IsFalse)

	var nilPromise *Promise
	qt.Assert(t, nilPromise.IsVoid(), qt.IsTrue)
}

func TestPromise_EqualTo(t *testing.T) {
	p1 := NewPromise(NewSymbol("thunk"))
	p2 := NewPromise(NewSymbol("thunk"))

	// Identity only
	qt.Assert(t, p1.EqualTo(p1), qt.IsTrue)
	qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	qt.Assert(t, p1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestPromise_SchemeString(t *testing.T) {
	unforced := NewPromise(NewSymbol("thunk"))
	qt.Assert(t, unforced.SchemeString(), qt.Equals, "#<promise>")

	forced := NewForcedPromise(NewInteger(42))
	qt.Assert(t, forced.SchemeString(), qt.Equals, "#<promise (forced)>")
}
