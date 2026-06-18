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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestPromise_NewPromise(t *testing.T) {
	thunk := newStubCallable(values.NewSymbol("thunk-placeholder"))
	p := values.NewPromise(thunk)

	qt.Assert(t, p.IsForced(), qt.IsFalse)
	qt.Assert(t, p.Thunk(), valuestest.SchemeEquals, thunk)
	qt.Assert(t, p.CachedResult() == nil, qt.IsTrue)
}

func TestPromise_NewForcedPromise(t *testing.T) {
	val := values.NewInteger(42)
	p := values.NewForcedPromise(val)

	qt.Assert(t, p.IsForced(), qt.IsTrue)
	qt.Assert(t, p.Thunk() == nil, qt.IsTrue)
	qt.Assert(t, p.CachedResult(), valuestest.SchemeEquals, val)
}

func TestPromise_IsVoid(t *testing.T) {
	p := values.NewPromise(newStubCallable(values.NewSymbol("thunk")))
	qt.Assert(t, p.IsVoid(), qt.IsFalse)

	var nilPromise *values.Promise
	qt.Assert(t, nilPromise.IsVoid(), qt.IsTrue)
}

func TestPromise_EqualTo(t *testing.T) {
	p1 := values.NewPromise(newStubCallable(values.NewSymbol("thunk")))
	p2 := values.NewPromise(newStubCallable(values.NewSymbol("thunk")))

	// Identity only
	qt.Assert(t, p1.EqualTo(p1), qt.IsTrue)
	qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	qt.Assert(t, p1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestPromise_SchemeString(t *testing.T) {
	unforced := values.NewPromise(newStubCallable(values.NewSymbol("thunk")))
	qt.Assert(t, unforced.SchemeString(), qt.Equals, "#<promise>")

	forced := values.NewForcedPromise(values.NewInteger(42))
	qt.Assert(t, forced.SchemeString(), qt.Equals, "#<promise (forced)>")
}
