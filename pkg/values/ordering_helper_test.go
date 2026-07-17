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
	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// assertOrder asserts the ordering between two numbers, where want is -1 for
// a < b, 1 for a > b, and 0 for numerically equal.
//
// The -1/0/1 vocabulary is a convenience for test tables, NOT a comparison
// primitive: the tower has only LessThan (Compare was removed — see
// values.Number), and this asserts BOTH directions of it rather than deriving a
// three-way answer from one call. That is deliberate and stronger. A single
// derived int cannot distinguish "equal" from "both directions true", so a
// broken LessThan that answered true both ways would read as -1 and pass; here
// it fails on the second assertion.
//
// NOT VALID FOR NaN, which is unordered: LessThan is false both ways, and this
// would read that as want == 0. NaN ordering is pinned in lessthan_nan_test.go,
// and NaN equivalence belongs to EqvNumber.
func assertOrder(c *qt.C, a, b values.Number, want int) {
	c.Helper()
	c.Assert(a.LessThan(b), qt.Equals, want < 0,
		qt.Commentf("%s < %s (want %d)", a.SchemeString(), b.SchemeString(), want))
	c.Assert(b.LessThan(a), qt.Equals, want > 0,
		qt.Commentf("%s < %s (want %d)", b.SchemeString(), a.SchemeString(), want))
}
