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

package compilation

import (
	"testing"

	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
)

// TestExpandWithSyntaxKeepsBindingPositions: the rebuilt binding list keeps each
// spine cell's own source position. It used to stamp every cell with the
// position of the whole list.
func TestExpandWithSyntaxKeepsBindingPositions(t *testing.T) {
	c := qt.New(t)
	env, expander := newExpanderEnv()
	form := parseSchemeExpr(t, env, "(with-syntax ((a 1)\n              (b 2))\n  0)").(*syntax.SyntaxPair)
	in := form.SyntaxCdr().(*syntax.SyntaxPair).SyntaxCar().(*syntax.SyntaxPair)

	out, err := expander.expandWithSyntaxForm(form.SyntaxCar().(*syntax.SyntaxSymbol), form.SyntaxCdr())
	c.Assert(err, qt.IsNil)
	got := out.(*syntax.SyntaxPair).SyntaxCdr().(*syntax.SyntaxPair).SyntaxCar().(*syntax.SyntaxPair)

	var want, have []*syntax.SourceContext
	for cell := range syntax.Spine(in) {
		want = append(want, cell.SourceContext())
	}
	for cell := range syntax.Spine(got) {
		have = append(have, cell.SourceContext())
	}
	c.Assert(want, qt.HasLen, 2)
	c.Assert(want[0], qt.Not(qt.Equals), want[1])
	c.Assert(have, qt.HasLen, len(want))
	for i := range want {
		c.Assert(have[i] == want[i], qt.IsTrue, qt.Commentf("binding %d", i))
	}
}
