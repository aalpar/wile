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

package parser

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// readOneList reads a single list datum from src and returns its elements as
// plain Scheme values, with the read asserted to have succeeded.
func readOneList(t *testing.T, src string) []values.Value {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	p := NewParserWithFile(env, true, strings.NewReader(src), "in.scm")
	q, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	tup, ok := q.UnwrapAll().(values.Tuple)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("not a list: %s", q.SchemeString()))
	var els []values.Value
	_, err = tup.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		els = append(els, v)
		return nil
	})
	qt.Assert(t, err, qt.IsNil)
	return els
}

// elementNames renders each element as a symbol name, or — for anything that is
// not a symbol — as its Scheme text. A spelling that split into a number plus a
// trailing identifier therefore shows up as an extra element rather than being
// swallowed by the comparison.
func elementNames(els []values.Value) []string {
	names := make([]string, 0, len(els))
	for _, el := range els {
		sym, ok := el.(*values.Symbol)
		if !ok {
			names = append(names, el.SchemeString())
			continue
		}
		names = append(names, sym.Key)
	}
	return names
}

// TestSignPrefixedIdentifiersKeepTheirWholeSpelling pins the symbol names the
// reader produces for the spellings the number scanner speculates on.
//
// Every arm that abandoned a numeric scan used to mint its symbol from the
// scanner's current position instead of the token's start, so the prefix was
// dropped: `+.abc` read as `bc`, and `-.f`, `+nabc`, `+node` and `+n` all read
// as the *empty* symbol — which silently made distinct identifiers eq?.
func TestSignPrefixedIdentifiersKeepTheirWholeSpelling(t *testing.T) {
	tcs := []struct {
		src  string
		want []string
	}{
		{src: "(+.abc -.f +.g)", want: []string{"+.abc", "-.f", "+.g"}},
		{src: "(+nabc +nxyz)", want: []string{"+nabc", "+nxyz"}},
		{src: "(+node +nap +n)", want: []string{"+node", "+nap", "+n"}},
		{src: "(+inf -inf +in -in)", want: []string{"+inf", "-inf", "+in", "-in"}},
		{src: "(+na -na +nan -nan)", want: []string{"+na", "-na", "+nan", "-nan"}},
		// A read error, not a symbol, before the fix: the arm left the
		// "expecting decimal fraction" diagnostic on p.err while reporting that
		// the run was not a number.
		{src: "(+nan +nane +nan_x)", want: []string{"+nan", "+nane", "+nan_x"}},
		// The whole run is one datum. These used to split into a number and a
		// trailing identifier, so `(a +i2 b)` read as four elements and
		// `(quote +ifoo)` as a two-argument quote.
		{src: "(a +i2 b)", want: []string{"a", "+i2", "b"}},
		{src: "(+ifoo -ibar)", want: []string{"+ifoo", "-ibar"}},
	}
	for _, tc := range tcs {
		t.Run(tc.src, func(t *testing.T) {
			c := qt.New(t)
			c.Check(elementNames(readOneList(t, tc.src)), qt.DeepEquals, tc.want)
		})
	}
}

// TestInfNanKeywordsStillReadAsNumbers is the control on the fallback: the four
// <infnan> spellings and the unit imaginaries are still numbers, and are still
// the only sign-prefixed spellings that are.
func TestInfNanKeywordsStillReadAsNumbers(t *testing.T) {
	srcs := []string{"+inf.0", "-inf.0", "+nan.0", "-nan.0", "+i", "-i", "+inf.0i", "+nan.0i", "+inf.0+2i", "+inf.0+i"}
	for _, src := range srcs {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			els := readOneList(t, "("+src+")")
			c.Assert(els, qt.HasLen, 1)
			_, isnum := els[0].(values.Number)
			c.Check(isnum, qt.IsTrue, qt.Commentf("read as %T %s", els[0], els[0].SchemeString()))
		})
	}
}
