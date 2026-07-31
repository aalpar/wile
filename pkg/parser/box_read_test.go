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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestReadSyntaxBox pins the TODO "Reader fixes" item
// "Boxes can be read. #& prefix: Denotes a box value, where #&5 means a box
// containing the number 5".
//
// The write side already exists and is the reason this is a defect rather than
// a feature request: values.Box renders as "#&" + contents
// (values/box.go schemeStringWithVisited, values/scheme_writer.go writeBox),
// and values.PrefixBox is that literal. The read side has no arm for it —
// '&' is not a letter, digit, or backslash, so the '#' dispatch in
// readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment
// (tokenizer_hash.go) falls to its default reject. A box therefore prints in a
// syntax the reader cannot accept back.
//
// RED until #& is read. Today every case fails in the tokenizer's # dispatch.
func TestReadSyntaxBox(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		// want is the boxed value; the box itself is checked structurally.
		want values.Value
	}{
		{"integer", "#&5", values.NewInteger(5)},
		{"negative integer", "#&-7", values.NewInteger(-7)},
		{"boolean", "#&#t", values.TrueValue},
		{"string", `#&"hi"`, values.NewString("hi")},
		{"character", `#&#\a`, values.NewCharacter('a')},
		{"empty list", "#&()", values.EmptyList},
		{"list", "#&(1 2)", values.List(values.NewInteger(1), values.NewInteger(2))},
		{"vector", "#&#(1)", values.NewVector(values.NewInteger(1))},
		{"nested box", "#&#&5", values.NewBox(values.NewInteger(5))},
		// #& introduces a datum, so the datum may carry its own prefixes.
		// Racket and Chez both read #&#x1f as #&31; this is what distinguishes
		// "dispatch once, then read a datum" from "a tag that decorates a
		// literal".
		{"radix-prefixed datum", "#&#x1f", values.NewInteger(31)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.UnwrapAll()
			box, ok := obj.(*values.Box)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *values.Box, got %T: %v", obj, obj))
			c.Assert(values.Equal(box.Unbox(), tc.want), qt.IsTrue,
				qt.Commentf("boxed value: got %v, want %v", box.Unbox(), tc.want))
		})
	}
}

// TestReadSyntaxBoxInCompound checks #& composes wherever a datum is expected
// rather than working only as a whole-input special case.
//
// RED until TestReadSyntaxBox is GREEN.
func TestReadSyntaxBoxInCompound(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(#&1 #(#&2))"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", syn.UnwrapAll()))

	first, ok := pair.Car().(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Box, got %T", pair.Car()))
	c.Assert(values.Equal(first.Unbox(), values.NewInteger(1)), qt.IsTrue)

	rest, ok := pair.Cdr().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", pair.Cdr()))
	vec, ok := rest.Car().(*values.Vector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Vector, got %T", rest.Car()))
	c.Assert(len(*vec), qt.Equals, 1)

	nested, ok := (*vec)[0].(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Box, got %T", (*vec)[0]))
	c.Assert(values.Equal(nested.Unbox(), values.NewInteger(2)), qt.IsTrue)
}

// TestBoxWriteReadRoundTrip closes the loop the write side opened: whatever the
// writer emits for a Box, the reader must accept and reconstruct as an equal
// Box. This is the invariant FuzzReadWriteRoundTrip asserts generally — a value
// the writer can render is one the reader can re-read — stated concretely for
// boxes so a regression names the type instead of surfacing as a fuzz crasher.
//
// RED until TestReadSyntaxBox is GREEN.
func TestBoxWriteReadRoundTrip(t *testing.T) {
	tcs := []struct {
		name string
		val  values.Value
	}{
		{"integer", values.NewBox(values.NewInteger(5))},
		{"string", values.NewBox(values.NewString("hi"))},
		{"list", values.NewBox(values.List(values.NewInteger(1), values.NewInteger(2)))},
		{"nested", values.NewBox(values.NewBox(values.NewInteger(5)))},
		{"box in list", values.List(values.NewBox(values.NewInteger(1)), values.NewInteger(2))},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			written, err := values.WriteValueToString(tc.val)
			c.Assert(err, qt.IsNil)

			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(written))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("writer emitted %q, which the reader rejects", written))

			got := syn.UnwrapAll()
			c.Assert(values.Equal(got, tc.val), qt.IsTrue,
				qt.Commentf("round trip through %q: got %v, want %v", written, got, tc.val))
		})
	}
}

// TestReadSyntaxSharedBoxDatumLabel pins the harder half of the round trip. The
// writer assigns a datum label to a box reached more than once
// (values/scheme_writer.go writeBox consults needsLabelBox and emits "#n="), so
// a shared or cyclic box comes out as "#0=#&...". Reading #& is not enough on
// its own: it has to work behind a label assignment and a #n# back-reference,
// which is what makes a self-referential box re-readable at all.
//
// RED until TestReadSyntaxBox is GREEN; the label machinery (R7RS §2.4) already
// works for pairs and vectors.
func TestReadSyntaxSharedBoxDatumLabel(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(#0=#&1 #0#)"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", syn.UnwrapAll()))

	first, ok := pair.Car().(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Box, got %T", pair.Car()))

	rest, ok := pair.Cdr().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", pair.Cdr()))
	second, ok := rest.Car().(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Box, got %T", rest.Car()))

	// A label reference denotes the same object, not a copy — eq?, not equal?.
	c.Assert(first == second, qt.IsTrue, qt.Commentf("#0# must resolve to the labelled box itself"))
}
