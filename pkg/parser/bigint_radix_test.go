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
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// The tests in this file pin the TODO "Reader fixes" item "BigInteger readers
// accept radix (eg, #z#x) tags", under the datum-introducer model.
//
// # Why that model
//
// R7RS §2.1 splits the # dispatch space in two. Exactly one entry composes:
// #e/#i/#b/#o/#d/#x form a two-slot product, radix × exactness, either order,
// at most one from each slot. Repeating a slot is an error; Racket's diagnostic
// names the rule as "misplaced radix specification", and Chez and MIT reject
// the same inputs. Every other entry (#\, #(, #u8(, #&, #;, #|, #!, #n=)
// dispatches once and then reads one complete datum, which is free to carry its
// own prefixes.
//
// #z is a Wile extension with no counterpart in any of those three
// implementations, which have no type tag in the numeric prefix space at all:
// bignum-versus-flonum is a consequence of the tower, not a reader decision.
// Making #z a third slot in the product would be unprecedented and would force
// every ordering to be enumerated. Making it a datum introducer follows what
// the rest of the # space already does:
//
//	#z <number datum>  ->  that number, widened to BigInteger
//
// Radix and exactness composition then comes for free, because the inner datum
// is read by the ordinary number reader: #z#x1f is "read #x1f, widen", and
// #z#e#x1f works without #z knowing that #e exists.
//
// # Consequences pinned below
//
//   - #z#x1f reads; #x#z1f does not. A radix prefix steers the digit scan of a
//     literal, so its operand has to be a literal. #e#z9 and #i#z9 do read, and
//     already do today: Wile implements exactness as a conversion applied to an
//     already-read datum (readExactnessMarker), not as a scanning mode. Radix
//     is lexical, exactness is post-hoc, and that is the whole reason one
//     composes with an introducer and the other does not.
//   - Digit-set validation is inherited, not reimplemented: #z#b19 fails
//     because #b19 fails.
//   - #z is a coercion, not a container, so it does not nest: #z#z5 is 5, where
//     the container introducer #& gives #&#&5 two boxes deep.
//   - The datum must denote an exact integer. #z1.5 and #z#i#x1f are errors.

// TestReadSyntaxBigIntegerWithRadix is the positive half: #z applied to a
// radix-prefixed datum yields that datum's value as a BigInteger.
//
// RED. Today the tokenizer's readBigNum (tokenizer_numbers.go) scans base 10
// inline and stops at the '#' of the radix tag, so the token text is a bare
// "#z" and the parser reports "invalid big integer: #z".
func TestReadSyntaxBigIntegerWithRadix(t *testing.T) {
	tcs := []struct {
		input string
		// expect is the value in base 10, however the input spelled it.
		expect string
	}{
		// The existing decimal spelling is the degenerate case of the model:
		// the datum simply carries no prefix. It must keep working.
		{"#z123", "123"},
		{"#z-789", "-789"},

		{"#z#x1f", "31"},
		{"#z#xFF", "255"},
		{"#z#b1011", "11"},
		{"#z#o777", "511"},
		{"#z#d99", "99"},

		// Both letters are case-insensitive, as #z and #x already are alone.
		{"#Z#X1F", "31"},
		{"#z#Xff", "255"},

		// The sign belongs to the inner datum, which is where the number reader
		// already handles it.
		{"#z#x-ff", "-255"},
		{"#z#x+ff", "255"},

		// Exactness composition arrives without #z knowing about #e: these are
		// just the two-slot product being read by the ordinary number reader.
		// Enumerating orderings inside #z would be the third-slot model.
		{"#z#e#x1f", "31"},
		{"#z#x#e1f", "31"},

		// The point of #z: magnitudes past int64 in a non-decimal base.
		{"#z#xFFFFFFFFFFFFFFFFFF", "4722366482869645213695"},
		{"#z#b" + strings.Repeat("1", 70), "1180591620717411303423"},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigInt, ok := obj.(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T: %v", obj, obj))

			expected := new(big.Int)
			expected.SetString(tc.expect, 10)
			c.Assert(bigInt.BigInt().Cmp(expected), qt.Equals, 0,
				qt.Commentf("got %s, want %s", bigInt.BigInt(), expected))
		})
	}
}

// TestReadSyntaxBigIntegerIsACoercionNotAContainer separates the two kinds of
// introducer. #& wraps, so #&#&5 is a box holding a box. #z widens, so #z#z5 is
// just 5 as a BigInteger — applying it twice cannot produce anything the single
// application did not.
//
// This is the assertion that fails loudest under a third-slot implementation,
// where a repeated tag is a "misplaced specification" error rather than a
// no-op, so it is worth stating rather than leaving implied.
//
// RED: today "#z#z5" stops at the inner '#' and reports "invalid big integer".
func TestReadSyntaxBigIntegerIsACoercionNotAContainer(t *testing.T) {
	tcs := []string{"#z#z5", "#z#z#x1f"}
	want := map[string]int64{"#z#z5": 5, "#z#z#x1f": 31}

	for _, tc := range tcs {
		t.Run(tc, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bigInt, ok := obj.(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T: %v", obj, obj))
			c.Assert(bigInt.BigInt().Int64(), qt.Equals, want[tc])
		})
	}
}

// TestReadSyntaxRadixPrefixDoesNotTakeAnIntroducer pins the asymmetry the model
// implies: a radix prefix selects the digit set for scanning a literal, so its
// operand must be a literal, and #z is not one. Under a third-slot model both
// orders would read, exactly as #e#x and #x#e both do.
//
// docs/reference/r7rs-differences.md currently records "#z ... does not combine
// with the radix prefixes #b / #o / #d / #x in either order". Half of that
// sentence becomes wrong when this lands, and the other half stays right for a
// reason worth writing down.
//
// The rejection is paired with its mirror-order control so the subtest cannot
// pass vacuously: today the reader rejects both orders, so a lone "must be
// rejected" assertion proves nothing.
func TestReadSyntaxRadixPrefixDoesNotTakeAnIntroducer(t *testing.T) {
	tcs := []struct {
		bad string
		// control is the same pair in the order the model permits.
		control string
		expect  int64
	}{
		{"#x#z1f", "#z#x1f", 31},
		{"#b#z1011", "#z#b1011", 11},
		{"#o#z777", "#z#o777", 511},
	}

	for _, tc := range tcs {
		t.Run(tc.bad, func(t *testing.T) {
			c := qt.New(t)

			ctlEnv := environment.NewNamespace().Runtime()
			ctlSyn, err := NewParser(ctlEnv, true, strings.NewReader(tc.control)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("control %q must read", tc.control))
			ctl, ok := ctlSyn.Unwrap().(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("control: expected BigInteger, got %T", ctlSyn.Unwrap()))
			c.Assert(ctl.BigInt().Int64(), qt.Equals, tc.expect)

			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.bad)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil,
				qt.Commentf("a radix or exactness prefix takes a number, not an introducer; got %v", syn))
		})
	}
}

// TestReadSyntaxBigIntegerIntroducerPreservesExistingBehavior collects the
// regression guards: these already behave correctly today and must survive the
// rework. They are green, so they are not RED tests — they are here because the
// #z rework runs straight through this path and each is a plausible way to
// break it silently.
//
// #e#z9 / #i#z9 are the other side of
// TestReadSyntaxRadixPrefixDoesNotTakeAnIntroducer, and the pair is what makes
// the model's line precise. Wile's #e / #i are implemented as "read a datum,
// then convert it" (readExactnessMarker in parser.go), so they already accept
// #z the way they accept any other numeric datum. Only the radix prefixes,
// which have to know the digit set before they scan, cannot.
//
// #z1.5 is the failure mode of the rework itself: replacing readBigNum's inline
// base-10 scan with "read a datum and widen it" invites accepting any number
// and truncating. The literal must stay rejected.
func TestReadSyntaxBigIntegerIntroducerPreservesExistingBehavior(t *testing.T) {
	c := qt.New(t)

	exactEnv := environment.NewNamespace().Runtime()
	exactSyn, err := NewParser(exactEnv, true, strings.NewReader("#e#z9")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	exact, ok := exactSyn.Unwrap().(values.Number)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected a Number, got %T", exactSyn.Unwrap()))
	c.Assert(exact.IsExact(), qt.IsTrue, qt.Commentf("#e#z9 must be exact, got %v", exact))

	inexactEnv := environment.NewNamespace().Runtime()
	inexactSyn, err := NewParser(inexactEnv, true, strings.NewReader("#i#z9")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	inexact, ok := inexactSyn.Unwrap().(values.Number)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected a Number, got %T", inexactSyn.Unwrap()))
	c.Assert(inexact.IsExact(), qt.IsFalse, qt.Commentf("#i#z9 must be inexact, got %v", inexact))

	for _, bad := range []string{"#z1.5", "#z1e3"} {
		env := environment.NewNamespace().Runtime()
		syn, err := NewParser(env, true, strings.NewReader(bad)).ReadSyntax(context.TODO())
		c.Assert(err, qt.IsNotNil, qt.Commentf("%q is not an exact integer; got %v", bad, syn))
	}
}

// TestReadSyntaxBigIntegerRequiresAnExactIntegerDatum pins the one constraint
// #z adds on top of "read a datum": the datum has to denote an exact integer,
// because that is what a BigInteger is. #m is the prefix for the inexact side.
//
// The #i case is the interesting one — it is not a malformed literal but a
// well-formed inexact number, so rejecting it is a decision about what #z
// means rather than an inherited parse failure.
//
// Each rejection is paired with a control that differs only in being an exact
// integer, so the subtest cannot pass vacuously.
func TestReadSyntaxBigIntegerRequiresAnExactIntegerDatum(t *testing.T) {
	tcs := []struct {
		bad     string
		control string
		expect  int64
	}{
		{"#z#x1.8", "#z#x18", 24},    // radix fraction is inexact
		{"#z#i#x1f", "#z#e#x1f", 31}, // #i asks for the inexact side
	}

	for _, tc := range tcs {
		t.Run(tc.bad, func(t *testing.T) {
			c := qt.New(t)

			ctlEnv := environment.NewNamespace().Runtime()
			ctlSyn, err := NewParser(ctlEnv, true, strings.NewReader(tc.control)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("control %q must read", tc.control))
			ctl, ok := ctlSyn.Unwrap().(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("control: expected BigInteger, got %T", ctlSyn.Unwrap()))
			c.Assert(ctl.BigInt().Int64(), qt.Equals, tc.expect)

			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.bad)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("#z requires an exact integer datum, got %v", syn))
		})
	}
}

// TestReadSyntaxBigIntegerRadixDigitsAreValidated checks that the digit-set
// constraint is *inherited* rather than reimplemented. Under the
// datum-introducer model #z#b19 must fail because #b19 fails, with #z never
// having looked at a digit. A third-slot implementation would have to scan the
// digits itself, and this is where such an implementation drifts: it keeps
// consuming decimal digits and reads #z#b19 as nineteen.
//
// Paired with an in-radix control for the same reason as above.
func TestReadSyntaxBigIntegerRadixDigitsAreValidated(t *testing.T) {
	tcs := []struct {
		bad string
		// control is the same prefix with digits inside the radix.
		control string
		expect  int64
	}{
		{"#z#b19", "#z#b101", 5},    // 9 is not a binary digit
		{"#z#o789", "#z#o707", 455}, // 8 and 9 are not octal digits
		{"#z#dff", "#z#d99", 99},    // f is not a decimal digit
	}

	for _, tc := range tcs {
		t.Run(tc.bad, func(t *testing.T) {
			c := qt.New(t)

			ctlEnv := environment.NewNamespace().Runtime()
			ctlSyn, err := NewParser(ctlEnv, true, strings.NewReader(tc.control)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("control %q must read", tc.control))
			ctl, ok := ctlSyn.Unwrap().(*values.BigInteger)
			c.Assert(ok, qt.IsTrue, qt.Commentf("control: expected BigInteger, got %T", ctlSyn.Unwrap()))
			c.Assert(ctl.BigInt().Int64(), qt.Equals, tc.expect)

			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.bad)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("out-of-radix digits must be rejected, got %v", syn))
		})
	}
}

// TestReadSyntaxBigIntegerWithRadixInCompound checks the form composes where a
// datum is expected, rather than working only as a whole-input special case.
// Mirrors TestReadSyntaxBigIntegerInList / ...InVector.
//
// RED until TestReadSyntaxBigIntegerWithRadix is GREEN.
func TestReadSyntaxBigIntegerWithRadixInCompound(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(#z#x10 #(#z#b101))"))

	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	pair, ok := syn.UnwrapAll().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", syn.UnwrapAll()))

	first, ok := pair.Car().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", pair.Car()))
	c.Assert(first.BigInt().Int64(), qt.Equals, int64(16))

	rest, ok := pair.Cdr().(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Pair, got %T", pair.Cdr()))
	vec, ok := rest.Car().(*values.Vector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Vector, got %T", rest.Car()))
	c.Assert(len(*vec), qt.Equals, 1)

	nested, ok := (*vec)[0].(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigInteger, got %T", (*vec)[0]))
	c.Assert(nested.BigInt().Int64(), qt.Equals, int64(5))
}
