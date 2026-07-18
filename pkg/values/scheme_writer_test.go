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
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// mustRender invokes a writer entry point on a depth-valid value, failing the
// test if it reports a depth error, and returns the rendered string.
func mustRender(t *testing.T, render func(values.Value) (string, error), v values.Value) string {
	t.Helper()
	s, err := render(v)
	qt.Assert(t, err, qt.IsNil)
	return s
}

// nestList returns a value nested `depth` lists deep around an atom:
// nestList(3) is ((( 0 ))). Its writer nesting depth — counted as the writer
// counts it, root = 1, +1 per descent — is depth+1 (the innermost atom).
func nestList(depth int) values.Value {
	q := values.Value(values.NewInteger(0))
	for range depth {
		q = values.List(q)
	}
	return q
}

// flatList returns a proper list of n integers 0..n-1. Its nesting depth is 2
// (the list, then its atom elements) regardless of n — length is not depth.
func flatList(n int) values.Value {
	elems := make([]values.Value, n)
	for i := range elems {
		elems[i] = values.NewInteger(int64(i))
	}
	return values.List(elems...)
}

func TestWriteValueToString_SimpleValues(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"integer", values.NewInteger(42), "42"},
		{"negative integer", values.NewInteger(-7), "-7"},
		{"float", values.NewFloat(3.14), "3.14"},
		{"string", values.NewString("hello"), "\"hello\""},
		{"symbol", values.NewSymbol("foo"), "foo"},
		{"true", values.TrueValue, "#t"},
		{"false", values.FalseValue, "#f"},
		{"void", values.Void, "#!void"},
		{"eof", values.EOFObject, "#!eof"},
		{"nil", nil, "#<void>"},
		{"character", values.NewCharacter('a'), "#\\a"},
		{"character space", values.NewCharacter(' '), "#\\space"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.WriteValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Lists(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"empty list", values.EmptyList, "()"},
		{"single element", values.List(values.NewInteger(1)), "(1)"},
		{"proper list", values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)), "(1 2 3)"},
		{"improper pair", values.NewCons(values.NewInteger(1), values.NewInteger(2)), "(1 . 2)"},
		{"nested list", values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)), "((1 2) 3)"},
		{"list with string", values.List(values.NewString("a"), values.NewString("b")), "(\"a\" \"b\")"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.WriteValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Vectors(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"empty vector", values.NewVector(), "#()"},
		{"single element", values.NewVector(values.NewInteger(1)), "#(1)"},
		{"multiple elements", values.NewVector(values.NewInteger(1), values.NewSymbol("a"), values.NewString("b")), "#(1 a \"b\")"},
		{"nil vector", (*values.Vector)(nil), "#()"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.WriteValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_CircularPair(t *testing.T) {
	// Create a circular list: (1 . #0#) where #0# is the pair itself
	p := values.NewCons(values.NewInteger(1), values.EmptyList)
	p[1] = p // make circular

	result := mustRender(t, values.WriteValueToString, p)
	qt.Assert(t, result, qt.Equals, "#0=(1 . #0#)")
}

func TestWriteValueToString_CircularVector(t *testing.T) {
	// Create a vector that contains itself
	v := values.NewVector(values.NewInteger(1), nil)
	(*v)[1] = v // make circular

	result := mustRender(t, values.WriteValueToString, v)
	qt.Assert(t, result, qt.Equals, "#0=#(1 #0#)")
}

func TestWriteSharedValueToString_SharedStructure(t *testing.T) {
	// Create shared structure: (#0=(1 2) #0#)
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	outer := values.List(shared, shared)

	result := mustRender(t, values.WriteSharedValueToString, outer)
	qt.Assert(t, result, qt.Equals, "(#0=(1 2) #0#)")
}

func TestWriteSharedValueToString_NoSharing(t *testing.T) {
	// No shared structure => no labels
	l := values.List(values.NewInteger(1), values.NewInteger(2))
	result := mustRender(t, values.WriteSharedValueToString, l)
	qt.Assert(t, result, qt.Equals, "(1 2)")
}

func TestWriteValueToString_SharedButNotCircular(t *testing.T) {
	// WriteModeWrite should NOT label shared-but-not-circular structures
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	outer := values.List(shared, shared)

	result := mustRender(t, values.WriteValueToString, outer)
	qt.Assert(t, result, qt.Equals, "((1 2) (1 2))")
}

func TestDisplayValueToString_Strings(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"string unquoted", values.NewString("hello"), "hello"},
		{"string with spaces", values.NewString("hello world"), "hello world"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.DisplayValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_Characters(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"char a", values.NewCharacter('a'), "a"},
		{"char space", values.NewCharacter(' '), " "},
		{"char newline", values.NewCharacter('\n'), "\n"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.DisplayValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_NonStringValues(t *testing.T) {
	// Non-string/character values are the same as write
	qt.Assert(t, mustRender(t, values.DisplayValueToString, values.NewInteger(42)), qt.Equals, "42")
	qt.Assert(t, mustRender(t, values.DisplayValueToString, values.NewSymbol("foo")), qt.Equals, "foo")
	qt.Assert(t, mustRender(t, values.DisplayValueToString, values.TrueValue), qt.Equals, "#t")
}

func TestDisplayValueToString_List(t *testing.T) {
	l := values.List(values.NewString("hello"), values.NewCharacter('!'))
	result := mustRender(t, values.DisplayValueToString, l)
	qt.Assert(t, result, qt.Equals, "(hello !)")
}

// TestSymbolDisplayVsWriteBars pins R7RS §6.13.3: display writes a symbol with no
// bar-escaping (the human-readable representation), whereas write produces the
// re-readable external representation, which bars a symbol whose name cannot be
// written bare. Previously both went through Symbol.SchemeString() and so display
// incorrectly emitted |a b| for (string->symbol "a b").
func TestSymbolDisplayVsWriteBars(t *testing.T) {
	tcs := []struct {
		name        string
		sym         values.Value
		wantDisplay string
		wantWrite   string
	}{
		{"space needs bars", values.NewSymbol("a b"), "a b", "|a b|"},
		{"empty needs bars", values.NewSymbol(""), "", "||"},
		{"bare identifier unbarred either way", values.NewSymbol("foo"), "foo", "foo"},
		{"bar in name escaped only by write", values.NewSymbol("a|b"), "a|b", `|a\|b|`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.DisplayValueToString, tc.sym), qt.Equals, tc.wantDisplay)
			qt.Assert(t, mustRender(t, values.WriteValueToString, tc.sym), qt.Equals, tc.wantWrite)
		})
	}
}

// TestSymbolDisplayInList pins that the no-bar display rule applies recursively:
// a barring symbol nested inside a list is still un-barred under display, barred
// under write.
func TestSymbolDisplayInList(t *testing.T) {
	l := values.List(values.NewSymbol("a b"), values.NewSymbol("c"))
	qt.Assert(t, mustRender(t, values.DisplayValueToString, l), qt.Equals, "(a b c)")
	qt.Assert(t, mustRender(t, values.WriteValueToString, l), qt.Equals, "(|a b| c)")
}

func TestWriteValueToString_NilPair(t *testing.T) {
	result := mustRender(t, values.WriteValueToString, (*values.Pair)(nil))
	qt.Assert(t, result, qt.Equals, "#<void>")
}

func TestWriteSharedValueToString_SharedVector(t *testing.T) {
	// Shared vector structure
	shared := values.NewVector(values.NewInteger(1))
	outer := values.NewVector(shared, shared)

	result := mustRender(t, values.WriteSharedValueToString, outer)
	qt.Assert(t, result, qt.Equals, "#(#0=#(1) #0#)")
}

// TestWriteValueToString_StringEscapesAreR7RS pins R7RS §7.1.1 string escaping:
// control chars use \xHH; (semicolon-terminated), named escapes use their
// mnemonics, quote/backslash are escaped, and printable runes (incl. Unicode)
// pass through. All re-read by the reader — unlike Go's %q, which emits \xHH
// without ';', plus \f, \v, \uHHHH. Regression for the FuzzReadWriteRoundTrip
// finding that a control char wrote as the non-re-readable "\x02".
func TestWriteValueToString_StringEscapesAreR7RS(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"STX control char", values.NewString("\x02"), `"\x2;"`},
		{"tab uses mnemonic", values.NewString("\t"), `"\t"`},
		{"quote and backslash", values.NewString("a\"b\\c"), `"a\"b\\c"`},
		{"printable unicode verbatim", values.NewString("café"), `"café"`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, mustRender(t, values.WriteValueToString, tc.in), qt.Equals, tc.out)
		})
	}
}

// TestWriteValueToString_DefaultDepthLimitProtects pins the shipped default:
// the primitive path (WriteValueToString) refuses a value nested deeper than
// DefaultMaxWriteDepth rather than overflow the host stack. nestList(N) puts
// its atom at depth N+1, so DefaultMaxWriteDepth wrappers is the first depth
// that exceeds the bound.
func TestWriteValueToString_DefaultDepthLimitProtects(t *testing.T) {
	_, err := values.WriteValueToString(nestList(values.DefaultMaxWriteDepth))
	qt.Assert(t, errors.Is(err, werr.ErrWriteDepthExceeded), qt.IsTrue)
}

// TestSchemeWriter_DepthLimitConfigurable checks the boundary on both sides
// with a small, cheap cap: a structure exactly at the cap writes, one level
// deeper trips. Counted as the writer counts (root = 1), so nestList(cap-1)
// reaches depth cap and nestList(cap) reaches cap+1.
func TestSchemeWriter_DepthLimitConfigurable(t *testing.T) {
	const limit = 100

	atLimit := values.NewSchemeWriter()
	atLimit.SetMaxDepth(limit)
	_, err := atLimit.WriteString(nestList(limit - 1))
	qt.Assert(t, err, qt.IsNil)

	overLimit := values.NewSchemeWriter()
	overLimit.SetMaxDepth(limit)
	_, err = overLimit.WriteString(nestList(limit))
	qt.Assert(t, errors.Is(err, werr.ErrWriteDepthExceeded), qt.IsTrue)
}

// TestSchemeWriter_DepthUnlimited confirms maxDepth = 0 disables the bound:
// nesting that would trip any positive default writes without error.
func TestSchemeWriter_DepthUnlimited(t *testing.T) {
	w := values.NewSchemeWriter()
	w.SetMaxDepth(0)
	s, err := w.WriteString(nestList(values.DefaultMaxWriteDepth + 1000))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, strings.HasPrefix(s, "(("), qt.IsTrue)
}

// TestWriteFlatListNotBoundedByDepth is the load-bearing test for this fix: a
// flat list's length is NOT its nesting depth. A list far longer than the cap
// must still write — list length must not be charged against the depth budget.
// (Pre-fix, findShared recursed once per cdr-spine element; a naive "bound all
// recursion" misfix would instead trip ErrWriteDepthExceeded here. Both are
// regressions this catches.)
func TestWriteFlatListNotBoundedByDepth(t *testing.T) {
	const limit = 100
	const length = 5000 // 50x the depth cap

	w := values.NewSchemeWriter()
	w.SetMaxDepth(limit)
	s, err := w.WriteString(flatList(length))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, strings.HasPrefix(s, "(0 1 2 3"), qt.IsTrue)
	qt.Assert(t, strings.HasSuffix(s, "4998 4999)"), qt.IsTrue)
}

// TestWriteLongFlatListNoOverflow exercises the iterative cdr-spine at scale
// under the default cap (a flat list nests only one level, so the cap never
// applies). It completes instantly because the spine is walked in a loop;
// reintroducing per-element cdr recursion would walk the spine on the Go
// stack. Output correctness is spot-checked at both ends. The motivating bug
// — (write (make-list 10000000)) — overflowed the host here pre-fix.
func TestWriteLongFlatListNoOverflow(t *testing.T) {
	const length = 1_000_000

	s, err := values.WriteValueToString(flatList(length))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, strings.HasPrefix(s, "(0 1 2 3"), qt.IsTrue)
	qt.Assert(t, strings.HasSuffix(s, "999998 999999)"), qt.IsTrue)
}

// TestWriteValueToString_LongSpineCircular exercises filterToCircular's
// iterative spine-tracking and batch on-stack pop with a cycle that closes over
// a long cdr-spine. The existing circular tests use length-1 self-cycles, which
// never stress the spine bookkeeping. The whole spine is on the DFS stack when
// the back-edge to the head is seen, so the head is the labeled cycle entry.
func TestWriteValueToString_LongSpineCircular(t *testing.T) {
	const n = 1000
	head := values.NewCons(values.NewInteger(0), values.EmptyList)
	prev := head
	for i := 1; i < n; i++ {
		next := values.NewCons(values.NewInteger(int64(i)), values.EmptyList)
		prev.SetCdr(next)
		prev = next
	}
	prev.SetCdr(head) // close the cycle through the entire spine

	s := mustRender(t, values.WriteValueToString, head)
	qt.Assert(t, strings.HasPrefix(s, "#0=(0 1 2 3"), qt.IsTrue)
	qt.Assert(t, strings.HasSuffix(s, ". #0#)"), qt.IsTrue)
}

// TestSchemeWriter_ReusableAcrossCalls pins that a SchemeWriter resets its
// per-call working state at WriteString entry. Regression for the crosscheck
// finding that the cached err (and label counter) were never reset, poisoning a
// reused writer.
func TestSchemeWriter_ReusableAcrossCalls(t *testing.T) {
	// (a) err reset: a write after a depth-limit refusal must not inherit the
	// stale error.
	w := values.NewSchemeWriter()
	w.SetMaxDepth(100)
	_, err := w.WriteString(nestList(100))
	qt.Assert(t, errors.Is(err, werr.ErrWriteDepthExceeded), qt.IsTrue)
	w.SetMaxDepth(0)
	s, err := w.WriteString(values.List(values.NewInteger(1), values.NewInteger(2)))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, s, qt.Equals, "(1 2)")

	// (b) label-counter reset: two circular values written by the same writer
	// must each label from #0, not continue the counter (which would make the
	// second print "#1=...").
	w2 := values.NewSchemeWriter()
	c1 := values.NewCons(values.NewInteger(1), values.EmptyList)
	c1.SetCdr(c1)
	s1, err := w2.WriteString(c1)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, s1, qt.Equals, "#0=(1 . #0#)")
	c2 := values.NewCons(values.NewInteger(2), values.EmptyList)
	c2.SetCdr(c2)
	s2, err := w2.WriteString(c2)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, s2, qt.Equals, "#0=(2 . #0#)")
}

func TestWriteSharedValueToString_SharedThroughHashtable(t *testing.T) {
	// A pair shared between a top-level list element and a hashtable value must
	// be labeled once (#0=) and back-referenced (#0#) inside the table.
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	h := values.NewEmptyHashtable()
	qt.Assert(t, h.Set(values.NewSymbol("k"), shared), qt.IsNil)
	outer := values.List(shared, h)

	result := mustRender(t, values.WriteSharedValueToString, outer)
	qt.Assert(t, result, qt.Equals, "(#0=(1 2) #hash((k . #0#)))")
}

func TestWriteValueToString_CycleThroughHashtable(t *testing.T) {
	// A self-cyclic hashtable (its own value slot points back at the table) must
	// render a datum label, not SchemeString's "..." depth guard.
	h := values.NewEmptyHashtable()
	qt.Assert(t, h.Set(values.NewSymbol("k"), h), qt.IsNil)

	result := mustRender(t, values.WriteValueToString, h)
	qt.Assert(t, result, qt.Equals, "#0=#hash((k . #0#))")
}
