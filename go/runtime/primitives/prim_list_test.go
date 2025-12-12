// Copyright 2025 Aaron Alpar
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

package primitives_test

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestCar(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "car of quoted list",
			prog: values.List(values.NewSymbol("car"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.NewInteger(1),
		},
		{
			name: "car of pair",
			prog: values.List(values.NewSymbol("car"),
				values.List(values.NewSymbol("quote"),
					values.NewCons(values.NewSymbol("a"), values.NewSymbol("b")))),
			out: values.NewSymbol("a"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCdr(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "cdr of quoted list",
			prog: values.List(values.NewSymbol("cdr"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "cdr of pair",
			prog: values.List(values.NewSymbol("cdr"),
				values.List(values.NewSymbol("quote"),
					values.NewCons(values.NewSymbol("a"), values.NewSymbol("b")))),
			out: values.NewSymbol("b"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCons(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "cons two values",
			prog: values.List(values.NewSymbol("cons"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewCons(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "cons with empty list",
			prog: values.List(values.NewSymbol("cons"),
				values.NewInteger(1),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.List(values.NewInteger(1)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list with three elements",
			prog: values.List(values.NewSymbol("list"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "list with no elements",
			prog: values.List(values.NewSymbol("list")),
			out:  values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAppend(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "append no arguments",
			prog: values.List(values.NewSymbol("append")),
			out:  values.EmptyList,
		},
		{
			name: "append single empty list",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "append single list",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "append two lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(3), values.NewInteger(4)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name: "append three lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("b"))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("c")))),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "append with empty list in middle",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1))),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(2)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "append with non-list as last argument (improper list result)",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2))),
				values.NewInteger(3)),
			out: values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3))),
		},
		{
			name: "append empty lists only",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "append nested lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.List(values.NewInteger(1), values.NewInteger(2)))),
				values.List(values.NewSymbol("quote"),
					values.List(values.List(values.NewInteger(3), values.NewInteger(4))))),
			out: values.List(
				values.List(values.NewInteger(1), values.NewInteger(2)),
				values.List(values.NewInteger(3), values.NewInteger(4))),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "length of three element list",
			// (length '(1 2 3))
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.NewInteger(3),
		},
		{
			name: "length of empty list",
			// (length '())
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.NewInteger(0),
		},
		{
			name: "length of single element list",
			// (length '(a))
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a")))),
			out: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestReverse(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "reverse list",
			// (reverse '(1 2 3))
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "reverse empty list",
			// (reverse '())
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "reverse single element",
			// (reverse '(a))
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a")))),
			out: values.List(values.NewSymbol("a")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list-ref first element",
			// (list-ref '(a b c) 0)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(0)),
			out: values.NewSymbol("a"),
		},
		{
			name: "list-ref middle element",
			// (list-ref '(a b c) 1)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(1)),
			out: values.NewSymbol("b"),
		},
		{
			name: "list-ref last element",
			// (list-ref '(a b c) 2)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(2)),
			out: values.NewSymbol("c"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListTail(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list-tail from beginning",
			// (list-tail '(a b c) 0)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(0)),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "list-tail skip one",
			// (list-tail '(a b c) 1)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(1)),
			out: values.List(values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "list-tail skip all",
			// (list-tail '(a b c) 3)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(3)),
			out: values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAssq(t *testing.T) {
	// Note: assq uses eq? (pointer equality). Booleans are singletons and work.
	// Symbols/integers inside nested quotes are not deduplicated (known limitation).
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assq finds boolean key",
			// (assq #t '((#f 1) (#t 2)))
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.FalseValue, values.NewInteger(1)),
						values.List(values.TrueValue, values.NewInteger(2))))),
			out: values.List(values.TrueValue, values.NewInteger(2)),
		},
		{
			name: "assq returns #f when not found",
			// (assq #t '((#f 1)))
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.FalseValue, values.NewInteger(1))))),
			out: values.FalseValue,
		},
		{
			name: "assq with empty list returns #f",
			// (assq #t '())
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAssv(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assv finds integer key",
			// (assv 2 '((1 a) (2 b) (3 c)))
			prog: values.List(values.NewSymbol("assv"),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1), values.NewSymbol("a")),
						values.List(values.NewInteger(2), values.NewSymbol("b")),
						values.List(values.NewInteger(3), values.NewSymbol("c"))))),
			out: values.List(values.NewInteger(2), values.NewSymbol("b")),
		},
		{
			name: "assv returns #f when not found",
			// (assv 4 '((1 a) (2 b) (3 c)))
			prog: values.List(values.NewSymbol("assv"),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1), values.NewSymbol("a")),
						values.List(values.NewInteger(2), values.NewSymbol("b")),
						values.List(values.NewInteger(3), values.NewSymbol("c"))))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAssoc(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assoc finds list key with equal?",
			// (assoc '(1 2) '(((1 2) found) ((3 4) other)))
			prog: values.List(values.NewSymbol("assoc"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found")),
						values.List(values.List(values.NewInteger(3), values.NewInteger(4)), values.NewSymbol("other"))))),
			out: values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found")),
		},
		{
			name: "assoc returns #f when not found",
			// (assoc '(5 6) '(((1 2) a) ((3 4) b)))
			prog: values.List(values.NewSymbol("assoc"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(5), values.NewInteger(6))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("a")),
						values.List(values.List(values.NewInteger(3), values.NewInteger(4)), values.NewSymbol("b"))))),
			out: values.FalseValue,
		},
		{
			name: "assoc with string key",
			// (assoc "hello" '(("hello" found) ("world" other)))
			prog: values.List(values.NewSymbol("assoc"),
				values.NewString("hello"),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewString("hello"), values.NewSymbol("found")),
						values.List(values.NewString("world"), values.NewSymbol("other"))))),
			out: values.List(values.NewString("hello"), values.NewSymbol("found")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMemq(t *testing.T) {
	// Note: memq uses eq? (pointer equality). Booleans are singletons and work.
	// Symbols inside nested quotes are not deduplicated (known limitation).
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "memq finds boolean",
			// (memq #t '(#f #t 1))
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(values.FalseValue, values.TrueValue, values.NewInteger(1)))),
			out: values.List(values.TrueValue, values.NewInteger(1)),
		},
		{
			name: "memq returns #f when not found",
			// (memq #t '(#f 1 2))
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(values.FalseValue, values.NewInteger(1), values.NewInteger(2)))),
			out: values.FalseValue,
		},
		{
			name: "memq with empty list returns #f",
			// (memq #t '())
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMemv(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "memv finds integer",
			// (memv 2 '(1 2 3))
			prog: values.List(values.NewSymbol("memv"),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "memv returns #f when not found",
			// (memv 4 '(1 2 3))
			prog: values.List(values.NewSymbol("memv"),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMember(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "member finds list with equal?",
			// (member '(2) '((1) (2) (3)))
			prog: values.List(values.NewSymbol("member"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1)),
						values.List(values.NewInteger(2)),
						values.List(values.NewInteger(3))))),
			out: values.List(values.List(values.NewInteger(2)), values.List(values.NewInteger(3))),
		},
		{
			name: "member finds string",
			// (member "hello" '("world" "hello" "foo"))
			prog: values.List(values.NewSymbol("member"),
				values.NewString("hello"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewString("world"), values.NewString("hello"), values.NewString("foo")))),
			out: values.List(values.NewString("hello"), values.NewString("foo")),
		},
		{
			name: "member returns #f when not found",
			// (member '(4) '((1) (2) (3)))
			prog: values.List(values.NewSymbol("member"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(4))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1)),
						values.List(values.NewInteger(2)),
						values.List(values.NewInteger(3))))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
