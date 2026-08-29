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

package charsets

import (
	"context"
	"sync"
	"unicode"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var primCharSetQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.CharSet)
	return ok
})

func primCharSetContains(mc machine.CallContext) error {
	cs, err := helpers.RequireArg[*values.CharSet](mc, 0, werr.ErrNotACharSet, "char-set-contains?")
	if err != nil {
		return err
	}
	ch, err := helpers.RequireArg[*values.Character](mc, 1, werr.ErrNotACharacter, "char-set-contains?")
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(cs.Contains(ch.Value)))
	return nil
}

var primCharSetSize = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set-size", func(cs *values.CharSet) values.Value {
	return values.NewInteger(int64(cs.Size()))
})

func primCharSetCtor(mc machine.CallContext) error {
	first, err := helpers.RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "%char-set")
	if err != nil {
		return err
	}
	runes := []rune{first.Value}

	// mc.Arg(1) is the variadic-rest list (always a Tuple — Pair or
	// EmptyList — never nil per Wile variadic convention for ParamCount=2).
	rest, ok := mc.Arg(1).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"%%char-set: rest argument: expected list of chars, got %T", mc.Arg(1))
	}
	err = helpers.ForEachList(mc.Context(), rest, "%char-set", func(_ context.Context, i int, _ bool, v values.Value) error {
		ch, isChar := v.(*values.Character)
		if !isChar {
			return werr.WrapForeignErrorf(werr.ErrNotACharacter,
				"%%char-set: argument %d: expected char, got %T", i+2, v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}

	mc.SetValue(values.NewCharSetFromRunes(runes))
	return nil
}

func primEmptyCharSet(mc machine.CallContext) error {
	mc.SetValue(values.NewCharSetFromRanges(nil))
	return nil
}

// CharSet is immutable; copy is identity at the Go level. Returning
// a fresh wrapper is unnecessary, but harmless if a future change adds
// hidden state. Identity is the cheapest correct answer.
var primCharSetCopy = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set-copy", func(cs *values.CharSet) values.Value {
	return cs
})

func primStringToCharSet(mc machine.CallContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string->char-set")
	if err != nil {
		return err
	}
	runes := []rune(str.Value)
	base, err := helpers.OptionalArg[*values.CharSet](mc.Arg(1), nil, werr.ErrNotACharSet, "string->char-set")
	if err != nil {
		return err
	}
	return setValueFromRunesAndBase(mc, runes, base)
}

func primListToCharSet(mc machine.CallContext) error {
	lst, ok := mc.Arg(0).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"list->char-set: argument 1: expected list, got %T", mc.Arg(0))
	}
	var runes []rune
	err := helpers.ForEachList(mc.Context(), lst, "list->char-set", func(_ context.Context, i int, _ bool, v values.Value) error {
		ch, isChar := v.(*values.Character)
		if !isChar {
			return werr.WrapForeignErrorf(werr.ErrNotACharacter,
				"list->char-set: list element %d: expected char, got %T", i, v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}
	base, err := helpers.OptionalArg[*values.CharSet](mc.Arg(1), nil, werr.ErrNotACharSet, "list->char-set")
	if err != nil {
		return err
	}
	return setValueFromRunesAndBase(mc, runes, base)
}

func primUcsRangeToCharSet(mc machine.CallContext) error {
	lo, ok := values.ExactInteger(mc.Arg(0))
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"ucs-range->char-set: argument 1: expected exact integer (in int64 range), got %T", mc.Arg(0))
	}
	hi, ok := values.ExactInteger(mc.Arg(1))
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"ucs-range->char-set: argument 2: expected exact integer (in int64 range), got %T", mc.Arg(1))
	}

	// Parse optional rest: (error? base-cs).
	//
	// Per CLAUDE.md "ParamCount: N, IsVariadic: true → Arg(N-1) = rest list"
	// the rest is always a Tuple (Pair or EmptyList), never nil.
	errorFlag := true
	var base *values.CharSet
	rest, ok := mc.Arg(2).(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"ucs-range->char-set: rest argument: expected list, got %T", mc.Arg(2))
	}
	if !rest.IsEmptyList() {
		// Scheme truthiness: only #f is false; any other value (including
		// symbols, integers, strings) is truthy. SRFI-14 spec calls error?
		// a "boolean" but doesn't pin down rejection behavior for non-booleans;
		// following Scheme convention here.
		errorFlag = rest.Car() != values.FalseValue
		cdr := rest.Cdr()
		cdrTuple, isTuple := cdr.(values.Tuple)
		if isTuple && !cdrTuple.IsEmptyList() {
			cs, isCs := cdrTuple.Car().(*values.CharSet)
			if !isCs {
				return werr.WrapForeignErrorf(werr.ErrNotACharSet,
					"ucs-range->char-set: optional base argument: expected char-set, got %T", cdrTuple.Car())
			}
			base = cs
			// Check for any further arguments beyond base-cs.
			tail, isTail := cdrTuple.Cdr().(values.Tuple)
			if isTail && !tail.IsEmptyList() {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
					"ucs-range->char-set: too many optional arguments")
			}
		}
	}

	// lo > hi is always an error regardless of errorFlag (malformed range, not
	// a codepoint overflow).
	if lo > hi {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"ucs-range->char-set: lo (%d) exceeds hi (%d)", lo, hi)
	}

	// Validate or clip the lower bound.
	lowerInclusive := lo
	if lowerInclusive < 0 {
		if errorFlag {
			return werr.WrapForeignErrorf(werr.ErrCodepointOutOfRange,
				"ucs-range->char-set: codepoint %d is negative", lo)
		}
		lowerInclusive = 0
	}

	// Validate or clip the upper bound.
	upperExclusive := hi
	if upperExclusive > int64(values.MaxCodepoint)+1 {
		if errorFlag {
			return werr.WrapForeignErrorf(werr.ErrCodepointOutOfRange,
				"ucs-range->char-set: codepoint %d exceeds 0x10FFFF", hi-1)
		}
		upperExclusive = int64(values.MaxCodepoint) + 1
	}

	// Half-open [lo, hi) → inclusive [lowerInclusive, upperExclusive-1].
	var cs *values.CharSet
	if lowerInclusive >= upperExclusive {
		cs = values.NewCharSetFromRanges(nil)
	} else {
		cs = values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: rune(lowerInclusive), Hi: rune(upperExclusive - 1)},
		})
	}
	if base != nil {
		cs = unionTwo(cs, base)
	}
	mc.SetValue(cs)
	return nil
}

var primCharSetToList = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set->list", func(cs *values.CharSet) values.Value {
	chars := make([]values.Value, 0, cs.Size())
	for c := range cs.Codepoints() {
		chars = append(chars, values.NewCharacter(c))
	}
	return values.List(chars...)
})

var primCharSetToString = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set->string", func(cs *values.CharSet) values.Value {
	runes := make([]rune, 0, cs.Size())
	for c := range cs.Codepoints() {
		runes = append(runes, c)
	}
	return values.NewString(string(runes))
})

var primCharSetRanges = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set-ranges", func(cs *values.CharSet) values.Value {
	var pairs []values.Value
	for r := range cs.All() {
		pairs = append(pairs, values.NewCons(values.NewInteger(int64(r.Lo)), values.NewInteger(int64(r.Hi))))
	}
	return values.List(pairs...)
})

// setValueFromRunesAndBase builds a char-set from runes, optionally unioned
// with a base char-set, and stores it on the machine context.
func setValueFromRunesAndBase(mc machine.CallContext, runes []rune, base *values.CharSet) error {
	cs := values.NewCharSetFromRunes(runes)
	if base != nil {
		cs = unionTwo(cs, base)
	}
	mc.SetValue(cs)
	return nil
}

func primCharSetEqual(mc machine.CallContext) error {
	sets, err := helpers.VariadicArgs[*values.CharSet](mc, 2, werr.ErrNotACharSet, "char-set=")
	if err != nil {
		return err
	}
	for i := 1; i < len(sets); i++ {
		if !sets[0].EqualTo(sets[i]) {
			mc.SetValue(values.FalseValue)
			return nil
		}
	}
	mc.SetValue(values.TrueValue)
	return nil
}

func primCharSetSubset(mc machine.CallContext) error {
	sets, err := helpers.VariadicArgs[*values.CharSet](mc, 2, werr.ErrNotACharSet, "char-set<=")
	if err != nil {
		return err
	}
	for i := 1; i < len(sets); i++ {
		if !isSubset(sets[i-1], sets[i]) {
			mc.SetValue(values.FalseValue)
			return nil
		}
	}
	mc.SetValue(values.TrueValue)
	return nil
}

// makeCharSetFold returns a variadic primitive that folds two or more char-sets
// left-to-right with the given binary operation. char-set-union, -intersection,
// -difference, and -xor differ only in the fold op and the error-context name,
// so they share this factory (SRFI-14).
func makeCharSetFold(name string, op func(a, b *values.CharSet) *values.CharSet) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		sets, err := helpers.VariadicArgs[*values.CharSet](mc, 2, werr.ErrNotACharSet, name)
		if err != nil {
			return err
		}
		out := sets[0]
		for i := 1; i < len(sets); i++ {
			out = op(out, sets[i])
		}
		mc.SetValue(out)
		return nil
	}
}

var primCharSetComplement = helpers.MakeUnaryAccessor(werr.ErrNotACharSet, "char-set-complement", func(cs *values.CharSet) values.Value {
	return complementOne(cs)
})

// unionTwo computes a ∪ b via NewCharSetFromUnsortedRanges, which canonicalizes
// (sorts + merges adjacent + drops invalid). Correct because the result is
// unconditionally re-canonicalized.
func unionTwo(a, b *values.CharSet) *values.CharSet {
	return values.NewCharSetFromUnsortedRanges(append(a.Ranges(), b.Ranges()...))
}

// intersectTwo computes a ∩ b via linear merge over sorted ranges.
func intersectTwo(a, b *values.CharSet) *values.CharSet {
	ar, br := a.Ranges(), b.Ranges()
	var out []values.CharSetRange
	i, j := 0, 0
	for i < len(ar) && j < len(br) {
		lo := max(ar[i].Lo, br[j].Lo)
		hi := min(ar[i].Hi, br[j].Hi)
		if lo <= hi {
			out = append(out, values.CharSetRange{Lo: lo, Hi: hi})
		}
		if ar[i].Hi < br[j].Hi {
			i++
		} else {
			j++
		}
	}
	return values.NewCharSetFromRanges(out) // already canonical
}

// differenceTwo computes a \ b via linear merge over sorted ranges.
func differenceTwo(a, b *values.CharSet) *values.CharSet {
	ar, br := a.Ranges(), b.Ranges()
	var out []values.CharSetRange
	j := 0
	for _, r := range ar {
		cur := r
		// Advance past b-ranges that end before cur starts.
		for j < len(br) && br[j].Hi < cur.Lo {
			j++
		}
		// Subtract each overlapping b-range from cur.
		for j < len(br) && br[j].Lo <= cur.Hi {
			if br[j].Lo > cur.Lo {
				out = append(out, values.CharSetRange{Lo: cur.Lo, Hi: br[j].Lo - 1})
			}
			if br[j].Hi >= cur.Hi {
				cur.Lo = cur.Hi + 1 // mark as fully consumed
				break
			}
			cur.Lo = br[j].Hi + 1
			j++
		}
		if cur.Lo <= cur.Hi {
			out = append(out, cur)
		}
	}
	return values.NewCharSetFromRanges(out)
}

// xorTwo computes a △ b = (a ∪ b) \ (a ∩ b).
func xorTwo(a, b *values.CharSet) *values.CharSet {
	return differenceTwo(unionTwo(a, b), intersectTwo(a, b))
}

// complementOne computes [0, MaxCodepoint] \ cs.
func complementOne(cs *values.CharSet) *values.CharSet {
	full := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 0, Hi: values.MaxCodepoint}})
	return differenceTwo(full, cs)
}

// isSubset reports whether every codepoint of a is also in b. Linear merge
// over canonical inversion lists: O(n_a + n_b).
func isSubset(a, b *values.CharSet) bool {
	ar := a.Ranges()
	br := b.Ranges()
	j := 0
	for _, ra := range ar {
		// Advance past b-ranges that end before this a-range starts.
		for j < len(br) && br[j].Hi < ra.Lo {
			j++
		}
		if j >= len(br) || br[j].Lo > ra.Lo || br[j].Hi < ra.Hi {
			return false
		}
	}
	return true
}

// namedCharSets is a process-global cache, intentionally not per-Engine.
//
// Safety: the cache is referentially transparent. Inputs are immutable
// unicode.RangeTable values from Go's stdlib (unicode.L, unicode.Ll,
// unicode.White_Space, etc.); outputs are deterministic functions of those
// inputs. Multiple Engines in the same process share the cache without
// synchronization beyond the mutex, and identity (eq? at the Scheme level)
// is consistent across Engines — which is the desired observable behavior
// for SRFI-14 named char-sets like char-set:letter.
//
// Maintenance constraint: any future change must preserve compatibility
// with Go's unicode package. The cache stores results derived from
// unicode.* tables; do not mix in inputs from other sources, or per-Engine
// caching will become necessary.
var (
	namedCharSetsMu sync.Mutex
	namedCharSets   = map[string]*values.CharSet{}
)

// makeNamedCharSet returns the cached or freshly-built named char-set for the
// given SRFI-14 name. Symbol-dispatched factory hitting Go's unicode.RangeTable.
func makeNamedCharSet(name string) (*values.CharSet, error) {
	namedCharSetsMu.Lock()
	defer namedCharSetsMu.Unlock()

	cs, ok := namedCharSets[name]
	if ok {
		return cs, nil
	}

	switch name {
	case "letter":
		cs = rangeTableToCharSet(unicode.L)
	case "lower-case":
		cs = rangeTableToCharSet(unicode.Ll)
	case "upper-case":
		cs = rangeTableToCharSet(unicode.Lu)
	case "title-case":
		cs = rangeTableToCharSet(unicode.Lt)
	case "digit":
		cs = rangeTableToCharSet(unicode.Nd)
	case "letter+digit":
		cs = unionTwo(rangeTableToCharSet(unicode.L), rangeTableToCharSet(unicode.Nd))
	case "graphic":
		// SRFI-14: a graphic character "would put ink on paper" — letter, mark,
		// digit, punctuation, symbol. That is exactly Go's PrintRanges
		// (L,M,N,P,S), which excludes Zs/Zl/Zp and Cc, so no whitespace is
		// present. Note that Go's GraphicRanges is NOT the same thing: it adds
		// Zs, which would put U+0020 in graphic and break the SRFI-14 relation
		// below. TestNamedCharSetGraphicPrintingRelation pins the exclusion.
		cs = rangeListToCharSet(unicode.PrintRanges)
	case "printing":
		// SRFI-14: char-set:printing is the union of char-set:graphic and
		// char-set:whitespace, making graphic a proper subset of printing.
		cs = unionTwo(rangeListToCharSet(unicode.PrintRanges),
			rangeTableToCharSet(unicode.White_Space))
	case "whitespace":
		cs = rangeTableToCharSet(unicode.White_Space)
	case "iso-control":
		cs = rangeTableToCharSet(unicode.Cc)
	case "punctuation":
		cs = rangeTableToCharSet(unicode.P)
	case "symbol":
		cs = rangeTableToCharSet(unicode.S)
	case "hex-digit":
		// ASCII-only per SRFI-14: 0-9, A-F, a-f
		cs = values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: '0', Hi: '9'}, {Lo: 'A', Hi: 'F'}, {Lo: 'a', Hi: 'f'},
		})
	case "blank":
		// Tab + space + Unicode Zs category
		zs := rangeTableToCharSet(unicode.Zs)
		cs = unionTwo(values.NewCharSetFromRanges([]values.CharSetRange{
			{Lo: '\t', Hi: '\t'}, {Lo: ' ', Hi: ' '},
		}), zs)
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%%make-named-charset: unknown named char-set: %s", name)
	}
	namedCharSets[name] = cs
	return cs, nil
}

// rangeTableToCharSet converts a *unicode.RangeTable into a CharSet.
// Handles both Range16 (BMP, codepoints ≤ 0xFFFF) and Range32 (supplementary
// planes). Stride > 1 means the range covers every Stride-th codepoint;
// expand into per-stride unit ranges.
//
// NOTE: the loop variable must be promoted to uint32 for R16 ranges. Using
// uint16 directly (the element type of Range16 fields) overflows when
// hi + stride > 65535, producing wraparound values that contaminate the set.
func rangeTableToCharSet(t *unicode.RangeTable) *values.CharSet {
	var rs []values.CharSetRange
	for _, r := range t.R16 {
		if r.Stride == 1 {
			rs = append(rs, values.CharSetRange{Lo: rune(r.Lo), Hi: rune(r.Hi)})
			continue
		}
		// Promote to uint32 to avoid uint16 wraparound when lo + stride > 65535.
		for cp := uint32(r.Lo); cp <= uint32(r.Hi); cp += uint32(r.Stride) {
			rs = append(rs, values.CharSetRange{Lo: rune(cp), Hi: rune(cp)})
		}
	}
	for _, r := range t.R32 {
		if r.Stride == 1 {
			rs = append(rs, values.CharSetRange{Lo: rune(r.Lo), Hi: rune(r.Hi)})
			continue
		}
		for cp := r.Lo; cp <= r.Hi; cp += r.Stride {
			rs = append(rs, values.CharSetRange{Lo: rune(cp), Hi: rune(cp)})
		}
	}
	return values.NewCharSetFromUnsortedRanges(rs)
}

// rangeListToCharSet unions multiple RangeTables (for unicode.GraphicRanges,
// PrintRanges which are []*RangeTable).
func rangeListToCharSet(tables []*unicode.RangeTable) *values.CharSet {
	out := values.NewCharSetFromRanges(nil)
	for _, t := range tables {
		out = unionTwo(out, rangeTableToCharSet(t))
	}
	return out
}

// primMakeNamedCharSet is the FFI dispatcher for %make-named-charset.
func primMakeNamedCharSet(mc machine.CallContext) error {
	sym, err := helpers.RequireArg[*values.Symbol](mc, 0, werr.ErrNotASymbol, "%make-named-charset")
	if err != nil {
		return err
	}
	cs, err := makeNamedCharSet(sym.Key)
	if err != nil {
		return err
	}
	mc.SetValue(cs)
	return nil
}
