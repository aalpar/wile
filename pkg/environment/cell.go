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

package environment

import (
	"math"

	"github.com/aalpar/wile/pkg/values"
)

// Cell is the unboxed-scalar carrier for the arithmetic fast path: a value that
// is either a boxed Scheme value (CellRef) or a raw Go scalar held without a
// heap object (CellFloat, CellFixnum).
//
// Motivation: float arithmetic allocates one *values.Float per operation, and a
// numeric kernel spends most of its time in the resulting GC, not in the
// arithmetic. Measured on the probes in pkg/values/numeric_alloc_bench_test.go,
// a float add costs ~4x an in-cache integer add — the same operation with the
// allocation removed — and that gap is the allocation plus its GC accounting.
// A Cell lets an intermediate stay a Go scalar until something actually needs a
// values.Value, so a chain of N operations materializes once rather than N
// times.
//
// This is NOT a tagged union in the C sense: no storage overlaps a pointer.
// Go's garbage collector is precise, so a field that is sometimes a pointer and
// sometimes an integer would be unsound. The two scalar arms share the bits
// lane — an overlap that is safe because neither arm there is ever a pointer —
// while ref is an honestly-typed interface the collector traces as one.
//
// Layout is 32 bytes: tag padded to 8, bits 8, ref 16. Giving the scalar arms
// separate float64 and int64 fields would cost 40.
//
// Cell lives in pkg/environment rather than pkg/machine because both consumers
// need it: the VM's register lane and the frame-slot lane on
// LocalEnvironmentFrame. It is deliberately not in pkg/values, which is public
// API for embedders and has no business exposing a VM representation choice.
type Cell struct {
	tag  uint8
	bits uint64
	ref  values.Value
}

// Cell tags. CellRef is the zero value, so a zero Cell is a ref cell holding a
// nil value — never a scalar, which keeps an uninitialized Cell from being read
// as the float 0.0.
const (
	CellRef uint8 = iota
	CellFloat
	CellFixnum
)

// Tag returns which arm of the Cell is live.
func (p Cell) Tag() uint8 {
	return p.tag
}

// Float returns the cell's float64 payload. Valid only when Tag is CellFloat;
// reading it under any other tag yields a meaningless reinterpretation of the
// bits lane, so callers must dispatch on Tag first.
func (p Cell) Float() float64 {
	return math.Float64frombits(p.bits)
}

// Fixnum returns the cell's int64 payload. Valid only when Tag is CellFixnum;
// see Float for the same caveat.
func (p Cell) Fixnum() int64 {
	return int64(p.bits)
}

// Ref returns the cell's boxed payload. Valid only when Tag is CellRef.
func (p Cell) Ref() values.Value {
	return p.ref
}

// FloatCell returns a Cell carrying f unboxed.
func FloatCell(f float64) Cell {
	return Cell{tag: CellFloat, bits: math.Float64bits(f)}
}

// FixnumCell returns a Cell carrying i unboxed.
func FixnumCell(i int64) Cell {
	return Cell{tag: CellFixnum, bits: uint64(i)}
}

// RefCell returns a Cell carrying an already-boxed value.
func RefCell(v values.Value) Cell {
	return Cell{tag: CellRef, ref: v}
}

// Box materializes a Cell as a values.Value. This is the single point at which
// an unboxed scalar acquires a heap object, and it costs exactly the one
// allocation today's code pays per operation — the win is in how many times it
// is reached, not in making any individual boxing cheaper.
//
// Box(Unbox(v)) is not pointer-identical to v for out-of-cache numbers, which
// is sound because Scheme numbers have no observable identity: R7RS §6.1 leaves
// eqv? on numbers to numeric equality, and eq? on numbers is explicitly
// unspecified. In-cache integers do come back identical, via NewInteger's
// flyweight.
func Box(c Cell) values.Value {
	switch c.tag {
	case CellFloat:
		return values.NewFloat(math.Float64frombits(c.bits))
	case CellFixnum:
		return values.NewInteger(int64(c.bits))
	default:
		return c.ref
	}
}

// Unbox lifts a values.Value into a Cell, stripping the heap object when the
// value is one of the two unboxable scalar types.
//
// The unboxable set is exactly {float64, int64} and cannot grow: every other
// numeric type in the tower (BigInteger, Rational, Complex, BigFloat) carries
// state wider than 64 bits, and every non-numeric type is a pointer. Those all
// take the ref arm unchanged.
func Unbox(v values.Value) Cell {
	switch n := v.(type) {
	case *values.Float:
		return Cell{tag: CellFloat, bits: math.Float64bits(n.Value)}
	case *values.Integer:
		return Cell{tag: CellFixnum, bits: uint64(n.Value)}
	default:
		return Cell{tag: CellRef, ref: v}
	}
}
