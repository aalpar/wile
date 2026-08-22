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

package machine

import (
	"fmt"
	"math"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNewNativeTemplate(t *testing.T) {
	NewNativeTemplate(0, 0, false, NewOperationPush())
}

func TestNativeTemplate_Doc(t *testing.T) {
	tpl := &NativeTemplate{}
	qt.Assert(t, tpl.Doc(), qt.Equals, "")
	tpl.SetDoc("Computes factorial.")
	qt.Assert(t, tpl.Doc(), qt.Equals, "Computes factorial.")
}

func TestNativeTemplate_SchemeString(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.SchemeString(), qt.Equals, "#<native-template>")
}

func TestNativeTemplate_IsVoid(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.IsVoid(), qt.IsFalse)

	var nilTpl *NativeTemplate
	qt.Assert(t, nilTpl.IsVoid(), qt.IsTrue)
}

func TestNativeTemplate_Copy(t *testing.T) {
	tpl := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl.MaybeAppendLiteral(values.NewInteger(42))
	tpl.MaybeAppendLiteral(values.NewSymbol("bindSymbolWithScopes"))

	cpy := tpl.Copy()

	// Verify copy is different object
	qt.Assert(t, cpy != tpl, qt.IsTrue)
	// Verify fields match
	qt.Assert(t, cpy.parameterCount, qt.Equals, tpl.parameterCount)
	qt.Assert(t, cpy.valueCount, qt.Equals, tpl.valueCount)
	qt.Assert(t, cpy.isVariadic, qt.Equals, tpl.isVariadic)
	qt.Assert(t, len(cpy.literals), qt.Equals, len(tpl.literals))
	qt.Assert(t, cpy.CodeLen(), qt.Equals, tpl.CodeLen())
}

func TestNativeTemplate_Copy_Nil(t *testing.T) {
	var nilTpl *NativeTemplate
	cpy := nilTpl.Copy()
	qt.Assert(t, cpy, qt.IsNil)
}

func TestNativeTemplate_EqualTo(t *testing.T) {
	tpl1 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl1.MaybeAppendLiteral(values.NewInteger(42))

	tpl2 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl2.MaybeAppendLiteral(values.NewInteger(42))

	tpl3 := NewNativeTemplate(2, 3, true, NewOperationPush())
	tpl3.MaybeAppendLiteral(values.NewInteger(99)) // Different literal

	// Equal templates
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsTrue)

	// Different literals
	qt.Assert(t, tpl1.EqualTo(tpl3), qt.IsFalse)

	// Different parameter count
	tpl4 := NewNativeTemplate(3, 3, true, NewOperationPush())
	tpl4.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl4), qt.IsFalse)

	// Different value count
	tpl5 := NewNativeTemplate(2, 4, true, NewOperationPush())
	tpl5.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl5), qt.IsFalse)

	// Different variadic flag
	tpl6 := NewNativeTemplate(2, 3, false, NewOperationPush())
	tpl6.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl6), qt.IsFalse)

	// Different operation count
	tpl7 := NewNativeTemplate(2, 3, true, NewOperationPush(), NewOperationPush())
	tpl7.MaybeAppendLiteral(values.NewInteger(42))
	qt.Assert(t, tpl1.EqualTo(tpl7), qt.IsFalse)

	// Different type
	qt.Assert(t, tpl1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil comparison
	var nilTpl *NativeTemplate
	qt.Assert(t, nilTpl.EqualTo(nilTpl), qt.IsTrue)
}

// Tests moved from coverage_additional_test.go
// TestNativeTemplateMethodsAdditional tests NativeTemplate methods
func TestNativeTemplateMethodsAdditional(t *testing.T) {
	tpl := NewNativeTemplate(2, 1, true)

	qt.Assert(t, tpl.ParameterCount(), qt.Equals, 2)
	qt.Assert(t, tpl.IsVariadic(), qt.IsTrue)
	qt.Assert(t, tpl.IsVoid(), qt.IsFalse)
	qt.Assert(t, tpl.SchemeString(), qt.Contains, "native-template")

	// Test EqualTo
	tpl2 := NewNativeTemplate(2, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl2), qt.IsTrue)

	tpl3 := NewNativeTemplate(3, 1, true)
	qt.Assert(t, tpl.EqualTo(tpl3), qt.IsFalse)

	var nilTpl *NativeTemplate
	qt.Assert(t, tpl.EqualTo(nilTpl), qt.IsFalse)
}

// TestNativeTemplateCopyNil tests Copy on nil NativeTemplate
func TestNativeTemplateCopyNil(t *testing.T) {
	var tpl *NativeTemplate
	cpy := tpl.Copy()
	qt.Assert(t, cpy, qt.IsNil)
}

// TestNativeTemplateEqualToDifferent tests NativeTemplate EqualTo with different templates
func TestNativeTemplateEqualToDifferent(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	tpl2 := NewNativeTemplate(2, 1, false) // Different parameter count
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)

	tpl3 := NewNativeTemplate(1, 2, false) // Different value count
	qt.Assert(t, tpl1.EqualTo(tpl3), qt.IsFalse)

	tpl4 := NewNativeTemplate(1, 1, true) // Different variadic
	qt.Assert(t, tpl1.EqualTo(tpl4), qt.IsFalse)

	// Different type
	qt.Assert(t, tpl1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Same
	tpl5 := NewNativeTemplate(1, 1, false)
	qt.Assert(t, tpl1.EqualTo(tpl5), qt.IsTrue)
}

// TestNativeTemplateEqualToNil tests NativeTemplate EqualTo with nil
func TestNativeTemplateEqualToNil(t *testing.T) {
	tpl1 := NewNativeTemplate(1, 1, false)
	var tpl2 *NativeTemplate
	qt.Assert(t, tpl1.EqualTo(tpl2), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl1), qt.IsFalse)
	qt.Assert(t, tpl2.EqualTo(tpl2), qt.IsTrue) // nil == nil
}

// TestNativeTemplateLiterals tests NativeTemplate literal methods
func TestNativeTemplateLiterals(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Add some literals
	idx1 := tpl.MaybeAppendLiteral(values.NewInteger(1))
	idx2 := tpl.MaybeAppendLiteral(values.NewInteger(2))
	idx3 := tpl.MaybeAppendLiteral(values.NewInteger(1)) // duplicate

	qt.Assert(t, idx1, qt.Equals, LiteralIndex(0))
	qt.Assert(t, idx2, qt.Equals, LiteralIndex(1))
	qt.Assert(t, idx3, qt.Equals, LiteralIndex(0)) // should be same as idx1

	// Reading a pooled literal back is the other half of the pool's contract,
	// and it is what the deleted findLiteral half of this test used to cover.
	qt.Assert(t, tpl.Literals()[idx2], valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, len(tpl.Literals()), qt.Equals, 2)
}

func TestMaybeAppendLiteral_Dedup(t *testing.T) {
	c := qt.New(t)
	tmpl := NewNativeTemplate(0, 0, false)

	// Same symbol should be deduplicated
	idx1 := tmpl.MaybeAppendLiteral(values.NewSymbol("foo"))
	idx2 := tmpl.MaybeAppendLiteral(values.NewSymbol("foo"))
	c.Assert(idx1, qt.Equals, idx2)

	// Different symbol should not
	idx3 := tmpl.MaybeAppendLiteral(values.NewSymbol("bar"))
	c.Assert(idx3, qt.Not(qt.Equals), idx1)

	// Same integer should be deduplicated
	idx4 := tmpl.MaybeAppendLiteral(values.NewInteger(42))
	idx5 := tmpl.MaybeAppendLiteral(values.NewInteger(42))
	c.Assert(idx4, qt.Equals, idx5)
}

func TestMaybeAppendLiteral_SignedZero(t *testing.T) {
	c := qt.New(t)
	tmpl := NewNativeTemplate(0, 0, false)

	// +0.0 and -0.0 must NOT be deduplicated (IEEE 754)
	idx1 := tmpl.MaybeAppendLiteral(values.NewFloat(0.0))
	idx2 := tmpl.MaybeAppendLiteral(values.NewFloat(math.Copysign(0.0, -1)))
	c.Assert(idx1, qt.Not(qt.Equals), idx2)
}

func TestMaybeAppendLiteral_DedupAfterCopy(t *testing.T) {
	c := qt.New(t)
	tmpl := NewNativeTemplate(0, 0, false)

	// Add a symbol to the original template.
	idx1 := tmpl.MaybeAppendLiteral(values.NewSymbol("foo"))

	// Copy() clones literals but not literalIndex.
	copied := tmpl.Copy()

	// Appending the same symbol to the copy must find the existing literal,
	// not create a duplicate.
	idx2 := copied.MaybeAppendLiteral(values.NewSymbol("foo"))
	c.Assert(idx2, qt.Equals, idx1)
	c.Assert(len(copied.literals), qt.Equals, 1)
}

func TestNativeTemplate_CachedBindings(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	// Empty initially
	c.Assert(tpl.CachedBindings(), qt.HasLen, 0)

	// Add a binding and verify it's accessible
	bd := environment.NewBinding(values.NewInteger(42), environment.BindingTypeVariable)
	idx := tpl.AppendCachedBinding(bd)
	c.Assert(idx, qt.Equals, int32(0))

	bindings := tpl.CachedBindings()
	c.Assert(bindings, qt.HasLen, 1)
	c.Assert(bindings[0], qt.Equals, bd)
}

func TestOpcodeRoundTrip(t *testing.T) {
	// Every opcode from OpInvalid+1 to opCount-1 must produce a non-nil
	// result from instructionToOperation, EXCEPT OpComplex (which requires
	// a side table entry and is not round-trippable).
	for op := OpCode(1); op < opCount; op++ {
		if op == OpComplex {
			continue
		}
		name := opcodeTable[op].name
		if name == "" {
			t.Errorf("opcode %d has no name in opcodeTable", op)
			continue
		}
		instr := Instruction{Op: op, Arg: 0}
		result := instructionToOperation(instr)
		if result == nil {
			t.Errorf("instructionToOperation returned nil for %s (opcode %d)", name, op)
		}
	}
}

func BenchmarkMaybeAppendLiteral(b *testing.B) {
	const n = 500
	syms := make([]values.Value, n)
	for i := range syms {
		syms[i] = values.NewSymbol(fmt.Sprintf("sym_%d", i))
	}

	b.ResetTimer()
	for range b.N {
		tmpl := NewNativeTemplate(0, 0, false)
		for _, s := range syms {
			tmpl.MaybeAppendLiteral(s)
		}
		// Now dedup — each lookup should find existing
		for _, s := range syms {
			tmpl.MaybeAppendLiteral(s)
		}
	}
}

// TestLiteralPoolDoesNotRetypeAcrossFloatKinds guards against a Float literal
// deduplicating onto an equal BigFloat pool slot.
//
// literalIdentical was asymmetric: a pooled *Float rejected a non-*Float candidate,
// but a pooled *BigFloat fell through to EqualTo, and BigFloat(1.0).EqualTo(Float(1.0))
// is true. An unrelated #m1.0 elsewhere in the same body therefore re-typed every equal
// float literal, changing its arithmetic.
//
// Assert the stored TYPE, not an arithmetic result. BigFloat.Divide's sign bug is what
// made this observable as (begin #m1.0 (/ 1.0 -0.0)) => +inf.0; testing that symptom
// would let THIS bug hide behind THAT fix.
func TestLiteralPoolDoesNotRetypeAcrossFloatKinds(t *testing.T) {
	c := qt.New(t)

	c.Run("BigFloat pooled first does not swallow an equal Float", func(c *qt.C) {
		tpl := NewNativeTemplate(0, 0, false)

		bigIdx := tpl.MaybeAppendLiteral(values.NewBigFloatFromFloat64(1.0))
		floatIdx := tpl.MaybeAppendLiteral(values.NewFloat(1.0))

		c.Assert(floatIdx, qt.Not(qt.Equals), bigIdx,
			qt.Commentf("Float 1.0 must not dedup onto the BigFloat 1.0 slot"))

		stored := tpl.Literals()[floatIdx]
		_, isFloat := stored.(*values.Float)
		c.Assert(isFloat, qt.IsTrue, qt.Commentf("pool re-typed the literal to %T", stored))
	})

	c.Run("Float pooled first does not swallow an equal BigFloat", func(c *qt.C) {
		tpl := NewNativeTemplate(0, 0, false)

		floatIdx := tpl.MaybeAppendLiteral(values.NewFloat(1.0))
		bigIdx := tpl.MaybeAppendLiteral(values.NewBigFloatFromFloat64(1.0))

		c.Assert(bigIdx, qt.Not(qt.Equals), floatIdx)

		stored := tpl.Literals()[bigIdx]
		_, isBig := stored.(*values.BigFloat)
		c.Assert(isBig, qt.IsTrue, qt.Commentf("pool re-typed the literal to %T", stored))
	})

	c.Run("identical Floats still dedup", func(c *qt.C) {
		tpl := NewNativeTemplate(0, 0, false)
		a := tpl.MaybeAppendLiteral(values.NewFloat(2.5))
		b := tpl.MaybeAppendLiteral(values.NewFloat(2.5))
		c.Assert(a, qt.Equals, b, qt.Commentf("dedup must still work within a kind"))
	})

	c.Run("+0.0 and -0.0 stay distinct", func(c *qt.C) {
		tpl := NewNativeTemplate(0, 0, false)
		pos := tpl.MaybeAppendLiteral(values.NewFloat(0))
		neg := tpl.MaybeAppendLiteral(values.NewFloat(math.Copysign(0, -1)))
		c.Assert(neg, qt.Not(qt.Equals), pos, qt.Commentf("signed zeros must not merge"))
	})
}

// TestNativeTemplate_MaybeAppendLiteral_GlobalIndexEnvNotDeduped asserts that a
// library-pinned GlobalIndex and an Env==nil GlobalIndex for the same symbol
// occupy DISTINCT literal-pool slots.
//
// GlobalIndex is not values.Hashable, so MaybeAppendLiteral takes the linear
// fallback → literalIdentical → GlobalIndex.EqualTo, which compares only Index.
// Collapsing the two slots silently retargets a load or a store at runtime: a
// user (define helper 1) writes into a library's private binding, or a macro's
// expansion reads the user's. Which one breaks depends on emission order.
func TestNativeTemplate_MaybeAppendLiteral_GlobalIndexEnvNotDeduped(t *testing.T) {
	c := qt.New(t)

	tmpl := NewNativeTemplate(0, 0, false)
	sym := values.NewSymbol("helper")
	libraryFrame := environment.NewNamespace().Runtime().GlobalEnvironment()

	libraryLoad := &environment.GlobalIndex{Index: sym, Env: libraryFrame}
	userStore := environment.NewGlobalIndex(sym)

	libraryIdx := tmpl.MaybeAppendLiteral(libraryLoad)
	userIdx := tmpl.MaybeAppendLiteral(userStore)

	c.Assert(libraryIdx, qt.Not(qt.Equals), userIdx,
		qt.Commentf("a library-pinned GlobalIndex must not dedup onto an Env==nil one"))

	pooledLibrary, ok := tmpl.literals[libraryIdx].(*environment.GlobalIndex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pooledLibrary.Env, qt.Equals, libraryFrame,
		qt.Commentf("the library slot must keep its resolving frame"))

	pooledUser, ok := tmpl.literals[userIdx].(*environment.GlobalIndex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pooledUser.Env, qt.IsNil,
		qt.Commentf("the store slot must stay unpinned"))
}

// TestNativeTemplate_MaybeAppendLiteral_GlobalIndexSameEnvDedups pins the
// precision side: dedup must still collapse two GlobalIndex that agree on both
// symbol key and resolving frame, so the fix cannot be "never dedup".
func TestNativeTemplate_MaybeAppendLiteral_GlobalIndexSameEnvDedups(t *testing.T) {
	c := qt.New(t)

	tmpl := NewNativeTemplate(0, 0, false)
	frame := environment.NewNamespace().Runtime().GlobalEnvironment()

	first := &environment.GlobalIndex{Index: values.NewSymbol("helper"), Env: frame}
	second := &environment.GlobalIndex{Index: values.NewSymbol("helper"), Env: frame}

	c.Assert(tmpl.MaybeAppendLiteral(first), qt.Equals, tmpl.MaybeAppendLiteral(second),
		qt.Commentf("same symbol key, same frame: one pool slot"))

	unpinnedA := environment.NewGlobalIndex(values.NewSymbol("g"))
	unpinnedB := environment.NewGlobalIndex(values.NewSymbol("g"))

	c.Assert(tmpl.MaybeAppendLiteral(unpinnedA), qt.Equals, tmpl.MaybeAppendLiteral(unpinnedB),
		qt.Commentf("same symbol key, both unpinned: one pool slot"))
}
