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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestERRename_Basic(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	rename := compilation.NewERRenameClosure(env.Expand(), syntax.Scopes{}, syntax.NewScope())

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(rename, values.NewSymbol("if"))
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	c.Assert(result, qt.Not(qt.IsNil))

	// Result must be a SyntaxSymbol.
	ss, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(ss.Key(), qt.Equals, "if")
}

func TestERRename_CachesResults(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	rename := compilation.NewERRenameClosure(env.Expand(), syntax.Scopes{}, syntax.NewScope())

	// First call.
	mc1 := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc1.ApplyCallable(rename, values.NewSymbol("tmp"))
	c.Assert(err, qt.IsNil)
	result1 := mc1.GetValue()

	// Second call with same symbol key.
	mc2 := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err = mc2.ApplyCallable(rename, values.NewSymbol("tmp"))
	c.Assert(err, qt.IsNil)
	result2 := mc2.GetValue()

	// Must be pointer-equal (eq? contract).
	c.Assert(result1 == result2, qt.IsTrue, qt.Commentf("rename must cache: same symbol key must yield same pointer"))
}

func TestERRename_SyntaxSymbolInput(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	rename := compilation.NewERRenameClosure(env.Expand(), syntax.Scopes{}, syntax.NewScope())

	// Pass a SyntaxSymbol instead of a plain Symbol.
	sctx := syntax.NewSourceContext("", "", syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
	input := syntax.NewSyntaxSymbol("define", sctx)

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(rename, input)
	c.Assert(err, qt.IsNil)

	// An identifier comes back unchanged: its scopes are its meaning.
	c.Assert(mc.GetValue() == values.Value(input), qt.IsTrue)
}

// TestERRename_ConvertsADatum: a datum that is neither a symbol nor an
// identifier is converted to syntax (it used to raise ErrNotASymbol), each
// symbol in it renamed through the per-name cache, and an identifier inside it
// left as it is.
func TestERRename_ConvertsADatum(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	rename := compilation.NewERRenameClosure(env.Expand(), syntax.Scopes{}, syntax.NewScope())
	apply := func(arg values.Value) values.Value {
		mc := machine.NewMachineContext(
			context.Background(),
			machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
		)
		_, err := mc.ApplyCallable(rename, arg)
		c.Assert(err, qt.IsNil)
		return mc.GetValue()
	}

	atom, ok := apply(values.NewInteger(42)).(syntax.SyntaxValue)
	c.Assert(ok, qt.IsTrue)
	c.Assert(atom.UnwrapAll(), qt.DeepEquals, values.NewInteger(42))

	sctx := syntax.NewSourceContext("", "", syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
	id := syntax.NewSyntaxSymbol("tmp", sctx)
	pair, ok := apply(values.List(values.NewSymbol("tmp"), id)).(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pair.SyntaxCar() == apply(values.NewSymbol("tmp")), qt.IsTrue,
		qt.Commentf("a symbol in a datum must rename through the cache"))
	second, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(second.SyntaxCar() == syntax.SyntaxValue(id), qt.IsTrue,
		qt.Commentf("an identifier in a datum must come back unchanged"))
}

func TestERCompare_SameUnboundSymbol(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := compilation.NewERCompareClosure(env, syntax.Scopes{})

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(compare, values.NewSymbol("xyz"), values.NewSymbol("xyz"))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.TrueValue)
}

func TestERCompare_DifferentUnboundSymbols(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := compilation.NewERCompareClosure(env, syntax.Scopes{})

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(compare, values.NewSymbol("abc"), values.NewSymbol("def"))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.FalseValue)
}

func TestERCompare_BoundVsUnbound(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := compilation.NewERCompareClosure(env, syntax.Scopes{})

	// "if" is bound (registered as primitive expander), "nonexistent" is not.
	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(compare, values.NewSymbol("if"), values.NewSymbol("nonexistent"))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.FalseValue)
}

func TestERCompare_RejectsNonSymbol(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := compilation.NewERCompareClosure(env, syntax.Scopes{})

	mc := machine.NewMachineContext(
		context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env),
	)
	_, err := mc.ApplyCallable(compare, values.NewInteger(1), values.NewSymbol("x"))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "symbol")
}
