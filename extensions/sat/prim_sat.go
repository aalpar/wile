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

package sat

import (
	"context"
	"sync"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// symbolUnknown is the interned symbol returned when the solver exhausts its
// conflict budget or the context is cancelled.
var symbolUnknown = values.NewSymbol("unknown")

// PrimSatCNFFlat implements (sat-cnf-flat? vec budget).
//
// vec is a flat vector of exact integers in DIMACS CNF format (non-zero
// literals separated by 0 terminators). budget is either #f (unlimited) or an
// exact integer giving the maximum number of conflicts.
//
// Returns #t on SAT, #f on UNSAT, or the symbol 'unknown on budget/context
// exhaustion.
func PrimSatCNFFlat(mc machine.CallContext) error {
	const primName = "sat-cnf-flat?"

	vec, ok := mc.Arg(0).(*values.Vector)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%s: first argument must be a vector, got %T", primName, mc.Arg(0))
	}

	budget := int64(-1)
	a1 := mc.Arg(1)
	if values.ValueToBool(a1) {
		b, bOK := values.ExactInteger(a1)
		if !bOK {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
				"%s: budget must be #f or an exact integer, got %T", primName, a1)
		}
		budget = b
	}

	clauses, numVars, err := parseCNF(vec)
	if err != nil {
		return err
	}

	if len(clauses) == 0 {
		storeModel(mc, values.NewVector())
		mc.SetValue(values.TrueValue)
		return nil
	}

	ctx := mc.Context()
	if ctx == nil {
		ctx = context.Background()
	}

	s := newSolver(ctx, clauses, numVars, budget)
	res := s.solve()
	switch res {
	case resultSAT:
		model := make([]values.Value, numVars+1)
		model[0] = values.FalseValue
		for v := int32(1); v <= numVars; v++ {
			model[v] = values.BoolToBoolean(s.assigns[v] == 1)
		}
		storeModel(mc, values.NewVector(model...))
		mc.SetValue(values.TrueValue)
		return nil
	case resultUNSAT:
		storeModel(mc, nil)
		mc.SetValue(values.FalseValue)
		return nil
	case resultUNKNOWN:
		storeModel(mc, nil)
		mc.SetValue(symbolUnknown)
		return nil
	}

	return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"%s: solver returned unrecognized result %d", primName, res)
}

// PrimSatCNFFlatModel implements (sat-cnf-flat-model).
//
// Returns the model vector from the most recent sat-cnf-flat? call that
// returned #t, or #f if no model is available. The vector is indexed 1..N
// where N is the largest variable index seen; index 0 is unused (#f).
func PrimSatCNFFlatModel(mc machine.CallContext) error {
	m := loadModel(mc)
	if m == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(m)
	return nil
}

// modelStore holds the most recent SAT model for each Engine instance.
//
// Wile provides no built-in extension-state API on Namespace. The canonical
// per-Engine state handle that every CallContext can reach is the *Namespace
// returned by mc.EnvironmentFrame().Namespace(). We key this package-level
// sync.Map on that pointer so each Engine (== one root Namespace) gets its
// own model slot without any modification to Wile internals.
//
// Invariant: the stored value is always a *values.Vector (never nil in the
// map — absent key and nil pointer both map to "no model"). storeModel stores
// nil by deleting the key rather than mapping to nil.
var modelStore sync.Map // map[*Namespace]*values.Vector (Namespace is environment.Namespace)

func storeModel(mc machine.CallContext, v *values.Vector) {
	ns := mc.EnvironmentFrame().Namespace()
	if v == nil {
		modelStore.Delete(ns)
		return
	}
	modelStore.Store(ns, v)
}

func loadModel(mc machine.CallContext) *values.Vector {
	ns := mc.EnvironmentFrame().Namespace()
	raw, ok := modelStore.Load(ns)
	if !ok {
		return nil
	}
	return raw.(*values.Vector)
}
