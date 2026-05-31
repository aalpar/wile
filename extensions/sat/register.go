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
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

// Extension is the SAT solver FFI extension.
var Extension = registry.NewDescribedExtension("sat",
	"CDCL SAT solver kernel backing (wile algebra sat). Accepts CNF as a flat vector of int literals with 0-terminated clauses; returns SAT/UNSAT plus a model on SAT, or 'unknown on conflict-budget exhaustion or ctx cancellation.",
	AddToRegistry)

// Builder aggregates all sat registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all sat primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "sat-cnf-flat?",
			ParamCount: 2,
			Impl:       PrimSatCNFFlat,
			Doc:        "Decide CNF satisfiability over a flat literal vector with 0 terminators. Variables are 1..N (N inferred from max |lit|). Returns #t/#f/'unknown.\n\nParameters:\n  cnf : vector of exact integers\n  budget : exact integer or #f (unlimited)\nReturns: boolean or symbol\nCategory: algebra-sat\nKeywords: sat, cdcl, decide",
			ParamNames: []string{"cnf", "budget"},
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeAny},
			Category:   "algebra-sat",
			Keywords:   []string{"sat", "cdcl", "decide", "satisfiability"},
		},
		{
			Name:       "sat-cnf-flat-model",
			ParamCount: 0,
			Impl:       PrimSatCNFFlatModel,
			Doc:        "Return the model from the most recent sat-cnf-flat? call, or #f. Model is a vector indexed 1..N of #t/#f; index 0 unused.\n\nReturns: vector or #f\nCategory: algebra-sat\nKeywords: sat, model, witness",
			ParamNames: []string{},
			ParamTypes: []values.TypeConstraint{},
			Category:   "algebra-sat",
			Keywords:   []string{"sat", "model", "witness"},
		},
	}, registry.PhaseSetRuntime)
	return nil
}
