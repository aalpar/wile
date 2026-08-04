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

package core

import (
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// addHashes registers the R6RS hash procedures. string-ci-hash is deliberately
// absent: it must agree with string-ci=?, which folds case with x/text's full
// Unicode fold, so it is registered beside that procedure in
// pkg/internal/extensions/all rather than restating the fold here.
func addHashes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "equal-hash", ParamCount: 1, Impl: PrimEqualHash,
			Doc:        "Returns an exact non-negative integer hash of OBJ, consistent with equal?: if (equal? a b) then (= (equal-hash a) (equal-hash b)). The converse does not hold. Terminates on cyclic input.\n\nHashes a BOUNDED PREFIX: values agreeing on their first 256 nodes hash alike, so two long lists differing only near the end collide. That is permitted by the one-directional contract and is what makes the hash terminate and cost O(1) on wide keys.\n\nExamples:\n  (= (equal-hash '(1 2)) (equal-hash (list 1 2)))  => #t",
			ParamNames: []string{"obj"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeInteger,
			Keywords: []string{"hash", "structural hash", "R6RS"},
			Identity: identityEqualHash},
		{Name: "string-hash", ParamCount: 1, Impl: PrimStringHash,
			Doc:        "Returns an exact non-negative integer hash of S, consistent with string=?. Unbounded; see (srfi 13) for the bounded variant.\n\nExamples:\n  (= (string-hash \"abc\") (string-hash \"abc\"))  => #t",
			ParamNames: []string{"s"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeString}, ReturnType: values.TypeInteger,
			Keywords: []string{"hash", "R6RS"}},
		{Name: "symbol-hash", ParamCount: 1, Impl: PrimSymbolHash,
			Doc:        "Returns an exact non-negative integer hash of SYM, consistent with symbol=?.\n\nExamples:\n  (= (symbol-hash 'foo) (symbol-hash 'foo))  => #t",
			ParamNames: []string{"sym"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeSymbol}, ReturnType: values.TypeInteger,
			Keywords: []string{"hash", "R6RS"}},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
