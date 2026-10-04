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
//
// The quartet is therefore split across two files, and its Category field splits
// with it. Its PHASE SET does not:
//
//   - PHASE SET follows the FAMILY. All four register PhaseSetRuntime, so none
//     is ambient in a macro body, and (import (for-syntax (rnrs hashtables)))
//     reaches all four at phase 1. Needing that import is a trade, not a cost:
//     each environment chooses whether it includes the hashes, at the price of
//     verbosity and of every environment being built alike. These three were
//     PhaseSetRuntime|PhaseSetExpand, as most of registry/core is, until
//     2026-10-04. Pinned by TestR6RSHashProceduresAreReachableAtPhaseOne (the
//     route) and TestR6RSHashesAreNotAmbientAtPhaseOne (the absence), both in
//     pkg/wile.
//   - CATEGORY follows locality, and that split is ACCEPTED. These three are
//     "hashtables"; string-ci-hash is "strings". Category answers where a reader
//     looks and what an embedder attenuates by name, and for that one the honest
//     answer is "beside string-ci=?". So WithoutCategory("hashtables") removes
//     three of four, deliberately: the survivor is a pure function on strings
//     that needs no hashtable. Pinned by TestWithoutCategoryKeepsStringCiHash.
func addHashes(r *registry.PrimitiveRegistry) error {
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
	}, registry.PhaseSetRuntime)

	return nil
}
