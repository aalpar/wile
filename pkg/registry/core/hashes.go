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
// The quartet is therefore split across two files, and two of its registration
// fields split with it. Both were decided on 2026-10-03 and NEITHER is held;
// what makes that consistent rather than sloppy is that the thing held is
// REACHABILITY, by whichever mechanism each member's placement allows:
//
//   - PHASE SET follows the LAYER, not the family. These three are
//     PhaseSetRuntime|PhaseSetExpand because registry/core registers that way;
//     string-ci-hash is PhaseSetRuntime because nothing in
//     pkg/internal/extensions/all is registered at phase 1. A PhaseSet governs
//     AMBIENT availability alone, so the phase-1 gap is closed where it belongs,
//     by an export: (rnrs hashtables) exports string-ci-hash, and
//     (import (for-syntax (rnrs hashtables))) reaches it — the same route its
//     contract partner has via (import (for-syntax (scheme char))). The
//     registration was briefly widened instead and reverted the same day; see
//     pkg/stdlib/lib/rnrs/hashtables.sld's NOT EXPORTED note. Pinned by
//     TestR6RSHashProceduresAreReachableAtPhaseOne (pkg/wile), whose rows name
//     each member's route, because the routes differ on purpose.
//   - CATEGORY follows locality, and that split is ACCEPTED too. These three are
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
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
