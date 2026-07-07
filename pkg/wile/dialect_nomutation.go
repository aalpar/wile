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

package wile

import (
	"slices"

	"github.com/aalpar/wile/pkg/internal/forms"
)

// NoMutation is a dialect that derives from the R7RS baseline and removes the
// in-place mutation surface from the top level: the set! special form (via
// InstallForms) and the mutation primitives (set-car!, vector-set!, string-set!,
// …, via the optional [PrimitiveRemover] capability). It is the first dialect to
// cross the forms-only ceiling — shaping both the forms registry and the primitive
// registry — delivering a top level in which (almost) nothing can be destructively
// updated.
//
// # Two documented retentions (vector-set!, string-set!)
//
// The eager bootstrap (registry/core's vector-map / string-map, loaded into the
// sealed base at engine construction) is itself implemented with vector-set! and
// string-set! — it builds a fresh result object by mutating it in place. Those two
// primitives therefore cannot be removed from the binding set without breaking
// NewEngine (a global reference resolves by slot at call time, so nothing can be
// unbound after bootstrap either). They are retained by necessity — the same
// precedent by which r7rs-minimal keeps its mutation primitives — and pinned by
// TestNoMutation_RetainedMutatorsAreBootstrapNecessary. See
// [bootstrapCoupledMutators]. The full conceptual set is [mutationPrimitives];
// NoMutation removes every member except the two bootstrap-coupled ones.
//
// # Boundary
//
// Removal is at the visible top level only. The full registry still backs library
// environments, so a program can reach any mutator (including the removed 11) again
// via (import (scheme base)). NoMutation is therefore a *language-surface* statement,
// not a hard capability guarantee — the same honesty caveat [R7RSMinimal] carries
// for set!. Making it airtight across the import surface is the expander-level
// dialect track. For a runtime capability sandbox use security.Authorizer, which
// composes orthogonally with any dialect.
var NoMutation Dialect = noMutationDialect{}

// noMutationDialect is the concrete NoMutation — see NoMutation. It implements both
// Dialect (removing the set! form) and PrimitiveRemover (removing the removable
// mutation primitives), so a single dialect object owns the whole mutation surface.
type noMutationDialect struct{}

func (noMutationDialect) Name() string {
	return "no-mutation"
}

// InstallForms removes the set! special form, mirroring r7rs-minimal. The
// mutation primitives are handled separately via RemovedPrimitives.
func (noMutationDialect) InstallForms(fr *forms.FormRegistry) error {
	fr.Remove("set!")
	return nil
}

// RemovedPrimitives names the mutation procedures to omit from the top level: the
// full canonical set minus the two the eager bootstrap depends on (which cannot be
// removed without breaking engine construction). Satisfies [PrimitiveRemover].
func (noMutationDialect) RemovedPrimitives() []string {
	coupled := bootstrapCoupledMutators()
	q := make([]string, 0, len(mutationPrimitives()))
	for _, name := range mutationPrimitives() {
		if !slices.Contains(coupled, name) {
			q = append(q, name)
		}
	}
	return q
}

// mutationPrimitives is the canonical in-place mutation primitive set — every
// destructive procedure registered by registry/core. It is the conceptual whole
// that NoMutation targets; RemovedPrimitives omits the bootstrap-coupled subset
// from it. Returns a fresh slice each call so callers may retain or mutate the
// result without corrupting the set.
//
// Scope: every bang-suffixed destructive procedure in registry/core, including the
// destination-mutating copy!/fill! variants (writing into an existing object is
// mutation; the non-destructive vector-copy / string-copy that allocate a fresh
// result are not here and stay).
func mutationPrimitives() []string {
	return []string{
		"set-car!",
		"set-cdr!",
		"vector-set!",
		"vector-fill!",
		"vector-copy!",
		"string-set!",
		"list-set!",
		"bytevector-u8-set!",
		"bytevector-copy!",
		"hashtable-set!",
		"hashtable-delete!",
		"hashtable-clear!",
		"set-box!",
	}
}

// bootstrapCoupledMutators is the subset of [mutationPrimitives] the eager bootstrap
// is itself built on — vector-map uses vector-set! and string-map uses string-set!
// to fill freshly-allocated results (registry/core bootstrap_procedures.scm). These
// are bound into the sealed base at construction, so removing them breaks NewEngine;
// NoMutation retains them by necessity. Returns a fresh slice each call.
//
// If those stdlib builders are ever rewritten without in-place mutation, this set
// can shrink — TestNoMutation_RetainedMutatorsAreBootstrapNecessary guards that each
// member here is genuinely load-bearing so the retention never silently over-broadens.
func bootstrapCoupledMutators() []string {
	return []string{
		"vector-set!",
		"string-set!",
	}
}
