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
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/registry/core"
)

// NoMutation is a dialect that derives from the R7RS baseline and removes the
// in-place mutation surface entirely: the set! special form (via InstallForms), ALL
// the mutation primitives (set-car!, vector-set!, string-set!, …, via the optional
// [PrimitiveRemover] capability), and — via [BootstrapProcedureRewriter] — the
// bootstrap's own use of them. The eager vector-map / string-map are built with
// vector-set! / string-set!; NoMutation swaps that bootstrap fragment for a
// mutation-free one (core.ImmutableVectorStringMapSource) so those two primitives can
// be removed with the rest, leaving a top level in which nothing can be destructively
// updated and vector-map / string-map still work (built functionally). The default
// engine keeps the faster mutating fragment — only a no-mutation engine pays the
// functional cost.
//
// Boundary: removal is at the visible top level only. The full registry still backs
// library environments, so a program can reach a mutator again via
// (import (scheme base)). NoMutation is therefore a *language-surface* statement, not
// a hard capability guarantee — the same honesty caveat [R7RSMinimal] carries for
// set!. For a runtime capability sandbox use security.Authorizer, which composes
// orthogonally with any dialect.
var NoMutation Dialect = noMutationDialect{}

// noMutationDialect is the concrete NoMutation — see NoMutation. It implements
// Dialect (removing the set! form), PrimitiveRemover (removing the mutation
// primitives), and BootstrapProcedureRewriter (swapping the mutation-dependent
// bootstrap fragment), so a single dialect object owns the whole mutation surface.
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

// RemovedPrimitives names the full in-place mutation primitive set to omit from the
// top level, satisfying [PrimitiveRemover]. All of them — including vector-set! and
// string-set! — because RewriteBootstrapProcedures removes the bootstrap's dependency
// on the latter two.
func (noMutationDialect) RemovedPrimitives() []string {
	return mutationPrimitives()
}

// RewriteBootstrapProcedures swaps the mutating vector-map/string-map fragment for
// the mutation-free one, satisfying [BootstrapProcedureRewriter]. This is what lets
// vector-set!/string-set! be removed: without it, the bootstrap's vector-map would
// fail to compile against the narrowed surface and NewEngine would error.
func (noMutationDialect) RewriteBootstrapProcedures(sources []string) []string {
	q := make([]string, len(sources))
	for i, src := range sources {
		if src == core.MutableVectorStringMapSource {
			q[i] = core.ImmutableVectorStringMapSource
			continue
		}
		q[i] = src
	}
	return q
}

// mutationPrimitives is the canonical in-place mutation primitive set — every
// destructive procedure registered by registry/core, all of which NoMutation removes.
// Returns a fresh slice each call so callers may retain or mutate the result without
// corrupting the set.
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
