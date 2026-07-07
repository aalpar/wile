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
)

// Dialect customizes an engine's special-form surface. At engine construction
// (via [WithDialect]) the engine forks a copy of the R7RS-default forms registry
// and hands it to InstallForms, which may add, remove, or rename special forms
// for that engine only. This is how a non-R7RS standard (a strict R5RS, R6RS, or
// a bespoke embedding dialect) plugs in like an extension — see the dialect
// roadmap in plans/ARCHITECTURE.local.md.
//
// The registry passed to InstallForms is a per-engine clone, copy-on-write over
// the shared default: mutating it (fr.Remove, fr.RegisterValidator,
// fr.RegisterCompiler, fr.Register) affects only the engine being built, never
// the default or any other engine. Since SP1 (per-engine codegen dispatch) a
// dialect can also install a form's compiler via fr.RegisterCompiler, so it may
// introduce forms with bespoke codegen — not only remove or rename existing ones.
//
// InstallForms should return a non-nil error (wrapped with a werr sentinel) to
// abort engine construction; the engine surfaces it wrapped in werr.ErrEngineInit.
//
// Boundary: InstallForms takes *forms.FormRegistry, which lives in the internal
// package pkg/internal/forms. Only code within this module (in-tree dialects such
// as the future R7RS/R6RS packages) can implement Dialect; an external embedder
// in another module cannot. A public embedder-facing dialect API is deferred to a
// later phase.
type Dialect interface {
	// Name identifies the dialect for diagnostics (e.g. "r6rs").
	Name() string

	// InstallForms customizes the per-engine forms registry. It runs once, at
	// engine origin, on a fresh clone of the R7RS default.
	InstallForms(fr *forms.FormRegistry) error
}

// PrimitiveRemover is an optional capability a Dialect may implement to shape the
// engine's surface beyond the forms layer: it names procedures to omit from the
// visible top level. The base Dialect interface is forms-only (InstallForms reaches
// the per-engine forms registry, which the validator and compiler read); the
// mutation procedures (set-car!, vector-set!, …) live in the separate per-engine
// *registry.Registry and are invisible to InstallForms. A dialect that also
// implements PrimitiveRemover crosses that ceiling.
//
// When a dialect passed to WithDialect also implements PrimitiveRemover, the engine
// omits the named primitives from the top-level binding set (via registry.Without,
// on a copy — the full registry is untouched). Referencing a removed name at the top
// level is then an unbound reference (werr.ErrNoSuchBinding), the same failure shape
// as a removed special form.
//
// Boundary: removal is at the *visible top level* only. The full registry still backs
// library environments, so (import (scheme base)) re-exposes a removed procedure. A
// PrimitiveRemover dialect is a *language-surface* statement (what the flat top level
// offers), NOT a capability sandbox — for the latter use security.Authorizer, which
// composes orthogonally. Airtight enforcement across the import surface is the
// expander-level dialect track. See [NoMutation].
type PrimitiveRemover interface {
	// RemovedPrimitives returns the names of procedures to omit from the engine's
	// top level. It should return a fresh slice each call (callers may retain it).
	RemovedPrimitives() []string
}

// WithDialect installs a Dialect on the engine, customizing its special-form
// surface at construction time. Last-wins if supplied more than once. A nil
// dialect (the default) applies [DefaultDialect], leaving the R7RS-default forms
// in place. If the dialect's InstallForms returns an error, NewEngine fails with
// that error wrapped in werr.ErrEngineInit.
func WithDialect(d Dialect) EngineOption {
	return func(cfg *engineConfig) {
		cfg.dialect = d
	}
}

// DefaultDialect is the R7RS baseline dialect, applied by every engine that does
// not supply its own via [WithDialect]. R7RS is thus a dialect like any other —
// the default one — not a hardcoded special case: engine construction always
// forks the R7RS-default forms registry and applies a dialect to it, uniformly.
//
// Its InstallForms is a no-op: the forked registry already carries the R7RS
// Tier-1 special forms (they are registered onto the package-default registry at
// init time, and the fork clones them). DefaultDialect names that baseline and
// gives derived dialects a base to start from — e.g. a mutation-free dialect that
// starts as DefaultDialect and removes set!.
var DefaultDialect Dialect = r7rsDialect{}

// r7rsDialect is the concrete DefaultDialect. Its InstallForms is a marker, not a
// mutator — see DefaultDialect.
type r7rsDialect struct{}

func (r7rsDialect) Name() string {
	return "r7rs"
}

func (r7rsDialect) InstallForms(_ *forms.FormRegistry) error {
	return nil
}
