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
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/values"
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
// *registry.PrimitiveRegistry and are invisible to InstallForms. A dialect that also
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

// BootstrapProcedureRewriter is an optional capability a Dialect may implement to
// substitute bootstrap procedure sources before they load into the sealed base. It
// is how a dialect crosses the last ceiling: a bootstrap procedure that depends on a
// primitive the dialect wants gone (the eager vector-map / string-map are built with
// vector-set! / string-set!). Removing the primitive alone breaks NewEngine — the
// bootstrap definition no longer compiles; a global reference resolves by slot at
// call time so it cannot be unbound afterward either. Rewriting the source is the
// clean fix.
//
// When a dialect passed to WithDialect also implements BootstrapProcedureRewriter,
// the engine calls RewriteBootstrapProcedures with the current procedure sources and
// binds the returned set instead (on a copy — the full registry that backs library
// environments is untouched). A no-mutation dialect swaps the mutating
// vector-map/string-map fragment for a mutation-free one, so the mutation primitives
// can be removed entirely rather than retained. See [NoMutation].
type BootstrapProcedureRewriter interface {
	// RewriteBootstrapProcedures receives the engine's bootstrap procedure sources
	// (in load order) and returns the set to bind instead. Return the input unchanged
	// to opt out. The result should preserve load order — later sources may reference
	// definitions from earlier ones.
	RewriteBootstrapProcedures(sources []string) []string
}

// WithDialect installs a Dialect on the engine, customizing its special-form
// surface at construction time. Last-wins if supplied more than once. A nil
// dialect (the default) applies [DefaultDialect], leaving the R7RS-default forms
// in place.
//
// Namespace-scoped: the forms registry the dialect writes to belongs to the
// namespace, so this option is namespace-consumed and cannot be passed to
// NewEngineWithNamespace — pass it to NewNamespace instead. If the dialect's
// InstallForms returns an error, whichever constructor performs the bootstrap
// (NewNamespace, or NewEngine when it builds the namespace itself) fails with
// that error wrapped in werr.ErrEngineInit.
func WithDialect(d Dialect) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.dialect = d
	})
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

// PhasedImport is one initial import a dialect declares: a library, and the
// phase the engine installs it AT.
//
// Phase is the INSTALL phase, not the source's. The two coincide for the default
// dialect and diverge under for-syntax, which is exactly why the field needs a
// name. A source's own phase lives in its BulkSource identity, one source per
// (store, phase) — see environment.BulkSource, whose interface deliberately
// takes no phase argument for the same reason.
type PhasedImport struct {
	// Library names the source. For the engine's own base this is
	// environment.BaseSourceName(), which is a reserved datum rather than an
	// importable library name: design section 4.1 declines to create a
	// (wile base) .sld, so origin and import denote different things.
	Library values.Value
	// Phase is where the row is INSTALLED in the importing store.
	Phase environment.Phase
}

// LanguageProvider is an optional capability a Dialect may implement to declare
// the engine's INITIAL IMPORTS: which library supplies which names, at which
// phase, before any user code runs.
//
// It crosses the ceiling that PrimitiveRemover and BootstrapProcedureRewriter
// left standing. Those two shape WHAT the base contains; this one states WHERE
// the base's names are visible from. Before it, the answer was "everywhere",
// because the sealed base occupied the ambient (ANY, sealed) coordinate that
// every phase's read reached. A declared initial import replaces that wildcard
// with a per-phase row, so the phase a name is visible at becomes a property of
// the dialect rather than of the store's coordinate space.
//
// When a dialect passed to WithDialect also implements LanguageProvider, the
// engine installs one bulk row per declaration into the namespace's store at
// engine origin, BEFORE the base is written. That ordering is safe, and load
// bearing: a bulk row is a live reference to the source store, not a snapshot,
// so a row installed at step 0 resolves the names steps 3 to 5 add afterwards.
// It is also what makes the import edge order-independent.
//
// Boundary: this declares the engine's own initial imports. A user's
// (import ...) is a separate mechanism reaching the same rows; a dialect that
// declares nothing gets today's behaviour, since the absent capability means
// "the default declaration", not "no imports".
type LanguageProvider interface {
	// InitialImports returns the dialect's declared initial imports. It should
	// return a fresh slice each call, as callers may retain it.
	InitialImports() []PhasedImport
}

// defaultInitialImports is the declaration every engine gets when its dialect
// does not implement LanguageProvider.
//
// One row: the base at phase 0, the whole of what the ambient tier used to
// supply, relocated onto one coordinate. The DECLARATIVE vocabulary, the names a
// syntax-rules macro needs without any import of its own, is not a declaration
// here: installInitialImports installs defaultMacroVocabulary at every macro
// phase whatever the dialect declares. That split is Racket's rule (i) and the
// reason a declarative macro survives Stage A untouched while a procedural one
// must declare (import (for-syntax (scheme base))).
//
// The vocabulary's membership is design section 9's Q2 and is pinned by
// TestPhase1VocabularyMembership rather than argued here. Two measurements
// constrain what the ratchet may claim, and both cut against the obvious
// justification: the ellipsis and underscore identifiers do NOT need a phase-1
// binding, because pkg/internal/match/syntax_compiler.go matches them by NAME
// (under WithoutAmbientBindings they hold zero slots anywhere and still expand);
// and syntax-rules holds an exact-phase-1 slot from the primitive-expander
// registration that deleting the ambient tier does not remove. So listing those
// three is right for a future scope-aware matcher and for stating the
// vocabulary explicitly, not because anything would break without it today.
func defaultInitialImports() []PhasedImport {
	return []PhasedImport{
		{Library: environment.BaseSourceName(), Phase: environment.PhaseRuntime},
	}
}

// MacroVocabularyName identifies the row a dialect declares at every macro
// phase, as distinct from the base row it declares at phase 0.
func MacroVocabularyName() values.Value {
	return values.NewSymbol("#%wile-macro-vocabulary")
}

// defaultMacroVocabulary is the set of base names visible from EVERY macro
// phase without an import of the writer's own.
//
// This is design section 9's Q2, and the split it draws is Racket's rule (i):
// the MACRO-WRITING kernel is ambient to a transformer, the RUNTIME library is
// not. A syntax-rules macro therefore needs no import — its template expands
// into the use site, which is phase 0 — while a procedural transformer that
// wants cadr must say (import (for-syntax (scheme base))). Widening this set is
// how D4's break gets quietly undone, so a name belongs here only if there is no
// import that could supply it.
//
// The membership was measured, not reasoned, by deleting the ambient tier and
// reading what a transformer body could no longer reach. Three groups, and each
// earns its place by having NO import route:
//
//   - the declarative vocabulary a syntax-rules macro's own definition needs;
//   - the bootstrap Scheme syntax layer's private helpers, which are definitions
//     in the base store itself and which no .sld exports or could export;
//   - the syntax-introspection primitives, which are registered at phase 1 and
//     which no library exports at any phase — measured, grep over
//     pkg/stdlib/lib finds no exporter of datum->syntax.
//
// Two names the Racket-derived presumption omits are here on measurement rather
// than by analogy: else and => are auxiliary keywords a pattern literal probe
// reaches at phase 1, and the ellipsis and underscore identifiers are here even
// though the Go matcher compares them by NAME (pkg/internal/match) and would
// work without them — the Scheme layer's own %ellipsis? compares them as
// bindings, so the two layers need them to agree.
//
// Built ONCE, into a package-level set. It was a per-call constructor, and
// macroVocabularyAdmits consults it per NAME LOOKUP while every library env
// rebuilt it at construction — a map plus sixty string keys, allocated on the
// resolution path. Measured as a large share of a +6% engine-startup
// regression. The map is read-only after init, so sharing it is safe.
var defaultMacroVocabularySet = buildDefaultMacroVocabulary()

// defaultMacroVocabulary returns the shared set.
func defaultMacroVocabulary() map[string]struct{} {
	return defaultMacroVocabularySet
}

func buildDefaultMacroVocabulary() map[string]struct{} {
	names := []string{
		// Declarative vocabulary.
		"syntax-rules", "...", "_", "else", "=>",
		// Syntax introspection: registered at phase 1, exported by nothing.
		"datum->syntax", "syntax->datum", "free-identifier=?", "bound-identifier=?",
		"syntax-local-value", "syntax-local-introduce", "syntax-local-identifier-as-binding",
		"er-macro-transformer", "quote-syntax", "syntax", "syntax-case", "with-syntax",
		// expand / expand-once exist ONLY to be called from a transformer body —
		// their argument is a syntax object and their result is the expansion — so
		// a phase-0-only registration would make them unusable at the one place
		// they are for. They come from the eval extension rather than the core
		// registry, which is why they are named here rather than inferred: a
		// vocabulary keyed on where a primitive was registered would miss them.
		"expand", "expand-once",
		"quasisyntax", "unsyntax", "unsyntax-splicing", "syntax-violation",
		"make-synthetic-identifier", "identifier?", "generate-temporaries",
		// Predicates and equality the macro layer's own bodies use.
		"procedure?", "symbol?", "pair?", "null?", "eq?", "eqv?", "equal?",
		"not", "car", "cdr", "cons", "list", "append", "length", "reverse",
		"apply", "error", "=", "+", "-",
	}
	q := make(map[string]struct{}, len(names))
	for _, n := range names {
		q[n] = struct{}{}
	}
	return q
}

// macroVocabularyAdmits reports whether a name belongs to the macro-writing
// kernel: an explicit member, or a %-prefixed bootstrap-private helper.
//
// The %-prefix arm is not a shortcut around enumerating. It is the tree's own
// marker for a definition that exists only to implement the syntax layer:
// bootstrap_syntax_procedures.scm alone defines 56 of them
// (%syntax-case-transform, %pattern-variable?, %syntax-map, %k-ellipsis ...),
// they live in the base store, and no .sld exports one or could — a %-name is
// unreachable by ANY import, so leaving it out would strand the Scheme syntax
// layer with no route at all rather than making it declare one. Enumerating 56
// names that move whenever that file does would be a ratchet on the wrong thing.
//
// It does not widen D4's break: a %-name is not something user code writes, and
// nothing a program can import is admitted by this arm.
func macroVocabularyAdmits(name string) bool {
	_, ok := defaultMacroVocabularySet[name]
	if ok {
		return true
	}
	return strings.HasPrefix(name, "%")
}

// initialImportsFor returns the declaration the engine should install for a
// dialect: the dialect's own if it provides them, else the default.
//
// An absent capability means "the default declaration", never "no imports" — a
// dialect that wanted an empty base would have to say so by returning an empty
// slice, and no dialect does.
func initialImportsFor(d Dialect) []PhasedImport {
	if d == nil {
		d = DefaultDialect
	}
	provider, ok := d.(LanguageProvider)
	if !ok {
		return defaultInitialImports()
	}
	return provider.InitialImports()
}
