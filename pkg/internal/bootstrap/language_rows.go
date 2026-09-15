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

package bootstrap

import (
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// The language rows: what an owner's store sees before any import of its own.
// They live here rather than in pkg/wile so that both bootstrap sequences, this
// package's initializeEnvironmentWithRegistry and the engine's, install them
// from one definition. Only the engine installed them before, so an owner this
// package built (a (wile <profile>) environment, every testhelpers environment)
// had no vocabulary above phase 1.

// InstallBaseRow installs the sealed row through which owner's store reads the
// base at phase, named library.
//
// The two phases really are different here (D11). The SOURCE phase is
// PhaseRuntime, because that is where LoadBootstrapCore writes the base and where
// a base name therefore lives. The INSTALL phase is phase, which is where a
// reference may see it from. Using phase for both makes a phase-1 row look for
// base names at phase 1 and find nothing, so the row installs, ranks, and
// supplies nothing.
//
// The row carries the empty scope set and sealed=true: the base is not
// macro-introduced, and it is not user-writable. It is a live reference to the
// store, not a snapshot, so installing it before LoadBootstrapCore writes the
// base is the intended order.
func InstallBaseRow(owner *environment.EnvironmentFrame, library values.Value, phase environment.Phase) {
	store := owner.GlobalEnvironment()
	src := environment.NewSealedStoreBulkSource(store, environment.PhaseRuntime, library)
	store.InstallBulkRow(src, nil, phase, true, environment.BulkOriginLanguage)
}

// InstallMacroVocabularyRow declares the macro vocabulary at EVERY macro phase
// of owner's store, not at an enumerated few: the tower is lazy and unbounded,
// so the row is a template the store installs as each phase view appears. It is
// a strict subset of the base. Widening it to the whole base is how D4's
// phase-distinctness break gets quietly undone, because a procedural transformer
// would then reach cadr again with no import.
func InstallMacroVocabularyRow(owner *environment.EnvironmentFrame) {
	store := owner.GlobalEnvironment()
	vocab := environment.NewFilteredBulkSource(
		environment.NewSealedStoreBulkSource(store, environment.PhaseRuntime, MacroVocabularyName()),
		MacroVocabularyAdmits,
		MacroVocabulary(),
		MacroVocabularyName(),
	)
	store.InstallMacroPhaseRow(vocab, nil, true, environment.BulkOriginLanguage)
}

// installDefaultLanguageRows installs the default dialect's language rows: the
// base at phase 0 (pkg/wile's defaultInitialImports) and the macro vocabulary.
// This package builds its owners from a fixed registry with no dialect, so the
// default declaration is the only one it can mean.
func installDefaultLanguageRows(owner *environment.EnvironmentFrame) {
	InstallBaseRow(owner, environment.BaseSourceName(), environment.PhaseRuntime)
	InstallMacroVocabularyRow(owner)
}

// MacroVocabularyName identifies the row declared at every macro phase, as
// distinct from the base row declared at phase 0.
func MacroVocabularyName() values.Value {
	return values.NewSymbol("#%wile-macro-vocabulary")
}

// macroVocabularySet is the set of base names visible from EVERY macro
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
// MacroVocabularyAdmits consults it per NAME LOOKUP while every library env
// rebuilt it at construction — a map plus sixty string keys, allocated on the
// resolution path. Measured as a large share of a +6% engine-startup
// regression. The map is read-only after init, so sharing it is safe.
var macroVocabularySet = buildMacroVocabulary()

// MacroVocabulary returns the shared set. Callers must not modify it.
func MacroVocabulary() map[string]struct{} {
	return macroVocabularySet
}

func buildMacroVocabulary() map[string]struct{} {
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

// MacroVocabularyAdmits reports whether a name belongs to the macro-writing
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
func MacroVocabularyAdmits(name string) bool {
	_, ok := macroVocabularySet[name]
	if ok {
		return true
	}
	return strings.HasPrefix(name, "%")
}
