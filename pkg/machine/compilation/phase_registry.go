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

package compilation

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PhaseEntry represents a named item to register in a phase environment.
type PhaseEntry[F any] struct {
	Name string
	Fn   F
}

// RegisterPhaseBindings binds all entries in the target phase environment.
// This is a generic helper for registering primitives at the expand phase or
// the ambient tier.
//
// Parameters:
//   - phaseEnv: Accessor for the target phase (e.g., env.Expand, or a taproot
//     closure over env.SealedWriteViewAt(environment.PhaseRuntime) for the
//     ambient tier). This alone determines the target; env is not consulted.
//   - entries: Slice of (name, function) pairs to register
//   - wrapper: Creates the values.Value wrapper from name and function
func RegisterPhaseBindings[F any](
	env *environment.EnvironmentFrame,
	phaseEnv func() *environment.EnvironmentFrame,
	entries []PhaseEntry[F],
	wrapper func(name string, fn F) values.Value,
) error {
	targetEnv := phaseEnv()
	for _, entry := range entries {
		sym := values.NewSymbol(entry.Name)
		val := wrapper(entry.Name, entry.Fn)
		// DefineOwnGlobal creates and writes under one key at the target view's own
		// coordinates. Hand-building an index instead would build a bare-symbol one,
		// which over the merged store resolves wildcard to the name's first live slot
		// at ANY coordinates. Nothing here needs the pin it returns.
		_, err := targetEnv.DefineOwnGlobal(sym, environment.BindingTypePrimitive, nil, val)
		if err != nil {
			return werr.WrapForeignErrorf(err,
				"RegisterPhaseBindings: failed to bind %s", entry.Name)
		}
	}
	return nil
}

// LookupPhaseBinding looks up a binding by symbol in the target phase environment.
// Returns the value cast to type T if found, or the zero value if not found
// or if the value is not of type T.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
//
// The ranked answer wins whenever it type-checks, which is what keeps a
// legitimately renamed export of a phase row reachable. A wrong-typed
// BindingTypePrimitive winner is the one case that falls through to a second
// probe, and it is not hypothetical:
// a phase-shifted import installs at (phase N>0, MUTABLE) — installImportedBinding's
// shadowable arm is guarded on PhaseRuntime — while every phase row is written
// sealed at its phase (RegisterPhaseBindings above, through SealedWriteViewAt).
// tierExactMutable outranks tierExactSealed and both carry the empty scope set,
// so an (import (for-syntax (scheme base))) puts base's value-less compile-time
// keywords — let, if, lambda, quote, define-syntax — on top of the primitive
// expanders that share their names. Taking the ranked miss as final made `let`
// stop being a core form for the expander, which is the whole of the "a phase-1
// base import is not behaviour-neutral" defect.
//
// The sealed row is out-ranked, never overwritten, so the fallback is a tier
// floor rather than a repair: ask what the startup set bound this name to at
// this phase, the same question validate.go's unshadowed-head arm asks and the
// same reader (SealedBindingAt). A name the dialect never registered has no
// sealed row, so an omitted or Scheme-layer-replaced form cannot be resurrected
// by it.
//
// THE BINDING TYPE, not the failed assertion alone, is the discriminator, and
// the difference is load-bearing. A user (define define-syntax …) is a mutable
// same-phase BindingTypeVariable that also fails the assertion, and it MUST keep
// masking the compiler — TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler
// pins exactly that, and it is the ordinary meaning of shadowing. So the guard
// below is the pre-existing `!= BindingTypePrimitive` test, moved rather than
// removed: everything it used to reject it still rejects, and the ONLY new
// behaviour is that a Primitive-typed winner holding the wrong value looks past
// itself.
//
// BindingTypePrimitive is not a user denotation. Registries mint it (the phase
// rows here, the syntax compilers, the value-less keywords), user code writes
// Variable or Syntax, and an import merely copies whatever the exporting library
// had. So the rule reads: a REGISTRY row of this name, or an import of one,
// cannot demote the registry row it collides with; anything the user wrote can.
//
// Import provenance was the other candidate and it is too narrow: measured, the
// row that masks `syntax-rules` under WithSchemeSyntaxForms carries no Imported
// meta at all (it is the sealed Go *SyntaxCompiler, reached from a phase above),
// so an IsImported() gate fixes three of the defect's four columns and leaves the
// fourth silently red.
//
// This is a READER-side repair of a WRITER-side defect: the masking slot
// survives. There is still no safe writer coordinate — (phase 1, sealed) would
// land an imported macro on a bootstrap transformer's exact coordinates and
// overwrite it engine-wide (installImportedBinding's own doc says so) — but the
// gate stays for a stronger reason than that, settled 2026-09-10 and recorded
// here so it is not rediscovered as a mystery. It is NOT that the ranking is
// wrong.
//
// R7RS-small has no answer, structurally. Its <import set> grammar (§5.6) admits
// five forms — <library name>, only, except, prefix, rename — and the tokens
// for-syntax, for-meta, for-template, phase and meta level appear NOWHERE in the
// library section. for-syntax is R6RS/Racket. The collision is about a construct
// the standard does not have, so there is no conformance answer to appeal to.
//
// The two reference implementations disagree, and neither does what a rank
// shuffle would do. Chez (R6RS (for … expand)) is SILENT, first-listed-wins —
// measured non-vacuously, with a library exporting lambda as car. Racket REFUSES
// require-vs-require ("identifier already required for syntax") but SHADOWS
// require-vs-language, and (begin-for-syntax (define lambda 7)) is accepted. So
// Racket's hierarchy is definition > require > language, and the discriminator
// is WHAT KIND the collidee is, not what phase it sits at.
//
// Wile's collidee is the LANGUAGE at phase 1 — RegisterPhaseBindings' rows, the
// analogue of #lang, not of a require — so under Racket's own rule it is
// shadowable and the import out-ranking it is RIGHT. Racket's raise-already-bound
// answers require-vs-require, a different relation, and one Wile already ships
// (importConflicts).
//
// What is left is an IDENTITY defect, not a rank defect. An import supplies a
// DIFFERENT OBJECT for a re-exported name: the winner passes
// BindingType() == BindingTypePrimitive while its VALUE is not a
// *PrimitiveExpander, which is the whole reason a reader has to look past it.
// Each library environment mints its own *Binding per re-exported name (memory
// "library envs are primitive islands"; sameLiteralBinding in
// internal/match/syntax_adapter.go widens for the same fact). Sharing binding
// identity across the import edge, or giving the base its own store, is what
// makes this gate removable, and it is the only thing that does; filed in
// TODO.md. Until then the gate is the cheap statement of the same fact, and
// reverting it alone is measured: TODO.md carries the per-gate partition.
func LookupPhaseBinding[T any](
	phaseEnv *environment.EnvironmentFrame,
	sym *values.Symbol,
	scopes []*syntax.Scope,
) T {
	var zero T
	q := syntax.ScopesOf(scopes)
	bnd := phaseEnv.GetBinding(sym, q)
	if bnd == nil {
		return zero
	}
	if bnd.BindingType() == environment.BindingTypePrimitive {
		val, ok := bnd.Value().(T)
		if ok {
			return val
		}
	}
	// The winner is not a phase row of type T. Only a registry-minted row gets
	// looked past; a user's own shadow of the name stands.
	if bnd.BindingType() != environment.BindingTypePrimitive {
		return zero
	}
	// Probe the tier the row would have been written at. An identical pointer
	// means the sealed row IS the winner and already failed the assertion above,
	// so there is nothing underneath it.
	sealed := phaseEnv.GlobalEnvironment().SealedBindingAt(sym, q, phaseEnv.PhaseLevel())
	if sealed == nil || sealed == bnd {
		return zero
	}
	if sealed.BindingType() != environment.BindingTypePrimitive {
		return zero
	}
	val, ok := sealed.Value().(T)
	if !ok {
		return zero
	}
	return val
}
