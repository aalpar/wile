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

package environment

import (
	"github.com/aalpar/wile/pkg/werr"
)

// The sealed axis: the phases that own a SEALED-WRITE view.
//
// A view's sealed flag says which tier its writes land in; the axis says which phases
// have such a view at all, and therefore where AtPhase's climb from a
// sealed-write view can still stay sealed.

// sealedAxis names the phases that own a sealed-write view, in construction
// order. Every owner's PhaseRegistry mints EVERY row (newPhaseRegistry); owners
// differ in what gets applied through those views, never in which phases they
// have.
//
// There is no kind column. What a binding IS is the binding's own BindingType,
// checked where it is reached (emitCachedBindingLoad's refusal,
// LookupPhaseBinding's tag filter); which frame holds it never carried that
// authority. Nor is there a placement left for a kind column to express:
// registry.Apply's phaseTargets (apply.go) takes this view for its expand-phase
// primitives, which therefore sit sealed alongside the bootstrap macros and the
// primitive expanders. (Apply reaches the phase-0 seal only through
// WithRuntimeTarget, which LoadBootstrapCore always supplies and a bare
// reg.Apply does not — see apply.go's phaseTargets bullet.)
//
// The rows are ONE FLAT set, not a hierarchy of phases: no phase ever resolves
// into the phase below it. Phases at or above 2 have no sealed-write view: a
// define-syntax inside a transformer body climbs off the sealed axis into the
// mutable phase-2 view.
//
// "Flat" rather than "ambient", deliberately, and the claim is about PHASE
// STRUCTURE: these are sealedAxis's phase rows, and no phase resolves into the
// one below it. It is not a claim about scope cardinality, and it is not a claim
// about bulk rows.
//
// The word was doing double duty here. The paragraph below retires the AMBIENT
// COORDINATE — the phase-blind (ANY, sealed) tier every phase once reached — so
// reading "ambient set" as that coordinate said the opposite of what the
// paragraph below says. A third, unrelated sense is still live elsewhere in this
// package: AmbientScopes names the EMPTY SCOPE SET, a hygiene concept, which
// Stage B fork D deliberately kept while renaming AmbientKeysAt → UnscopedKeysAt.
// This sentence is about none of that. Do not read it as saying the coordinate
// survives.
//
// The FIRST entry must be PhaseRuntime because that is where the base is
// written, not because the coordinate is special: writeCoordinates once mapped a
// sealed write there to a phase-blind ANY coordinate that every other phase
// reached, and that coordinate no longer exists. A sealed phase-0 write lands at
// (phase 0, sealed) like any other, and what carries it to another phase is a
// declared bulk row.
// This array is the ONLY statement of which phases own a sealed-write view, and
// it is deliberately not summarized by a bound. The membership test is
// sealedViewAt's second return value; a `phase > ceiling` comparison answers the
// same question for 2 and up, says nothing at PhaseTemplate (-1), and would
// re-open phase 2 the moment the rows stopped being contiguous. All three of
// those are holes an `ok` test does not have, so the rows need no ordering
// premise and there is no derived ceiling constant to keep in step with them.
//
// Phases off the axis have no sealed tier at all. For phase 2 and up that is
// deliberate: adding a row buys the D3 property (a macro's own compile-time
// helper is not user-overwritable) only once a bootstrap macro has a PROCEDURAL
// transformer body that defines a macro, and none does, so the row would be
// construction with no consumer and would force an unanswered question about the
// new frame's parent. See TODO.md, "The seal is exactly one level deep".
//
// What the axis DOES get is a direction-dependent answer to sealedViewAt's miss:
// a write refuses (SealedWriteViewAt), a read falls through (AtPhase's climb).
// Those are the function's only two consumers.
var sealedAxis = [...]Phase{PhaseRuntime, PhaseExpand}

// IsNamespaceRuntime reports whether this frame is its namespace's own runtime
// frame — the engine root or a profile child, as opposed to a library env's runtime
// frame from NewChildRuntime (which shares the root namespace) or any inner lexical
// frame.
//
// Use this only for that narrow question: pkg/wile asks it when deciding whether
// to re-register docs per import, and widening it there would re-run ApplyDocs on
// every import — wasted work, and a data race under concurrent SRFI-18 imports.
// The immutable-top-level define gate wants IsOwnerRoot instead, which also
// recognizes the sealed-write root.
func (p *EnvironmentFrame) IsNamespaceRuntime() bool {
	return p.namespace != nil && p.namespace.runtime == p
}

// SealedWriteViewAt returns the view sealed registrations write through at this
// phase: this owner's sealed-write view over its one store, whose writes land in
// the sealed tier.
//
// An owner root asking for a phase OFF the sealed axis is REFUSED with a wrapped
// ErrNoSealedWriteView panic. There is no sealed tier there, so the
// pre-2026-10-03 answer — the ordinary mutable view — was a silent downgrade at
// the one call whose whole job is to name a sealed target: a registration meant
// for the seal landed where user code can overwrite it and nothing said so.
// Unreachable from production today (every non-test caller passes a literal
// PhaseRuntime or PhaseExpand), which is what makes this a ratchet rather than a
// behaviour change.
//
// The condition is sealedViewAt's MISS, not an upper bound, and the difference is
// load-bearing in both directions: PhaseTemplate (-1) is a named phase below the
// first row and owns no sealed view, so a `phase > ceiling` test falls through
// there and reproduces the whole defect at a constant the package exports; and a
// non-contiguous axis would put a gap inside the bound. `ok` is the membership
// test already, and it is the complete refusal condition — sealedViewAt's own doc
// says a false answer means "this phase has no row", never "this owner skipped
// one".
//
// The refusal is the OWNER-ROOT path only, and that is the honest bound: the test
// sits inside the ownsSealedAxis arm, so a non-owner receiver still falls through
// to unsealedTargetAt at every phase, as its own paragraph below documents. A
// non-owner never had a sealed view to downgrade from, so there is nothing there
// to refuse.
//
// A READ off the axis keeps falling through — AtPhase's climb from a sealed view
// leaves the sealed axis above phase 1 rather than raising. Opposite polarity on
// the same sealedViewAt miss, because the questions differ: a read asking for a
// frame that does not exist can be answered with the one that does, a write
// cannot.
//
// The receiver must OWN a sealed axis — be the phase-0 entry of its own phase
// registry. Any other frame gets its own view at that phase instead. The
// precondition is what makes this a registration API rather than a way to obtain
// a sealed writer: `phases` is inherited by every lexical child
// (NewEnvironmentFrameWithParent), so testing it alone would let a lambda body
// hand out the owner's sealed-write view and write past the mutable tier. Rank
// living on the view rather than in the topology removed the frame identity this
// guard once protected; it did not remove the capability it gates.
//
// Restoring it costs no reach. A sealed-write view asking for a HIGHER phase
// falls through to unsealedTargetAt → AtPhase, whose climb from a sealed view
// stays sealed, so it lands on the same frame either way.
//
// The fallback resolves through the PhaseRegistry for any phase but the
// receiver's own and must not be called while holding the registry's lock.
func (p *EnvironmentFrame) SealedWriteViewAt(phase Phase) *EnvironmentFrame {
	if p.ownsSealedAxis() {
		sealed, ok := p.phases.sealedViewAt(phase)
		if !ok {
			panic(werr.WrapForeignErrorf(
				werr.ErrNoSealedWriteView,
				"SealedWriteViewAt: phase %d (%s) owns no sealed-write view; the sealed axis is %v",
				int8(phase), phase, sealedAxis,
			))
		}
		return sealed
	}
	return p.unsealedTargetAt(phase)
}

// ownsSealedAxis reports whether this frame is the phase-0 entry of its OWN phase
// registry — an owner root. True for a namespace's mutable runtime and for a
// library env's (each owns a registry whose phase-0 entry is itself); false for an
// inner lexical frame, which merely shares its parent's registry, and false for a
// sealed-write view, whose registry's phase-0 entry is the mutable root.
func (p *EnvironmentFrame) ownsSealedAxis() bool {
	return p.phases != nil && p.phases.runtime == p
}

// unsealedTargetAt returns this frame's own view at phase: itself when phase is
// its own level (reaching it through the registry would resolve a library env's
// phase 0 to the parent namespace's), else the phase view from the registry.
func (p *EnvironmentFrame) unsealedTargetAt(phase Phase) *EnvironmentFrame {
	if phase == p.phaseLevel {
		return p
	}
	return p.AtPhase(phase)
}
