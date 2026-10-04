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
// into the phase below it. A phase with no row has no sealed-write view; what
// that does and does not cause is the climb paragraph below.
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
//
// This array is the ONLY statement of which phases own a sealed-write view, and
// it is deliberately not summarized by a bound. The membership test is
// sealedViewAt's second return value; a `phase > ceiling` comparison answers the
// same question for 2 and up, says nothing at PhaseTemplate (-1), and would
// re-open phase 2 the moment the rows stopped being contiguous. All three of
// those are holes an `ok` test does not have, so the rows need no ordering
// premise and there is no derived ceiling constant to keep in step with them.
//
// Phases off the axis have no sealed-write view. (They can still hold sealed
// BINDINGS: an import at phase 2 or -1 installs its bindings sealed through the
// store, not through a view.) For phase 2 and up that is deliberate, and a row
// alone would not buy the D3 property (a macro's own compile-time helper is not
// user-overwritable); see the climb below. See TODO.md, "The seal is exactly one
// level deep".
//
// sealedViewAt's miss has two consumers, and only one of them is loud.
// SealedWriteViewAt refuses. AtPhase falls through to the mutable view, and it is
// a WRITE path: the compile-time writers reach their target phase through it,
// define-syntax, define-for-syntax, begin-for-syntax, eval-when (expand/compile)
// and a define in any transformer right-hand side via NextPhase, and meta via
// Expand(). AtPhase stays sealed only when the receiver IS a sealed-write view
// and the target is above it, on a row. So three exits are silent: a sealed view
// climbing off the axis; any lexical child of a sealed view, which does not
// inherit `sealed`, even onto a row (a bootstrap procedure body is one; a
// transformer body is a lexical child at phase 1, so its climb to phase 2 takes
// this exit and the off-axis one); and meta from the phase-1 sealed view, whose
// absolute phase 1 is no climb and resolves to the mutable view. All three are
// latent, and open in the TODO.md row cited above.
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
// phase: the owner's sealed-write view over its one store, whose writes land in
// the sealed tier.
//
// A phase OFF the sealed axis is REFUSED with a wrapped ErrNoSealedWriteView
// panic. There is no sealed-write view there, so the pre-2026-10-03 answer — the
// ordinary mutable view — was a silent downgrade at the one call whose whole job
// is to name a sealed target: a registration meant for the seal landed where user
// code can overwrite it and nothing said so. Unreachable from production today
// (every non-test caller passes a literal PhaseRuntime or PhaseExpand), which is
// what makes this a ratchet rather than a behaviour change.
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
// Two kinds of receiver are answered from the registry, and so refused off the
// axis: an owner root (the phase-0 entry of its own phase registry), and a
// sealed-write view, which shares its owner's registry. A sealed-write view was
// once routed through AtPhase instead, whose climb stays sealed only UPWARD and
// only onto a row, so it was handed a mutable view below its own phase and off
// the axis: the downgrade the refusal closes, reached through the other sealed
// receiver.
//
// Any other frame gets its own view at that phase and is refused nowhere. The
// precondition keeps a lambda body from being handed the owner's sealed-write
// view by accident: `phases` is inherited by every lexical child
// (NewEnvironmentFrameWithParent) and `sealed` is not, so testing `phases` alone
// would let any body write past the mutable tier. It is an accident guard, not a
// capability boundary: the store's exported writes (CreateGlobalBindingAt) take
// the sealed flag directly. Admitting sealed-write views does open one route
// AtPhase does not, a phase-1 sealed view reaching the phase-0 seal; no non-test
// caller passes a sealed-write view.
//
// AtPhase's climb is NOT covered, and it is a write path; see sealedAxis.
//
// The fallback resolves through the PhaseRegistry for any phase but the
// receiver's own and must not be called while holding the registry's lock.
func (p *EnvironmentFrame) SealedWriteViewAt(phase Phase) *EnvironmentFrame {
	if !p.ownsSealedAxis() && !p.sealed {
		return p.unsealedTargetAt(phase)
	}
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
