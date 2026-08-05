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

// The sealed axis: the immutable frames that run parallel to the phase frames.
// Each sealed frame is the lexical parent of the mutable frame at its phase, so
// a redefine shadows in the mutable child instead of overwriting the sealed slot
// in place.
//
// Sealing is a property of the phase alone — see sealedAxis below, which is the
// only place that decides.

// sealedAxis names the phases that own a seal, in construction order. Every
// owner of a sealed axis builds EVERY row (newSealedAxisFrames); the frames live
// per owner on its PhaseRegistry. Owners differ in what gets applied into their
// seals, never in which phases they seal.
//
// There is no kind column. What a binding IS is the binding's own
// BindingType, checked where it is reached (emitCachedBindingLoad's refusal,
// LookupPhaseBinding's tag filter); which frame holds it never carried that
// authority. The one placement the kind used to express — registry expand-phase
// primitives stay in the MUTABLE expand child — is encoded by its only writer,
// registry.Apply's phaseTargets (apply.go), which writes through env.Expand()
// and never consulted a seal.
//
// The rows are ONE ambient set, not a hierarchy of phases: no phase frame ever
// resolves into the phase below it. Phases at or above 2 have no seal: a
// define-syntax inside a transformer body climbs off the sealed axis into the
// mutable compile frame.
//
// The FIRST entry must be PhaseRuntime: it is the graph root (parent nil) and
// every later row's frame parents to it.
var sealedAxis = [...]Phase{PhaseRuntime, PhaseExpand}

// SealedBase returns this Namespace's immutable phase-0 frame: Go primitives and sealed
// stdlib runtime procedures, lexical parent of the mutable runtime global, and the root of
// the whole frame graph (parent nil). PER-NAMESPACE (NOT root-delegated, unlike
// immutableLiterals): each Namespace OWNS its sealed base so a profile child's curated apply
// does not write into the engine root's base. Report namespaces copy the parent's sealed base
// into their own (see NewSchemeReportNamespace).
func (p *Namespace) SealedBase() *EnvironmentFrame {
	return p.sealedBase
}

// SealedExpandBase returns this Namespace's immutable phase-1 frame: bootstrap macros and
// special-form primitive expanders, lexical parent of the mutable expand child. PER-NAMESPACE
// (like SealedBase), reached only via the parent chain, never a PhaseRegistry entry — which is
// why enumeration goes through SealedFrames rather than naming frames one at a time. See
// plans/2026-07-22-free-template-id-hygiene-impl.local.md (D1).
func (p *Namespace) SealedExpandBase() *EnvironmentFrame {
	return p.sealedExpandBase
}

// SealedAt returns this NAMESPACE's seal for the phase, and whether the phase
// has one. False means the mutable frame for the phase is the target — a modeled
// absence, not a failure. Callers that want the fallback applied for them use
// EnvironmentFrame.SealedTargetAt, which asks the FRAME's own registry and so
// answers correctly for a library env too.
func (p *Namespace) SealedAt(phase Phase) (*EnvironmentFrame, bool) {
	return p.phases.sealAt(phase)
}

// SealedFrames returns every distinct sealed frame this namespace owns, in phase
// order. Name enumeration and introspection must span these: they are reached only
// through the parent chain, never as PhaseRegistry entries, so a walk over phase
// frames alone misses every primitive, bootstrap procedure, and bootstrap macro.
func (p *Namespace) SealedFrames() []*EnvironmentFrame {
	q := make([]*EnvironmentFrame, 0, len(sealedAxis))
	for _, phase := range sealedAxis {
		frame, ok := p.phases.sealAt(phase)
		if !ok {
			continue
		}
		q = append(q, frame)
	}
	return q
}

// IsSealed reports whether frame is one of this namespace's sealed frames. A nil frame
// is never sealed. Note the receiver: this asks about the NAMESPACE's axis, so a
// library env's own seal answers false here and true on its own registry.
func (p *Namespace) IsSealed(frame *EnvironmentFrame) bool {
	return p.phases.isSeal(frame)
}

// IsNamespaceRuntime reports whether this frame is its namespace's own runtime
// frame — the engine root or a profile child, as opposed to a library env's runtime
// frame from NewChildRuntime (which shares the root namespace) or any inner lexical
// frame.
//
// It is NOT the sealed-routing discriminator — ownsSealedAxis is, and it answers
// true for a library runtime frame, which owns its own seal. Use this one only for
// the narrower "is this the namespace's own runtime?" question.
func (p *EnvironmentFrame) IsNamespaceRuntime() bool {
	return p.namespace != nil && p.namespace.runtime == p
}

// SealedTargetAt returns the frame that should RECEIVE a sealed binding of this
// phase when this frame is the registration target. A frame that owns its
// registry's sealed axis routes to the seal; any phase with no seal in that
// registry falls back to this frame's own frame at that phase — which is what leaves
// expand-phase primitives, and a library's primitive expanders, exactly where they were.
//
// The receiver is a registration target, so in practice a phase-0 frame; the
// fallback resolves through the PhaseRegistry for any other phase and must not be
// called while holding the registry's lock.
func (p *EnvironmentFrame) SealedTargetAt(phase Phase) *EnvironmentFrame {
	if p.ownsSealedAxis() {
		sealed, ok := p.phases.sealAt(phase)
		if ok {
			return sealed
		}
	}
	return p.unsealedTargetAt(phase)
}

// ownsSealedAxis reports whether this frame is the phase-0 entry of its own phase
// registry — the frame a sealed binding is routed AWAY from. True for a namespace's
// mutable runtime and for a library env's mutable runtime (each owns a registry whose
// phase-0 entry is itself); false for an inner lexical frame, which shares its parent's
// registry, and false for a seal, whose registry's phase-0 entry is the mutable child.
//
// This is the sealed-routing discriminator, replacing IsNamespaceRuntime. That method
// stays: pkg/wile asks the narrower "is this the namespace's own runtime, as opposed to
// a library env?" question when deciding whether to re-register docs per import, and
// widening it there would re-run ApplyDocs on every import — wasted work, and a data
// race under concurrent SRFI-18 imports.
func (p *EnvironmentFrame) ownsSealedAxis() bool {
	return p.phases != nil && p.phases.runtime == p
}

// unsealedTargetAt returns this frame's own frame at phase: itself when phase is
// its own level (a flat frame is its own phase-0 frame, and reaching it through
// the registry would resolve a child's phase 0 to the parent's), else the phase
// frame from the registry.
func (p *EnvironmentFrame) unsealedTargetAt(phase Phase) *EnvironmentFrame {
	if phase == p.phaseLevel {
		return p
	}
	return p.AtPhase(phase)
}
