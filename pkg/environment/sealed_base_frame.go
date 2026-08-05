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
	"fmt"
)

// The sealed axis: the immutable frames that run parallel to the phase frames.
// Each sealed frame is the lexical parent of the mutable frame at its phase, so
// a redefine shadows in the mutable child instead of overwriting the sealed slot
// in place.
//
// Sealing is a property of the (phase, kind) PAIR, not of the phase alone — see
// sealedAxis below, which is the only place that decides.

// SealKind classifies what a binding is, which together with its phase selects
// the sealed frame that owns it.
type SealKind uint8

const (
	// SealKindValue is a binding reachable by ordinary value resolution:
	// a Go primitive or a bootstrap procedure.
	SealKindValue SealKind = iota

	// SealKindHandler is a binding consulted by the compiler or expander rather
	// than evaluated: a syntax compiler, a primitive expander, or a bootstrap
	// macro's transformer.
	//
	// The kind records which resolution path OUGHT to reach a binding, but it
	// currently DISCRIMINATES in exactly one cell. sealedAt asks whether the row
	// covers the kind and then returns sealAt(phase), which ignores the kind — and
	// the phase-0 row covers both. So the only pair where the argument changes the
	// answer is phase 1: handler lands in the phase-1 seal, value falls through to
	// the mutable expand child, which is what keeps a registry expand-phase
	// primitive out of the seal and lets a user define-syntax shadow a bootstrap
	// macro rather than share its frame.
	//
	// At phase 0 both kinds are the same frame, so a phase-0 handler IS reachable at
	// the frame-graph level by ordinary value resolution — GetBinding still finds
	// it. Passing SealKindHandler at phase 0 therefore states intent and routes
	// nowhere different.
	//
	// So do not read the kind as a reachability guarantee. Two other things carry
	// that: registering a primitive expander at phase 1 (where the expand chain is
	// off the phase-0 value path), and compilation's BindingType refusal —
	// emitCachedBindingLoad and tryResolvedBinding's pin arm both refuse any
	// non-Variable binding — which is why `(display define-syntax)` is a compile
	// error (ErrSyntacticKeywordAsVariable) rather than printing
	// #<syntax-compiler:define-syntax>.
	SealKindHandler
)

// String returns a human-readable name for the kind.
func (p SealKind) String() string {
	switch p {
	case SealKindValue:
		return "value"
	case SealKindHandler:
		return "handler"
	}
	return fmt.Sprintf("sealkind(%d)", uint8(p))
}

// sealKindSet is a bitset over SealKind, naming which kinds a phase seals.
type sealKindSet uint8

const (
	sealsValue   = sealKindSet(1) << SealKindValue
	sealsHandler = sealKindSet(1) << SealKindHandler
)

// has reports whether the set contains kind.
func (p sealKindSet) has(kind SealKind) bool {
	return p&(sealKindSet(1)<<kind) != 0
}

// sealedAxis is the sealed model, as data: one row per sealed phase, naming which
// kinds that phase seals. Every owner of a sealed axis builds EVERY row
// (newSealedAxisFrames), so this table describes a namespace and a library env
// alike; the frames themselves live per owner on its PhaseRegistry, because a
// library env deliberately shares its parent's Namespace and so cannot hang a seal
// there. Owners differ in what gets applied into their seals, never in which phases
// they seal — a per-owner subset would mean this table described only some of them.
//
// Phase 0 seals both kinds because its seal is also the graph root, so every
// frame reaches it through the parent chain; phase 1 seals handlers only, which
// is why expand-phase primitives land in the mutable expand child. Phases at or
// above 2 have no seal: a define-syntax inside a transformer body climbs off the
// sealed axis and into the mutable compile frame.
//
// The rows are ONE ambient set indexed by (phase, kind), not a hierarchy of
// phases. A later row's frame parents to the phase-0 seal because that is how the
// set is stitched together, not because phase N+1 inherits from phase N — no phase
// frame ever resolves into the phase below it, which is the whole point of the
// mutable axis's no-phase->phase-edge invariant.
//
// ADDING A SEALED PHASE: add a row here. newSealedAxisFrames builds it for every
// owner, so there is no second place to register it and no per-owner opt-in. A row
// needing a different lexical parent is a change to that builder, and to this
// comment.
//
// The FIRST row must be PhaseRuntime: it is the graph root (parent nil) and every
// later row hangs off it.
var sealedAxis = [...]struct {
	phase Phase
	kinds sealKindSet
}{
	{PhaseRuntime, sealsValue | sealsHandler},
	{PhaseExpand, sealsHandler},
}

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

// SealedAt returns the immutable frame owning bindings of this (phase, kind) in this
// NAMESPACE's registry, and whether the pair is sealed at all. False means the mutable
// frame for the phase is the target — a modeled absence, not a failure. Callers that
// want the fallback applied for them use EnvironmentFrame.SealedTargetAt, which asks
// the FRAME's own registry and so answers correctly for a library env too.
func (p *Namespace) SealedAt(phase Phase, kind SealKind) (*EnvironmentFrame, bool) {
	return p.phases.sealedAt(phase, kind)
}

// SealedFrames returns every distinct sealed frame this namespace owns, in phase
// order. Name enumeration and introspection must span these: they are reached only
// through the parent chain, never as PhaseRegistry entries, so a walk over phase
// frames alone misses every primitive, bootstrap procedure, and bootstrap macro.
func (p *Namespace) SealedFrames() []*EnvironmentFrame {
	q := make([]*EnvironmentFrame, 0, len(sealedAxis))
	for _, row := range sealedAxis {
		frame, ok := p.phases.sealAt(row.phase)
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
// (phase, kind) when this frame is the registration target. A frame that owns its
// registry's sealed axis routes to the seal; any (phase, kind) with no seal in that
// registry falls back to this frame's own frame at that phase — which is what leaves
// expand-phase primitives, and a library's primitive expanders, exactly where they were.
//
// The receiver is a registration target, so in practice a phase-0 frame; the
// fallback resolves through the PhaseRegistry for any other phase and must not be
// called while holding the registry's lock.
func (p *EnvironmentFrame) SealedTargetAt(phase Phase, kind SealKind) *EnvironmentFrame {
	if p.ownsSealedAxis() {
		sealed, ok := p.phases.sealedAt(phase, kind)
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
