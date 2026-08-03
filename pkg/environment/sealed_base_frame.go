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

	"github.com/aalpar/wile/pkg/werr"
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
	// macro's transformer. Sealing these matters beyond immutability — a handler
	// on a phase-0 value frame is reachable by runtime value resolution, which
	// leaks a dialect-removed form's #<primitive-expander:…> into the value world.
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

// sealedAxis is the whole sealed model, as data: one row per phase that has a
// seal, naming which kinds that phase seals and which frame owns them. SealedAt
// and SealedFrames both read it, so a sealed phase cannot be reachable by
// routing yet invisible to enumeration.
//
// Phase 0 seals both kinds because its seal is also the graph root, so every
// frame reaches it through the parent chain; phase 1 seals handlers only, which
// is why expand-phase primitives land in the mutable expand child. Phases at or
// above 2 have no seal: a define-syntax inside a transformer body climbs off the
// sealed axis and into the mutable compile frame.
//
// ADDING A SEALED PHASE: add a row here, build the frame in wireRuntimeFrames,
// and decide the frame's lexical parent (the mutable axis forbids phase->phase
// parent edges; the sealed axis has one already, sealedExpandBase -> sealedBase).
var sealedAxis = [...]struct {
	phase Phase
	kinds sealKindSet
	frame func(*Namespace) *EnvironmentFrame
}{
	{PhaseRuntime, sealsValue | sealsHandler, (*Namespace).SealedBase},
	{PhaseExpand, sealsHandler, (*Namespace).SealedExpandBase},
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

// mustSeal asserts that a seal the model DECLARES actually exists. Returning nil here
// instead would be indistinguishable from "this pair has no seal", and every routing
// caller reads that as license to use the mutable frame — which silently lands a
// bootstrap macro or special-form expander somewhere a user can overwrite in place,
// surfacing far away as a dead let-syntax. A namespace built without wireRuntimeFrames
// must fail here, loudly, the way it did before the seals were table-driven.
func mustSeal(frame *EnvironmentFrame, phase Phase) *EnvironmentFrame {
	if frame == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrUnexpectedNil,
			"sealedAxis: a seal is declared at phase %s but the namespace's frame is nil", phase,
		))
	}
	return frame
}

// SealedAt returns the immutable frame owning bindings of this (phase, kind), and
// whether the pair is sealed at all. False means the mutable frame for the phase is
// the target — a modeled absence, not a failure. Callers that want the fallback
// applied for them use EnvironmentFrame.SealedTargetAt.
//
// Panics if the model declares a seal here and the namespace has no frame for it;
// see mustSeal.
func (p *Namespace) SealedAt(phase Phase, kind SealKind) (*EnvironmentFrame, bool) {
	for _, row := range sealedAxis {
		if row.phase != phase || !row.kinds.has(kind) {
			continue
		}
		return mustSeal(row.frame(p), phase), true
	}
	return nil, false
}

// sealedFrameAt returns the seal for a phase regardless of kind, and whether the phase
// has one. STRUCTURAL questions use this: a phase frame's lexical parent is one link,
// not one per kind, so asking SealedAt with an arbitrary kind would orphan a seal whose
// row happens not to cover that kind. Routing questions, which must not send a value to
// a handler-only seal, use SealedAt.
func (p *Namespace) sealedFrameAt(phase Phase) (*EnvironmentFrame, bool) {
	for _, row := range sealedAxis {
		if row.phase != phase {
			continue
		}
		return mustSeal(row.frame(p), phase), true
	}
	return nil, false
}

// SealedFrames returns every distinct sealed frame this namespace owns, in phase
// order. Name enumeration and introspection must span these: they are reached only
// through the parent chain, never as PhaseRegistry entries, so a walk over phase
// frames alone misses every primitive, bootstrap procedure, and bootstrap macro.
func (p *Namespace) SealedFrames() []*EnvironmentFrame {
	q := make([]*EnvironmentFrame, 0, len(sealedAxis))
	for _, row := range sealedAxis {
		q = append(q, mustSeal(row.frame(p), row.phase))
	}
	return q
}

// IsSealed reports whether frame is one of this namespace's sealed frames. A nil frame
// is never sealed — without the guard it would match a nil row frame, answering "yes,
// that is one of mine" about a frame that does not exist.
func (p *Namespace) IsSealed(frame *EnvironmentFrame) bool {
	if frame == nil {
		return false
	}
	for _, row := range sealedAxis {
		if row.frame(p) == frame {
			return true
		}
	}
	return false
}

// IsNamespaceRuntime reports whether this frame is its namespace's own runtime
// frame — the engine root or a profile child, as opposed to a flat library frame
// from NewChildRuntime (which shares the root namespace and owns no seal) or any
// inner lexical frame. It is the layered-vs-flat discriminator: only a namespace
// runtime frame routes sealed bindings away from itself.
func (p *EnvironmentFrame) IsNamespaceRuntime() bool {
	return p.namespace != nil && p.namespace.runtime == p
}

// SealedTargetAt returns the frame that should RECEIVE a sealed binding of this
// (phase, kind) when this frame is the registration target. A namespace runtime
// frame routes to the seal; a flat library frame, and any (phase, kind) with no
// seal, fall back to this frame's own frame at that phase — which is what leaves
// library environments and expand-phase primitives exactly where they were.
//
// The receiver is a registration target, so in practice a phase-0 frame; the
// fallback resolves through the PhaseRegistry for any other phase and must not be
// called while holding the registry's lock.
func (p *EnvironmentFrame) SealedTargetAt(phase Phase, kind SealKind) *EnvironmentFrame {
	if p.IsNamespaceRuntime() {
		sealed, ok := p.namespace.SealedAt(phase, kind)
		if ok {
			return sealed
		}
	}
	return p.unsealedTargetAt(phase)
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
