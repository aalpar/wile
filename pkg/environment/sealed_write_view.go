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

// The sealed axis: the phases that own a SEALED-WRITE view.
//
// A view's rank says which tier its writes land in; the axis says which phases
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
// authority. The one placement the kind used to express — registry expand-phase
// primitives stay in the MUTABLE expand tier — is encoded by its only writer,
// registry.Apply's phaseTargets (apply.go), which writes through env.Expand()
// and never consulted a seal.
//
// The rows are ONE ambient set, not a hierarchy of phases: no phase ever
// resolves into the phase below it. Phases at or above 2 have no sealed-write
// view: a define-syntax inside a transformer body climbs off the sealed axis
// into the mutable phase-2 view.
//
// The FIRST entry must be PhaseRuntime: writeCoordinates maps a sealed write
// there to the AMBIENT coordinate, which is the set every other phase reaches.
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
// the sealed tier. A phase with no such view (2 and up) falls back to the
// receiver's own view at that phase, which is what leaves a library's primitive
// expanders exactly where they were.
//
// The receiver is a registration target, so in practice an owner root view; the
// fallback resolves through the PhaseRegistry for any other phase and must not be
// called while holding the registry's lock.
func (p *EnvironmentFrame) SealedWriteViewAt(phase Phase) *EnvironmentFrame {
	if p.phases != nil {
		sealed, ok := p.phases.sealedViewAt(phase)
		if ok {
			return sealed
		}
	}
	return p.unsealedTargetAt(phase)
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
