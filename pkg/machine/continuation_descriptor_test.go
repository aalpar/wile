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

package machine

// continuation_descriptor_test.go holds the single reviewed source of truth for
// how each vmState / MachineContinuation field moves between a MachineContext and
// a MachineContinuation at the five save/restore/copy sites. It is DATA, not code
// that runs in production: the method bodies stay exactly as they are, and the
// oracle (continuation_descriptor_oracle_test.go) drives each real method against
// probe state and asserts the observed behavior matches this table.
//
// See plans/2026-07-02-continuation-vmstate-descriptor-oracle.md for the design
// and the per-site derivation. This supersedes the save/restore/copy rows of
// vmStateFieldCoverage (vm_state_test.go) — that map's cells are inert strings
// nothing checks; these rules are driven against the methods.
//
// It is test-only (D-a): reviewed in-test, no production dead-data or lint noise.

import (
	"reflect"

	"github.com/aalpar/wile/pkg/values"
)

// contOp identifies one of the five field-transcription sites.
type contOp int

const (
	// opSave is NewMachineContinuationFromMachineContext (machine_continuation.go),
	// mc → cont: the field copies that build a saved frame.
	opSave contOp = iota
	// opSaveWrap is SaveContinuation (machine_context_continuation.go), the wrapper
	// that calls opSave then does the inline-evals decision + marks=nil + callDepth++.
	// It is PARTIAL: it does not transcribe every field, so it is exempt from the
	// completeness ratchet and verified by explicit assertions in its oracle test.
	opSaveWrap
	// opRestore is Restore (call/cc re-entry), cont → mc; copies evals.
	opRestore
	// opRestoreAndRelease is RestoreAndRelease (normal return), cont → mc; branches
	// on shared and transfers/pools when unshared. The sharpest split.
	opRestoreAndRelease
	// opCopy is MachineContinuation.Copy (delimited clone), cont → cont.
	opCopy
)

func (op contOp) String() string {
	switch op {
	case opSave:
		return "Save"
	case opSaveWrap:
		return "SaveContinuation"
	case opRestore:
		return "Restore"
	case opRestoreAndRelease:
		return "RestoreAndRelease"
	case opCopy:
		return "Copy"
	}
	return "unknown"
}

// verb is the per-field discipline observed at a site. Small closed set → the
// classifier gets exhaustive-switch safety (D-a: verbs stay a typed enum).
type verb int

const (
	// verbShare: dest aliases src (pointer/value copy). SHARE.
	verbShare verb = iota
	// verbClone: deep, independent copy. CLONE.
	verbClone
	// verbTransfer: move ownership — dest takes src's referent AND src relinquishes
	// it (nil'd, or replaced by a fresh distinct object). TRANSFER.
	verbTransfer
	// verbInline: evals-specific inline-slot save/restore. INLINE.
	verbInline
	// verbReset: force zero value. RESET.
	verbReset
	// verbDerive: computed, not copied (callDepth±1, parent = mc.cont). DERIVE.
	verbDerive
	// verbOffset: pc + off (save only). OFFSET.
	verbOffset
	// verbSkip: dest left unchanged by this method. SKIP.
	verbSkip
)

func (v verb) String() string {
	switch v {
	case verbShare:
		return "SHARE"
	case verbClone:
		return "CLONE"
	case verbTransfer:
		return "TRANSFER"
	case verbInline:
		return "INLINE"
	case verbReset:
		return "RESET"
	case verbDerive:
		return "DERIVE"
	case verbOffset:
		return "OFFSET"
	case verbSkip:
		return "SKIP"
	}
	return "unknown"
}

// cond is one atomic frame predicate. A guard is the conjunction (AND) of a set of
// conds; the empty guard means ALWAYS. Splitting the shared×inline branching into
// explicit conds (rather than one flattened "conditional") forces the oracle to
// exercise both arms individually (D-b).
type cond int

const (
	// condShared: cont.shared is true (captured continuation frame).
	condShared cond = iota
	// condUnshared: cont.shared is false (the pool-and-transfer path).
	condUnshared
	// condInline: the eval stack fits inline (evals == nil sentinel; n ≤ inlineEvalsCap).
	condInline
	// condHeap: the eval stack is heap-backed (evals != nil; n > inlineEvalsCap).
	condHeap
	// condOldEnvPooledDistinct: mc's old env was pooled and differs from the restored env.
	condOldEnvPooledDistinct
	// condPresent: an optional field is populated (windingStack len > 0, in Copy).
	condPresent
	// condAbsent: the optional field is empty (windingStack len == 0, in Copy) — the
	// complementary arm to condPresent, so Copy's winding-absent branch is exercised
	// and the "exactly one active rule per field" invariant holds in both arms.
	condAbsent
)

// guard is a conjunction of conds. nil / empty means ALWAYS.
type guard []cond

// holds reports whether every cond in the guard is satisfied by cfg.
func (g guard) holds(cfg probeConfig) bool {
	for _, c := range g {
		if !c.holds(cfg) {
			return false
		}
	}
	return true
}

// holds reports whether a single cond is satisfied by cfg.
func (c cond) holds(cfg probeConfig) bool {
	switch c {
	case condShared:
		return cfg.shared
	case condUnshared:
		return !cfg.shared
	case condInline:
		return cfg.inline
	case condHeap:
		return !cfg.inline
	case condOldEnvPooledDistinct:
		return cfg.oldEnvPooledDistinct
	case condPresent:
		return cfg.windingPresent
	case condAbsent:
		return !cfg.windingPresent
	}
	// A cond that reaches here was added to the enum but not wired into holds; every
	// guard using it would silently never fire. Fail loud (test-only) rather than
	// let a field's discipline go unenforced.
	panic("continuation_descriptor: unhandled cond in holds")
}

// probeConfig captures the orthogonal boolean axes a probe can be built along. A
// rule's guard is evaluated against it to decide whether the rule is active for a
// given probe.
type probeConfig struct {
	shared               bool // cont.shared
	inline               bool // evals fits inline (true) vs heap (false)
	oldEnvPooledDistinct bool // mc's old env was pooled and differs from the restored env
	windingPresent       bool // windingStack is non-empty (Copy)
}

// rule declares one field's discipline under a guard. Field identity is the struct
// field NAME (string), reflect-derived, so descriptor / accessor map / struct share
// one namespace (D-a).
type rule struct {
	field string
	when  guard // nil = ALWAYS
	verb  verb
}

// effectVerb is a pool-release / GC-hygiene side effect. Kept a separate enum from
// verb so the field-classifier and effect-classifier each get exhaustive switches.
type effectVerb int

const (
	// effReleaseOldStack: releaseStack(oldEvals) — bumps StackPoolReleases.
	effReleaseOldStack effectVerb = iota
	// effReleaseOldEnv: releaseEnvFrame(oldEnv) (guarded) — bumps EnvFramePoolReleases.
	effReleaseOldEnv
	// effPoolFrame: releaseContinuation(cont) — bumps ContinuationPoolReleases.
	effPoolFrame
	// effBreakRefs: nil inline slots / cont.evals before pooling (GC hygiene).
	effBreakRefs
)

// sideEffect declares a pool/GC effect under a guard.
type sideEffect struct {
	when   guard
	effect effectVerb
}

// destKind records which struct a site writes into. It decides the oracle's SKIP
// semantics and which fields carry a real (verifiable) discipline vs a
// presence-only SKIP.
type destKind int

const (
	// destCont: the site writes into a MachineContinuation (opSave, opCopy). Both
	// vmState fields and continuation-only fields carry real disciplines.
	destCont destKind = iota
	// destMc: the site writes into a MachineContext's vmState (opRestore,
	// opRestoreAndRelease). Continuation-only fields have no destination
	// slot here, so they are declared SKIP for the ratchet and not value-checked.
	destMc
)

// siteSpec is one reviewed row of the descriptor.
type siteSpec struct {
	dest destKind
	// partial marks a wrapper site (opSaveWrap) that does not transcribe every
	// field; it is exempt from the completeness ratchet and verified by explicit
	// assertions in its oracle test rather than the generic field loop.
	partial bool
	fields  []rule
	effects []sideEffect
}

// always builds ALWAYS-guarded rules for a set of fields sharing a verb.
func always(v verb, fields ...string) []rule {
	q := make([]rule, len(fields))
	for i, f := range fields {
		q[i] = rule{field: f, verb: v}
	}
	return q
}

// guarded builds guard-conditioned rules for a set of fields sharing a verb.
func guarded(g guard, v verb, fields ...string) []rule {
	q := make([]rule, len(fields))
	for i, f := range fields {
		q[i] = rule{field: f, when: g, verb: v}
	}
	return q
}

// cat flattens rule groups into one slice.
func cat(groups ...[]rule) []rule {
	var q []rule
	for _, g := range groups {
		q = append(q, g...)
	}
	return q
}

// contDescriptor is THE reviewed table. Five entries. Transcribed field-by-field
// from the five method bodies; see the plan's "Filled descriptor" section for the
// per-field derivation and the drift-bait asymmetries (multiValues SHARE in Save
// but CLONE in Copy; marks SHARE in unshared-RAR but CLONE in the re-invocable
// paths; parent DERIVE on save/restore but SHARE in Copy).
//
// The mc.cont pointer derivation (p.cont = cont.parent on the restore paths; the
// new frame on SaveContinuation) is NOT a vmState/continuation field, so it is not
// a rule here — it is verified by an explicit assertion in each site's oracle test.
var contDescriptor = map[contOp]siteSpec{
	// Save — NewMachineContinuationFromMachineContext, mc → cont.
	opSave: {
		dest: destCont,
		fields: cat(
			always(verbShare, "env", "template", "singleValue", "multiValues",
				"evals", "threadID", "envPooled", "marks", "barrierValid"),
			always(verbOffset, "pc"),
			always(verbDerive, "callDepth", "parent"),
			// Oracle limit (plan): these six are not actively zeroed — the method
			// relies on acquireContinuation() returning a zeroed frame. The RESET
			// check here is observationally a SKIP off a zeroed-pool frame.
			always(verbReset, "windingStack", "promptTag", "promptHandler",
				"shared", "inlineEvals", "inlineEvalsLen"),
		),
	},

	// SaveWrap — SaveContinuation. PARTIAL and dual-sided (it post-processes mc AND
	// the produced cont: mc.marks=nil, mc.callDepth++, and the evals inline/heap
	// decision on the new frame). It does not transcribe every field in one
	// direction, so it carries NO field rules here — driving inert rules that nothing
	// checks would re-create the exact "strings that could lie" problem this table
	// exists to kill. Its discipline is verified by explicit assertions in
	// TestOracle_SaveContinuation (+ the ErrCallDepthExceeded guard test).
	opSaveWrap: {
		dest:    destMc,
		partial: true,
	},

	// Restore — cont → mc (call/cc re-entry). Copies evals.
	opRestore: {
		dest: destMc,
		fields: cat(
			always(verbShare, "env", "template", "pc", "callDepth", "barrierValid"),
			always(verbReset, "envPooled"), // never pool old env on re-entry
			always(verbClone, "marks"),
			always(verbSkip, "singleValue", "multiValues", "windingStack",
				"promptTag", "threadID",
				// continuation-only fields have no mc destination slot: SKIP.
				"parent", "promptHandler", "shared", "inlineEvals", "inlineEvalsLen"),
			guarded(guard{condInline}, verbInline, "evals"),
			guarded(guard{condHeap}, verbClone, "evals"),
		),
		effects: []sideEffect{
			{when: guard{condHeap}, effect: effReleaseOldStack},
		},
	},

	// RestoreAndRelease — cont → mc (normal return). The sharpest split.
	opRestoreAndRelease: {
		dest: destMc,
		fields: cat(
			always(verbShare, "env", "template", "pc", "callDepth", "barrierValid"),
			always(verbSkip, "singleValue", "multiValues", "windingStack",
				"promptTag", "threadID",
				"parent", "promptHandler", "shared", "inlineEvals", "inlineEvalsLen"),
			guarded(guard{condShared}, verbReset, "envPooled"),
			guarded(guard{condUnshared}, verbShare, "envPooled"),
			guarded(guard{condShared}, verbClone, "marks"),
			guarded(guard{condUnshared}, verbShare, "marks"),
			guarded(guard{condShared, condInline}, verbInline, "evals"),
			guarded(guard{condShared, condHeap}, verbClone, "evals"),
			guarded(guard{condUnshared, condInline}, verbInline, "evals"),
			guarded(guard{condUnshared, condHeap}, verbTransfer, "evals"),
		),
		effects: []sideEffect{
			{when: guard{condShared, condHeap}, effect: effReleaseOldStack},
			{when: guard{condUnshared, condHeap}, effect: effReleaseOldStack},
			{when: guard{condUnshared, condInline}, effect: effBreakRefs},
			{when: guard{condUnshared, condHeap}, effect: effBreakRefs},
			{when: guard{condUnshared}, effect: effPoolFrame},
			{when: guard{condUnshared, condOldEnvPooledDistinct}, effect: effReleaseOldEnv},
		},
	},

	// Copy — cont → cont (delimited clone). The only site touching
	// windingStack/promptTag/promptHandler; multiValues is CLONE here (SHARE elsewhere).
	opCopy: {
		dest: destCont,
		fields: cat(
			always(verbShare, "env", "template", "singleValue", "pc", "promptTag",
				"threadID", "callDepth", "barrierValid", "parent", "promptHandler",
				"inlineEvals", "inlineEvalsLen"),
			always(verbClone, "multiValues", "marks"),
			guarded(guard{condPresent}, verbClone, "windingStack"),
			guarded(guard{condAbsent}, verbSkip, "windingStack"), // src empty → cp.windingStack stays zero
			always(verbReset, "envPooled", "shared"),
			guarded(guard{condHeap}, verbClone, "evals"),
			guarded(guard{condInline}, verbSkip, "evals"), // stays nil; carried via inlineEvals SHARE
		),
	},
}

// fieldProbe observes one vmState field's value through direct (in-package) field
// access. reflect can enumerate field names but cannot read unexported values
// (Interface() panics; unsafe is banned), so values are read here (D-a).
//
// ref returns a comparable identity token (pointer, slice data-pointer, or scalar
// value) used for SHARE / TRANSFER / SKIP. perturb mutates the referent in place so
// CLONE (independent) can be told from SHARE (aliased); observe reads a sample that
// perturb changes. perturb/observe are nil for scalar/immutable fields. isZero
// reports the Go zero value for RESET.
type fieldProbe struct {
	ref     func(*vmState) any
	perturb func(*vmState)
	observe func(*vmState) any
	isZero  func(*vmState) bool
}

// contFieldProbe is the same, over the continuation-only fields (parent, shared,
// promptHandler, inlineEvals*), which live on *MachineContinuation, not vmState.
type contFieldProbe struct {
	ref    func(*MachineContinuation) any
	isZero func(*MachineContinuation) bool
}

// sliceIdent returns a slice's backing-array pointer as a comparable identity, or 0
// for a nil slice. Distinct backing ⟺ CLONE; identical ⟺ SHARE. reflect (not
// unsafe, which is banned) reads the data pointer without touching element values.
func sliceIdent[T any](s []T) uintptr {
	if s == nil {
		return 0
	}
	return reflect.ValueOf(s).Pointer()
}

// vmFieldProbes has one entry per vmState field. Its key set is cross-checked
// against reflect-enumerated vmState field names by the completeness ratchet.
var vmFieldProbes = map[string]fieldProbe{
	"env": {
		ref:    func(s *vmState) any { return s.env },
		isZero: func(s *vmState) bool { return s.env == nil },
	},
	"template": {
		ref:    func(s *vmState) any { return s.template },
		isZero: func(s *vmState) bool { return s.template == nil },
	},
	"singleValue": {
		ref:    func(s *vmState) any { return s.singleValue },
		isZero: func(s *vmState) bool { return s.singleValue == nil },
	},
	"multiValues": {
		ref: func(s *vmState) any { return sliceIdent(s.multiValues) },
		perturb: func(s *vmState) {
			if len(s.multiValues) > 0 {
				s.multiValues[0] = probeSentinel
			}
		},
		observe: func(s *vmState) any {
			if len(s.multiValues) > 0 {
				return s.multiValues[0]
			}
			return nil
		},
		isZero: func(s *vmState) bool { return s.multiValues == nil },
	},
	"evals": {
		ref: func(s *vmState) any { return s.evals },
		perturb: func(s *vmState) {
			if s.evals != nil {
				s.evals.Push(probeSentinel)
			}
		},
		observe: func(s *vmState) any {
			if s.evals == nil {
				return -1
			}
			return s.evals.Len()
		},
		isZero: func(s *vmState) bool { return s.evals == nil },
	},
	"pc": {
		ref:    func(s *vmState) any { return s.pc },
		isZero: func(s *vmState) bool { return s.pc == 0 },
	},
	"windingStack": {
		ref:    func(s *vmState) any { return sliceIdent(s.windingStack) },
		isZero: func(s *vmState) bool { return len(s.windingStack) == 0 },
	},
	"promptTag": {
		ref:    func(s *vmState) any { return s.promptTag },
		isZero: func(s *vmState) bool { return s.promptTag == nil },
	},
	"threadID": {
		ref:    func(s *vmState) any { return s.threadID },
		isZero: func(s *vmState) bool { return s.threadID == 0 },
	},
	"callDepth": {
		ref:    func(s *vmState) any { return s.callDepth },
		isZero: func(s *vmState) bool { return s.callDepth == 0 },
	},
	"envPooled": {
		ref:    func(s *vmState) any { return s.envPooled },
		isZero: func(s *vmState) bool { return !s.envPooled },
	},
	"marks": {
		ref: func(s *vmState) any { return sliceIdent(s.marks) },
		perturb: func(s *vmState) {
			if len(s.marks) > 0 {
				s.marks[0].key = probeSentinel
			}
		},
		observe: func(s *vmState) any {
			if len(s.marks) > 0 {
				return s.marks[0].key
			}
			return nil
		},
		isZero: func(s *vmState) bool { return s.marks == nil },
	},
	"barrierValid": {
		ref:    func(s *vmState) any { return s.barrierValid },
		isZero: func(s *vmState) bool { return s.barrierValid == nil },
	},
}

// contFieldProbes has one entry per continuation-only field. Cross-checked against
// reflect-enumerated MachineContinuation-only field names by the ratchet.
var contFieldProbes = map[string]contFieldProbe{
	"parent": {
		ref:    func(c *MachineContinuation) any { return c.parent },
		isZero: func(c *MachineContinuation) bool { return c.parent == nil },
	},
	"promptHandler": {
		ref:    func(c *MachineContinuation) any { return c.promptHandler },
		isZero: func(c *MachineContinuation) bool { return c.promptHandler == nil },
	},
	"shared": {
		ref:    func(c *MachineContinuation) any { return c.shared },
		isZero: func(c *MachineContinuation) bool { return !c.shared },
	},
	"inlineEvalsLen": {
		ref:    func(c *MachineContinuation) any { return c.inlineEvalsLen },
		isZero: func(c *MachineContinuation) bool { return c.inlineEvalsLen == 0 },
	},
	"inlineEvals": {
		ref:    func(c *MachineContinuation) any { return c.inlineEvals },
		isZero: func(c *MachineContinuation) bool { return c.inlineEvals == [inlineEvalsCap]values.Value{} },
	},
}

// probeSentinel is a distinguished value pushed/written during perturbation so
// CLONE (independent copy — sentinel does not appear) can be told from SHARE
// (aliased — sentinel appears).
var probeSentinel = values.NewInteger(918273645)
