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

// continuation_descriptor_oracle_test.go drives each real save/restore/copy method
// against probe state and asserts the observed per-field behavior matches
// contDescriptor. It is the entire delta over the pre-existing (inert) string map
// vmStateFieldCoverage: it turns a documented "transfer" into a checked claim, and
// catches drift (a SHARE that should be CLONE, a TRANSFER that forgot to relinquish
// the source, a release firing under the wrong guard).
//
// What it does NOT catch, by construction: spec-is-wrong and upstream-invariant-
// violated continuation bugs (the tail-frame-recycling / c1 reverts). The
// continuation red-suite + A/B /crosscheck remains the gate for any semantic change.

import (
	"errors"
	"reflect"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// saveOffset is the pc offset used when driving the save sites. Non-zero so the
// OFFSET verb (pc = src.pc + off) is distinguishable from a plain SHARE.
const saveOffset = 5

// sentinelParentDepth is the callDepth stamped on the sentinel mc.cont used by the
// save probe, so opSave's callDepth DERIVE (parent.callDepth + 1) is observable.
const sentinelParentDepth = 3

// probe bundles a fully-populated mc and cont. Every field of both carries a
// DISTINCT sentinel so the classifier can tell SHARE (identity) from CLONE (equal,
// distinct) from SKIP (unchanged) from RESET (zero).
type probe struct {
	mc   *MachineContext
	cont *MachineContinuation
}

// buildProbe wires a distinct pointer/value into every field of a fresh bare mc and
// a fresh pooled cont. The construction idiom is pinned against pool_test.go
// (TestRestoreAndRelease_*): a bare &MachineContext{} has p.pools == nil, so
// releaseStack/releaseEnvFrame/releaseContinuation fall back to the global pools and
// no threadPools wiring is needed; counters read absolute from mc.counters (fresh
// mc ⇒ each starts at 0).
//
//	evalsLen ≤ inlineEvalsCap (=2) → cfg.inline shape; > 2 → heap shape.
//	cfg.shared               → cont.shared = true (single-frame; MarkChainShared is
//	                           only for multi-frame chains).
//	cfg.oldEnvPooledDistinct → mc.env = acquireEnvFrame(); mc.envPooled = true; cont.env distinct.
func buildProbe(t *testing.T, cfg probeConfig) probe {
	t.Helper()

	mc := &MachineContext{}
	mc.evals = acquireStack()
	if cfg.inline {
		mc.evals.Push(values.NewInteger(9001))
	} else {
		mc.evals.Push(values.NewInteger(9001))
		mc.evals.Push(values.NewInteger(9002))
		mc.evals.Push(values.NewInteger(9003))
	}
	mc.env = &environment.EnvironmentFrame{}
	if cfg.oldEnvPooledDistinct {
		mc.env = acquireEnvFrame()
		mc.envPooled = true
	}
	mc.template = &NativeTemplate{}
	mc.singleValue = values.NewInteger(1001)
	mc.multiValues = MultipleValues{values.NewInteger(1002)}
	mc.pc = 11
	mc.windingStack = WindingStack{NewDynamicWindFrame(nil, nil)}
	mc.promptTag = NewPromptTag("mc")
	mc.threadID = 100
	mc.callDepth = 1
	mc.marks = []markEntry{{key: values.NewInteger(1003), val: values.NewInteger(1004)}}
	mc.barrierValid = NewBarrierToken()
	// A distinct sentinel parent, so opSave's parent DERIVE (= mc.cont) and the
	// restore paths' mc.cont = cont.parent are observable.
	mc.cont = &MachineContinuation{vmState: vmState{callDepth: sentinelParentDepth}}

	cont := acquireContinuation()
	cont.env = &environment.EnvironmentFrame{}
	cont.template = &NativeTemplate{}
	cont.singleValue = values.NewInteger(2001)
	cont.multiValues = MultipleValues{values.NewInteger(2002)}
	cont.pc = 22
	cont.windingStack = WindingStack{NewDynamicWindFrame(nil, nil)}
	cont.promptTag = NewPromptTag("cont")
	cont.threadID = 200
	cont.callDepth = 7
	cont.envPooled = true // sentinel: RAR-unshared SHAREs this onto mc.envPooled
	cont.marks = []markEntry{{key: values.NewInteger(2003), val: values.NewInteger(2004)}}
	cont.barrierValid = NewBarrierToken()
	cont.parent = &MachineContinuation{vmState: vmState{callDepth: 6}}
	cont.promptHandler = &MachineClosure{}
	cont.shared = cfg.shared

	if cfg.inline {
		cont.evals = nil
		cont.inlineEvals[0] = values.NewInteger(2101)
		cont.inlineEvalsLen = 1
	} else {
		s := acquireStack()
		s.Push(values.NewInteger(2101))
		s.Push(values.NewInteger(2102))
		s.Push(values.NewInteger(2103))
		cont.evals = s
		cont.inlineEvalsLen = 0
	}

	return probe{mc: mc, cont: cont}
}

// siteCall bundles everything the classifier needs to check one invocation: the
// post-call states, pre-call reference snapshots, and the DERIVE inputs.
type siteCall struct {
	op   contOp
	dest destKind

	mc       *MachineContext
	destVm   *vmState
	srcVm    *vmState
	destCont *MachineContinuation
	srcCont  *MachineContinuation

	preSrcVm   map[string]any
	preDestVm  map[string]any
	preSrcCont map[string]any

	destPreExists bool
	// srcAlive is false when the method pools/invalidates the src continuation
	// (RAR unshared), so perturb-based confirmation must be skipped — identity
	// snapshots taken before the call still verify SHARE/CLONE/TRANSFER.
	srcAlive   bool
	inlineSnap []values.Value

	off                int
	preMcPC            int
	preMcCallDepth     int
	preParentCallDepth int
	preMcCont          *MachineContinuation
}

// vmRefs snapshots the identity of every vmState field.
func vmRefs(s *vmState) map[string]any {
	q := make(map[string]any, len(vmFieldProbes))
	for name, fp := range vmFieldProbes {
		q[name] = fp.ref(s)
	}
	return q
}

// contRefs snapshots the identity of every continuation-only field.
func contRefs(c *MachineContinuation) map[string]any {
	q := make(map[string]any, len(contFieldProbes))
	for name, fp := range contFieldProbes {
		q[name] = fp.ref(c)
	}
	return q
}

// inlineSnapshot captures cont.inlineEvals[:inlineEvalsLen] before the call (the
// unshared restore paths nil these slots afterward).
func inlineSnapshot(c *MachineContinuation) []values.Value {
	q := make([]values.Value, c.inlineEvalsLen)
	for i := range q {
		q[i] = c.inlineEvals[i]
	}
	return q
}

// stackContents returns a copy of the stack's live values.
func stackContents(s *Stack) []values.Value {
	n := s.Len()
	q := make([]values.Value, n)
	for i := range n {
		q[i] = (*s)[i]
	}
	return q
}

// equalValues compares two value slices element-wise by interface identity.
func equalValues(a, b []values.Value) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// runChecks verifies every guard-active rule and side effect of the site.
func runChecks(t *testing.T, cfg probeConfig, sc siteCall) {
	t.Helper()
	spec := contDescriptor[sc.op]
	for _, r := range spec.fields {
		if !r.when.holds(cfg) {
			continue
		}
		checkField(t, sc, r)
	}
	verifyEffects(t, spec, cfg, sc)
}

// checkField dispatches one rule to the vmState or continuation-only classifier.
func checkField(t *testing.T, sc siteCall, r rule) {
	t.Helper()
	fp, ok := vmFieldProbes[r.field]
	if ok {
		checkVmField(t, sc, r, fp)
		return
	}
	// Continuation-only field. At an mc-destination site it has no slot to write —
	// the descriptor must declare it SKIP, and there is nothing to value-check.
	if sc.dest == destMc {
		if r.verb != verbSkip {
			t.Errorf("%s: continuation-only field %q at mc-dest site must be SKIP, got %s",
				sc.op, r.field, r.verb)
		}
		return
	}
	checkContField(t, sc, r, contFieldProbes[r.field])
}

// checkVmField is the SHARE/CLONE/TRANSFER/RESET/SKIP/INLINE/DERIVE classifier for a
// vmState field.
func checkVmField(t *testing.T, sc siteCall, r rule, fp fieldProbe) {
	t.Helper()
	switch r.verb {
	case verbShare:
		got := fp.ref(sc.destVm)
		want := sc.preSrcVm[r.field]
		if got != want {
			t.Errorf("%s.%s SHARE: dest %v != src %v", sc.op, r.field, got, want)
		}
		perturbCheck(t, sc, r, fp, true)
	case verbClone:
		if fp.ref(sc.destVm) == sc.preSrcVm[r.field] {
			t.Errorf("%s.%s CLONE: dest shares src backing", sc.op, r.field)
		}
		if fp.isZero(sc.destVm) {
			t.Errorf("%s.%s CLONE: dest is zero (nothing copied)", sc.op, r.field)
		}
		perturbCheck(t, sc, r, fp, false)
	case verbTransfer:
		if fp.ref(sc.destVm) != sc.preSrcVm[r.field] {
			t.Errorf("%s.%s TRANSFER: dest did not take src referent", sc.op, r.field)
		}
		if fp.ref(sc.srcVm) == sc.preSrcVm[r.field] {
			t.Errorf("%s.%s TRANSFER: src did not relinquish referent", sc.op, r.field)
		}
	case verbReset:
		if !fp.isZero(sc.destVm) {
			t.Errorf("%s.%s RESET: dest not zero", sc.op, r.field)
		}
	case verbSkip:
		if sc.destPreExists {
			if fp.ref(sc.destVm) != sc.preDestVm[r.field] {
				t.Errorf("%s.%s SKIP: dest changed", sc.op, r.field)
			}
			return
		}
		if !fp.isZero(sc.destVm) {
			t.Errorf("%s.%s SKIP: created dest not zero", sc.op, r.field)
		}
	case verbInline:
		checkInlineRestore(t, sc, r)
	case verbDerive, verbOffset:
		runDeriveCheck(t, sc, r)
	}
}

// checkContField is the classifier for a continuation-only field at a
// continuation-destination site (Save, Copy).
func checkContField(t *testing.T, sc siteCall, r rule, fp contFieldProbe) {
	t.Helper()
	switch r.verb {
	case verbShare:
		got := fp.ref(sc.destCont)
		want := sc.preSrcCont[r.field]
		if got != want {
			t.Errorf("%s.%s SHARE: dest %v != src %v", sc.op, r.field, got, want)
		}
	case verbReset:
		if !fp.isZero(sc.destCont) {
			t.Errorf("%s.%s RESET: dest not zero", sc.op, r.field)
		}
	case verbDerive:
		runDeriveCheck(t, sc, r)
	case verbSkip:
		if !fp.isZero(sc.destCont) {
			t.Errorf("%s.%s SKIP: created dest not zero", sc.op, r.field)
		}
	default:
		t.Errorf("%s.%s: unexpected verb %s for continuation-only field", sc.op, r.field, r.verb)
	}
}

// perturbCheck mutates the src referent and confirms the alias relationship: a
// SHARE'd field's dest reflects the change; a CLONE'd field's does not. Skipped when
// perturb is unavailable (scalar/immutable) or the src is no longer alive (pooled).
func perturbCheck(t *testing.T, sc siteCall, r rule, fp fieldProbe, share bool) {
	t.Helper()
	if fp.perturb == nil || !sc.srcAlive {
		return
	}
	before := fp.observe(sc.destVm)
	fp.perturb(sc.srcVm)
	after := fp.observe(sc.destVm)
	if share && before == after {
		t.Errorf("%s.%s SHARE: perturbing src did not affect dest (not aliased)", sc.op, r.field)
	}
	if !share && before != after {
		t.Errorf("%s.%s CLONE: perturbing src affected dest (not independent)", sc.op, r.field)
	}
}

// checkInlineRestore verifies a cont→mc INLINE: the restored mc.evals holds exactly
// the pre-call inline slot values.
func checkInlineRestore(t *testing.T, sc siteCall, r rule) {
	t.Helper()
	if sc.destVm.evals == nil {
		t.Errorf("%s.%s INLINE: dest evals is nil", sc.op, r.field)
		return
	}
	got := stackContents(sc.destVm.evals)
	if !equalValues(got, sc.inlineSnap) {
		t.Errorf("%s.%s INLINE: restored stack %v != inline snapshot %v", sc.op, r.field, got, sc.inlineSnap)
	}
}

// deriveCheck verifies a DERIVE/OFFSET field against its site-specific formula.
type deriveCheck func(t *testing.T, sc siteCall)

// deriveChecks registers the closed set of computed fields. A DERIVE/OFFSET rule
// with no registered check fails loudly rather than passing silently.
var deriveChecks = map[contOp]map[string]deriveCheck{
	opSave: {
		"pc": func(t *testing.T, sc siteCall) {
			want := sc.preMcPC + sc.off
			if sc.destCont.pc != want {
				t.Errorf("Save.pc OFFSET: got %d, want %d", sc.destCont.pc, want)
			}
		},
		"callDepth": func(t *testing.T, sc siteCall) {
			want := 0
			if sc.preMcCont != nil {
				want = sc.preParentCallDepth + 1
			}
			if sc.destCont.callDepth != want {
				t.Errorf("Save.callDepth DERIVE: got %d, want %d", sc.destCont.callDepth, want)
			}
		},
		"parent": func(t *testing.T, sc siteCall) {
			if sc.destCont.parent != sc.preMcCont {
				t.Errorf("Save.parent DERIVE: got %p, want %p", sc.destCont.parent, sc.preMcCont)
			}
		},
	},
	opPop: {
		"callDepth": func(t *testing.T, sc siteCall) {
			want := sc.preMcCallDepth - 1
			if sc.mc.callDepth != want {
				t.Errorf("Pop.callDepth DERIVE: got %d, want %d", sc.mc.callDepth, want)
			}
		},
	},
}

// runDeriveCheck dispatches to the registered formula, failing if none exists.
func runDeriveCheck(t *testing.T, sc siteCall, r rule) {
	t.Helper()
	byField, ok := deriveChecks[sc.op]
	if !ok {
		t.Fatalf("%s.%s: no derive checks registered for site", sc.op, r.field)
		return
	}
	fn, ok := byField[r.field]
	if !ok {
		t.Fatalf("%s.%s: no derive check registered for field", sc.op, r.field)
		return
	}
	fn(t, sc)
}

// verifyEffects asserts the pool/GC side effects fired exactly under their guards,
// read as absolute counters (fresh mc ⇒ each starts at 0).
func verifyEffects(t *testing.T, spec siteSpec, cfg probeConfig, sc siteCall) {
	t.Helper()
	var expStack, expCont, expEnv uint64
	breakRefs := false
	for _, e := range spec.effects {
		if !e.when.holds(cfg) {
			continue
		}
		switch e.effect {
		case effReleaseOldStack:
			expStack++
		case effPoolFrame:
			expCont++
		case effReleaseOldEnv:
			expEnv++
		case effBreakRefs:
			breakRefs = true
		}
	}
	if sc.mc != nil {
		if sc.mc.counters.StackPoolReleases != expStack {
			t.Errorf("%s: StackPoolReleases = %d, want %d", sc.op, sc.mc.counters.StackPoolReleases, expStack)
		}
		if sc.mc.counters.ContinuationPoolReleases != expCont {
			t.Errorf("%s: ContinuationPoolReleases = %d, want %d", sc.op, sc.mc.counters.ContinuationPoolReleases, expCont)
		}
		if sc.mc.counters.EnvFramePoolReleases != expEnv {
			t.Errorf("%s: EnvFramePoolReleases = %d, want %d", sc.op, sc.mc.counters.EnvFramePoolReleases, expEnv)
		}
	}
	if breakRefs {
		if sc.srcCont.evals != nil {
			t.Errorf("%s: BREAK_REFS: cont.evals not nil after pooling", sc.op)
		}
		for i := range inlineEvalsCap {
			if sc.srcCont.inlineEvals[i] != nil {
				t.Errorf("%s: BREAK_REFS: inline slot %d not nil after pooling", sc.op, i)
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Per-site oracle tests.
// ---------------------------------------------------------------------------

func TestOracle_Pop(t *testing.T) {
	for _, cfg := range []probeConfig{{inline: true}, {inline: false}} {
		t.Run(evalsShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			mc, cont := p.mc, p.cont
			mc.cont = cont
			mc.callDepth = 5

			preSrcVm := vmRefs(&cont.vmState)
			preDestVm := vmRefs(&mc.vmState)
			inlineSnap := inlineSnapshot(cont)
			preMcCallDepth := mc.callDepth
			preContParent := cont.parent

			got, err := mc.PopContinuation()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, cont)

			sc := siteCall{
				op: opPop, dest: destMc, mc: mc,
				destVm: &mc.vmState, srcVm: &cont.vmState, srcCont: cont,
				preSrcVm: preSrcVm, preDestVm: preDestVm, destPreExists: true,
				srcAlive: true, inlineSnap: inlineSnap, preMcCallDepth: preMcCallDepth,
			}
			runChecks(t, cfg, sc)
			qt.Assert(t, mc.cont, qt.Equals, preContParent)
		})
	}
}

func TestOracle_Restore(t *testing.T) {
	for _, cfg := range []probeConfig{{inline: true}, {inline: false}} {
		t.Run(evalsShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			mc, cont := p.mc, p.cont

			preSrcVm := vmRefs(&cont.vmState)
			preDestVm := vmRefs(&mc.vmState)
			inlineSnap := inlineSnapshot(cont)
			preContParent := cont.parent

			mc.Restore(cont)

			sc := siteCall{
				op: opRestore, dest: destMc, mc: mc,
				destVm: &mc.vmState, srcVm: &cont.vmState, srcCont: cont,
				preSrcVm: preSrcVm, preDestVm: preDestVm, destPreExists: true,
				srcAlive: true, inlineSnap: inlineSnap,
			}
			runChecks(t, cfg, sc)
			qt.Assert(t, mc.cont, qt.Equals, preContParent)
		})
	}
}

func TestOracle_RestoreAndRelease(t *testing.T) {
	cfgs := []probeConfig{
		{shared: true, inline: true},
		{shared: true, inline: false},
		{shared: false, inline: true},
		{shared: false, inline: false},
		{shared: false, inline: true, oldEnvPooledDistinct: true},
		{shared: false, inline: false, oldEnvPooledDistinct: true},
	}
	for _, cfg := range cfgs {
		t.Run(rarShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			mc, cont := p.mc, p.cont

			preSrcVm := vmRefs(&cont.vmState)
			preDestVm := vmRefs(&mc.vmState)
			inlineSnap := inlineSnapshot(cont)
			preContParent := cont.parent

			mc.RestoreAndRelease(cont)

			sc := siteCall{
				op: opRestoreAndRelease, dest: destMc, mc: mc,
				destVm: &mc.vmState, srcVm: &cont.vmState, srcCont: cont,
				preSrcVm: preSrcVm, preDestVm: preDestVm, destPreExists: true,
				srcAlive: cfg.shared, inlineSnap: inlineSnap,
			}
			runChecks(t, cfg, sc)
			qt.Assert(t, mc.cont, qt.Equals, preContParent)
			if cfg.shared {
				qt.Assert(t, mc.counters.SharedFrameRestores, qt.Equals, uint64(1))
			} else {
				qt.Assert(t, mc.counters.SharedFrameRestores, qt.Equals, uint64(0))
			}
		})
	}
}

func TestOracle_Save(t *testing.T) {
	for _, cfg := range []probeConfig{{inline: true}, {inline: false}} {
		t.Run(evalsShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			mc := p.mc

			preSrcVm := vmRefs(&mc.vmState)
			preMcPC := mc.pc
			preMcCont := mc.cont
			preParentCallDepth := 0
			if mc.cont != nil {
				preParentCallDepth = mc.cont.callDepth
			}

			got := NewMachineContinuationFromMachineContext(mc, saveOffset)

			sc := siteCall{
				op: opSave, dest: destCont, mc: mc,
				destVm: &got.vmState, srcVm: &mc.vmState, destCont: got, srcCont: got,
				preSrcVm: preSrcVm, destPreExists: false, srcAlive: true,
				off: saveOffset, preMcPC: preMcPC, preMcCont: preMcCont,
				preParentCallDepth: preParentCallDepth,
			}
			runChecks(t, cfg, sc)
		})
	}
}

func TestOracle_SaveContinuation(t *testing.T) {
	for _, cfg := range []probeConfig{{inline: true}, {inline: false}} {
		t.Run(evalsShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			mc := p.mc

			preCallDepth := mc.callDepth
			preMcCont := mc.cont
			preEvals := mc.evals
			preEvalContents := stackContents(mc.evals)

			err := mc.SaveContinuation(saveOffset)
			qt.Assert(t, err, qt.IsNil)

			// mc.cont is DERIVE: a fresh, distinct frame.
			newCont := mc.cont
			qt.Assert(t, newCont, qt.IsNotNil)
			qt.Assert(t, newCont != preMcCont, qt.IsTrue)
			// mc.marks RESET; mc.callDepth DERIVE (++); ContinuationsSaved bumped.
			qt.Assert(t, mc.marks, qt.IsNil)
			qt.Assert(t, mc.callDepth, qt.Equals, preCallDepth+1)
			qt.Assert(t, mc.counters.ContinuationsSaved, qt.Equals, uint64(1))

			if cfg.inline {
				qt.Assert(t, newCont.evals, qt.IsNil)
				qt.Assert(t, newCont.inlineEvalsLen, qt.Equals, uint8(len(preEvalContents)))
				inline := make([]values.Value, newCont.inlineEvalsLen)
				for i := range inline {
					inline[i] = newCont.inlineEvals[i]
				}
				qt.Assert(t, equalValues(inline, preEvalContents), qt.IsTrue)
				// INLINE reuses mc's stack, cleared for the callee.
				qt.Assert(t, mc.evals, qt.Equals, preEvals)
				qt.Assert(t, mc.evals.Len(), qt.Equals, 0)
				qt.Assert(t, mc.counters.InlineEvalsSaved, qt.Equals, uint64(1))
			} else {
				// TRANSFER: cont keeps the stack; mc re-acquires a fresh one.
				qt.Assert(t, newCont.evals, qt.Equals, preEvals)
				qt.Assert(t, mc.evals != preEvals, qt.IsTrue)
				qt.Assert(t, mc.evals.Len(), qt.Equals, 0)
			}
		})
	}
}

func TestOracle_SaveContinuation_CallDepthExceeded(t *testing.T) {
	p := buildProbe(t, probeConfig{inline: true})
	mc := p.mc
	mc.maxCallDepth = 1
	mc.callDepth = 1

	err := mc.SaveContinuation(saveOffset)
	qt.Assert(t, errors.Is(err, werr.ErrCallDepthExceeded), qt.IsTrue)
	// The failed save rolls callDepth back.
	qt.Assert(t, mc.callDepth, qt.Equals, 1)
}

func TestOracle_Copy(t *testing.T) {
	for _, cfg := range []probeConfig{
		{inline: true, windingPresent: true},
		{inline: false, windingPresent: true},
	} {
		t.Run(evalsShape(cfg), func(t *testing.T) {
			p := buildProbe(t, cfg)
			cont := p.cont

			preSrcVm := vmRefs(&cont.vmState)
			preSrcCont := contRefs(cont)

			cp := cont.Copy()

			sc := siteCall{
				op: opCopy, dest: destCont, mc: nil,
				destVm: &cp.vmState, srcVm: &cont.vmState, destCont: cp, srcCont: cont,
				preSrcVm: preSrcVm, preSrcCont: preSrcCont, destPreExists: false, srcAlive: true,
			}
			runChecks(t, cfg, sc)
		})
	}
}

// ---------------------------------------------------------------------------
// Completeness ratchet (extends testVmStateFieldCoverage, which walks only vmState).
// ---------------------------------------------------------------------------

func TestContDescriptor_Complete(t *testing.T) {
	vmFields := vmStateFieldNames()
	contFields := contOnlyFieldNames()
	union := append(append([]string{}, vmFields...), contFields...)
	known := make(map[string]bool, len(union))
	for _, f := range union {
		known[f] = true
	}

	// 1. Every non-partial site declares a rule for every field, and no rule names
	//    a field that no longer exists.
	for op, spec := range contDescriptor {
		if spec.partial {
			continue
		}
		covered := make(map[string]bool, len(spec.fields))
		for _, r := range spec.fields {
			covered[r.field] = true
			if !known[r.field] {
				t.Errorf("site %s declares a rule for unknown field %q — remove the stale rule", op, r.field)
			}
		}
		for _, f := range union {
			if !covered[f] {
				t.Errorf("site %s does not declare a discipline for field %q — add a rule or explicit SKIP", op, f)
			}
		}
	}

	// 2. One-namespace guard (D-a): probe-map keys == reflect-enumerated field names.
	assertKeySet(t, "vmFieldProbes", probeKeys(vmFieldProbes), vmFields)
	assertKeySet(t, "contFieldProbes", contProbeKeys(contFieldProbes), contFields)
}

// TestOracle_RejectsWrongDescriptor proves the oracle has teeth: a descriptor that
// lies about a field must be rejected. Pop SHAREs marks; asserting CLONE is a lie,
// and the classifier must fail on it. Guards against a no-op oracle that
// rubber-stamps everything.
func TestOracle_RejectsWrongDescriptor(t *testing.T) {
	cfg := probeConfig{inline: false}
	p := buildProbe(t, cfg)
	mc, cont := p.mc, p.cont
	mc.cont = cont
	mc.callDepth = 5

	preSrcVm := vmRefs(&cont.vmState)
	preDestVm := vmRefs(&mc.vmState)

	_, err := mc.PopContinuation()
	qt.Assert(t, err, qt.IsNil)

	sc := siteCall{
		op: opPop, dest: destMc, mc: mc,
		destVm: &mc.vmState, srcVm: &cont.vmState, srcCont: cont,
		preSrcVm: preSrcVm, preDestVm: preDestVm, destPreExists: true, srcAlive: true,
	}

	// The truth is SHARE; feed the classifier a CLONE lie and capture its verdict.
	lie := rule{field: "marks", verb: verbClone}
	spy := &testing.T{}
	checkField(spy, sc, lie)
	if !spy.Failed() {
		t.Errorf("oracle rubber-stamped a wrong descriptor: Pop marks is SHARE, but a CLONE claim was accepted")
	}
}

// ---------------------------------------------------------------------------
// Reflection + set helpers.
// ---------------------------------------------------------------------------

// vmStateFieldNames reflect-enumerates the vmState field names (metadata only; no
// unexported value reads).
func vmStateFieldNames() []string {
	typ := reflect.TypeFor[vmState]()
	q := make([]string, 0, typ.NumField())
	for i := range typ.NumField() {
		q = append(q, typ.Field(i).Name)
	}
	return q
}

// contOnlyFieldNames reflect-enumerates the MachineContinuation fields other than
// the embedded vmState.
func contOnlyFieldNames() []string {
	typ := reflect.TypeFor[MachineContinuation]()
	var q []string
	for i := range typ.NumField() {
		f := typ.Field(i)
		if f.Anonymous && f.Name == "vmState" {
			continue
		}
		q = append(q, f.Name)
	}
	return q
}

func probeKeys(m map[string]fieldProbe) []string {
	q := make([]string, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	return q
}

func contProbeKeys(m map[string]contFieldProbe) []string {
	q := make([]string, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	return q
}

// assertKeySet asserts two string sets are equal, reporting missing/extra keys.
func assertKeySet(t *testing.T, name string, got, want []string) {
	t.Helper()
	gotSet := make(map[string]bool, len(got))
	for _, k := range got {
		gotSet[k] = true
	}
	wantSet := make(map[string]bool, len(want))
	for _, k := range want {
		wantSet[k] = true
	}
	for k := range wantSet {
		if !gotSet[k] {
			t.Errorf("%s is missing a probe for field %q", name, k)
		}
	}
	for k := range gotSet {
		if !wantSet[k] {
			t.Errorf("%s has a probe for field %q that is not a struct field", name, k)
		}
	}
}

// evalsShape / rarShape name sub-tests by their probe configuration.
func evalsShape(cfg probeConfig) string {
	if cfg.inline {
		return "inline"
	}
	return "heap"
}

func rarShape(cfg probeConfig) string {
	q := "unshared"
	if cfg.shared {
		q = "shared"
	}
	q += "-" + evalsShape(cfg)
	if cfg.oldEnvPooledDistinct {
		q += "-oldenvpooled"
	}
	return q
}
