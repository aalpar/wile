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

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"

	qt "github.com/frankban/quicktest"
)

// expectedPromotedPure and expectedPromotedMutators restate the partition
// independently of the descriptors' mutates flags. Restating it is the point: a
// promoted primitive added with the zero-value flag would otherwise be silently
// classified as pure and skip the immutability ratchet, which is exactly how the
// tail-position set-cdr! bypass survived — the guard existed, and nothing
// asserted it ran.
var expectedPromotedPure = map[string]struct{}{
	"eq?": {}, "vector?": {}, "vector-ref": {}, "null?": {}, "pair?": {},
	"car": {}, "cdr": {}, "cons": {}, "+": {}, "-": {}, "*": {}, "/": {},
	"<": {}, "<=": {}, ">": {}, ">=": {}, "=": {},
}

var expectedPromotedMutators = map[string]struct{}{
	"set-cdr!": {},
}

// TestPromotedOpsAreClassified pins each descriptor's mutates flag against the
// expected partition, and pins the partition against the table's size so a new
// promoted primitive cannot be added without being classified here.
func TestPromotedOpsAreClassified(t *testing.T) {
	for _, op := range promotedOps {
		_, pure := expectedPromotedPure[op.name]
		_, mutator := expectedPromotedMutators[op.name]
		if pure == mutator {
			t.Errorf("promoted op %q must be listed in exactly one of expectedPromotedPure/expectedPromotedMutators (pure=%v, mutator=%v)",
				op.name, pure, mutator)
			continue
		}
		if op.mutates != mutator {
			t.Errorf("promoted op %q has mutates=%v, expected %v", op.name, op.mutates, mutator)
		}
	}
	known := len(expectedPromotedPure) + len(expectedPromotedMutators)
	if known != len(promotedOps) {
		t.Errorf("classification lists name %d ops, promotedOps holds %d", known, len(promotedOps))
	}
	names := PromotedMutatorNames()
	if len(names) != len(expectedPromotedMutators) {
		t.Errorf("PromotedMutatorNames returned %v, expected %d entries", names, len(expectedPromotedMutators))
	}
}

// TestOpReleaseEnvFrameKeepsEngineStateReachable asserts the released-frame
// condition DIRECTLY, not through a raised error. That distinction is
// load-bearing: once an aggregate carries its own immutability flag, a guard
// reads the value and never touches mc.env, so a behavioural test would pass
// while the namespace-loss class went unexercised.
//
// OpReleaseEnvFrame is the one release site that leaves mc.env pointing at a
// frame it just handed to the pool; ResetForPool zeroes that frame's namespace
// back-pointer. Both consumers of the namespace fail OPEN on nil —
// ImmutableLiterals.IsImmutable returns false on a nil receiver, and
// security.CheckWithAuthorizer treats a nil authorizer as allow — so losing the
// namespace here silently disarms both.
func TestOpReleaseEnvFrameKeepsEngineStateReachable(t *testing.T) {
	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.ConsoleAuthorizer())
	root := ns.Runtime()
	qt.Assert(t, root.Namespace(), qt.IsNotNil)

	// A pool-owned child frame, exactly what OpReleaseEnvFrame recycles.
	frame := envFramePool.Acquire()
	root.InitApplyFrameWithParent(frame, root)

	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationReleaseEnvFrame())
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, frame))
	mc.envPooled = true

	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// The precondition this test exists for: the frame really was blanked.
	qt.Assert(t, mc.env, qt.Equals, frame)
	qt.Assert(t, mc.env.Namespace(), qt.IsNil)

	// And the engine state a promoted op reads is still reachable.
	qt.Assert(t, mc.ImmutableLiterals(), qt.IsNotNil)
	qt.Assert(t, mc.ImmutableLiterals(), qt.Equals, ns.ImmutableLiterals())
	qt.Assert(t, mc.Authorizer(), qt.IsNotNil)
}

// TestNewSubContextCarriesEngineState pins the second half of the same fix. A
// sub-context starts on a pool-zeroed struct, so without the copy every eval,
// load, once-do! and parameter-converter child reproduces the released-frame
// defect at a new site.
func TestNewSubContextCarriesEngineState(t *testing.T) {
	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.ConsoleAuthorizer())
	root := ns.Runtime()

	parent := NewMachineContext(context.Background(),
		NewMachineContinuation(nil, NewNativeTemplate(0, 0, false), root))
	parent.snapshotEngineState()
	qt.Assert(t, parent.immutableLiterals, qt.IsNotNil)

	sub := parent.NewSubContext()
	defer ReleaseSubContext(sub)
	qt.Assert(t, sub.immutableLiterals, qt.Equals, parent.immutableLiterals)
	qt.Assert(t, sub.authorizer, qt.Equals, parent.authorizer)

	// The snapshot is a fallback, never the authority: a live namespace on the
	// sub-context's own env still wins, so a sub-context handed a different
	// namespace's environment does not inherit the parent's engine state.
	other := environment.NewNamespace()
	sub.env = other.Runtime()
	qt.Assert(t, sub.ImmutableLiterals(), qt.Equals, other.ImmutableLiterals())
	var nilAuth security.Authorizer
	qt.Assert(t, sub.Authorizer(), qt.Equals, nilAuth)
}

// TestPromotedOpsCoverPromotedOpcodes pins the promotedOps registry against the
// opcode range it describes. A descriptor that exists but is left off the list
// would otherwise be invisible to promotedOpForName, silently disabling
// promotion for that primitive while every other test still passed.
func TestPromotedOpsCoverPromotedOpcodes(t *testing.T) {
	covered := make(map[OpCode]string)
	for _, op := range promotedOps {
		for _, code := range []OpCode{op.nonTail, op.tail} {
			prev, dup := covered[code]
			if dup {
				t.Errorf("opcode %s claimed by both %q and %q", code, prev, op.name)
			}
			covered[code] = op.name
		}
	}
	for code := OpEqQ; code <= OpSetCdrTail; code++ {
		_, ok := covered[code]
		if !ok {
			t.Errorf("opcode %s is in the promoted range but no promotedOps entry claims it", code)
		}
	}
	if len(covered) != int(OpSetCdrTail-OpEqQ+1) {
		t.Errorf("promotedOps covers %d opcodes, promoted range holds %d",
			len(covered), OpSetCdrTail-OpEqQ+1)
	}
}

// TestPromotedOpForNameMatchesDescriptors pins that the name lookup the peephole
// optimizer uses agrees with each descriptor, and that a non-promoted name
// resolves to OpInvalid.
func TestPromotedOpForNameMatchesDescriptors(t *testing.T) {
	for _, op := range promotedOps {
		nonTail, tail, arity := promotedOpForName(op.name)
		if nonTail != op.nonTail || tail != op.tail || arity != op.arity {
			t.Errorf("promotedOpForName(%q) = (%s, %s, %d), want (%s, %s, %d)",
				op.name, nonTail, tail, arity, op.nonTail, op.tail, op.arity)
		}
	}
	nonTail, tail, arity := promotedOpForName("vector-set!")
	if nonTail != OpInvalid || tail != OpInvalid || arity != 0 {
		t.Errorf("promotedOpForName(non-promoted) = (%s, %s, %d), want (OpInvalid, OpInvalid, 0)",
			nonTail, tail, arity)
	}
}
