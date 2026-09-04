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

import "github.com/aalpar/wile/pkg/values"

// markIndex returns the position of key in ms, or -1. Keys compare with eq?
// semantics (values.EqIdentity), the one comparison every mark scan uses.
func markIndex(ms []markEntry, key values.Value) int {
	for i := range ms {
		if values.EqIdentity(ms[i].key, key) {
			return i
		}
	}
	return -1
}

// lookupMark returns the value marked under key in ms, or nil when ms carries no
// such mark. nil is the sole absence sentinel: mark values are never nil (see
// GetMark), which is what lets one lookup serve first-match, contains, and
// nearest-frame consumers alike.
func lookupMark(ms []markEntry, key values.Value) values.Value {
	i := markIndex(ms, key)
	if i < 0 {
		return nil
	}
	return ms[i].val
}

// forEachReachableMarkFrame visits every mark frame reachable from p, nearest
// first, until visit returns false. Frames carrying no marks are skipped. The
// order and the stop rule are the contract; parameter reads, handler lookup, and
// the call/cc capture-time snapshot all depend on them:
//
//  1. p's own live marks. The walk starts at p, not p.parentMC: parameterize and
//     with-continuation-mark set the LIVE marks of the current frame, which
//     SliceContinuationAt never copies into a captured segment.
//  2. each frame of p's continuation chain, innermost first.
//  3. at an isolatedMarks context (a re-invoked continuation running a grafted
//     chain) the capture-time snapshot, capturedMarks, then STOP. Isolation cuts
//     off parentMC so the invoker's marks do not bleed into a resumed call/cc
//     continuation, but that continuation's own marks from ABOVE the sub-context
//     boundary it was captured behind must still resolve; the snapshot
//     (collectReachableMarks / SnapshotReachableMarksInto) carries them.
//  4. otherwise hop to parentMC and repeat, mirroring how dynamic-wind extents
//     are inherited by NewSubContext. Without the hop an outer parameterize would
//     be invisible inside the sub-contexts created by
//     call-with-continuation-prompt, apply, call-with-values, etc.
//
// visit is a callback rather than an iter.Seq because this walk runs on every
// parameter read and every raise (ResolveParameterValue): a returned iterator
// closure escapes to the heap, a non-escaping callback does not.
// TestFindParameterInMarks_NoAlloc pins that.
func (p *MachineContext) forEachReachableMarkFrame(visit func(frame []markEntry) bool) {
	for mc := p; mc != nil; mc = mc.parentMC {
		if len(mc.marks) > 0 && !visit(mc.marks) {
			return
		}
		for c := mc.cont; c != nil; c = c.parent {
			if len(c.marks) > 0 && !visit(c.marks) {
				return
			}
		}
		if mc.isolatedMarks {
			if len(mc.capturedMarks) > 0 {
				visit(mc.capturedMarks)
			}
			return
		}
	}
}
