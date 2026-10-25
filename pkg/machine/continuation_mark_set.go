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
	"github.com/aalpar/wile/pkg/values"
)

// ContinuationMarkSet is an immutable snapshot of continuation marks
// collected from a walk of the continuation chain.
//
// The frames slice contains per-frame mark slices, nearest frame first.
// Only frames with non-nil marks are included. Each frame is a []markEntry
// searched with values.EqIdentity (eq? semantics).
type ContinuationMarkSet struct {
	frames [][]markEntry
}

func (p *ContinuationMarkSet) SchemeString() string {
	return "#<continuation-mark-set>"
}

func (p *ContinuationMarkSet) IsVoid() bool {
	return p == nil
}

func (p *ContinuationMarkSet) EqualTo(o values.Value) bool {
	v, ok := o.(*ContinuationMarkSet)
	if !ok {
		return false
	}
	return p == v
}

// ToList returns a list of values for key across all frames, nearest first.
// Returns the empty list if no frame contains the key.
// Uses eq? semantics (values.EqIdentity) for key comparison.
func (p *ContinuationMarkSet) ToList(key values.Value) values.Tuple {
	var collected []values.Value
	for _, frame := range p.frames {
		v := lookupMark(frame, key)
		if v != nil {
			collected = append(collected, v)
		}
	}
	return values.List(collected...)
}

// First returns the value for key from the nearest frame, or defaultVal
// if no frame contains the key.
// Uses eq? semantics (values.EqIdentity) for key comparison.
func (p *ContinuationMarkSet) First(key, defaultVal values.Value) values.Value {
	for _, frame := range p.frames {
		v := lookupMark(frame, key)
		if v != nil {
			return v
		}
	}
	return defaultVal
}

// ToListStar returns a list of vectors for multiple keys across all frames.
// Each vector corresponds to a frame that has at least one of the requested keys.
// Vector positions correspond to the keys slice; noneVal fills missing keys.
// Uses eq? semantics (values.EqIdentity) for key comparison.
//
// Racket §10.5: continuation-mark-set->list*
func (p *ContinuationMarkSet) ToListStar(keys []values.Value, noneVal values.Value) values.Tuple {
	var collected []values.Value
	for _, frame := range p.frames {
		vec := make([]values.Value, len(keys))
		found := false
		for i, key := range keys {
			vec[i] = noneVal
			v := lookupMark(frame, key)
			if v != nil {
				vec[i] = v
				found = true
			}
		}
		if found {
			collected = append(collected, values.NewVector(vec...))
		}
	}
	return values.List(collected...)
}

// appendChainMarks appends a clone of every non-empty mark frame from cont
// outward, nearest first, stopping at the first frame whose promptTag matches
// tag (exclusive) or after last (inclusive; nil for no such bound). Pass
// DefaultPromptTag for an unbounded walk. It is the one chain walk behind both
// ContinuationMarkSet collectors.
//
// The prompt frame's own marks are excluded because they are not inside the
// prompt: RunBodyUnderFrame moves the marks of the activation that INSTALLED the
// prompt onto that frame, so they belong to the continuation beyond the delimiter.
func appendChainMarks(frames [][]markEntry, cont, last *MachineContinuation, tag *PromptTag) [][]markEntry {
	for c := cont; c != nil; c = c.parent {
		if c.promptTag == tag {
			break
		}
		if len(c.marks) > 0 {
			frames = append(frames, cloneMarks(c.marks))
		}
		if c == last {
			break
		}
	}
	return frames
}

// CollectMarksFromCapture builds the ContinuationMarkSet of a call/cc capture: the
// capturing activation's live marks first, then the segment's chain from its head.
// Walking the chain alone loses the first part when the call/cc was in tail
// position of a with-continuation-mark body, because those marks sit in the
// registers and the segment has no frame carrying them. In the non-tail case
// SaveContinuation has already moved them onto the chain and liveMarks is empty, so
// nothing is reported twice. Like CollectContinuationMarks, the live marks are never
// cut by tag: they are inside any prompt the capture is delimited at. liveMarks is
// already a private clone made at capture and is never mutated, so the set shares it.
//
// The walk ends at the segment's bottom frame (inclusive), not at a nil parent: the
// first invocation hands out the original segment (AcquireSegment) and
// GraftContinuation hangs its bottom onto the invoker's chain, so walking on would
// report the invoker's marks as the continuation's own.
func CollectMarksFromCapture(comp *ComposableContinuation, tag *PromptTag) *ContinuationMarkSet {
	var frames [][]markEntry
	if len(comp.liveMarks) > 0 {
		frames = append(frames, comp.liveMarks)
	}
	return &ContinuationMarkSet{frames: appendChainMarks(frames, comp.cont, comp.bottom, tag)}
}

// CollectContinuationMarks builds a ContinuationMarkSet snapshot of the marks
// on p's dynamic continuation up to the nearest prompt tagged tag: each
// context's live marks, then its chain, hopping parentMC so code an eval or load
// runs in a sub-context sees its caller's marks, as in Racket. It stops at the
// context PromptOwner names (after that context's frames inside the prompt) and,
// with no such prompt, where PromptOwner's walk stops.
func (p *MachineContext) CollectContinuationMarks(tag *PromptTag) *ContinuationMarkSet {
	var frames [][]markEntry
	for mc := p; mc != nil; mc = mc.parentMC {
		if len(mc.marks) > 0 {
			frames = append(frames, cloneMarks(mc.marks))
		}
		frames = appendChainMarks(frames, mc.cont, nil, tag)
		_, found := mc.FindPrompt(tag)
		if found || mc.isolatedMarks {
			break
		}
	}
	return &ContinuationMarkSet{frames: frames}
}
