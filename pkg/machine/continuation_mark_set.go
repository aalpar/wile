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
// tag (inclusive). Pass DefaultPromptTag for an unbounded walk. It is the one
// chain walk behind both ContinuationMarkSet collectors.
func appendChainMarks(frames [][]markEntry, cont *MachineContinuation, tag *PromptTag) [][]markEntry {
	for c := cont; c != nil; c = c.parent {
		if len(c.marks) > 0 {
			frames = append(frames, cloneMarks(c.marks))
		}
		if c.promptTag == tag {
			break
		}
	}
	return frames
}

// CollectMarksFromContinuation builds a ContinuationMarkSet snapshot from a
// captured MachineContinuation chain, starting at cont. Used by the
// (continuation-marks cont) primitive to extract marks from a continuation
// captured via call/cc.
func CollectMarksFromContinuation(cont *MachineContinuation, tag *PromptTag) *ContinuationMarkSet {
	return &ContinuationMarkSet{frames: appendChainMarks(nil, cont, tag)}
}

// CollectContinuationMarks builds a ContinuationMarkSet snapshot from the
// current frame's marks followed by the continuation chain.
//
// The MachineContext's own promptTag is NOT checked — it represents the
// execution boundary established by call-with-continuation-prompt on a
// sub-context, and collection is always called from within that boundary.
// Contrast with FindPrompt, which checks p.promptTag for escape detection;
// that check is not needed here because current-continuation-marks is only
// callable from code already inside the prompt scope.
func (p *MachineContext) CollectContinuationMarks(tag *PromptTag) *ContinuationMarkSet {
	var frames [][]markEntry
	if len(p.marks) > 0 {
		frames = append(frames, cloneMarks(p.marks))
	}
	return &ContinuationMarkSet{frames: appendChainMarks(frames, p.cont, tag)}
}
