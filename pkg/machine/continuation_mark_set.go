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

// CollectMarksFromContinuation walks a captured MachineContinuation chain
// and builds a ContinuationMarkSet snapshot. Used by the (continuation-marks
// cont) primitive to extract marks from a continuation captured via call/cc.
//
// Starts from cont (nearest frame) and walks parent links, stopping at the
// first frame whose promptTag matches tag (inclusive). Pass DefaultPromptTag
// for an unbounded walk.
func CollectMarksFromContinuation(cont *MachineContinuation, tag *PromptTag) *ContinuationMarkSet {
	var frames [][]markEntry
	for c := cont; c != nil; c = c.parent {
		if len(c.marks) > 0 {
			frames = append(frames, cloneMarks(c.marks))
		}
		if c.promptTag == tag {
			break
		}
	}
	return &ContinuationMarkSet{frames: frames}
}

// CollectContinuationMarks walks the continuation chain and builds a
// ContinuationMarkSet snapshot. Collects marks from the current frame
// and all continuation frames up to and including the nearest frame
// with a matching promptTag.
func (p *MachineContext) CollectContinuationMarks(tag *PromptTag) *ContinuationMarkSet {
	var frames [][]markEntry

	// Current frame
	if len(p.marks) > 0 {
		frames = append(frames, cloneMarks(p.marks))
	}

	// Walk continuation chain, stopping at a frame with matching promptTag.
	// The MachineContext's own promptTag is NOT checked — it represents the
	// execution boundary established by call-with-continuation-prompt on a
	// sub-context, and collection is always called from within that boundary.
	// Contrast with FindPrompt, which checks p.promptTag for escape detection;
	// that check is not needed here because current-continuation-marks is only
	// callable from code already inside the prompt scope.
	for cont := p.cont; cont != nil; cont = cont.parent {
		if len(cont.marks) > 0 {
			frames = append(frames, cloneMarks(cont.marks))
		}
		if cont.promptTag == tag {
			break
		}
	}

	return &ContinuationMarkSet{frames: frames}
}
