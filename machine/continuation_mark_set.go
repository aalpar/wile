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
	"maps"

	"github.com/aalpar/wile/values"
)

// ContinuationMarkSet is an immutable snapshot of continuation marks
// collected from a walk of the continuation chain.
//
// The frames slice contains per-frame mark maps, nearest frame first.
// Only frames with non-nil marks are included.
type ContinuationMarkSet struct {
	frames []map[values.Value]values.Value
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
func (p *ContinuationMarkSet) ToList(key values.Value) values.Tuple {
	var collected []values.Value
	for _, frame := range p.frames {
		v, ok := frame[key]
		if ok {
			collected = append(collected, v)
		}
	}
	return values.List(collected...)
}

// First returns the value for key from the nearest frame, or defaultVal
// if no frame contains the key.
func (p *ContinuationMarkSet) First(key, defaultVal values.Value) values.Value {
	for _, frame := range p.frames {
		v, ok := frame[key]
		if ok {
			return v
		}
	}
	return defaultVal
}

// CollectContinuationMarks walks the continuation chain and builds a
// ContinuationMarkSet snapshot. Collects marks from the current frame
// and all continuation frames up to and including the nearest frame
// with a matching promptTag.
func (p *MachineContext) CollectContinuationMarks(tag *PromptTag) *ContinuationMarkSet {
	var frames []map[values.Value]values.Value

	// Current frame
	if p.marks != nil {
		frames = append(frames, maps.Clone(p.marks))
	}

	// Walk continuation chain, stopping at a frame with matching promptTag.
	// The MachineContext's own promptTag is NOT checked — it represents
	// the execution boundary, and all frames in the chain are within it.
	for cont := p.cont; cont != nil; cont = cont.parent {
		if cont.marks != nil {
			frames = append(frames, maps.Clone(cont.marks))
		}
		if cont.promptTag == tag {
			break
		}
	}

	return &ContinuationMarkSet{frames: frames}
}
