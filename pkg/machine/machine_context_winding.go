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

// WindingStack returns the current winding stack.
func (p *MachineContext) WindingStack() WindingStack {
	return p.windingStack
}

// SetWindingStack sets the winding stack. Used by tests; production code
// should prefer NewSubContextWithWinding for override sites.
func (p *MachineContext) SetWindingStack(stack WindingStack) {
	p.windingStack = stack
}

// PushWindingFrame adds a frame to the winding stack.
func (p *MachineContext) PushWindingFrame(frame *DynamicWindFrame) {
	p.windingStack.Push(frame)
}

// PopWindingFrame removes the innermost frame from the winding stack.
func (p *MachineContext) PopWindingFrame() *DynamicWindFrame {
	return p.windingStack.Pop()
}

// UnwindTo runs after thunks from innermost to the common ancestor.
// Returns error if any after thunk fails.
func (p *MachineContext) UnwindTo(commonDepth int) error {
	return p.unwindStackTo(p.windingStack, commonDepth)
}

// unwindStackTo runs after thunks from innermost to commonDepth on the given stack,
// then sets p.windingStack to the common ancestor prefix.
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
	// Run after thunks from innermost to outermost (reverse order).
	for i := len(stack) - 1; i >= commonDepth; i-- {
		frame := stack[i]
		if frame.After != nil {
			// Truncated stack: the after thunk runs at depth i, not the parent's full depth.
			sub := p.NewSubContextWithWinding(stack[:i:i])
			// Run the after thunk in the dynamic-wind's ENTRY mark environment (R7RS
			// §6.10), not whatever marks are live at the unwind point — so a
			// parameterize established inside the body does not leak its value into the
			// after thunk when the reconcile runs before the captured chain is restored.
			// Always isolate (even when entryMarks is nil): isolation is what stops the
			// resolver from walking into the still-live body marks; nil entryMarks then
			// resolves parameters to their base value, the correct entry behaviour when
			// no mark was set above the dynamic-wind.
			sub.capturedMarks = frame.entryMarks
			sub.isolatedMarks = true
			_, err := sub.ApplyCallable(frame.After)
			if err != nil {
				ReleaseSubContext(sub)
				// Truncate to reflect that extents > i are already exited.
				p.windingStack = stack[:i:i]
				return err
			}
			err = sub.RunWithinBoundary()
			ReleaseSubContext(sub)
			if err != nil {
				// Propagate escapes and exceptions.
				p.windingStack = stack[:i:i]
				return err
			}
		}
		// This extent is now exited; update winding stack immediately.
		p.windingStack = stack[:i:i]
	}
	// Update current winding stack to common ancestor.
	p.windingStack = stack[:commonDepth:commonDepth]
	return nil
}

// RewindTo runs before thunks from common ancestor to target depth.
// Returns error if any before thunk fails.
func (p *MachineContext) RewindTo(target WindingStack, commonDepth int) error {
	// Run before thunks from outermost to innermost (forward order)
	for i := commonDepth; i < len(target); i++ {
		frame := target[i]
		if frame.Before != nil {
			sub := p.NewSubContext()
			// Run the before thunk in the dynamic-wind's entry mark environment (R7RS
			// §6.10) on re-entry, mirroring the after thunk in unwindStackTo.
			sub.capturedMarks = frame.entryMarks
			sub.isolatedMarks = true
			_, err := sub.ApplyCallable(frame.Before)
			if err != nil {
				ReleaseSubContext(sub)
				return err
			}
			err = sub.RunWithinBoundary()
			ReleaseSubContext(sub)
			if err != nil {
				return err
			}
		}
		// Add this frame to current winding stack
		p.windingStack = append(p.windingStack, frame)
	}
	return nil
}

// RestoreWithWindingFrom restores a continuation with proper dynamic-wind handling:
// it unwinds from sourceStack, rewinds to targetStack, then restores the machine
// state.
//
// The source stack is explicit because the escape may have originated in a
// sub-context whose winding stack differs from the restoring context's. When
// call/cc captures inside a sub-context and the escape propagates up, the source
// stack holds frames the top-level context never saw. Pass p.WindingStack() to
// unwind from the restoring context's own extent.
//
// If cont is nil (continuation captured in a sub-context), the winding runs but
// machine state is not restored; the caller handles continued execution.
//
// Parameters:
//   - cont: The continuation to restore to
//   - sourceStack: The winding stack where the escape originated (for unwinding)
//   - targetStack: The winding stack to restore to (for rewinding)
func (p *MachineContext) RestoreWithWindingFrom(cont *MachineContinuation, sourceStack, targetStack WindingStack) error {
	// Find common ancestor between source and target
	commonDepth := FindCommonWindingPrefix(sourceStack, targetStack)

	// Unwind: run after thunks for frames being exited (from source)
	err := p.unwindStackTo(sourceStack, commonDepth)
	if err != nil {
		return err
	}

	// Rewind: run before thunks for frames being entered (to target)
	err = p.RewindTo(targetStack, commonDepth)
	if err != nil {
		return err
	}

	// Restore the machine state (if we have a valid continuation).
	// The continuation chain was already marked shared at capture time
	// (MarkChainShared in CurrentContinuation / PrimCallCC), so
	// RestoreAndRelease will copy evals and skip pooling for these frames.
	if cont != nil {
		p.Restore(cont)
	}
	return nil
}
