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

import "context"

// WindingStack returns the current winding stack.
func (p *MachineContext) WindingStack() WindingStack {
	return p.windingStack
}

// PushWindingFrame adds a frame to the winding stack.
func (p *MachineContext) PushWindingFrame(frame DynamicWindFrame) {
	p.windingStack.Push(frame)
}

// PopWindingFrame removes the innermost frame from the winding stack, reporting
// false when the stack was already empty.
func (p *MachineContext) PopWindingFrame() (DynamicWindFrame, bool) {
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
			// And outside any with-timeout the dynamic-wind encloses
			// (DynamicWindFrame.entryCtx).
			sub.ctx = p.windingThunkContext(frame.entryCtx)
			_, err := sub.ApplyCallable(frame.After)
			if err != nil {
				err = p.windingThunkFailure(sub, err)
				ReleaseSubContext(sub)
				// Truncate to reflect that extents > i are already exited.
				p.windingStack = stack[:i:i]
				return err
			}
			err = sub.RunWithinBoundary()
			if err != nil {
				err = p.windingThunkFailure(sub, err)
			}
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
			sub.ctx = p.windingThunkContext(frame.entryCtx)
			_, err := sub.ApplyCallable(frame.Before)
			if err != nil {
				err = p.windingThunkFailure(sub, err)
				ReleaseSubContext(sub)
				return err
			}
			err = sub.RunWithinBoundary()
			if err != nil {
				err = p.windingThunkFailure(sub, err)
			}
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

	// Restore the machine state (if we have a valid continuation). This uses
	// Restore, not RestoreAndRelease, so evals are copied and the frame is left
	// for GC rather than pooled — required because the chain was marked shared at
	// capture time (MarkChainShared in CurrentContinuation / PrimCallCC). Both
	// production callers pass cont == nil, so this branch is test-only today.
	if cont != nil {
		p.Restore(cont)
	}
	return nil
}

// windingThunkContext returns the context a dynamic-wind thunk runs in during a
// winding reconcile, given the context its dynamic-wind was entered under. R7RS
// runs the thunks in the dynamic environment of the dynamic-wind call, so the
// thunk keeps entryCtx when the innermost timer entryCtx is inside (timerCtxKey)
// is one p arms now. A timer p does not arm (its with-timeout has ended, or it
// belongs to an earlier evaluation) is stepped out of, to the context outside it,
// and the same question asked there. Inside no timer p arms, the thunk runs in the
// context outside every timer p arms, since none of them is part of the
// dynamic-wind call's environment; that context is live, unlike a frame's from an
// earlier evaluation. So does a frame with no recorded context (built by
// NewDynamicWindFrame and pushed directly). With no timer armed, p's own context.
//
// Only p's own timers are stepped out of. On a context below the timer's owner
// (eval, load) the owner's timers stay in force, as they do for the after thunk a
// normal exit runs inline. So a frame a composable continuation, or one reinstated
// at a prompt inside a with-timeout, re-enters inside it runs its before thunk
// outside that timer when p owns the timer and under it otherwise; whether the
// timer should bound such a frame is open (TODO.md, I050).
func (p *MachineContext) windingThunkContext(entryCtx context.Context) context.Context {
	ctx := entryCtx
	for ctx != nil {
		inner, _ := ctx.Value(timerCtxKey{}).(*timerState)
		if inner == nil {
			break
		}
		if p.armsTimer(inner) {
			return ctx
		}
		ctx = inner.savedCtx
	}
	outside := p.ctx
	for t := p.timer; t != nil; t = t.parent {
		outside = t.savedCtx
	}
	return outside
}

// armsTimer reports whether t is one of p's armed timers.
func (p *MachineContext) armsTimer(t *timerState) bool {
	for armed := p.timer; armed != nil; armed = armed.parent {
		if armed == t {
			return true
		}
	}
	return false
}

// windingThunkFailure returns err, the failure of a winding thunk that ran on sub,
// marked unclaimed (unclaimedFailure) when a fired timer of p's would claim it but
// the thunk's own context was not cancelled by a timer: the thunk ran outside that
// timer (windingThunkContext), so its failure is its own. p reconciles the winding
// before it leaves the with-timeout's extent, with the boundary still on its chain
// and its ctx still the timer's, so judged by p's state the failure became the
// timeout: the handler ran, the escape was lost, and the after thunk ran again.
// The mark answers for p's timers alone (judgedUnclaimed).
func (p *MachineContext) windingThunkFailure(sub *MachineContext, err error) error {
	if !p.timerClaims(err) || sub.timerClaims(err) {
		return err
	}
	return &unclaimedFailure{err: err, timer: p.timer}
}
