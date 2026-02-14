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

package match

import "fmt"

// ByteCodeSkipIfEmpty implements while-loop semantics for ellipsis patterns.
//
// Problem: Without this, ellipsis patterns use do-while semantics, executing
// the loop body at least once. This breaks patterns like (foo e1 e2 ...)
// when matching (foo x) - the e2... part should match zero elements.
//
// Solution: Check if the list is empty BEFORE entering the loop body.
// If empty, skip forward by Offset instructions to exit the loop.
//
// This is the key fix for zero-iteration ellipsis matching in R7RS.
type ByteCodeSkipIfEmpty struct {
	Offset int
}

func (p ByteCodeSkipIfEmpty) String() string {
	return fmt.Sprintf("SkipIfEmpty(%d)", p.Offset)
}

// ByteCodeSkipIfTailCount implements ellipsis-in-middle pattern matching.
//
// R7RS §4.3.2 allows patterns like (a ... b c) where the ellipsis is followed
// by additional pattern elements. This instruction enables matching such patterns
// by checking if exactly Count elements remain in the list.
//
// Behavior:
//   - If remaining elements == Count: jump forward by Offset (exit loop, match tail)
//   - If remaining elements > Count: continue (match more ellipsis iterations)
//   - If remaining elements < Count: return ErrNotAMatch (not enough for tail)
//
// When Count == 0, this behaves identically to ByteCodeSkipIfEmpty.
type ByteCodeSkipIfTailCount struct {
	Offset int // Instructions to skip forward when exiting loop
	Count  int // Number of elements required for trailing pattern
}

func (p ByteCodeSkipIfTailCount) String() string {
	return fmt.Sprintf("SkipIfTailCount(%d, count=%d)", p.Offset, p.Count)
}

// ByteCodeJump performs an unconditional jump by a relative offset.
// Used for looping back in ellipsis patterns.
type ByteCodeJump struct {
	Offset int
}

func (p ByteCodeJump) String() string {
	return fmt.Sprintf("Jump(%d)", p.Offset)
}

// ByteCodePushContext starts a new capture context for an ellipsis iteration.
// EllipsisID identifies which ellipsis pattern this context belongs to,
// enabling multiple independent ellipsis patterns in the same clause.
type ByteCodePushContext struct {
	EllipsisID int
}

func (p ByteCodePushContext) String() string {
	return fmt.Sprintf("PushContext(%d)", p.EllipsisID)
}

// ByteCodePopContext ends the current capture context.
type ByteCodePopContext struct {
	EllipsisID int
}

func (p ByteCodePopContext) String() string {
	return fmt.Sprintf("PopContext(%d)", p.EllipsisID)
}
