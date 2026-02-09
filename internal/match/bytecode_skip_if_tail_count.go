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
