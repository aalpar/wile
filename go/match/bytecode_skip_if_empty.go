// Copyright 2025 Aaron Alpar
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
