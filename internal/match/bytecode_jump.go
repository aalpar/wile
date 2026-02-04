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

// ByteCodeJump performs an unconditional jump by a relative offset.
// Used for looping back in ellipsis patterns.
type ByteCodeJump struct {
	Offset int
}

func (p ByteCodeJump) String() string {
	return fmt.Sprintf("Jump(%d)", p.Offset)
}
