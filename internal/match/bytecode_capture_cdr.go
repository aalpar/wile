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

// ByteCodeCaptureCdr captures the current cdr as a pattern variable binding.
// This is used for improper list patterns like (_ a . rest) where rest
// should capture the remaining elements of the input list.
//
// R7RS §4.3.2: In a pattern, an identifier followed by . and another
// identifier matches any input that is a list of one or more elements,
// binding the first identifier to the first element and the second
// identifier to the rest of the list.
type ByteCodeCaptureCdr struct {
	Binding string
}

func (p ByteCodeCaptureCdr) String() string {
	return fmt.Sprintf("CaptureCdr(%s)", p.Binding)
}
