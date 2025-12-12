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

package machine

import (
	"fmt"
	"wile/syntax"
	"strings"
)

// StackFrame represents one frame in a Scheme stack trace.
type StackFrame struct {
	FunctionName string                // Function name (or "<anonymous>")
	CallSite     *syntax.SourceContext // Where the call was made
	CurrentLoc   *syntax.SourceContext // Current execution point
}

// String formats the frame for display.
func (f *StackFrame) String() string {
	name := f.FunctionName
	if name == "" {
		name = "<anonymous>"
	}

	if f.CurrentLoc != nil {
		return fmt.Sprintf("  at %s (%s:%d:%d)",
			name,
			f.CurrentLoc.File,
			f.CurrentLoc.Start.Line(),
			f.CurrentLoc.Start.Column())
	}
	if f.CallSite != nil {
		return fmt.Sprintf("  at %s (called from %s:%d:%d)",
			name,
			f.CallSite.File,
			f.CallSite.Start.Line(),
			f.CallSite.Start.Column())
	}
	return fmt.Sprintf("  at %s", name)
}

// StackTrace is a list of stack frames.
type StackTrace []StackFrame

// String formats the entire stack trace.
func (st StackTrace) String() string {
	if len(st) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("Stack trace:\n")
	for _, frame := range st {
		b.WriteString(frame.String())
		b.WriteString("\n")
	}
	return b.String()
}
