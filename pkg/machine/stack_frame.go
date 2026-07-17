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
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/syntax"
)

// StackFrame represents one frame in a Scheme stack trace.
type StackFrame struct {
	FunctionName string                // Function name (or "<anonymous>")
	CallSite     *syntax.SourceContext // Where the call was made
	CurrentLoc   *syntax.SourceContext // Current execution point
}

// String formats the frame for display.
//
// Both locations render through SourceContext.Location, so a context carrying no
// position contributes nothing rather than a bare ":0:0". A frame whose
// CurrentLoc is position-less therefore falls through to its CallSite, and one
// with no position at all degrades to the name alone.
func (p *StackFrame) String() string {
	name := p.FunctionName
	if name == "" {
		name = "<anonymous>"
	}

	currentLoc := p.CurrentLoc.Location()
	if currentLoc != "" {
		return fmt.Sprintf("  at %s (%s)", name, currentLoc)
	}
	callSite := p.CallSite.Location()
	if callSite != "" {
		return fmt.Sprintf("  at %s (called from %s)", name, callSite)
	}
	return fmt.Sprintf("  at %s", name)
}

// StackTrace is a list of stack frames.
type StackTrace []StackFrame

// String formats the entire stack trace.
func (p StackTrace) String() string {
	if len(p) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("Stack trace:\n")
	for _, frame := range p {
		b.WriteString(frame.String())
		b.WriteString("\n")
	}
	return b.String()
}
