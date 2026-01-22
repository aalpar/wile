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

package registry

import "strings"

// Phase indicates when a primitive is available.
type Phase int

const (
	// PhaseRuntime indicates the primitive is available at runtime.
	PhaseRuntime Phase = 1 << iota
	// PhaseExpand indicates the primitive is available during macro expansion.
	PhaseExpand
	// PhaseCompile indicates the primitive is a compile-time binding (no value).
	PhaseCompile
)

// HasRuntime returns true if the phase includes runtime.
func (p Phase) HasRuntime() bool {
	return p&PhaseRuntime != 0
}

// HasExpand returns true if the phase includes expand time.
func (p Phase) HasExpand() bool {
	return p&PhaseExpand != 0
}

// HasCompile returns true if the phase includes compile time.
func (p Phase) HasCompile() bool {
	return p&PhaseCompile != 0
}

// String returns a string representation of the phase.
func (p Phase) String() string {
	var parts []string
	if p.HasRuntime() {
		parts = append(parts, "runtime")
	}
	if p.HasExpand() {
		parts = append(parts, "expand")
	}
	if p.HasCompile() {
		parts = append(parts, "compile")
	}
	if len(parts) == 0 {
		return "none"
	}
	return strings.Join(parts, "|")
}
