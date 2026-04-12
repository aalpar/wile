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

package values

// DebugLocation holds file/line/column for debug and error display.
// This is a simple struct for presentation layers (REPL, debugger UI),
// distinct from the SourceLocation interface which is a full Value type
// used by procedure-source-location.
type DebugLocation struct {
	File   string
	Line   int
	Column int
}

// DebugState provides read-only access to VM execution state.
// Implemented by the VM's MachineContext; consumed by presentation
// layers (REPL, debugger UI) without importing machine/.
type DebugState interface {
	// CurrentLocation returns the source location at the current
	// execution point, or nil if no source info is available.
	CurrentLocation() *DebugLocation

	// FormatStackTrace returns a human-readable stack trace string,
	// walking at most maxDepth frames.
	FormatStackTrace(maxDepth int) string
}
