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

package wile

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
)

// CompiledCode represents compiled Scheme code ready for execution.
//
// CompiledCode captures the environment from the Engine that compiled it and
// always executes using that captured environment, regardless of which Engine
// is used to run it. Using a different Engine instance affects only that
// Engine's own bookkeeping (for example, evaluation counters), not the
// environment bindings or syntax interning.
//
// CompiledCode can be run multiple times. It is not safe for concurrent
// execution (the underlying Engine is not goroutine-safe).
//
// Stability contract:
//
//   - In-memory and process-local. CompiledCode holds live pointers into the
//     compiling Engine's machine template and environment. It is not
//     serializable and cannot be persisted to disk, sent over the wire, or
//     reloaded in a different process. There is no on-disk bytecode format.
//   - No cross-version format stability. The internal bytecode and template
//     representation are implementation details that change between releases
//     (often within a minor version). Do not depend on their shape.
//   - Trusted-input only. A CompiledCode is the output of this process's own
//     compiler. There is no facility to construct one from untrusted bytes,
//     so there is no untrusted-bytecode attack surface to validate.
//
// To "cache" compilation, keep the CompiledCode value alive within the
// running program; to share work across processes, share the Scheme source.
type CompiledCode struct {
	template *machine.NativeTemplate
	env      *environment.EnvironmentFrame
}

// String returns a string representation of the compiled code.
func (p *CompiledCode) String() string {
	return "#<compiled-code>"
}
