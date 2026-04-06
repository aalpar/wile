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
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
)

// CallContext is the extension-facing subset of MachineContext.
// Extensions and ForeignFunctions should depend on this interface,
// not on *MachineContext directly.
//
// This interface captures the 7 methods that extensions actually use,
// reducing coupling from 30+ methods to 7. *MachineContext satisfies
// this interface with zero implementation cost.
//
// Internal code that needs full VM access (sub-context creation,
// continuation manipulation, exception handling) should type-assert
// to *MachineContext.
// Compile-time assertion: *MachineContext must satisfy CallContext.
var _ CallContext = (*MachineContext)(nil)

type CallContext interface {
	// Arg returns the argument at the given positional index.
	// For variadic functions, the last parameter index holds the rest list.
	Arg(index int) values.Value

	// SetValue sets the single return value.
	SetValue(v values.Value)

	// SetValues sets multiple return values for R7RS values/call-with-values.
	SetValues(vs ...values.Value)

	// Authorizer returns the security authorizer for permission checks.
	Authorizer() security.Authorizer

	// Context returns the Go context for cancellation and timeout.
	Context() context.Context

	// EnvironmentFrame returns the current lexical environment frame.
	EnvironmentFrame() *environment.EnvironmentFrame

	// Thread returns the SRFI-18 thread object (nil for primordial thread).
	Thread() *values.Thread
}
