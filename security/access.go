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

// Package security provides fine-grained authorization for Scheme
// runtime operations. It defines the Authorizer interface and context
// helpers that gate primitives like file I/O, code loading, and
// process control.
//
// The package has no dependencies on the rest of wile and can be
// imported independently.
package security

// AccessRequest describes an operation that requires authorization.
// Resource and Action use well-known string constants defined below.
// Target is operation-specific (e.g., a file path, environment
// variable name, or library name).
type AccessRequest struct {
	Resource string
	Action   string
	Target   string
}

// Well-known resource constants. Extensions may define additional
// resources without modifying this package.
const (
	ResourceFile    = "file"
	ResourceCode    = "code"
	ResourceEnv     = "env"
	ResourceProcess = "process"
)

// Well-known action constants. Extensions may define additional
// actions without modifying this package.
const (
	ActionRead   = "read"
	ActionWrite  = "write"
	ActionDelete = "delete"
	ActionStat   = "stat"
	ActionLoad   = "load"
	ActionExit   = "exit"
)
