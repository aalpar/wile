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
// The package depends only on werr/ for error types and can be
// imported with minimal dependencies.
package security

// AccessRequest describes an operation that requires authorization.
// Resource and Action use well-known string constants defined below.
// Target is operation-specific (e.g., a file path, environment
// variable name, or library name).
type AccessRequest struct {
	Resource string
	Action   string
	Target   string
	// TargetSource names the namespace Target is drawn from. The zero value is
	// the host OS filesystem — the only kind that existed before this field, so
	// no built-in authorizer changes meaning for a request that omits it. A
	// non-empty value (today only SourceVirtualFS) says Target is a path inside
	// a virtual fs.FS supplied through WithSourceFS, and is therefore meaningless
	// to OS path containment: "evil.scm" names a file in that fs.FS, while
	// resolving it as an OS path silently reinterprets it against the process
	// working directory.
	TargetSource string
}

// Well-known resource constants. Extensions may define additional
// resources without modifying this package.
const (
	// ResourceFile covers every host path the program names, under the chmod
	// triple ActionRead / ActionWrite / ActionExec (plus ActionStat and
	// ActionDelete). Enforce them together: they are one resource with one
	// Target, so a single containment predicate decides all of them, and an
	// authorizer that confines reads and writes but not exec confines nothing --
	// an executable outside the root is a general-purpose file accessor.
	//
	// A primitive whose argument denotes a host path MUST file it here, whatever
	// else it also asks. set-current-directory! files file:write on the
	// destination rather than an opaque process request, and process-spawn files
	// file:exec on the resolved binary, for exactly this reason: a path-confining
	// authorizer can only confine paths it is shown.
	ResourceFile    = "file"
	ResourceCode    = "code"
	ResourceEnv     = "env"
	ResourceProcess = "process"
	// ResourceNamespace covers constructing a first-class environment whose
	// capability surface is not already the engine's. It is deliberately not
	// ResourceCode: "may run new code" and "may acquire primitives this engine
	// never registered" are different questions, and an authorizer that permits
	// eval under a confined root would otherwise also hand over gointerop.
	ResourceNamespace = "namespace"
	// ResourceStream covers the host process's standard streams, which the io
	// extension pre-opens as current-{input,output,error}-port. It is deliberately
	// not ResourceFile: the streams are handed to the engine at construction
	// rather than named by the program, so there is no path to confine, and an
	// authorizer that permits reads under a filesystem root is not thereby saying
	// the program may drain the host's stdin. Target is one of StreamStdin,
	// StreamStdout, StreamStderr; Action is ActionRead or ActionWrite. The gate
	// runs once per engine, when io.NewState builds the port parameters -- a
	// refusal means the port is never opened, not that each write is checked.
	ResourceStream = "stream"
)

// Well-known ResourceStream targets: the three host streams the io extension
// binds to current-{input,output,error}-port.
const (
	StreamStdin  = "stdin"
	StreamStdout = "stdout"
	StreamStderr = "stderr"
)

// SourceVirtualFS is the TargetSource of a path served by a virtual filesystem
// (an embedder's WithSourceFS). It is the only named source: an fs.FS is an
// anonymous interface value with no name to report, and deriving one from its
// Go type would make the authorizer's answer depend on the embedder's type
// names. There is deliberately no wildcard — "any source" is the zero value's
// absence of a claim, not a value.
const SourceVirtualFS = "virtual-fs"

// Well-known action constants. Extensions may define additional
// actions without modifying this package.
const (
	ActionRead   = "read"
	ActionWrite  = "write"
	ActionDelete = "delete"
	ActionStat   = "stat"
	ActionLoad   = "load" // load+run code from a resolved file path
	ActionEval   = "eval" // compile+run code from an in-memory datum (eval/compile)
	ActionExit   = "exit"
	// ActionExec is the chmod x bit, and it asks a different question of each
	// resource. On ResourceProcess it is the CAPABILITY: may this program spawn a
	// subprocess at all (process-spawn). On ResourceFile it is the OBJECT: may it
	// run THIS binary, and -- POSIX x on a directory being traverse -- may a child
	// START in this directory. A spawn asks both, so a path-confining authorizer
	// sees the binary and the working directory it would otherwise never be shown.
	ActionExec      = "exec"
	ActionExecShell = "exec-shell" // shell command execution (system)
	ActionCreate    = "create"     // construct a capability-bearing object (namespace)
)
