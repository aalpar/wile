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

// Package process provides subprocess execution primitives.
//
// This extension is NOT included in SafeExtensions(). Embedders must
// opt in explicitly with WithExtension(process.Extension).
//
// Two security actions gate process creation:
//   - security.ActionExec gates process-spawn (structured, no shell)
//   - security.ActionExecShell gates system (shell command string)
//
// Both use security.ResourceProcess with the command as target.
//
// # Shell Execution
//
//   - system
//
// # Structured Process Control
//
//   - process-spawn, process-wait, process-kill
//   - process-stdout, process-stderr, process-stdin
//
// # Process Predicates
//
//   - process?
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package process
