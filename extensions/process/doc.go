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
// This extension is NOT included in the Console profile. Embedders must
// opt in explicitly with WithExtension(process.Extension) or use
// WithProfile(KitchenSink).
//
// Spawning asks two KINDS of question, and both must be answered before a child
// runs.
//
// The CAPABILITY question is security.ResourceProcess with the command as
// target: ActionExec for process-spawn (structured, no shell), ActionExecShell
// for system (shell command string). It asks whether this program may spawn a
// subprocess at all.
//
// The OBJECT questions are security.ResourceFile with ActionExec — the chmod x
// bit — asked twice: once on the binary that will actually run, and once on the
// directory the child will start in (POSIX x on a directory is traverse). They
// ask which binary, from where. Without them a path-confining authorizer never
// sees either: the capability request carries a command STRING, and /bin/sh is a
// general-purpose unconfined file accessor that no root ever bounded.
//
// Two details the gates depend on, both in prim_process.go:
//   - The gated binary is cmd.Path AFTER exec.Command's LookPath resolution, not
//     the caller's string, joined against cmd.Dir when relative — otherwise the
//     authorized file and the executed file can differ.
//   - The child's start directory is the authorizer's ConfinementRoot when it
//     reports one, not the host's inherited working directory.
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
