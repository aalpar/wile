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

// Package repl provides composable components for building interactive
// Scheme REPLs on top of the Wile engine.
//
// Components can be used independently or composed into a full REPL:
//
//   - [REPL]: Full read-eval-print loop with readline support
//   - [MetaCommandHandler]: Comma-prefixed commands (,doc, ,apropos, etc.)
//   - [Completer]: Tab completion for bindings and commands
//   - [DebugContext]: Breakpoints, stepping, backtrace
//   - [DocProvider]: Documentation lookup interface
//   - [RegistryDocProvider]: Registry-backed documentation
//
// All components that need engine access take [*wile.Engine] at construction
// time. The REPL composes the other components with sensible defaults; embedders
// can construct and configure individual components for custom use.
package repl
