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

// Package repl provides an interactive Read-Eval-Print Loop for Wile Scheme.
//
// The package offers both a full-featured REPL with readline support and
// a simple fallback mode:
//
//	repl := repl.New(env,
//	    repl.WithHistoryFile("~/.wile_history"),
//	    repl.WithPrompt("> "),
//	)
//	repl.Run(ctx)
//
// # Features
//
//   - Readline with history, line editing, and continuation prompts
//   - Integrated source-level debugger with breakpoints and stepping
//   - Multi-line input accumulation for incomplete expressions
//   - Ctrl-C clears input, Ctrl-D exits cleanly
//
// # Debug Commands
//
// Commands start with comma (,) when the input buffer is empty:
//
//	,break FILE:LINE    Set breakpoint
//	,step               Step into next expression
//	,next               Step over (same frame)
//	,continue           Continue execution
//	,backtrace          Show stack trace
package repl
