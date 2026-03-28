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

// Package main provides the entry point for the Wile Scheme interpreter binary.
//
// The scheme command provides an interactive REPL and file execution mode:
//
//	scheme                        # Start interactive REPL
//	scheme file.scm               # Execute file
//	scheme --file file.scm        # Execute file (explicit flag)
//	scheme --file file.scm -i     # Execute file, then enter REPL
//	scheme --version              # Print version and exit
//	scheme --mcp                  # Start MCP server on stdio
//
// Library search paths can be configured via the -L flag or SCHEME_LIBRARY_PATH
// environment variable. The command uses BSD sysexits.h exit codes.
package main
