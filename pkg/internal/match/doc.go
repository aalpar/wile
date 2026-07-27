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

// Package match implements the pattern matching engine for syntax-rules and syntax-case.
//
// The package provides four layers:
//
// # Pattern Compiler
//
// [SyntaxCompiler] compiles pattern S-expressions into bytecode at macro
// definition time. Bytecode instructions handle literal matching, variable
// capture, and ellipsis repetition.
//
// # Matching VM
//
// [Matcher] executes bytecode against input forms at macro invocation time,
// capturing pattern variable bindings. The VM uses two stacks:
//   - Syntax stack: tracks position in the input tree
//   - Capture stack: tracks captured bindings with nesting for ellipsis
//
// # Syntax Adapter
//
// [SyntaxMatcher] adds hygiene: scope-aware literal matching and template
// expansion with intro scopes per Flatt's "sets of scopes" model.
//
// # Template Expansion (syntax_expand.go)
//
//   - [SyntaxMatcher.Expand]: expand template with captured bindings via [ExpandOptions]
//
// Reference: R7RS Section 4.3.2 (syntax-rules pattern language).
package match
