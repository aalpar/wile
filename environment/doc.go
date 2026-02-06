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

// Package environment provides variable binding and scoping for the Scheme compiler.
//
// The environment system manages the relationship between variables and values across:
//   - Lexical scoping via parent chain traversal
//   - Phase separation (Runtime, Expand, Compile)
//   - Hygienic macros via Flatt's "sets of scopes" model
//   - Per-instance symbol interning for eq? identity
//   - R7RS library import/export mappings
//
// # Architecture
//
// Each Wile VM instance owns a [TopLevelEnvironment] that provides:
//   - Symbol interning (thread-safe, per-instance)
//   - Phase registry for accessing phase-specific environments
//   - The runtime [EnvironmentFrame] tree
//
// [EnvironmentFrame] nodes form the lexical scope chain, each containing:
//   - Local bindings ([LocalEnvironmentFrame]) for lambda params and let-bound vars
//   - Global bindings ([GlobalEnvironmentFrame]) for top-level defines
//   - Parent pointer for enclosing scopes
//
// # Binding Lookup
//
// [EnvironmentFrame.GetBinding] performs two-phase lookup:
//  1. Local phase: traverse local bindings up the parent chain
//  2. Global phase: search global bindings up the parent chain
//
// [EnvironmentFrame.GetBindingWithScopes] adds hygiene awareness, using
// [Scope] sets to match identifiers according to Flatt's algorithm.
package environment
