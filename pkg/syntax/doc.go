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

// Package syntax implements Scheme syntax representation with hygiene support.
//
// The package wraps Scheme values with source location and scope information:
//
// # Syntax Types
//
//   - [SyntaxSymbol]: symbols with scope tracking for hygiene
//   - [SyntaxPair]: cons cells with recursive scope propagation
//   - [SyntaxVector]: vectors with source context
//   - [SyntaxObject]: wrapper for self-evaluating values
//
// All syntax types implement [SyntaxValue], which provides:
//   - [SyntaxValue.SourceContext]: source location, scopes, and origin
//   - [SyntaxValue.Unwrap]: shallow unwrap to underlying value
//   - [SyntaxValue.UnwrapAll]: deep recursive unwrap
//
// # Hygiene
//
// The package implements Flatt's "sets of scopes" model for macro hygiene:
//   - [Scope]: unique identifier with optional rebinding flag
//   - [SourceContext.Scopes]: scope set attached to syntax objects
//   - [ScopesMatch]: binding/reference scope subset check
//
// # Source Tracking
//
//   - [SourceContext]: file, line, column, text, scopes, and origin
//   - [SourceIndexes]: position within source (line, column, byte offset)
//   - [OriginInfo]: macro expansion chain for debugging
package syntax
