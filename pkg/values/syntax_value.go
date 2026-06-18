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

package values

// SyntaxValue is the interface for all syntax objects.
// It provides access to source context and unwrapping capabilities.
//
// The interface is defined in package values (rather than in the syntax
// package) so that the empty-list singleton (values.EmptyList) can directly
// implement it. This collapses the historical duality between
// values.emptyListType and the (now removed) syntaxEmptyListType — the
// empty list carries no symbols, no scopes, and no source-attachable
// hygiene content, matching Chez's `(equal? (syntax ()) '()) → #t`.
type SyntaxValue interface {
	Value
	SourceContext() *SourceContext
	Unwrap() Value
	UnwrapAll() Value
}
