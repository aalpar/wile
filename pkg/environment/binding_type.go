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

package environment

// BindingType represents the type of a binding in the environment.
//
// Three of the four constants are observable:
//
//   - BindingTypeVariable — regular runtime variables.
//   - BindingTypeSyntax — syntax transformers.
//   - BindingTypePrimitive — compile-time bindings (special forms, etc).
//
// BindingTypeUnknown (the zero value) is internal scaffolding only. It is
// the type of pre-allocated slots in NewLocalEnvironment before they are
// assigned a real binding type, and is never observed by GetBinding or any
// external consumer. It exists solely to be a meaningful zero value for
// the [BindingType] field of a freshly constructed [Binding] in a
// pre-allocated frame slot. Removing it would require a sentinel layer
// (e.g. nil-binding markers in LocalEnvironmentFrame.bindings) and is
// not justified by the current call-site set.
type BindingType int

const (
	// BindingTypeUnknown is the zero value used for pre-allocated, not-yet-
	// bound slots in LocalEnvironmentFrame. It must not be observed by
	// callers outside the environment package.
	BindingTypeUnknown = BindingType(iota)
	// BindingTypeVariable indicates a regular variable binding (from define, let, lambda parameters).
	BindingTypeVariable
	// BindingTypeSyntax indicates a syntax transformer binding (from define-syntax).
	// These bindings live in the expand phase environment.
	BindingTypeSyntax
	// BindingTypePrimitive indicates a compile-time binding (special forms, auxiliary syntax).
	BindingTypePrimitive
)
