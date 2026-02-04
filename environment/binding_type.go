// Copyright 2025 Aaron Alpar
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
type BindingType int

const (
	// BindingTypeUnknown indicates a binding with unknown or uninitialized type.
	BindingTypeUnknown = BindingType(iota)
	// BindingTypeVariable indicates a regular variable binding (from define, let, lambda parameters).
	BindingTypeVariable
	// BindingTypeSyntax indicates a syntax transformer binding (from define-syntax).
	// These bindings live in the expand phase environment.
	BindingTypeSyntax
	// BindingTypePrimitive indicates a built-in primitive procedure.
	BindingTypePrimitive
)
