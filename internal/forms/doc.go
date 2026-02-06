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

// Package forms provides a unified registry for special form handlers.
//
// The package maps Scheme keywords (if, lambda, define, etc.) to their
// validation and compilation functions. This decouples the validate and
// machine packages by providing a shared dispatch table.
//
// # Registration
//
//	forms.RegisterValidator("if", validateIf)
//	forms.RegisterCompiler("if", compileIf)
//
// # Lookup
//
//	spec := forms.Lookup("if")
//	if spec != nil && spec.Validate != nil {
//	    result := spec.Validate(ctx, env, pair, validationResult)
//	}
//
// The registry uses [any] for function parameter types to break circular
// import cycles. Type safety is enforced at call sites via runtime assertions.
package forms
