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

// Package forms provides a shared registry for special form validators.
//
// The package maps Scheme keywords (if, lambda, define, etc.) to their
// validation functions. This decouples the validate and machine/compilation
// packages by providing a shared name table that both can reference.
//
// A form's codegen function is registered here too, on [FormSpec.Compile].
// That field is [any]-typed because its concrete type
// (machine/compilation.CompilerFunc) cannot be named without a
// forms → machine/compilation import cycle. machine/compilation asserts it back
// to the typed signature at the dispatch site, and its VerifyCompilers
// backstops a mis-typed Compile.
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
package forms
