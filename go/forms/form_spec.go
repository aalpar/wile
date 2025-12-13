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

// Package forms provides a unified registry for special form handlers.
// It maps keywords to their validation and compilation functions, allowing
// both the validate and machine packages to share the same dispatch table.
package forms

import (
	"context"
)

// ValidatorFunc is the signature for validation functions.
// Parameters:
//   - ctx: context for cancellation
//   - pair: *syntax.SyntaxPair - the form to validate
//   - result: *validate.ValidationResult - collects errors
//
// Returns: validate.ValidatedExpr
type ValidatorFunc func(ctx context.Context, pair any, result any) any

// CompilerFunc is the signature for compilation functions.
// Parameters:
//   - ctc: *machine.CompileTimeContinuation - compiler state
//   - ctctx: machine.CompileTimeCallContext - call context
//   - expr: validate.ValidatedExpr - the validated expression
//
// Returns: error
type CompilerFunc func(ctc any, ctctx any, expr any) error

// FormSpec defines how a special form is validated and compiled.
type FormSpec struct {
	// Name is the keyword that triggers this form (e.g., "if", "lambda").
	Name string

	// Validate is called during the validation phase to produce a ValidatedExpr.
	// If nil, the form passes through as ValidatedLiteral.
	Validate ValidatorFunc

	// Compile is called during compilation to emit bytecode.
	// If nil, the form cannot be compiled (error).
	Compile CompilerFunc
}

// registry holds all registered special forms.
var registry = make(map[string]*FormSpec)

// Register adds a FormSpec to the registry.
// If a spec with the same name exists, it is replaced.
func Register(spec *FormSpec) {
	registry[spec.Name] = spec
}

// RegisterValidator sets the validator for an existing form or creates a new entry.
func RegisterValidator(name string, fn ValidatorFunc) {
	spec := registry[name]
	if spec == nil {
		spec = &FormSpec{Name: name}
		registry[name] = spec
	}
	spec.Validate = fn
}

// RegisterCompiler sets the compiler for an existing form or creates a new entry.
func RegisterCompiler(name string, fn CompilerFunc) {
	spec := registry[name]
	if spec == nil {
		spec = &FormSpec{Name: name}
		registry[name] = spec
	}
	spec.Compile = fn
}

// Lookup returns the FormSpec for a keyword, or nil if not found.
func Lookup(name string) *FormSpec {
	return registry[name]
}

// Names returns all registered form names.
func Names() []string {
	names := make([]string, 0, len(registry))
	for name := range registry {
		names = append(names, name)
	}
	return names
}
