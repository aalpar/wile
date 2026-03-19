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
// It maps keywords to their validation and compilation functions, allowing
// both the validate and machine packages to share the same dispatch table.
package forms

import (
	"context"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

// ValidatedExpr is the interface for all validated expressions.
// It lives in the forms package (rather than validate) to break the
// validate → forms ← machine import cycle while preserving type safety.
type ValidatedExpr interface {
	SetFormName(name string)
	FormName() string
	Source() *syntax.SourceContext
}

// ValidatorFunc is the signature for validation functions.
// The result parameter remains [any] (*validate.ValidationResult) because
// validate imports forms, so forms cannot import validate.
type ValidatorFunc func(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result any) ValidatedExpr

// CompilerFunc is the signature for compilation functions.
// The ctc (*machine.CompileTimeContinuation) and ctctx (machine.CompileTimeCallContext)
// parameters remain [any] because machine imports forms.
type CompilerFunc func(ctc any, ctctx any, expr ValidatedExpr) error

// FormSpec defines how a special form is validated and compiled.
//
// Parameters that cross the validate/machine boundary use [any] to break
// import cycles. Type safety is restored at registration time:
// validate/register.go wraps typed validators into [ValidatorFunc], and
// machine/register.go wraps typed compilers into [CompilerFunc].
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

// expandTimeOnlyForms are forms handled entirely during expansion that
// legitimately have no compiler. They never reach the compilation phase.
var expandTimeOnlyForms = map[string]bool{
	"let-syntax":    true, // primitive expander, wrapper disappears after expansion
	"letrec-syntax": true, // primitive expander, wrapper disappears after expansion
	"syntax-rules":  true, // compiled inline by CompileSyntaxRules within define-syntax
}

// Verify checks that every registered form has both a validator and a compiler,
// except for forms that are handled entirely during expansion. Returns an error
// listing any forms with missing handlers, or nil if all pairings are consistent.
func Verify() error {
	var missing []string
	for name, spec := range registry {
		if spec.Validate == nil {
			missing = append(missing, name+": missing validator")
		}
		if spec.Compile == nil && !expandTimeOnlyForms[name] {
			missing = append(missing, name+": missing compiler")
		}
	}
	if len(missing) == 0 {
		return nil
	}
	var b strings.Builder
	for _, m := range missing {
		b.WriteString("\n  ")
		b.WriteString(m)
	}
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"form registration inconsistencies:%s", b.String())
}

// Names returns all registered form names.
func Names() []string {
	names := make([]string, 0, len(registry))
	for name := range registry {
		names = append(names, name)
	}
	return names
}
