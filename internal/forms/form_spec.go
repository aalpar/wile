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

package forms

import (
	"context"
	"slices"
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

// FormSpec defines how a special form is validated.
//
// The result parameter in ValidatorFunc uses [any] to break the
// forms → validate import cycle. Type safety is restored at registration
// time in validate/register.go.
//
// Compiler dispatch lives in machine/compilation (typed, no [any]).
type FormSpec struct {
	// Name is the keyword that triggers this form (e.g., "if", "lambda").
	Name string

	// Validate is called during the validation phase to produce a ValidatedExpr.
	// If nil, the form passes through as ValidatedLiteral.
	Validate ValidatorFunc
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

// Lookup returns the FormSpec for a keyword, or nil if not found.
func Lookup(name string) *FormSpec {
	return registry[name]
}

// Verify checks that every registered form has a validator. Returns an error
// listing any forms with missing validators, or nil if all are consistent.
//
// Compiler registration consistency is checked separately by
// machine/compilation.VerifyCompilers, which has access to the typed
// compiler registry without requiring any type erasure.
func Verify() error {
	var missing []string
	for name, spec := range registry {
		if spec.Validate == nil {
			missing = append(missing, name+": missing validator")
		}
	}
	if len(missing) == 0 {
		return nil
	}
	slices.Sort(missing)
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
