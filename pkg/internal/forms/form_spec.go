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
	"maps"
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
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

// FormRegistry is a per-engine set of special-form specs. The package-level
// default (defaultRegistry) backs the delegator functions and the built-in
// R7RS forms; a dialect forks a Clone at engine origin (later tasks).
type FormRegistry struct {
	specs map[string]*FormSpec
}

// NewFormRegistry returns an empty registry.
func NewFormRegistry() *FormRegistry {
	return &FormRegistry{specs: make(map[string]*FormSpec)}
}

// Register adds or replaces a complete FormSpec.
func (r *FormRegistry) Register(spec *FormSpec) {
	r.specs[spec.Name] = spec
}

// RegisterValidator sets the validator for a form. It is copy-on-write: a fresh
// FormSpec is assigned rather than mutating an existing one in place, so an
// override on a clone cannot corrupt the default (or any clone sharing the
// pointer). Safe because FormSpec carries only Name+Validate.
func (r *FormRegistry) RegisterValidator(name string, fn ValidatorFunc) {
	r.specs[name] = &FormSpec{Name: name, Validate: fn}
}

// Lookup returns the FormSpec for a keyword, or nil if not found.
func (r *FormRegistry) Lookup(name string) *FormSpec {
	return r.specs[name]
}

// Remove deletes a form so a dialect can drop it: the validator disappears and
// the keyword is thereafter validated as a call. No-op if the form is absent.
func (r *FormRegistry) Remove(name string) {
	delete(r.specs, name)
}

// Names returns all registered form names (unordered).
func (r *FormRegistry) Names() []string {
	names := make([]string, 0, len(r.specs))
	for name := range r.specs {
		names = append(names, name)
	}
	return names
}

// Verify checks that every registered form has a validator. Returns an error
// listing any forms with missing validators, or nil if all are consistent.
//
// Compiler registration consistency is checked separately by
// machine/compilation.VerifyCompilers, which has access to the typed
// compiler registry without requiring any type erasure.
func (r *FormRegistry) Verify() error {
	var missing []string
	for name, spec := range r.specs {
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

// Clone returns a shallow copy. Safe because RegisterValidator is copy-on-write.
func (r *FormRegistry) Clone() *FormRegistry {
	return &FormRegistry{specs: maps.Clone(r.specs)}
}

// defaultRegistry backs the package-level delegators and holds the built-in
// forms registered by validate/register.go's init(). EAGER (never lazy): the
// delegators dereference it unguarded, and validate's init() populates it at
// import time.
var defaultRegistry = NewFormRegistry()

// DefaultRegistry returns the package default (the R7RS baseline forms).
func DefaultRegistry() *FormRegistry {
	return defaultRegistry
}

// Register adds or replaces a FormSpec in the package default.
func Register(spec *FormSpec) {
	defaultRegistry.Register(spec)
}

// RegisterValidator sets a validator on the package default.
func RegisterValidator(name string, fn ValidatorFunc) {
	defaultRegistry.RegisterValidator(name, fn)
}

// Lookup returns the FormSpec for a keyword from the package default.
func Lookup(name string) *FormSpec {
	return defaultRegistry.Lookup(name)
}

// Names returns all form names in the package default.
func Names() []string {
	return defaultRegistry.Names()
}

// Verify checks the package default for forms missing validators.
func Verify() error {
	return defaultRegistry.Verify()
}
