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

package validate

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// DefaultMaxOriginDepth is the default maximum number of macro expansions to show
// in error messages. Set to 0 for unlimited depth.
const DefaultMaxOriginDepth = 10

// ValidationError captures location and details for error reporting
type ValidationError struct {
	Source  *syntax.SourceContext
	Message string
	Form    string // p.g., "if", "define", "lambda"
}

func (p ValidationError) Error() string {
	return p.ErrorWithMaxOriginDepth(DefaultMaxOriginDepth)
}

// ErrorWithMaxOriginDepth returns the error message with a configurable origin chain depth.
func (p ValidationError) ErrorWithMaxOriginDepth(maxDepth int) string {
	var msg string
	if loc := p.Source.Location(); loc != "" {
		msg = fmt.Sprintf("%s: %s in %s form", loc, p.Message, p.Form)

		// Add origin chain if present
		if p.Source.Origin != nil {
			msg += syntax.FormatOriginChain(p.Source.Origin, maxDepth)
		}
	} else {
		msg = fmt.Sprintf("%s in %s form", p.Message, p.Form)
	}
	return msg
}

// ValidationResult collects all errors from validation
type ValidationResult struct {
	Expr            ValidatedExpr     // nil if validation failed
	Errors          []ValidationError // All errors encountered
	mutatedBindings map[environment.BindingID]bool

	// mutatedKeys and definedKeyCount track set! targets and define occurrences
	// by symbol Key across the whole unit, independent of binding creation.
	// Top-level global bindings are created by the compiler, not the validator,
	// so mutatedBindings (keyed by BindingID) cannot see a top-level set! inside
	// a (begin ...) unit. These power StableInUnit, computed syntactically and
	// conservatively by Key (over-match ⇒ non-stable). See finalizeStability.
	mutatedKeys     map[string]bool
	definedKeyCount map[string]int
}

// markMutated records that a local binding is targeted by set!.
// Uses BindingID (frame pointer + slot index) instead of *Binding
// because LocalEnvironmentFrame.bindings is []Binding (value type);
// pointers into the slice become stale when append reallocates.
func (p *ValidationResult) markMutated(bid environment.BindingID) {
	if p.mutatedBindings == nil {
		p.mutatedBindings = make(map[environment.BindingID]bool)
	}
	p.mutatedBindings[bid] = true
}

// isMutated returns true if the binding was targeted by set!.
func (p *ValidationResult) isMutated(bid environment.BindingID) bool {
	return p.mutatedBindings[bid]
}

// markMutatedKey records that a symbol named key is a set! target somewhere in
// the unit. Recorded by symbol Key (not BindingID) so a top-level set! is
// captured even before the compiler creates the binding. Over-approximates: a
// set! to any same-Key binding (including a local shadow) marks the Key.
func (p *ValidationResult) markMutatedKey(key string) {
	if p.mutatedKeys == nil {
		p.mutatedKeys = make(map[string]bool)
	}
	p.mutatedKeys[key] = true
}

// recordDefinedKey counts a define of a symbol named key in the unit. Used to
// enforce defined-once: a Key defined twice (e.g. a top-level redefinition or a
// same-name internal define) is not in-unit-stable.
func (p *ValidationResult) recordDefinedKey(key string) {
	if p.definedKeyCount == nil {
		p.definedKeyCount = make(map[string]int)
	}
	p.definedKeyCount[key]++
}

// finalizeStability stamps StableInUnit on every top-level define once the unit
// is fully validated (so mutatedKeys/definedKeyCount are complete). A define is
// in-unit-stable iff its name is defined exactly once and never set! in the
// unit. Top-level only: internal/local defines never reach the compiler's
// global Stable stamp, so they are intentionally not visited here.
func (p *ValidationResult) finalizeStability() {
	if p.Expr == nil {
		return
	}
	collectTopLevelDefines([]ValidatedExpr{p.Expr}, func(d *ValidatedDefine) {
		key := d.name.Key()
		d.StableInUnit = p.definedKeyCount[key] == 1 && !p.mutatedKeys[key]
	})
}

// Ok returns true if no validation errors were encountered.
func (p *ValidationResult) Ok() bool {
	return len(p.Errors) == 0
}

func (p *ValidationResult) Error() string {
	if len(p.Errors) == 0 {
		return ""
	}
	if len(p.Errors) == 1 {
		return p.Errors[0].Error()
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%d validation errors:\n", len(p.Errors))
	for _, e := range p.Errors {
		sb.WriteString("  ")
		sb.WriteString(e.Error())
		sb.WriteString("\n")
	}
	return sb.String()
}

func (p *ValidationResult) addError(source *syntax.SourceContext, form, message string) {
	p.Errors = append(p.Errors, ValidationError{
		Source:  source,
		Message: message,
		Form:    form,
	})
}

func (p *ValidationResult) addErrorf(source *syntax.SourceContext, form, format string, args ...any) {
	p.addError(source, form, fmt.Sprintf(format, args...))
}
