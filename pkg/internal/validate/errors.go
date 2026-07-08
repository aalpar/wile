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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
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
	Expr   ValidatedExpr     // nil if validation failed
	Errors []ValidationError // All errors encountered

	// mutated records every set! target in the unit, keyed by BindingRef so a
	// single map names both halves of the binding domain. Each set! records
	// two facts at the two precisions its consumers need:
	//   - a precise LocalRef when the target resolves to a local slot (read by
	//     markMutableBindings to set LetBinding.Mutable);
	//   - a conservative GlobalRef(name) unconditionally (read by
	//     finalizeStability for StableInUnit). The global arm is symbolic
	//     because a top-level binding is created by the compiler, not the
	//     validator, so no slot exists at validation time; marking it even for
	//     a local shadow of the name is the deliberate over-approximation that
	//     keeps the frame-reclaim Stable stamp sound (over-match ⇒ non-stable).
	mutated map[environment.BindingRef]bool

	// definedKeyCount counts define occurrences by symbol Key across the unit.
	// A distinct signal from mutation: powers the defined-once half of
	// StableInUnit. See finalizeStability.
	definedKeyCount map[string]int
}

// markMutated records that the binding named by ref is a set! target. The ref
// is a precise LocalRef (frame + slot) for a resolved local — stable across
// []Binding realloc, unlike a *Binding pointer — or a symbolic GlobalRef(Key)
// for a global or a not-yet-created top-level binding.
func (p *ValidationResult) markMutated(ref environment.BindingRef) {
	if p.mutated == nil {
		p.mutated = make(map[environment.BindingRef]bool)
	}
	p.mutated[ref] = true
}

// isMutated returns true if the binding named by ref was targeted by set!.
func (p *ValidationResult) isMutated(ref environment.BindingRef) bool {
	return p.mutated[ref]
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
		d.StableInUnit = p.definedKeyCount[key] == 1 && !p.isMutated(environment.GlobalRef(key))
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
