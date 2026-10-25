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

package compilation

import (
	"errors"
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// SourcedError wraps a compilation error with the source location where it
// occurred. The compiler tracks source context via pushSource/popSource;
// wrapCompilationError attaches the current source to errors so that callers
// (especially the public Engine API) can report file:line:col.
//
// Use errors.As to extract the source from an error chain:
//
//	var se *compilation.SourcedError
//	if errors.As(err, &se) && se.Source != nil { ... }
type SourcedError struct {
	Source *syntax.SourceContext
	Cause  error
}

// Error renders the cause with a position and a macro-origin chain, each once
// across the whole chain of SourcedErrors:
//   - the location is that of the deepest SourcedError that has one, the most
//     specific (the exact sub-expression, not the enclosing form); a deeper
//     position-less node, such as a syntax-case template pair, does not hide it;
//   - the chain is that of the deepest SourcedError, the node that failed. A
//     chain on an outer link would blame a macro for syntax it merely passed
//     through.
//
// Outer SourcedErrors stay in the chain for programmatic extraction via
// errors.As but add nothing to the text.
func (p *SourcedError) Error() string {
	chain := ""
	var inner *SourcedError
	if !errors.As(p.Cause, &inner) {
		chain = syntax.OriginChainText(p.Source)
	}
	loc := ""
	if !locatedBelow(p.Cause) {
		loc = p.Source.Location()
	}
	return renderSourced(loc, p.Cause.Error(), chain)
}

// locatedBelow reports whether err holds a SourcedError with a location, which
// then renders it instead of any link above.
func locatedBelow(err error) bool {
	var se *SourcedError
	for e := err; errors.As(e, &se); e = se.Cause {
		if se.Source.Location() != "" {
			return true
		}
	}
	return false
}

// renderSourced is the one layout of a compile-time failure's where: the
// location prefixed, the expansion chain appended.
func renderSourced(loc, text, chain string) string {
	if loc == "" {
		return text + chain
	}
	return loc + ": " + text + chain
}

func (p *SourcedError) Unwrap() error {
	return p.Cause
}

// SourceContext returns the context this error was stamped with. It exists so
// that pkg/machine can recognise the type structurally: this package imports
// pkg/machine, so the concrete type cannot be named from that side of the edge,
// and an errors.As target there has to be an interface. Source is the same field
// and stays exported for the callers that already read it.
func (p *SourcedError) SourceContext() *syntax.SourceContext {
	return p.Source
}

// wrapSourcedError attaches source location to an error.
// For use in standalone functions that lack a CompileTimeContinuation receiver.
// If src is nil, returns the error unchanged.
func wrapSourcedError(src *syntax.SourceContext, err error) error {
	if err == nil || src == nil {
		return err
	}
	return &SourcedError{Source: src, Cause: err}
}

// validationFailure carries a failed validation as an error. A single error is
// stamped on the node it names, so errors.As reaches that node's position and
// macro origin rather than the top-level form being compiled.
//
// Several errors have no one owning node. Each line renders its own node's
// location and chain, and the list is stamped with a nil Source: the deepest
// SourcedError, so the enclosing form's stamp above it renders its location but
// not its chain a second time, and the Scheme condition cuts its message there
// (machine.failureChain) with the whole list intact.
func validationFailure(result *validate.ValidationResult) error {
	if len(result.Errors) == 1 {
		e := result.Errors[0]
		return wrapSourcedError(e.Source, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", e.Failure()))
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%d validation errors:\n", len(result.Errors))
	for _, e := range result.Errors {
		sb.WriteString("  ")
		sb.WriteString(renderSourced(e.Source.Location(), e.Failure(), syntax.OriginChainText(e.Source)))
		sb.WriteString("\n")
	}
	return &SourcedError{Cause: werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", sb.String())}
}
