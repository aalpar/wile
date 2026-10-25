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

// Error renders the cause with its position and the macro-origin chain of the
// node it was stamped on. Only the innermost SourcedError renders either: it
// names the node that failed, so its location is the most specific (the exact
// sub-expression, not the enclosing form) and its chain is the one that
// describes that node. Outer SourcedErrors stay in the chain for programmatic
// extraction via errors.As but add nothing to the text; a chain on an outer
// link would blame a macro for syntax it merely passed through.
func (p *SourcedError) Error() string {
	var inner *SourcedError
	if errors.As(p.Cause, &inner) {
		return p.Cause.Error()
	}
	return renderSourced(p.Source, p.Cause.Error())
}

// renderSourced is the one renderer of a compile-time failure's where: the
// location prefixed, the expansion chain appended.
func renderSourced(src *syntax.SourceContext, text string) string {
	q := text
	loc := src.Location()
	if loc != "" {
		q = loc + ": " + q
	}
	return q + validate.OriginChainText(src)
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
// macro origin rather than the top-level form being compiled. Several errors
// have no one owning node: each line is rendered by renderSourced and the list
// is left for the caller to stamp with the enclosing form.
func validationFailure(result *validate.ValidationResult) error {
	if len(result.Errors) == 1 {
		e := result.Errors[0]
		return wrapSourcedError(e.Source, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", e.Failure()))
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%d validation errors:\n", len(result.Errors))
	for _, e := range result.Errors {
		sb.WriteString("  ")
		sb.WriteString(renderSourced(e.Source, e.Failure()))
		sb.WriteString("\n")
	}
	return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", sb.String())
}
