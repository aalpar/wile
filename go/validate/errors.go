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

package validate

import (
	"fmt"
	"strings"

	"wile/syntax"
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
	if p.Source != nil {
		msg = fmt.Sprintf("%s:%d:%d: %s in %s form",
			p.Source.File, p.Source.Start.Line(), p.Source.Start.Column(),
			p.Message, p.Form)

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
	sb.WriteString(fmt.Sprintf("%d validation errors:\n", len(p.Errors)))
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

func (p *ValidationResult) addErrorf(source *syntax.SourceContext, form, format string, args ...interface{}) {
	p.addError(source, form, fmt.Sprintf(format, args...))
}
