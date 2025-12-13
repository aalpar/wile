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
	Form    string // e.g., "if", "define", "lambda"
}

func (e ValidationError) Error() string {
	return e.ErrorWithMaxOriginDepth(DefaultMaxOriginDepth)
}

// ErrorWithMaxOriginDepth returns the error message with a configurable origin chain depth.
func (e ValidationError) ErrorWithMaxOriginDepth(maxDepth int) string {
	var msg string
	if e.Source != nil {
		msg = fmt.Sprintf("%s:%d:%d: %s in %s form",
			e.Source.File, e.Source.Start.Line(), e.Source.Start.Column(),
			e.Message, e.Form)

		// Add origin chain if present
		if e.Source.Origin != nil {
			msg += syntax.FormatOriginChain(e.Source.Origin, maxDepth)
		}
	} else {
		msg = fmt.Sprintf("%s in %s form", e.Message, e.Form)
	}
	return msg
}

// ValidationResult collects all errors from validation
type ValidationResult struct {
	Expr   ValidatedExpr     // nil if validation failed
	Errors []ValidationError // All errors encountered
}

func (r *ValidationResult) Ok() bool {
	return len(r.Errors) == 0
}

func (r *ValidationResult) Error() string {
	if len(r.Errors) == 0 {
		return ""
	}
	if len(r.Errors) == 1 {
		return r.Errors[0].Error()
	}
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("%d validation errors:\n", len(r.Errors)))
	for _, e := range r.Errors {
		sb.WriteString("  ")
		sb.WriteString(e.Error())
		sb.WriteString("\n")
	}
	return sb.String()
}

func (r *ValidationResult) addError(source *syntax.SourceContext, form, message string) {
	r.Errors = append(r.Errors, ValidationError{
		Source:  source,
		Message: message,
		Form:    form,
	})
}

func (r *ValidationResult) addErrorf(source *syntax.SourceContext, form, format string, args ...interface{}) {
	r.addError(source, form, fmt.Sprintf(format, args...))
}
