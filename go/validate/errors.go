package validate

import (
	"fmt"
	"strings"

	"wile/syntax"
)

// ValidationError captures location and details for error reporting
type ValidationError struct {
	Source  *syntax.SourceContext
	Message string
	Form    string // e.g., "if", "define", "lambda"
}

func (e ValidationError) Error() string {
	if e.Source != nil {
		return fmt.Sprintf("%s:%d:%d: %s in %s form",
			e.Source.File, e.Source.Start.Line(), e.Source.Start.Column(),
			e.Message, e.Form)
	}
	return fmt.Sprintf("%s in %s form", e.Message, e.Form)
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
