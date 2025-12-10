package validate

import (
	"skeme/syntax"
	"skeme/values"
)

// ValidateExpression validates a syntax expression and returns
// a validated form or a list of errors
func ValidateExpression(expr syntax.SyntaxValue) *ValidationResult {
	result := &ValidationResult{}
	validated := validateExpr(expr, result)
	result.Expr = validated
	return result
}

func validateExpr(expr syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	switch e := expr.(type) {
	case *syntax.SyntaxPair:
		return validateForm(e, result)
	case *syntax.SyntaxSymbol:
		return &ValidatedSymbol{source: e.SourceContext(), Symbol: e}
	case *syntax.SyntaxObject:
		return validateSyntaxObject(e, result)
	default:
		// Self-evaluating: numbers, strings, booleans, etc.
		return &ValidatedLiteral{source: nil, Value: expr}
	}
}

func validateSyntaxObject(obj *syntax.SyntaxObject, result *ValidationResult) ValidatedExpr {
	wrapped := obj.Unwrap()
	switch wrapped.(type) {
	case *values.Symbol:
		// This shouldn't happen - symbols should be SyntaxSymbol, not SyntaxObject
		// But handle it defensively
		result.addError(obj.SourceContext(), "expression", "unexpected symbol wrapped in SyntaxObject")
		return nil
	default:
		// Self-evaluating literal wrapped in syntax
		return &ValidatedLiteral{source: obj.SourceContext(), Value: obj}
	}
}

func validateForm(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	// Get the first element to determine the form type
	car := pair.Car()

	// Check if it's a special form by looking at the head
	if sym, ok := car.(*syntax.SyntaxSymbol); ok {
		symVal, ok := sym.Unwrap().(*values.Symbol)
		if ok {
			switch symVal.Key {
			case "if":
				return validateIf(pair, result)
			case "define":
				return validateDefine(pair, result)
			case "lambda":
				return validateLambda(pair, result)
			case "case-lambda":
				return validateCaseLambda(pair, result)
			case "set!":
				return validateSetBang(pair, result)
			case "quote":
				return validateQuote(pair, result)
			case "begin":
				return validateBegin(pair, result)
			case "quasiquote":
				return validateQuasiquote(pair, result)
			// Macro and library forms - structural validation before compiler handles them
			case "define-syntax":
				return validateDefineSyntax(pair, result)
			case "syntax-rules":
				return validateSyntaxRules(pair, result)
			case "import":
				return validateImport(pair, result)
			case "export":
				return validateExport(pair, result)
			case "define-library", "library":
				return validateDefineLibrary(pair, result)
			case "include", "include-ci":
				return validateInclude(pair, result)
			case "cond-expand":
				return validateCondExpand(pair, result)
			case "meta":
				// Meta passes through to compiler directly
				return &ValidatedLiteral{source: pair.SourceContext(), Value: pair}
			}
		}
	}

	// Not a special form - it's a function call
	return validateCall(pair, result)
}

// collectList converts a syntax list to a slice of elements.
// Returns the elements and whether the list is improper.
func collectList(pair *syntax.SyntaxPair) ([]syntax.SyntaxValue, bool) {
	var elements []syntax.SyntaxValue
	var current values.Value = pair

	for {
		p, ok := current.(*syntax.SyntaxPair)
		if !ok {
			// Not a pair - improper list
			return elements, true
		}

		// Check for empty list marker (SyntaxPair with nil values)
		if p.IsEmptyList() {
			return elements, false // proper list
		}

		// Get the car element
		car := p.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			elements = append(elements, carSyntax)
		} else if car != nil {
			// Wrap non-syntax values
			elements = append(elements, syntax.NewSyntaxObject(car, nil))
		}
		current = p.Cdr()
	}
}

// getSourceContext extracts SourceContext from a SyntaxValue if available
func getSourceContext(v syntax.SyntaxValue) *syntax.SourceContext {
	switch sv := v.(type) {
	case *syntax.SyntaxPair:
		return sv.SourceContext()
	case *syntax.SyntaxSymbol:
		return sv.SourceContext()
	case *syntax.SyntaxObject:
		return sv.SourceContext()
	default:
		return nil
	}
}

// isSyntaxSymbol checks if a SyntaxValue is a symbol
func asSyntaxSymbol(v syntax.SyntaxValue) (*syntax.SyntaxSymbol, bool) {
	if sym, ok := v.(*syntax.SyntaxSymbol); ok {
		return sym, true
	}
	// Also check for SyntaxObject wrapping a symbol
	if obj, ok := v.(*syntax.SyntaxObject); ok {
		if _, ok := obj.Unwrap().(*values.Symbol); ok {
			// This is unusual - symbols should be SyntaxSymbol
			return nil, false
		}
	}
	return nil, false
}
