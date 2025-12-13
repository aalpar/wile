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

package machine

// expander_time_continuation.go implements macro expansion for syntax-rules.
//
// The expander runs after parsing and before compilation. It walks the syntax
// tree looking for macro invocations and expands them to their definitions.
//
// Expansion Process:
//   1. For each expression, check if it's a macro invocation
//   2. If yes, invoke the transformer closure (OperationSyntaxRulesTransform)
//   3. The transformer returns the expanded syntax
//   4. Recursively expand the result (macros can expand to other macro calls)
//   5. Return the fully expanded syntax tree to the compiler
//
// Macro Detection:
//   When ExpandSyntaxExpression sees a symbol, it checks the environment
//   for a binding with BindingTypeSyntax. If found, the binding's value
//   is a MachineClosure (the compiled transformer), which is invoked.
//
// The expander is separate from the compiler because:
//   - Macros must be expanded before compiling (they change the syntax)
//   - Expansion may need to run compiled code (the transformer)
//   - Hygiene scopes are added during expansion, not compilation
//
// Reference: R7RS Section 4.3 (Macros)

import (
	"context"
	"errors"
	"fmt"

	"wile/environment"
	"wile/syntax"
	"wile/values"
)

// ExpanderTimeContinuation is a continuation used during the expansion phase.
//
// It walks the syntax tree, detecting and expanding macro invocations.
// The env field provides access to macro definitions (BindingTypeSyntax bindings).
type ExpanderTimeContinuation struct {
	env *environment.EnvironmentFrame
	mc  *MachineContext // Used to run transformer closures
}

func NewExpanderTimeContinuation(env *environment.EnvironmentFrame) *ExpanderTimeContinuation {
	q := &ExpanderTimeContinuation{
		env: env,
	}
	return q
}

func (p *ExpanderTimeContinuation) ExpandExpression(ectx ExpandTimeCallContext, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	switch stx := expr.(type) {
	case *syntax.SyntaxPair:
		// Handle empty list - no expansion needed
		if syntax.IsSyntaxEmptyList(stx) {
			return stx, nil
		}
		car, ok := stx.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return nil, fmt.Errorf("expected a syntax value in list expression, got %T", stx.SyntaxCar())
		}
		cdr, ok := stx.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return nil, fmt.Errorf("expected a list expression expansion, got %T", stx.SyntaxCdr())
		}
		return p.ExpandSyntaxOrProcedureCall(ectx, car, cdr)
	case *syntax.SyntaxSymbol:
		return p.ExpandSymbol(ectx, stx)
	case *syntax.SyntaxObject:
		// Self-evaluating value (integer, boolean, string, etc.)
		return stx, nil
	}
	return p.ExpandSelfEvaluating(ectx, expr)
}

func (p *ExpanderTimeContinuation) ExpandSymbol(ectx ExpandTimeCallContext, expr *syntax.SyntaxSymbol) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandSyntaxOrProcedureCall handles a list expression. The car may be a
// symbol (possibly a macro), a nested pair (computed procedure), or a
// self-evaluating value (like in quoted data or malformed expressions).
func (p *ExpanderTimeContinuation) ExpandSyntaxOrProcedureCall(ectx ExpandTimeCallContext, car syntax.SyntaxValue, cdr *syntax.SyntaxPair) (syntax.SyntaxValue, error) {
	switch v := car.(type) {
	case *syntax.SyntaxPair:
		// Car is a pair - expand it (computed procedure), then expand arguments
		newCar, err := p.ExpandExpression(ectx, v)
		if err != nil {
			return nil, fmt.Errorf("failed to expand car expression: %w", err)
		}
		rest1, err := p.ExpandSyntaxArgumentList(ectx, cdr)
		if err != nil {
			return nil, fmt.Errorf("failed to expand argument list: %w", err)
		}
		return syntax.NewSyntaxCons(newCar, rest1, newCar.SourceContext()), nil
	case *syntax.SyntaxSymbol:
		// Car is a symbol - check if it's a macro, expand arguments either way
		return p.ExpandSyntaxExpression(ectx, v, cdr)
	case *syntax.SyntaxObject:
		// Car is a self-evaluating value - just expand arguments
		rest1, err := p.ExpandSyntaxArgumentList(ectx, cdr)
		if err != nil {
			return nil, fmt.Errorf("failed to expand argument list: %w", err)
		}
		return syntax.NewSyntaxCons(car, rest1, car.SourceContext()), nil
	default:
		// Unknown car type - return expression unchanged
		return syntax.NewSyntaxCons(car, cdr, car.SourceContext()), nil
	}
}

func (p *ExpanderTimeContinuation) ExpandSelfEvaluating(ectx ExpandTimeCallContext, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return expr, nil
}

// ExpandPrimitiveForm handles expansion within primitive forms like if, begin,
// lambda, define, etc. Some primitives need their subexpressions expanded
// (like if, begin) while others should be left unchanged (like quote, define-syntax).
//
// This function looks up the primitive expander in the expand environment registry.
// If found, it invokes the expander; otherwise returns the form unchanged.
func (p *ExpanderTimeContinuation) ExpandPrimitiveForm(ectx ExpandTimeCallContext, primName string, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	// Look up the primitive expander in the registry
	symVal := p.env.InternSymbol(values.NewSymbol(primName))
	scopes := sym.Scopes()

	if pe := LookupPrimitiveExpander(p.env, symVal, scopes); pe != nil {
		return pe.Expand(p, ectx, sym, expr)
	}
	// Unknown primitive - return unchanged (safe default)
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandQuote returns the form unchanged - quote should not expand its argument.
func (p *ExpanderTimeContinuation) expandQuote(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandDefineSyntax returns the form unchanged - define-syntax should not expand the transformer.
func (p *ExpanderTimeContinuation) expandDefineSyntax(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandQuasiquote returns the form unchanged.
// TODO: quasiquote has special expansion rules (only unquote parts).
func (p *ExpanderTimeContinuation) expandQuasiquote(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandUnquote returns the form unchanged - only valid inside quasiquote.
func (p *ExpanderTimeContinuation) expandUnquote(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandUnquoteSplicing returns the form unchanged - only valid inside quasiquote.
func (p *ExpanderTimeContinuation) expandUnquoteSplicing(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandInclude returns the form unchanged - files are read at compile time.
func (p *ExpanderTimeContinuation) expandInclude(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandIncludeCi returns the form unchanged - files are read at compile time.
func (p *ExpanderTimeContinuation) expandIncludeCi(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandCondExpand returns the form unchanged - feature requirements use and/or/not
// which are NOT macros in this context, but special syntax.
func (p *ExpanderTimeContinuation) expandCondExpand(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandSyntaxForm returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandSyntaxForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandSyntaxCase returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandSyntaxCase(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandQuasisyntax returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandQuasisyntax(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandUnsyntax returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandUnsyntax(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandUnsyntaxSplicing returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandUnsyntaxSplicing(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandWithSyntax returns the form unchanged - compile-time form.
func (p *ExpanderTimeContinuation) expandWithSyntax(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandBeginForm expands (begin expr ...) by expanding all subexpressions.
func (p *ExpanderTimeContinuation) expandBeginForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	if exprPair, ok := expr.(*syntax.SyntaxPair); ok && !syntax.IsSyntaxEmptyList(exprPair) {
		expandedArgs, err := p.ExpandSyntaxArgumentList(ectx, exprPair)
		if err != nil {
			return nil, fmt.Errorf("failed to expand begin body: %w", err)
		}
		return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
	}
	return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
}

// expandIfForm expands (if test consequent [alternative])
func (p *ExpanderTimeContinuation) expandIfForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Expand test
	test := pair.SyntaxCar()
	testStx, ok := test.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("if: invalid test expression")
	}
	expandedTest, err := p.ExpandExpression(ectx, testStx)
	if err != nil {
		return nil, fmt.Errorf("if: failed to expand test: %w", err)
	}

	// Get consequent
	cdrVal := pair.SyntaxCdr()
	cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return nil, fmt.Errorf("if: missing consequent")
	}

	conseq := cdrPair.SyntaxCar()
	conseqStx, ok := conseq.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("if: invalid consequent expression")
	}
	expandedConseq, err := p.ExpandExpression(ectx, conseqStx)
	if err != nil {
		return nil, fmt.Errorf("if: failed to expand consequent: %w", err)
	}

	// Check for alternative
	altCdr := cdrPair.SyntaxCdr()
	altPair, ok := altCdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(altPair) {
		// No alternative - (if test conseq)
		// Build: (test . (conseq . ()))
		args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq)
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	// Expand alternative
	alt := altPair.SyntaxCar()
	altStx, ok := alt.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("if: invalid alternative expression")
	}
	expandedAlt, err := p.ExpandExpression(ectx, altStx)
	if err != nil {
		return nil, fmt.Errorf("if: failed to expand alternative: %w", err)
	}

	// Build (if test conseq alt)
	args := syntax.SyntaxList(sym.SourceContext(), expandedTest, expandedConseq, expandedAlt)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandSetForm expands (set! var value)
func (p *ExpanderTimeContinuation) expandSetForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Keep variable unchanged
	varExpr := pair.SyntaxCar()
	varStx, ok := varExpr.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("set!: invalid variable")
	}

	// Expand value
	cdrVal := pair.SyntaxCdr()
	cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	value := cdrPair.SyntaxCar()
	valueStx, ok := value.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("set!: invalid value expression")
	}
	expandedValue, err := p.ExpandExpression(ectx, valueStx)
	if err != nil {
		return nil, fmt.Errorf("set!: failed to expand value: %w", err)
	}

	// Build (set! var expanded-value)
	args := syntax.SyntaxList(sym.SourceContext(), varStx, expandedValue)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandDefineForm expands (define var value) or (define (name . args) body ...)
func (p *ExpanderTimeContinuation) expandDefineForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	first := pair.SyntaxCar()
	firstStx, ok := first.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("define: invalid first argument")
	}

	cdrVal := pair.SyntaxCdr()
	cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Check if it's a function definition (define (name args...) body...)
	if _, isSymbol := first.(*syntax.SyntaxSymbol); !isSymbol {
		// Function definition - first is (name args...)
		// Expand the body expressions
		expandedBody, err := p.ExpandSyntaxArgumentList(ectx, cdrPair)
		if err != nil {
			return nil, fmt.Errorf("define: failed to expand body: %w", err)
		}
		args := syntax.NewSyntaxCons(firstStx, expandedBody, sym.SourceContext())
		return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
	}

	// Simple definition (define var value)
	value := cdrPair.SyntaxCar()
	valueStx, ok := value.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("define: invalid value expression")
	}
	expandedValue, err := p.ExpandExpression(ectx, valueStx)
	if err != nil {
		return nil, fmt.Errorf("define: failed to expand value: %w", err)
	}

	// Build (define var expanded-value)
	args := syntax.SyntaxList(sym.SourceContext(), firstStx, expandedValue)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandLambdaForm expands (lambda (args...) body...)
func (p *ExpanderTimeContinuation) expandLambdaForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Keep formals unchanged
	formals := pair.SyntaxCar()
	formalsStx, ok := formals.(syntax.SyntaxValue)
	if !ok {
		return nil, fmt.Errorf("lambda: invalid formals")
	}

	// Expand body
	cdrVal := pair.SyntaxCdr()
	cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	expandedBody, err := p.ExpandSyntaxArgumentList(ectx, cdrPair)
	if err != nil {
		return nil, fmt.Errorf("lambda: failed to expand body: %w", err)
	}

	// Build (lambda formals expanded-body...)
	args := syntax.NewSyntaxCons(formalsStx, expandedBody, sym.SourceContext())
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}

// expandCaseLambdaForm expands (case-lambda (formals body...) ...)
func (p *ExpanderTimeContinuation) expandCaseLambdaForm(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Expand each clause
	var expandedClauses []syntax.SyntaxValue
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		clauseVal := current.SyntaxCar()
		clauseStx, ok := clauseVal.(syntax.SyntaxValue)
		if !ok {
			return nil, fmt.Errorf("case-lambda: invalid clause")
		}

		// Each clause is (formals body...)
		clausePair, ok := clauseStx.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(clausePair) {
			// Keep malformed clauses as-is, let validator report errors
			expandedClauses = append(expandedClauses, clauseStx)
		} else {
			// Keep formals unchanged
			formals := clausePair.SyntaxCar()
			formalsStx, ok := formals.(syntax.SyntaxValue)
			if !ok {
				return nil, fmt.Errorf("case-lambda: invalid formals in clause")
			}

			// Expand body
			cdrVal := clausePair.SyntaxCdr()
			cdrPair, ok := cdrVal.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
				// No body - keep clause as-is
				expandedClauses = append(expandedClauses, clauseStx)
			} else {
				expandedBody, err := p.ExpandSyntaxArgumentList(ectx, cdrPair)
				if err != nil {
					return nil, fmt.Errorf("case-lambda: failed to expand clause body: %w", err)
				}

				// Build (formals expanded-body...)
				expandedClause := syntax.NewSyntaxCons(formalsStx, expandedBody, clausePair.SourceContext())
				expandedClauses = append(expandedClauses, expandedClause)
			}
		}

		cdr := current.SyntaxCdr()
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	// Build (case-lambda expanded-clauses...)
	clauseList := syntax.SyntaxList(sym.SourceContext(), expandedClauses...)
	return syntax.NewSyntaxCons(sym, clauseList, sym.SourceContext()), nil
}

// ExpandSyntaxExpression checks if sym is a macro and expands it, or returns
// the expression as a procedure call if not.
//
// This is where macro invocation happens:
//  1. Look up the symbol in the expand environment
//  2. If bound with BindingTypeSyntax, it's a macro - invoke the transformer
//  3. If it's a primitive (like quote, if, define-syntax), don't expand args
//  4. Otherwise, treat as procedure call and expand arguments
//
// The transformer closure (MachineClosure from CompileSyntaxRules) is invoked
// by creating a MachineContext and running it. The transformer:
//   - Receives the full macro invocation form on the eval stack
//   - Pattern matches against its clauses (OperationSyntaxRulesTransform)
//   - Expands the matching template with captured bindings
//   - Adds an intro scope to the expansion for hygiene
//   - Returns the expanded syntax in the value register
//
// The expanded result may itself contain macro invocations, so the caller
// should recursively expand it.
func (p *ExpanderTimeContinuation) ExpandSyntaxExpression(ectx ExpandTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	sym0, ok := sym.Unwrap().(*values.Symbol) // Ensure sym is a symbol
	if !ok {
		return nil, fmt.Errorf("expected a symbol for syntax, got %T", sym.Unwrap())
	}
	// Look up syntax bindings in the expand phase environment
	// R7RS requires syntax bindings to be separate from runtime bindings
	expandEnv := p.env.Expand()
	bnd := expandEnv.GetBinding(sym0)

	// Check if it's a macro binding
	if !values.IsVoid(bnd) && bnd.BindingType() == environment.BindingTypeSyntax {
		// This is a macro - invoke the transformer (handled below)
	} else {
		// Not a macro - check if it's a primitive (quote, if, define-syntax, etc.)
		// First check for compile-time primitives that don't have runtime bindings
		// Check for primitive expander in the Expand environment
		symVal := p.env.InternSymbol(sym0)
		if pe := LookupPrimitiveExpander(p.env, symVal, sym.Scopes()); pe != nil {
			return pe.Expand(p, ectx, sym, expr)
		}

		// Regular procedure call - expand arguments (they might contain macro calls)
		if exprPair, ok := expr.(*syntax.SyntaxPair); ok && !syntax.IsSyntaxEmptyList(exprPair) {
			expandedArgs, err := p.ExpandSyntaxArgumentList(ectx, exprPair)
			if err != nil {
				return nil, fmt.Errorf("failed to expand arguments: %w", err)
			}
			return syntax.NewSyntaxCons(sym, expandedArgs, sym.SourceContext()), nil
		}
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Check that expr is a syntax pair (the arguments)
	mcls, ok := bnd.Value().(*MachineClosure)
	if !ok {
		return nil, fmt.Errorf("not a machine closure: %T", bnd.Value())
	}
	// Create a machine context from the closure
	mc := NewMachineContextFromMachineClosure(mcls)
	if mc == nil {
		return nil, fmt.Errorf("failed to create machine context from closure")
	}

	// For syntax-rules transformers, we pass the entire input form as an argument.
	// The transformer expects the full form including the macro name.
	inputForm := syntax.NewSyntaxCons(sym, expr, sym.SourceContext())

	// Apply the transformer with the input form as the argument
	// This sets up the local environment binding for parameter 0
	_, err := mc.Apply(mcls, inputForm)
	if err != nil {
		return nil, fmt.Errorf("failed to apply transformer: %w", err)
	}

	err = mc.Run(context.TODO())
	if err != nil && !errors.Is(err, ErrMachineHalt) {
		return nil, err
	}
	// Check if the transformer produced a result
	if mc.value == nil || len(mc.value) == 0 {
		return nil, fmt.Errorf("syntax transformer produced no result")
	}
	// The transformer should return the expanded syntax
	result := mc.value[0]
	if result == nil {
		return nil, fmt.Errorf("syntax transformer returned nil")
	}

	// For syntax-rules transformers, the result should be the expanded form.
	// The expanded result may itself contain macro invocations (especially for
	// recursive macros like `and`, `or`, `let*`, etc.), so we must recursively
	// expand it.
	if stx, ok := result.(syntax.SyntaxValue); ok {
		// Recursively expand the result to handle nested macro calls
		return p.ExpandExpression(ectx, stx)
	}
	return nil, fmt.Errorf("syntax transformer returned non-syntax value: %T", result)
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).
// If the input is a macro call, it expands it once and returns (result, true, nil).
// If the input is not a macro call, it returns (input, false, nil).
// Unlike ExpandExpression, this does NOT recursively expand the result.
func (p *ExpanderTimeContinuation) ExpandOnce(ectx ExpandTimeCallContext, expr syntax.SyntaxValue) (syntax.SyntaxValue, bool, error) {
	// Only pairs can be macro calls
	stxPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return expr, false, nil
	}

	// Handle empty list
	if syntax.IsSyntaxEmptyList(stxPair) {
		return expr, false, nil
	}

	// Check if the car is a symbol
	car, ok := stxPair.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return expr, false, nil
	}

	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return expr, false, nil
	}

	sym0, ok := sym.Unwrap().(*values.Symbol)
	if !ok {
		return expr, false, nil
	}

	// Look up syntax bindings in the expand phase environment
	expandEnv := p.env.Expand()
	bnd := expandEnv.GetBinding(sym0)

	// Check if it's a macro binding
	if values.IsVoid(bnd) || bnd.BindingType() != environment.BindingTypeSyntax {
		// Not a macro - no expansion
		return expr, false, nil
	}

	// Get the transformer closure
	mcls, ok := bnd.Value().(*MachineClosure)
	if !ok {
		return nil, false, fmt.Errorf("not a machine closure: %T", bnd.Value())
	}

	// Create a machine context from the closure
	mc := NewMachineContextFromMachineClosure(mcls)
	if mc == nil {
		return nil, false, fmt.Errorf("failed to create machine context from closure")
	}

	// Build the input form
	cdr, ok := stxPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		cdr = syntax.SyntaxList(sym.SourceContext())
	}
	inputForm := syntax.NewSyntaxCons(sym, cdr, sym.SourceContext())

	// Apply the transformer
	_, err := mc.Apply(mcls, inputForm)
	if err != nil {
		return nil, false, fmt.Errorf("failed to apply transformer: %w", err)
	}

	err = mc.Run(context.TODO())
	if err != nil && !errors.Is(err, ErrMachineHalt) {
		return nil, false, err
	}

	// Check if the transformer produced a result
	if mc.value == nil || len(mc.value) == 0 {
		return nil, false, fmt.Errorf("syntax transformer produced no result")
	}

	result := mc.value[0]
	if result == nil {
		return nil, false, fmt.Errorf("syntax transformer returned nil")
	}

	// Return the result WITHOUT recursive expansion
	if stx, ok := result.(syntax.SyntaxValue); ok {
		return stx, true, nil
	}

	return nil, false, fmt.Errorf("syntax transformer returned non-syntax value: %T", result)
}

// ExpandSyntaxArgumentList expands each argument in the argument list.
// It returns a new syntax list with the expanded arguments.
func (p *ExpanderTimeContinuation) ExpandSyntaxArgumentList(ccnt ExpandTimeCallContext, args *syntax.SyntaxPair) (*syntax.SyntaxPair, error) {
	// instantiate result list
	q := syntax.SyntaxList(args.SourceContext())
	// go through each argument and expand it
	// and append to result list
	// if any error, return error
	// if not a proper list, return error
	// finally return the new list
	tail, err := syntax.SyntaxForEach(nil, args, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
		v0, err := p.ExpandExpression(ccnt, v)
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to expand argument list")
		}
		// append to result list
		cdr := syntax.SyntaxList(v0.SourceContext(), v0)
		q = q.SyntaxAppend(cdr).(*syntax.SyntaxPair)
		return nil
	})
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "failed to expand argument list")
	}
	// tail contains the last element of the list, which should be an empty list. anything else is an error.
	if !syntax.IsSyntaxEmptyList(tail) {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxList, "expected a list of arguments, got %T", tail)
	}
	return q, nil
}

// ExpandQuasiquote handles the expansion of quasiquoted expressions.
func (p *ExpanderTimeContinuation) ExpandQuasiquote(ectx ExpandTimeCallContext, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return nil, nil
}

// ExpandQuote handles the expansion of quoted expressions.
func (p *ExpanderTimeContinuation) ExpandQuote(ectx ExpandTimeCallContext, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return nil, nil
}
