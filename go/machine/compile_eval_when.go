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

import (
	"context"
	"errors"

	"wile/syntax"
	"wile/values"
)

// CompileEvalWhen handles (eval-when (phase ...) body ...).
//
// This form controls when code is evaluated based on phase specifiers:
//   - expand: evaluate during macro expansion (at compile time)
//   - compile: evaluate during compilation (currently same as expand)
//   - run: evaluate at runtime (generate code for normal execution)
//
// Multiple phases can be specified. If both expand and run are specified,
// the body is evaluated at compile time AND code is generated for runtime.
//
// Examples:
//
//	(eval-when (expand)
//	  (display "at expansion time"))
//
//	(eval-when (run)
//	  (display "at runtime"))
//
//	(eval-when (expand run)
//	  (display "both times"))
func (p *CompileTimeContinuation) CompileEvalWhen(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if p.env == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "eval-when: nil environment")
	}
	if p.template == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "eval-when: nil template")
	}

	// expr is ((phase ...) body ...) - the args after 'eval-when'
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: expected (phase ...) and body")
	}

	// Get the phase list
	phasesExpr := argsPair.Car()
	phasesStx, ok := phasesExpr.(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: phase list must be syntax")
	}
	phases, err := p.parseEvalWhenPhases(phasesStx)
	if err != nil {
		return err
	}

	// Get the body expressions
	bodyCdr := argsPair.Cdr()
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: expected body expressions")
	}

	// Check which phases are specified
	hasExpand := phases["expand"] || phases["compile"]
	hasRun := phases["run"] || phases["load"] || phases["eval"]

	// If expand phase, evaluate at compile time
	if hasExpand {
		if err := p.evalWhenExecuteAtCompileTime(ccnt, bodyPair); err != nil {
			return err
		}
	}

	// If run phase, compile for runtime execution
	if hasRun {
		if err := p.evalWhenCompileForRuntime(ccnt, bodyPair); err != nil {
			return err
		}
	} else {
		// No runtime effect - emit void if we haven't already emitted code
		// Note: if hasExpand was true, we still need to emit void for runtime
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(
			p.template.MaybeAppendLiteral(values.Void),
		))
	}

	return nil
}

// parseEvalWhenPhases parses the phase list from an eval-when form.
// Returns a map of phase names to booleans.
// Accepts both (expand run) and (expand compile run) forms.
// Also accepts R6RS-style phase names: load, eval, visit.
func (p *CompileTimeContinuation) parseEvalWhenPhases(phasesExpr syntax.SyntaxValue) (map[string]bool, error) {
	phases := make(map[string]bool)

	phasesPair, ok := phasesExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: phase list must be a list")
	}

	// Handle empty phase list
	if syntax.IsSyntaxEmptyList(phasesPair) {
		return phases, nil
	}

	// Iterate through phase symbols
	current := phasesPair
	for !syntax.IsSyntaxEmptyList(current) {
		phaseVal := current.Car()
		phaseSym, ok := phaseVal.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "eval-when: phase must be a symbol")
		}

		phaseName := phaseSym.Key
		switch phaseName {
		case "expand", "compile", "run", "load", "eval", "visit":
			phases[phaseName] = true
		default:
			return nil, values.NewForeignErrorf("eval-when: unknown phase %q", phaseName)
		}

		// Move to next
		cdr := current.Cdr()
		cdrStx, ok := cdr.(syntax.SyntaxValue)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: invalid phase list")
		}
		if syntax.IsSyntaxEmptyList(cdrStx) {
			break
		}
		current, ok = cdr.(*syntax.SyntaxPair)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: improper phase list")
		}
	}

	return phases, nil
}

// evalWhenExecuteAtCompileTime executes body expressions at compile time.
// Similar to begin-for-syntax behavior.
func (p *CompileTimeContinuation) evalWhenExecuteAtCompileTime(ccnt CompileTimeCallContext, bodyPair *syntax.SyntaxPair) error {
	// Handle empty body
	if syntax.IsSyntaxEmptyList(bodyPair) {
		return nil
	}

	// Get expand phase environment for execution
	expandEnv := p.env.Expand()

	// Create expander for macro expansion
	ectx := NewExpandTimeCallContext()
	expander := NewExpanderTimeContinuation(p.env)

	// Process each expression
	current := bodyPair
	for !syntax.IsSyntaxEmptyList(current) {
		// Get current expression
		exprVal := current.Car()
		if exprVal == nil {
			return values.WrapForeignErrorf(values.ErrUnexpectedNil, "eval-when: nil expression")
		}

		stxVal, ok := exprVal.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: invalid expression")
		}

		// Expand the expression (it may contain macros)
		expandedExpr, err := expander.ExpandExpression(ectx, stxVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "eval-when: expansion failed")
		}

		// Compile the expression to a temporary template
		tmpTpl := NewNativeTemplate(0, 0, false)
		tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)

		if err := tmpCcnt.CompileExpression(ccnt, expandedExpr); err != nil {
			return values.WrapForeignErrorf(err, "eval-when: compilation failed")
		}

		// Execute the compiled code at compile time
		cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
		mc := NewMachineContext(cont)
		if err := mc.Run(context.Background()); err != nil {
			if !errors.Is(err, ErrMachineHalt) {
				return values.WrapForeignErrorf(err, "eval-when: evaluation failed")
			}
		}

		// Move to next expression
		cdr := current.Cdr()
		cdrStx, ok := cdr.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: invalid cdr")
		}
		if syntax.IsSyntaxEmptyList(cdrStx) {
			break
		}
		current, ok = cdr.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: improper list")
		}
	}

	return nil
}

// evalWhenCompileForRuntime compiles body expressions for runtime execution.
// Similar to normal begin behavior.
func (p *CompileTimeContinuation) evalWhenCompileForRuntime(ccnt CompileTimeCallContext, bodyPair *syntax.SyntaxPair) error {
	// Handle empty body
	if syntax.IsSyntaxEmptyList(bodyPair) {
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(
			p.template.MaybeAppendLiteral(values.Void),
		))
		return nil
	}

	// Create expander for macro expansion
	ectx := NewExpandTimeCallContext()
	expander := NewExpanderTimeContinuation(p.env)

	// Collect all expressions
	var exprs []syntax.SyntaxValue
	current := bodyPair
	for !syntax.IsSyntaxEmptyList(current) {
		exprVal := current.Car()
		if exprVal == nil {
			return values.WrapForeignErrorf(values.ErrUnexpectedNil, "eval-when: nil expression")
		}

		stxVal, ok := exprVal.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: invalid expression")
		}

		exprs = append(exprs, stxVal)

		// Move to next
		cdr := current.Cdr()
		cdrStx, ok := cdr.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "eval-when: invalid cdr")
		}
		if syntax.IsSyntaxEmptyList(cdrStx) {
			break
		}
		current, ok = cdr.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "eval-when: improper list")
		}
	}

	// Compile each expression, only the last one in tail position
	for i, stxVal := range exprs {
		isLast := i == len(exprs)-1

		// Expand the expression
		expandedExpr, err := expander.ExpandExpression(ectx, stxVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "eval-when: expansion failed")
		}

		// Create context - only last expression can be in tail position
		exprCcnt := ccnt
		if !isLast {
			exprCcnt = ccnt.NotInTail()
		}

		// Compile the expression
		if err := p.CompileExpression(exprCcnt, expandedExpr); err != nil {
			return values.WrapForeignErrorf(err, "eval-when: compilation failed")
		}

		// Pop intermediate results (except for the last one)
		if !isLast {
			p.AppendOperations(NewOperationPop())
		}
	}

	return nil
}
