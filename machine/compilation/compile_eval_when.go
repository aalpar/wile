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
	"context"

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// evalWhenBehavior categorizes what an eval-when phase name triggers.
type evalWhenBehavior int

const (
	evalWhenExpand  evalWhenBehavior = 1 << iota // execute at compile time
	evalWhenRuntime                              // compile for runtime execution
)

// evalWhenPhaseTable maps eval-when phase name strings to their behaviors.
//
// Phase names follow Chez Scheme's eval-when (R. Kent Dybvig, "The Scheme
// Programming Language", §12.10). "compile", "load", "eval", and "visit"
// are Chez phase modes; "expand" and "run" are Wile-specific aliases that
// map to compile-time execution and runtime code generation respectively.
//
// eval-when is not part of R7RS-small; it is a Wile extension.
var evalWhenPhaseTable = map[string]evalWhenBehavior{
	"expand":  evalWhenExpand,  // Wile: expand-time execution
	"compile": evalWhenExpand,  // Chez: compile-time (same as expand)
	"run":     evalWhenRuntime, // Wile: runtime execution
	"load":    evalWhenRuntime, // Chez: load-time (same as run)
	"eval":    evalWhenRuntime, // Chez: eval-time (same as run)
	"visit":   0,               // Chez: visit-time (accepted, no effect)
}

// CompileEvalWhen handles (eval-when (phase ...) body ...).
//
// This form controls when code is evaluated based on phase specifiers.
// Phase names follow Chez Scheme's eval-when (Dybvig, TSPL §12.10):
//   - expand: evaluate during macro expansion (at compile time)
//   - compile: evaluate during compilation (currently same as expand)
//   - run: evaluate at runtime (generate code for normal execution)
//
// Multiple phases can be specified. If both expand and run are specified,
// the body is evaluated at compile time AND code is generated for runtime.
//
// eval-when is not part of R7RS-small; it is a Wile extension.
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
func (p *CompileTimeContinuation) CompileEvalWhen(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("eval-when")
	if err != nil {
		return err
	}

	// expr is ((phase ...) body ...) - the args after 'eval-when'
	argsPair, err := formArgs(expr, "eval-when", "phase list and body")
	if err != nil {
		return err
	}

	// Get the phase list
	phasesExpr := argsPair.SyntaxCar()
	phasesStx := phasesExpr
	behavior, err := p.parseEvalWhenPhases(ctctx.ctx, phasesStx)
	if err != nil {
		return err
	}

	// Get the body expressions
	bodyCdr := argsPair.Cdr()
	if values.IsEmptyList(bodyCdr) {
		// Empty body — emit void
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(
			p.template.MaybeAppendLiteral(values.Void),
		))
		return nil
	}
	bodyPair, ok := bodyCdr.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "eval-when: expected body expressions"))
	}

	// If expand phase, evaluate at compile time
	if behavior&evalWhenExpand != 0 {
		err := p.evalWhenExecuteAtCompileTime(ctctx, bodyPair)
		if err != nil {
			return err
		}
	}

	// If run phase, compile for runtime execution
	if behavior&evalWhenRuntime != 0 {
		err := p.evalWhenCompileForRuntime(ctctx, bodyPair)
		if err != nil {
			return err
		}
	} else {
		// No runtime effect - emit void if we haven't already emitted code
		// Note: if hasExpand was true, we still need to emit void for runtime
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(
			p.template.MaybeAppendLiteral(values.Void),
		))
	}

	return nil
}

// parseEvalWhenPhases parses the phase list from an eval-when form.
// Returns the combined evalWhenBehavior flags for the specified phases.
// Accepts both (expand run) and (expand compile run) forms.
// Also accepts Chez-style phase names: load, eval, visit.
func (p *CompileTimeContinuation) parseEvalWhenPhases(ctx context.Context, phasesExpr syntax.SyntaxValue) (evalWhenBehavior, error) {
	var behavior evalWhenBehavior

	if syntax.IsSyntaxEmptyList(phasesExpr) {
		return behavior, nil
	}
	phasesPair, ok := phasesExpr.(*syntax.SyntaxPair)
	if !ok {
		return 0, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "eval-when: phase list must be a list"))
	}

	// Iterate through phase symbols
	current := phasesPair
	v, err := current.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, phaseVal syntax.SyntaxValue) error {
		phaseSym, ok := phaseVal.(*syntax.SyntaxSymbol)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "eval-when: phase must be a symbol"))
		}

		phaseName := phaseSym.Sym.Key
		b, ok := evalWhenPhaseTable[phaseName]
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidArgument, "eval-when: unknown phase %q", phaseName))
		}
		behavior |= b
		return nil
	})
	if err != nil {
		return 0, err
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return 0, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAList, "eval-when: improper phase list"))
	}
	return behavior, nil
}

// evalWhenExecuteAtCompileTime executes body expressions at compile time.
func (p *CompileTimeContinuation) evalWhenExecuteAtCompileTime(ctctx CompileTimeCallContext, bodyPair *syntax.SyntaxPair) error {
	if syntax.IsSyntaxEmptyList(bodyPair) {
		return nil
	}
	return p.executeFormsAtCompileTime(ctctx, "eval-when", bodyPair)
}

// evalWhenCompileForRuntime compiles body expressions for runtime execution.
// Similar to normal begin behavior.
func (p *CompileTimeContinuation) evalWhenCompileForRuntime(ctctx CompileTimeCallContext, bodyPair *syntax.SyntaxPair) error {
	// Handle empty body
	if syntax.IsSyntaxEmptyList(bodyPair) {
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(
			p.template.MaybeAppendLiteral(values.Void),
		))
		return nil
	}

	// Create expander for macro expansion
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env, p.evaluator)

	// Collect all expressions
	var exprs []syntax.SyntaxValue
	current := bodyPair
	v, err := current.SyntaxForEach(ctctx.ctx, func(_ context.Context, _ int, _ bool, stxVal syntax.SyntaxValue) error {
		exprs = append(exprs, stxVal)
		return nil
	})
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "eval-when: error processing body expressions"))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAList, "eval-when: improper body expressions list"))
	}

	// Compile each expression, only the last one in tail position
	for i, stxVal := range exprs {
		isLast := i == len(exprs)-1

		// Expand the expression
		expandedExpr, err := expander.ExpandExpression(stxVal)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "eval-when: expansion failed"))
		}

		// Create context - only last expression can be in tail position
		exprCcnt := ctctx
		if !isLast {
			exprCcnt = ctctx.NotInTail()
		}

		// Compile the expression
		err = p.CompileExpression(exprCcnt, expandedExpr)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "eval-when: compilation failed"))
		}

		// Pop intermediate results (except for the last one)
		if !isLast {
			p.AppendOperations(machine.NewOperationPop())
		}
	}

	return nil
}
