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
	"maps"
	"slices"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileSyntaxCase compiles the (syntax-case expr (literal ...) clause ...) form.
//
// R6RS syntax-case is a pattern matching form that provides procedural macro facilities.
// Unlike syntax-rules which expands to templates, syntax-case evaluates arbitrary
// Scheme code in the body, with pattern variables bound as local variables.
//
// Syntax:
//
//	(syntax-case expr (literal ...)
//	  (pattern body)
//	  (pattern fender body)
//	  ...)
//
// Compilation strategy:
//  1. Compile expr to get the input syntax object
//  2. For each clause, generate pattern matching and body code
//  3. Pattern variables are bound as local variables in the body's scope
//  4. If fender exists, it's evaluated as a guard condition
func (p *CompileTimeContinuation) CompileSyntaxCase(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in register.go).
	// So expr = (input-expr (literals) clause ...)
	argsPair, err := formArgs(expr, "syntax-case", "expression and clauses")
	if err != nil {
		return err
	}

	// Get the input expression (CAR of args)
	inputExpr := argsPair.SyntaxCar()

	// Get the rest ((literals) clause ...)
	rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || rest.IsEmptyList() {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: expected literals list and clauses"))
	}

	// Extract literals list (CAR of rest)
	literalsExpr := rest.SyntaxCar()

	literalSyntax := make(map[string]*syntax.SyntaxSymbol)
	if !syntax.IsSyntaxEmptyList(literalsExpr) {
		literalsPair, ok := literalsExpr.(*syntax.SyntaxPair)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: literals must be a list, got %T", literalsExpr))
		}
		// syntax-case always uses the default ellipsis "..."
		// Use extractLiteralsWithSyntax to enable scope-aware literal matching.
		// Pass a non-nil literals map because extractLiteralsWithSyntax
		// unconditionally writes to it; only literalSyntax is used downstream.
		literals := values.NewStringSet(0)
		err := extractLiteralsWithSyntax(ctctx.ctx, literalsPair, literals, literalSyntax, match.DefaultEllipsis)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "syntax-case: invalid literals list"))
		}
	}

	// Get clauses (CDR of rest)
	clausesCdr, ok := rest.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || clausesCdr.IsEmptyList() {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: expected at least one clause"))
	}

	// Compile the input expression (leaves value in value register)
	err = p.CompileExpression(ctctx.NotInTail(), inputExpr)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "syntax-case: error compiling input expression"))
	}

	// Store input in the per-context syntax-case state for clause matching (not
	// on eval stack to avoid interference with body procedure calls)
	p.AppendOperations(NewOperationStoreSyntaxCaseInput())

	// Collect jump addresses that need to be patched after all clauses are compiled
	var successJumps []jumpPatch // Jumps to end after successful match
	var failJumps []jumpPatch    // Jumps to next clause on match failure

	clauseIndex := 0
	v, err := clausesCdr.SyntaxForEach(ctctx.ctx, func(_ context.Context, i int, hasNext bool, clauseVal syntax.SyntaxValue) error {
		clausePair, ok := clauseVal.(*syntax.SyntaxPair)
		if !ok || clausePair.IsEmptyList() {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: clause must be a list"))
		}

		// Extract pattern
		pattern := clausePair.SyntaxCar()

		// Get the rest (body or fender + body)
		clauseRest, ok := clausePair.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok || clauseRest.IsEmptyList() {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: expected body in clause"))
		}

		// Determine if there's a fender
		var fender syntax.SyntaxValue
		var body syntax.SyntaxValue

		bodyOrFender := clauseRest.SyntaxCar()

		restAfterFirst, ok := clauseRest.SyntaxCdr().(*syntax.SyntaxPair)
		if ok && !restAfterFirst.IsEmptyList() {
			// There's a fender
			fender = bodyOrFender
			body = restAfterFirst.SyntaxCar()
		} else {
			// No fender, just body
			body = bodyOrFender
		}

		// Compile the clause
		err := p.compileSyntaxCaseClause(ctctx, pattern, fender, body, literalSyntax, clauseIndex, &successJumps, &failJumps)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "syntax-case: error compiling clause %d", clauseIndex+1))
		}

		clauseIndex++
		return nil
	})
	if err != nil {
		return err
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-case: expected proper list of clauses"))
	}

	// Add error operation for when no clause matches
	p.AppendOperations(NewOperationSyntaxCaseNoMatch())

	// Patch all success jumps to point here (end of syntax-case)
	endIndex := p.template.CodeLen()
	for _, patch := range successJumps {
		p.patchBranchTarget(patch.opIndex, endIndex)
	}

	// Clear the input from the per-context syntax-case state
	p.AppendOperations(NewOperationClearSyntaxCaseInput())

	return nil
}

// jumpPatch represents a branch operation that needs to be patched later
type jumpPatch struct {
	clauseIndex int
	opIndex     int
}

// compileSyntaxCaseClause compiles a single syntax-case clause.
func (p *CompileTimeContinuation) compileSyntaxCaseClause(
	ctctx CompileTimeCallContext,
	pattern, fender, body syntax.SyntaxValue,
	literalSyntax map[string]*syntax.SyntaxSymbol,
	clauseIndex int,
	successJumps, failJumps *[]jumpPatch,
) error {
	// Collect pattern variables from the pattern
	// Note: isFirst=false because syntax-case patterns don't have a required leading keyword
	// (unlike syntax-rules which always has the macro name as the first element)
	// literalSyntax names the pattern literals, so they are excluded here; it is
	// also stored on the clause below, which is what carries their definition-site
	// scopes to the matcher for the R7RS §4.3.2 binding check.
	patternVars := values.NewStringSet(0)
	// patternVarSyntax records each pattern variable's syntax symbol (with its
	// scopes) so the ellipsis template-expansion path can do scope-aware
	// substitution for nested-macro hygiene — the same data the syntax-rules
	// path stores on SyntaxRulesClause.PatternVarSyntax.
	patternVarSyntax := make(map[string]*syntax.SyntaxSymbol)
	err := collectPatternVariablesWithEllipsis(pattern, literalSyntax, false, patternVars, patternVarSyntax, match.DefaultEllipsis)
	if err != nil {
		return err
	}

	// R7RS §4.3.2: _ is a wildcard that matches anything without binding.
	// The pattern compiler handles _ as a non-capturing wildcard, but
	// collectPatternVariablesWithEllipsis includes it. Remove it so BindPatternVars
	// doesn't create a void binding for _.
	_, wildcardIsLiteral := literalSyntax["_"]
	if !wildcardIsLiteral {
		patternVars.Unset("_")
		delete(patternVarSyntax, "_")
	}

	// Compile the pattern to bytecode.
	// syntax-case patterns don't have a leading macro keyword (unlike syntax-rules),
	// so match all elements including the first.
	compiled, err := match.CompileSyntaxPattern(ctctx.ctx, pattern, patternVars, &match.CompilePatternOpts{
		MatchAllElements: true,
	})
	if err != nil {
		return err
	}

	// Create a wrapper for the compiled pattern. p.env is the macro's definition
	// environment, so LiteralDefs is the definition-site resolution that
	// OperationSyntaxCaseMatch, running in the use site's environment, cannot
	// recompute.
	clauseWrapper := &SyntaxCaseClause{
		Bytecode:       compiled.Codes,
		PatternVars:    patternVars,
		LiteralSyntax:  literalSyntax,
		LiteralDefs:    resolveLiteralDefinitions(p.env, literalSyntax),
		EllipsisVars:   compiled.EllipsisVars,
		EllipsisDepths: compiled.EllipsisDepths,
	}

	// Store the clause in literals
	clauseIdx := p.template.MaybeAppendLiteral(clauseWrapper)

	// Generate code:
	// 1. Load compiled clause from literals
	// 2. Call match operation (leaves match result in value register)
	// 3. Branch on match failure to next clause
	// 4. Bind pattern variables
	// 5. If fender, evaluate and branch on false. The fender is an expression
	//    over the pattern variables, so it must follow the binding step.
	// 6. Execute body
	// 7. Jump to end

	// Load the clause
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(clauseIdx))

	// Match operation: takes clause from value reg, reads the input from the
	// per-context syntax-case state.
	// Result (#t or #f) goes into value register
	p.AppendOperations(NewOperationSyntaxCaseMatch())

	// Branch on match failure (reads value register directly, no Push needed)
	failBranchIdx := p.template.CodeLen()
	p.AppendOperations(machine.NewOperationBranchOnFalseValueOffsetImmediate(0)) // Offset patched later
	*failJumps = append(*failJumps, jumpPatch{clauseIndex, failBranchIdx})

	// Create environment with pattern variables bound (shared between fender and body)
	bodyEnv := p.createPatternVarEnvironment(patternVars)
	bodyCompiler := NewCompileTimeContinuation(p.template, bodyEnv, p.evaluator)
	bodyCompiler.SetInlineThreshold(p.inlineThreshold)
	// Carry the clause's hygiene context into CompileSyntax so that ellipsis
	// templates expand hygienically. libraryScope is not on bodyEnv (it is a
	// separate field on CompileTimeContinuation), so propagate it explicitly for
	// cross-library free-id resolution.
	bodyCompiler.libraryScope = p.libraryScope
	bodyCompiler.patternVars = patternVars
	bodyCompiler.patternVarSyntax = patternVarSyntax
	// Inherit the parent compiler's current source so that operations emitted
	// by bodyCompiler (like BindPatternVars) are tagged with the syntax-case
	// form's source location.
	bodyCompiler.pushSource(p.currentSource())

	// Bind pattern variables from the match result.
	//
	// This is emitted UNCONDITIONALLY and must stay that way. The frame it
	// pushes carries the form's syntax-case state under a reserved key, which is
	// what (syntax ...) resolves against by a nearest-marked-ancestor walk, so
	// the frame is load-bearing regardless of how many pattern variables the
	// clause has. Do NOT add a "skip the frame when patternVars is empty"
	// shortcut. It is worth naming the shape that will tempt someone: a
	// whole-clause pattern of () is the first clause form with zero pattern
	// variables, and CompileSyntaxPattern refuses it today
	// (internal/match/syntax_adapter.go gates on *syntax.SyntaxPair), so the
	// temptation arrives with the fix for that.
	bodyCompiler.AppendOperations(NewOperationBindPatternVars(patternVars))

	// Create expander for the body environment (to expand macros like let)
	bodyExpander := NewExpanderTimeContinuation(ctctx.ctx, bodyEnv, p.evaluator)

	// If there's a fender, evaluate it and branch to cleanup on false
	var fenderBranchIdx int
	if fender != nil {
		// Expand the fender first
		expandedFender, err := bodyExpander.ExpandExpression(fender)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "error expanding fender"))
		}
		err = bodyCompiler.CompileExpression(ctctx.NotInTail(), expandedFender)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "error compiling fender"))
		}

		// Branch on false fender to cleanup block (reads value register directly)
		fenderBranchIdx = p.template.CodeLen()
		p.AppendOperations(machine.NewOperationBranchOnFalseValueOffsetImmediate(0))
	}

	// Expand and compile the body
	expandedBody, err := bodyExpander.ExpandExpression(body)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "error expanding body"))
	}
	err = bodyCompiler.CompileExpression(ctctx, expandedBody)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "error compiling body"))
	}

	// Restore the parent environment on the MATCHED path. There are three exit
	// edges out of a clause, not two, and each needs its own accounting:
	//
	//  1. pattern mismatch — branches from failBranchIdx, which precedes the
	//     BindPatternVars push entirely, so it is already balanced. Do NOT add a
	//     pop there.
	//  2. fender false — popped by the cleanup block below.
	//  3. match succeeded and the body ran — this pop. Without it the
	//     pattern-variable frame stays current for everything after the form, so
	//     trailing code doing a depth-relative LOCAL access reads through a frame
	//     one level too deep: `(begin (syntax-case stx () ((_ a) 1)) y)` returned
	//     the pattern variable instead of y, and three trailing locals indexed
	//     off the end of the pattern-variable frame.
	//
	// A clause body ending in a tail CALL never reaches this instruction, which
	// is harmless: a tail call does not return to this frame, and its
	// continuation restores the environment on its own.
	p.AppendOperations(machine.NewOperationPopEnv())

	// Jump to end on success
	successJumpIdx := p.template.CodeLen()
	p.AppendOperations(machine.NewOperationBranchOffsetImmediate(0)) // Offset patched later
	*successJumps = append(*successJumps, jumpPatch{clauseIndex, successJumpIdx})

	// If there was a fender, add cleanup block: PopEnv, then branch to next clause
	if fender != nil {
		// Patch fender branch to jump here (cleanup block start)
		cleanupStart := p.template.CodeLen()
		p.patchBranchTarget(fenderBranchIdx, cleanupStart)

		// Restore parent environment
		p.AppendOperations(machine.NewOperationPopEnv())

		// Branch to next clause (will be patched below)
		cleanupBranchIdx := p.template.CodeLen()
		p.AppendOperations(machine.NewOperationBranchOffsetImmediate(0))
		*failJumps = append(*failJumps, jumpPatch{clauseIndex, cleanupBranchIdx})
	}

	// Patch fail jumps for this clause to point to the next clause
	nextClauseStart := p.template.CodeLen()
	for _, patch := range slices.Backward(*failJumps) {
		if patch.clauseIndex == clauseIndex {
			p.patchBranchTarget(patch.opIndex, nextClauseStart)
		}
	}

	return nil
}

// createPatternVarEnvironment creates a child environment with local bindings
// for the pattern variables. Uses sorted order for consistent indexing with runtime.
func (p *CompileTimeContinuation) createPatternVarEnvironment(patternVars values.StringSet) *environment.EnvironmentFrame {
	vars := slices.Sorted(maps.Keys(patternVars))

	localEnv := environment.NewLocalEnvironment(len(patternVars))
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, p.env)

	// Create bindings for each pattern variable in sorted order
	for _, varName := range vars {
		sym := values.NewSymbol(varName)
		childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
	}

	return childEnv
}

// SyntaxCaseClause is defined in syntax_bridge_types.go in this package.
