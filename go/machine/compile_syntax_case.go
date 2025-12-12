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
	"sort"

	"wile/environment"
	"wile/match"
	"wile/syntax"
	"wile/values"
)

// CompileSyntaxCase compiles the (syntax-case expr (literal ...) clause ...) form.
//
// R6RS syntax-case is a pattern matching form that provides procedural macro facilities.
// Unlike syntax-rules which expands to templates, syntax-case evaluates arbitrary
// Scheme code in the body, with pattern variables bound as local variables.
//
// Syntax:
//   (syntax-case expr (literal ...)
//     (pattern body)
//     (pattern fender body)
//     ...)
//
// Compilation strategy:
//   1. Compile expr to get the input syntax object
//   2. For each clause, generate pattern matching and body code
//   3. Pattern variables are bound as local variables in the body's scope
//   4. If fender exists, it's evaluated as a guard condition
func (p *CompileTimeContinuation) CompileSyntaxCase(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (already has keyword stripped by CompilePrimitiveOrProcedureCall).
	// So expr = (input-expr (literals) clause ...)
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || argsPair.IsEmptyList() {
		return values.NewForeignError("syntax-case: expected expression and clauses")
	}

	// Get the input expression (CAR of args)
	inputExpr, ok := argsPair.Car().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("syntax-case: expected expression")
	}

	// Get the rest ((literals) clause ...)
	rest, ok := argsPair.Cdr().(*syntax.SyntaxPair)
	if !ok || rest.IsEmptyList() {
		return values.NewForeignError("syntax-case: expected literals list and clauses")
	}

	// Extract literals list (CAR of rest)
	literalsExpr, ok := rest.Car().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("syntax-case: expected literals list")
	}

	literals := make(map[string]struct{})
	if literalsPair, ok := literalsExpr.(*syntax.SyntaxPair); ok {
		if !literalsPair.IsEmptyList() {
			if err := extractLiterals(literalsPair, literals); err != nil {
				return values.WrapForeignErrorf(err, "syntax-case: invalid literals list")
			}
		}
	}

	// Get clauses (CDR of rest)
	clausesCdr, ok := rest.Cdr().(*syntax.SyntaxPair)
	if !ok || clausesCdr.IsEmptyList() {
		return values.NewForeignError("syntax-case: expected at least one clause")
	}

	// Compile the input expression (leaves value in value register)
	err := p.CompileExpression(ccnt.NotInTail(), inputExpr)
	if err != nil {
		return values.WrapForeignErrorf(err, "syntax-case: error compiling input expression")
	}

	// Store input in global for clause matching (not on eval stack to avoid interference with body procedure calls)
	p.AppendOperations(NewOperationStoreSyntaxCaseInput())

	// Collect jump addresses that need to be patched after all clauses are compiled
	var successJumps []jumpPatch // Jumps to end after successful match
	var failJumps []jumpPatch    // Jumps to next clause on match failure

	clauseIndex := 0
	for current := clausesCdr; current != nil && !current.IsEmptyList(); {
		clauseStart := len(p.template.operations)
		_ = clauseStart // Used for patching

		clauseVal, ok := current.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("syntax-case: expected clause")
		}

		clausePair, ok := clauseVal.(*syntax.SyntaxPair)
		if !ok || clausePair.IsEmptyList() {
			return values.NewForeignError("syntax-case: clause must be a list")
		}

		// Extract pattern
		pattern, ok := clausePair.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("syntax-case: expected pattern")
		}

		// Get the rest (body or fender + body)
		clauseRest, ok := clausePair.Cdr().(*syntax.SyntaxPair)
		if !ok || clauseRest.IsEmptyList() {
			return values.NewForeignError("syntax-case: expected body in clause")
		}

		// Determine if there's a fender
		var fender syntax.SyntaxValue
		var body syntax.SyntaxValue

		bodyOrFender, ok := clauseRest.Car().(syntax.SyntaxValue)
		if !ok {
			return values.NewForeignError("syntax-case: expected body or fender")
		}

		restAfterFirst, ok := clauseRest.Cdr().(*syntax.SyntaxPair)
		if ok && !restAfterFirst.IsEmptyList() {
			// There's a fender
			fender = bodyOrFender
			body, ok = restAfterFirst.Car().(syntax.SyntaxValue)
			if !ok {
				return values.NewForeignError("syntax-case: expected body after fender")
			}
		} else {
			// No fender, just body
			body = bodyOrFender
		}

		// Compile the clause
		err := p.compileSyntaxCaseClause(ccnt, pattern, fender, body, literals, clauseIndex, &successJumps, &failJumps)
		if err != nil {
			return values.WrapForeignErrorf(err, "syntax-case: error compiling clause %d", clauseIndex+1)
		}

		clauseIndex++

		// Move to next clause
		currentCdr := current.Cdr()
		if currentCdr == nil {
			break
		}
		current, _ = currentCdr.(*syntax.SyntaxPair)
	}

	// Add error operation for when no clause matches
	p.AppendOperations(NewOperationSyntaxCaseNoMatch())

	// Patch all success jumps to point here (end of syntax-case)
	endIndex := len(p.template.operations)
	for _, patch := range successJumps {
		op := p.template.operations[patch.opIndex].(*OperationBranchOffsetImmediate)
		op.Offset = endIndex - patch.opIndex
	}

	// Clear the syntax-case input global
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
	ccnt CompileTimeCallContext,
	pattern, fender, body syntax.SyntaxValue,
	literals map[string]struct{},
	clauseIndex int,
	successJumps, failJumps *[]jumpPatch,
) error {
	// Collect pattern variables from the pattern
	// Note: isFirst=false because syntax-case patterns don't have a required leading keyword
	// (unlike syntax-rules which always has the macro name as the first element)
	patternVars := make(map[string]struct{})
	err := collectPatternVariables(pattern, literals, false, patternVars)
	if err != nil {
		return err
	}

	// Compile the pattern to bytecode
	compiled, err := match.CompileSyntaxPatternFull(pattern, patternVars)
	if err != nil {
		return err
	}

	// Create a wrapper for the compiled pattern
	clauseWrapper := &syntaxCaseClause{
		bytecode:     compiled.Codes,
		patternVars:  patternVars,
		ellipsisVars: compiled.EllipsisVars,
	}

	// Store the clause in literals
	clauseIdx := p.template.MaybeAppendLiteral(clauseWrapper)

	// Generate code:
	// 1. Load compiled clause from literals
	// 2. Peek input from eval stack
	// 3. Call match operation (leaves match result in value register)
	// 4. Branch on match failure to next clause
	// 5. If fender, evaluate and branch on false
	// 6. Bind pattern variables
	// 7. Execute body
	// 8. Jump to end

	// Load the clause
	p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(clauseIdx))

	// Match operation: takes clause from value reg, peeks input from eval stack
	// Result (#t or #f) goes into value register
	p.AppendOperations(NewOperationSyntaxCaseMatch())

	// Push match result to stack for BranchOnFalse (which pops from stack)
	p.AppendOperations(NewOperationPush())

	// Branch on match failure
	failBranchIdx := len(p.template.operations)
	p.AppendOperations(NewOperationBranchOnFalseOffsetImmediate(0)) // Offset patched later
	*failJumps = append(*failJumps, jumpPatch{clauseIndex, failBranchIdx})

	// Create environment with pattern variables bound (shared between fender and body)
	bodyEnv := p.createPatternVarEnvironment(patternVars)
	bodyCompiler := NewCompiletimeContinuation(p.template, bodyEnv)

	// Bind pattern variables from the match result
	bodyCompiler.AppendOperations(NewOperationBindPatternVars(patternVars))

	// Create expander for the body environment (to expand macros like let)
	bodyExpander := NewExpanderTimeContinuation(bodyEnv)
	ectx := NewExpandTimeCallContext()

	// If there's a fender, evaluate it and branch to cleanup on false
	var fenderBranchIdx int
	if fender != nil {
		// Expand the fender first
		expandedFender, err := bodyExpander.ExpandExpression(ectx, fender)
		if err != nil {
			return values.WrapForeignErrorf(err, "error expanding fender")
		}
		err = bodyCompiler.CompileExpression(ccnt.NotInTail(), expandedFender)
		if err != nil {
			return values.WrapForeignErrorf(err, "error compiling fender")
		}

		// Push fender result to stack for BranchOnFalse
		p.AppendOperations(NewOperationPush())

		// Branch on false fender to cleanup block (patched below)
		fenderBranchIdx = len(p.template.operations)
		p.AppendOperations(NewOperationBranchOnFalseOffsetImmediate(0))
	}

	// Expand and compile the body
	expandedBody, err := bodyExpander.ExpandExpression(ectx, body)
	if err != nil {
		return values.WrapForeignErrorf(err, "error expanding body")
	}
	err = bodyCompiler.CompileExpression(ccnt, expandedBody)
	if err != nil {
		return values.WrapForeignErrorf(err, "error compiling body")
	}

	// Jump to end on success
	successJumpIdx := len(p.template.operations)
	p.AppendOperations(NewOperationBranchOffsetImmediate(0)) // Offset patched later
	*successJumps = append(*successJumps, jumpPatch{clauseIndex, successJumpIdx})

	// If there was a fender, add cleanup block: PopEnv, then branch to next clause
	if fender != nil {
		// Patch fender branch to jump here (cleanup block start)
		cleanupStart := len(p.template.operations)
		fenderOp := p.template.operations[fenderBranchIdx].(*OperationBranchOnFalseOffsetImmediate)
		fenderOp.Offset = cleanupStart - fenderBranchIdx

		// Restore parent environment
		p.AppendOperations(NewOperationPopEnv())

		// Branch to next clause (will be patched below)
		cleanupBranchIdx := len(p.template.operations)
		p.AppendOperations(NewOperationBranchOffsetImmediate(0))
		*failJumps = append(*failJumps, jumpPatch{clauseIndex, cleanupBranchIdx})
	}

	// Patch fail jumps for this clause to point to the next clause
	nextClauseStart := len(p.template.operations)
	for i := len(*failJumps) - 1; i >= 0; i-- {
		patch := (*failJumps)[i]
		if patch.clauseIndex == clauseIndex {
			offset := nextClauseStart - patch.opIndex
			switch op := p.template.operations[patch.opIndex].(type) {
			case *OperationBranchOnFalseOffsetImmediate:
				op.Offset = offset
			case *OperationBranchOffsetImmediate:
				op.Offset = offset
			}
		}
	}

	return nil
}

// createPatternVarEnvironment creates a child environment with local bindings
// for the pattern variables. Uses sorted order for consistent indexing with runtime.
func (p *CompileTimeContinuation) createPatternVarEnvironment(patternVars map[string]struct{}) *environment.EnvironmentFrame {
	// Sort pattern variables for consistent indexing
	vars := make([]string, 0, len(patternVars))
	for v := range patternVars {
		vars = append(vars, v)
	}
	sort.Strings(vars)

	localEnv := environment.NewLocalEnvironment(len(patternVars))
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, p.env)

	// Create bindings for each pattern variable in sorted order
	for _, varName := range vars {
		sym := p.env.InternSymbol(values.NewSymbol(varName))
		childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable)
	}

	return childEnv
}

// syntaxCaseClause wraps compiled pattern info for storage in literals
type syntaxCaseClause struct {
	bytecode     []match.SyntaxCommand
	patternVars  map[string]struct{}
	ellipsisVars map[int]map[string]struct{}
}

func (c *syntaxCaseClause) EqualTo(other values.Value) bool {
	return false
}

func (c *syntaxCaseClause) IsVoid() bool {
	return false
}

func (c *syntaxCaseClause) SchemeString() string {
	return "#<syntax-case-clause>"
}
