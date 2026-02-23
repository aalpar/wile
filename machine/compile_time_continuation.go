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

package machine

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/values"
)

// CompileTimeContinuation is a continuation used during the compilation phase
type CompileTimeContinuation struct {
	env         *environment.EnvironmentFrame
	template    *NativeTemplate
	sourceStack []*syntax.SourceContext
	// libraryCallback is called when a library is compiled (for LoadLibrary)
	libraryCallback func(*CompiledLibrary)
}

// NewCompiletimeContinuation creates a new CompileTimeContinuation
func NewCompiletimeContinuation(tpl *NativeTemplate, env *environment.EnvironmentFrame) *CompileTimeContinuation {
	q := &CompileTimeContinuation{
		env:      env,
		template: tpl,
	}
	return q
}

// formArgs extracts the argument list from a compiled form's expression.
// expr is the CDR of the form (keyword already stripped by the dispatcher).
// usage describes what the form expects (e.g. "bindings and body") for error
// messages. Returns the arguments as a non-empty SyntaxPair, or an error if
// expr is not a SyntaxPair or is the empty list.
func formArgs(expr syntax.SyntaxValue, formName, usage string) (*syntax.SyntaxPair, error) {
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: expected %s", formName, usage)
	}
	return argsPair, nil
}

// formSingleArg extracts exactly one argument from a compiled form's expression.
// Returns an error if the form does not have exactly one argument.
func formSingleArg(expr syntax.SyntaxValue, formName string) (syntax.SyntaxValue, error) {
	argsPair, err := formArgs(expr, formName, "exactly one argument")
	if err != nil {
		return nil, err
	}
	arg := argsPair.SyntaxCar()
	if !syntax.IsSyntaxEmptyList(argsPair.SyntaxCdr()) {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s: expected exactly one argument", formName)
	}
	return arg, nil
}

// SetLibraryCallback sets a callback function that will be called when a library
// is compiled via CompileDefineLibrary. This is used by LoadLibrary to capture
// the compiled library.
func (p *CompileTimeContinuation) SetLibraryCallback(cb func(*CompiledLibrary)) {
	p.libraryCallback = cb
}

// CompileSymbol compiles a syntax symbol expression.
func (p *CompileTimeContinuation) CompileSymbol(ctctx CompileTimeCallContext, expr *syntax.SyntaxSymbol) error {
	sym := p.env.InternSymbol(expr.Sym)

	// Check for pre-resolved binding from macro expansion
	// This handles cross-library hygiene: free identifiers in macro templates
	// carry their definition-time GlobalIndex so they resolve correctly
	// even when the macro is used in a different library context.
	if expr.ResolvedBinding != nil {
		gi, ok := expr.ResolvedBinding.(*environment.GlobalIndex)
		if ok && gi != nil {
			bd := gi.Env.GetOwnGlobalBinding(gi)
			if bd != nil {
				idx := p.template.AppendCachedBinding(bd)
				p.AppendOperations(
					NewOperationLoadCachedBinding(idx),
				)
				return nil
			}
			// Binding not yet defined at compile time — fall back to runtime resolution
			i := p.template.MaybeAppendLiteral(gi)
			p.AppendOperations(
				NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
			)
			return nil
		}
		// If ResolvedBinding is set but not a GlobalIndex (or is nil),
		// fall through to normal resolution
	}

	// Get the scopes from the syntax symbol for hygiene checking.
	// Both this path and the expander's hasLocalVariableBinding (expander_time_continuation.go)
	// check bindingScopes ⊆ useScopes via syntax.ScopesMatch. This path uses the environment's
	// maximality algorithm (GetLocalIndexWithScopes) to find the most specific binding for
	// codegen dispatch; the expander only needs a yes/no shadow check for a single binding.
	symbolScopes := expr.Scopes()

	// If the symbol has no scopes (e.g., from user code, not from macro expansion),
	// use the regular binding resolution
	if len(symbolScopes) == 0 {
		// Try local binding first
		li := p.env.GetLocalIndex(sym)
		if li != nil {
			p.AppendOperations(
				NewOperationLoadLocalByLocalIndexImmediate(li),
			)
			return nil
		}

		// Try global binding
		gi := p.env.GetGlobalIndex(sym)
		if gi == nil {
			return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such local or global binding %q", sym.Key)
		}

		bd := p.env.GetGlobalBinding(gi)
		if bd != nil {
			idx := p.template.AppendCachedBinding(bd)
			p.AppendOperations(
				NewOperationLoadCachedBinding(idx),
			)
			return nil
		}
		// Binding not yet defined at compile time — fall back to runtime resolution
		i := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Sym has scopes (from macro expansion), use scope-aware binding resolution
	// Check if it's a local binding with matching scopes
	li := p.env.GetLocalIndexWithScopes(sym, symbolScopes)
	if li != nil {
		// Found a local binding with matching scopes
		p.AppendOperations(
			NewOperationLoadLocalByLocalIndexImmediate(li),
		)
		return nil
	}

	// Check global binding with scope matching
	globalBinding := p.env.GetBindingWithScopes(sym, symbolScopes)
	if globalBinding == nil {
		// No binding found that matches the scopes
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such binding %q with compatible scopes", sym.Key)
	}

	// It must be a global binding (since local lookup failed).
	// globalBinding was found by GetBindingWithScopes — use it directly
	// as a cached binding to skip runtime map/lock overhead.
	idx := p.template.AppendCachedBinding(globalBinding)
	p.AppendOperations(
		NewOperationLoadCachedBinding(idx),
	)
	return nil
}

// CompileSyntaxPrimitive compiles a syntax primitive if sym corresponds to one.
func (p *CompileTimeContinuation) CompileSyntaxPrimitive(ctctx CompileTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (bool, error) {
	symVal := p.env.InternSymbol(sym.Sym)
	scopes := sym.Scopes()

	// Dynamic lookup in the compile environment.
	// Syntax compilers are bound as SyntaxCompiler values in env.Compile().
	// All syntax compilers are registered in syntax_compilers_registry.go.
	pc := LookupSyntaxCompiler(p.env, symVal, scopes)
	if pc != nil {
		err := pc.Compile(p, ctctx, expr)
		if err != nil {
			return true, err
		}
		return true, nil
	}

	// Not a primitive - caller should treat as procedure call.
	// Core forms (define, lambda, quote, quasiquote, if, set!, begin) are handled
	// by compileValidated* methods and never reach here.
	return false, values.WrapForeignErrorf(values.ErrNotAPrimitive, "compileSyntaxCompilerCall: no syntax compiler for form")
}

// CompileMeta compiles a meta expression.
func (p *CompileTimeContinuation) CompileMeta(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%T is not a pair", expr)
	}
	// Get the expand environment and compile expressions in it
	metaEnv := p.env.Expand()
	metaCont := NewCompiletimeContinuation(p.template, metaEnv)
	err := metaCont.compileExpressionList(ctctx, rest)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile meta")
	}
	return nil
}

func (p *CompileTimeContinuation) compileProcedureArgumentList(ctctx CompileTimeCallContext, args syntax.SyntaxValue) error {
	tail, err := syntax.SyntaxForEach(ctctx.ctx, args, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		err := p.CompileExpression(ctctx.NotInTail(), v)
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to compile procedure argument list")
		}
		p.AppendOperations(
			NewOperationPush(),
		)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile procedure argument list")
	}
	if !syntax.IsSyntaxEmptyList(tail) {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list of arguments, got %T", tail)
	}
	return nil
}

func (p *CompileTimeContinuation) compileExpressionList(ctctx CompileTimeCallContext, expr *syntax.SyntaxPair) error {
	if !expr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list of expressions, got %T", expr)
	}
	tail, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(_ context.Context, _ int, hasNext bool, v syntax.SyntaxValue) error {
		ctctx0 := ctctx
		if hasNext {
			ctctx0 = ctctx.NotInTail()
		}
		err := p.CompileExpression(ctctx0, v)
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to compile expression list")
		}
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression list")
	}
	if !syntax.IsSyntaxEmptyList(tail) {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected expression list, got %T", tail)
	}
	return nil
}

// CompileProcedureCall compiles a procedure call expression.
// It assumes that the initial element is not a primitive and compiles it as a procedure call.
// The compiled code will leave the result of the procedure call on the stack.
//
// Tail Call Optimization: When ctctx.inTail is true, we skip SaveContinuation.
// This allows the called function's RestoreContinuation to return directly
// to our caller's continuation, implementing proper tail call optimization
// per R7RS Section 3.5.
func (p *CompileTimeContinuation) CompileProcedureCall(ctctx CompileTimeCallContext, initial syntax.SyntaxValue, expr syntax.SyntaxValue) error {
	var operationSaveContinuationIndex int
	if !ctctx.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.emitPatchableSaveContinuation()
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	err := p.CompileExpression(ctctx.NotInTail(), initial)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression")
	}
	p.AppendOperations(
		NewOperationPush(),
	)
	// compile as a procedure call
	err = p.compileProcedureArgumentList(ctctx, expr)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression list")
	}
	p.AppendOperations(
		NewOperationPull(),
		NewOperationApply(),
	)

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(operationSaveContinuationIndex)
	}
	return nil
}

// CompilePrimitiveOrProcedureCall compiles either a primitive or a procedure call.
// It first checks if the initial element is a syntax symbol that corresponds to a primitive.
// If so, it compiles the primitive. If not, it treats it as a procedure call.
func (p *CompileTimeContinuation) CompilePrimitiveOrProcedureCall(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	stx0pr, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", expr)
	}
	initial := stx0pr.SyntaxCar()
	stx1cdr := stx0pr.SyntaxCdr()
	switch v := initial.(type) {
	case *syntax.SyntaxSymbol:
		ok, err := p.CompileSyntaxPrimitive(ctctx, v, stx1cdr)
		if !ok {
			return p.CompileProcedureCall(ctctx, v, stx1cdr)
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to compile primitive or call")
		}
	case *syntax.SyntaxPair:
		err := p.CompileProcedureCall(ctctx, v, stx1cdr)
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to compile expression")
		}
		return nil
	}
	return nil
}

// CompileExpression compiles a general expression.
// Pushes the expression's source context onto the source stack so that all
// operations emitted during compilation (including infrastructure ops like
// Branch and Push) are tagged with the source location.
func (p *CompileTimeContinuation) CompileExpression(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	p.pushSource(expr.SourceContext())
	defer p.popSource()

	// Validate the expression first
	// Pass the environment so validation can check for local variable shadowing
	// of special forms (R7RS §4.2.2)
	result := validate.ValidateExpression(ctctx.ctx, p.env, expr)
	if !result.Ok() {
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "%s", result.Error())
	}
	// Compile the validated form
	return p.compileValidated(ctctx, result.Expr)
}

// internSymbolsInValue recursively interns all symbols in a value using the environment.
// This ensures symbol identity (eq?) works correctly across compilation boundaries per R7RS 6.5:
// "Two symbols are identical (in the sense of eq?) if and only if their names are spelled the same way."
func (p *CompileTimeContinuation) internSymbolsInValue(v values.Value) values.Value {
	switch val := v.(type) {
	case *values.Symbol:
		return p.env.InternSymbol(val)
	case *values.Pair:
		if val == nil {
			return nil
		}
		car := p.internSymbolsInValue(val.Car())
		cdr := p.internSymbolsInValue(val.Cdr())
		if car == val.Car() && cdr == val.Cdr() {
			return val
		}
		return values.NewCons(car, cdr)
	case *values.Vector:
		if val == nil || len(*val) == 0 {
			return val
		}
		changed := false
		newElements := make([]values.Value, len(*val))
		for i, elem := range *val {
			interned := p.internSymbolsInValue(elem)
			newElements[i] = interned
			if interned != elem {
				changed = true
			}
		}
		if !changed {
			return val
		}
		return values.NewVector(newElements...)
	default:
		return v
	}
}

// CompileSelfEvaluating compiles a self-evaluating expression (literal).
func (p *CompileTimeContinuation) CompileSelfEvaluating(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if expr == nil {
		// Load void for nil expressions
		p.AppendOperations(
			NewOperationLoadVoid(),
		)
		return nil
	}
	// Intern symbols to ensure eq? identity per R7RS 6.5
	// Use UnwrapAll() to fully unwrap syntax values (including vector elements)
	// so that equal? comparisons work correctly on literal vectors.
	val := p.internSymbolsInValue(expr.UnwrapAll())
	li := p.template.MaybeAppendLiteral(val)
	p.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(li),
	)
	return nil
}

// AppendOperations appends operations tagged with the current source from the source stack.
// Routes through the integer-dispatch code[] path: Wave 1-3 operations become
// direct instructions, everything else goes via OpComplex to the sideTable.
func (p *CompileTimeContinuation) AppendOperations(ops ...Operation) {
	p.template.AppendOperationsWithSource(p.currentSource(), ops...)
}

// emitPatchableSaveContinuation emits a SaveContinuation with a placeholder
// offset of 0. Returns the code[] index for later patching via
// patchSaveContinuationOffset.
func (p *CompileTimeContinuation) emitPatchableSaveContinuation() int {
	idx := p.template.CodeLen()
	p.AppendOperations(NewOperationSaveContinuationOffsetImmediate(0))
	return idx
}

// patchSaveContinuationOffset patches a previously emitted SaveContinuation
// placeholder with the correct relative offset from the placeholder to the
// current position.
func (p *CompileTimeContinuation) patchSaveContinuationOffset(idx int) {
	offset := p.template.CodeLen() - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

// patchBranchOnFalseValueOffset patches a previously emitted BranchOnFalseValue
// instruction with the target offset.
func (p *CompileTimeContinuation) patchBranchOnFalseValueOffset(idx, targetIdx int) {
	offset := targetIdx - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

// patchBranchOffset patches a previously emitted Branch instruction with the
// target offset.
func (p *CompileTimeContinuation) patchBranchOffset(idx, targetIdx int) {
	offset := targetIdx - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

func (p *CompileTimeContinuation) pushSource(src *syntax.SourceContext) {
	p.sourceStack = append(p.sourceStack, src)
}

func (p *CompileTimeContinuation) popSource() {
	if len(p.sourceStack) > 0 {
		p.sourceStack = p.sourceStack[:len(p.sourceStack)-1]
	}
}

func (p *CompileTimeContinuation) currentSource() *syntax.SourceContext {
	if len(p.sourceStack) == 0 {
		return nil
	}
	return p.sourceStack[len(p.sourceStack)-1]
}
