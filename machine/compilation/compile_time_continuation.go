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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// inlineCandidate holds a let-bound lambda eligible for call-site inlining.
type inlineCandidate struct {
	lambda *validate.ValidatedLambda
	// env is the compile-time environment at registration. Inlining is only
	// safe when the call site is in the same scope — nested scopes may shadow
	// free variables that the lambda captured at its definition site.
	env *environment.EnvironmentFrame
}

// CompileTimeContinuation is a continuation used during the compilation phase
type CompileTimeContinuation struct {
	env         *environment.EnvironmentFrame
	template    *machine.NativeTemplate
	sourceStack []*syntax.SourceContext
	// libraryCallback is called when a library is compiled (for LoadLibrary)
	libraryCallback func(*CompiledLibrary)
	// libraryScope is the unique scope for the library being compiled.
	// Non-nil only when compiling inside a define-library body.
	// Threaded to CompileSyntaxRules so free identifiers in macro templates
	// can carry the library scope for cross-library hygiene.
	libraryScope *syntax.Scope
	// fileResolver controls how include/load resolves files.
	// Defaults to the resolver stored on Namespace (usually
	// OSFileResolver); set to EmbedFileResolver for bootstrap.
	fileResolver FileResolver
	// evaluator abstracts VM execution for compile-time evaluation
	// and transformer invocation so the compiler can be tested
	// without the concrete VM.
	evaluator machine.MacroEvaluator

	// inlineCandidates maps BindingID → lambda for let-bound closures eligible
	// for call-site inlining. Populated by CompileValidatedLet, consumed by
	// compileValidatedCall. Keyed by BindingID for stable cross-scope identity.
	inlineCandidates map[environment.BindingID]inlineCandidate
	// currentlyInlining tracks bindings being inlined to prevent infinite
	// recursion for self-referential letrec bindings.
	currentlyInlining map[environment.BindingID]struct{}
	// inlineThreshold is the maximum body length (in top-level expressions)
	// for inlining eligibility. 0 disables inlining.
	inlineThreshold int
}

// NewCompileTimeContinuation creates a new CompileTimeContinuation.
// The file resolver defaults to the one stored on the Namespace.
// If none is set, falls back to a fresh OSFileResolver.
func NewCompileTimeContinuation(tpl *machine.NativeTemplate, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) *CompileTimeContinuation {
	resolver := env.FileResolver()
	if resolver == nil {
		resolver = NewOSFileResolver(env)
	}
	q := &CompileTimeContinuation{
		env:             env,
		template:        tpl,
		fileResolver:    resolver,
		evaluator:       evaluator,
		inlineThreshold: DefaultInlineThreshold,
	}
	return q
}

// SetFileResolver overrides the file resolver used by include/load.
// Nil resets to the environment's resolver (or OSFileResolver as fallback).
func (p *CompileTimeContinuation) SetFileResolver(r FileResolver) {
	if r == nil {
		r = p.env.FileResolver()
		if r == nil {
			r = NewOSFileResolver(p.env)
		}
	}
	p.fileResolver = r
}

// DefaultInlineThreshold is the default maximum body length for procedure
// inlining. A lambda body with more expressions than this is not inlined.
const DefaultInlineThreshold = 5

// SetInlineThreshold sets the maximum body length for procedure inlining.
// 0 disables inlining entirely.
func (p *CompileTimeContinuation) SetInlineThreshold(n int) {
	p.inlineThreshold = n
}

// formArgs extracts the argument list from a compiled form's expression.
// expr is the CDR of the form (keyword stripped by registerSyntaxCompiler in register.go).
// usage describes what the form expects (e.g. "bindings and body") for error
// messages. Returns the arguments as a non-empty SyntaxPair, or an error if
// expr is not a SyntaxPair or is the empty list.
func formArgs(expr syntax.SyntaxValue, formName, usage string) (*syntax.SyntaxPair, error) {
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(argsPair) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected %s", formName, usage)
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
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: expected exactly one argument", formName)
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
	sym := expr.Sym
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
					machine.NewOperationLoadCachedBinding(idx),
				)
				return nil
			}
			// Binding not yet defined at compile time — fall back to runtime resolution
			i := p.template.MaybeAppendLiteral(gi)
			p.AppendOperations(
				machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
			)
			return nil
		}
		// If ResolvedBinding is set but not a GlobalIndex (or is nil),
		// fall through to normal resolution
	}

	// Get the scopes from the syntax symbol for hygiene checking.
	// Both this path and the expander's hasLocalVariableBinding (expander_time_continuation.go)
	// check bindingScopes ⊆ useScopes via syntax.ScopesMatch. This path uses the environment's
	// maximality algorithm (GetLocalIndex) to find the most specific binding for
	// codegen dispatch; the expander only needs a yes/no shadow check for a single binding.
	symbolScopes := expr.Scopes()

	// Fast path: if the symbol has no scopes, skip scope-aware resolution.
	//
	// Safety invariant: empty scopes implies no local bindings in scope.
	// Every binding form that creates locals (lambda, and by extension let/let*/letrec)
	// goes through expandLambdaForm, which adds a lambdaScope to all body identifiers
	// BEFORE inner expansion. A symbol with empty scopes can therefore only appear at
	// top level, where GetLocalIndex returns nil and falls through to globals.
	if len(symbolScopes) == 0 {
		// Try local binding first
		li := p.env.GetLocalIndex(sym, nil)
		if li != nil {
			p.AppendOperations(
				machine.NewOperationLoadLocalByLocalIndexImmediate(li),
			)
			return nil
		}

		// Try global binding
		gi := p.env.GetGlobalIndex(sym)
		if gi == nil {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local or global binding %q", sym.Key)
		}

		bd := p.env.GetGlobalBinding(gi)
		if bd != nil {
			idx := p.template.AppendCachedBinding(bd)
			p.AppendOperations(
				machine.NewOperationLoadCachedBinding(idx),
			)
			return nil
		}
		// Binding not yet defined at compile time — fall back to runtime resolution
		i := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Sym has scopes (from macro expansion), use scope-aware binding resolution
	// Check if it's a local binding with matching scopes
	li := p.env.GetLocalIndex(sym, symbolScopes)
	if li != nil {
		// Found a local binding with matching scopes
		p.AppendOperations(
			machine.NewOperationLoadLocalByLocalIndexImmediate(li),
		)
		return nil
	}

	// Library scope lookup takes priority over general scope matching.
	// When a macro's free identifier carries a library scope, we redirect
	// to the library's env via the TLE scope registry. This must come
	// before GetBinding because the outer expansion of
	// define-library may create placeholder bindings with empty scopes
	// in the caller env, which would falsely match any reference scopes.
	libGI := p.env.GetGlobalIndexFromLibraryScopes(sym, symbolScopes)
	if libGI != nil {
		// Use runtime resolution via GlobalIndex so the binding value
		// is read at execution time (after the library template runs).
		i := p.template.MaybeAppendLiteral(libGI)
		p.AppendOperations(
			machine.NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Check global binding with scope matching
	globalBinding := p.env.GetBinding(sym, symbolScopes)
	if globalBinding != nil {
		// It must be a global binding (since local lookup failed).
		// globalBinding was found by GetBinding — use it directly
		// as a cached binding to skip runtime map/lock overhead.
		idx := p.template.AppendCachedBinding(globalBinding)
		p.AppendOperations(
			machine.NewOperationLoadCachedBinding(idx),
		)
		return nil
	}

	// No binding found that matches the scopes
	return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q with compatible scopes", sym.Key)
}

// CompileMeta compiles a meta expression.
func (p *CompileTimeContinuation) CompileMeta(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "%T is not a pair", expr)
	}
	// Get the expand environment and compile expressions in it
	metaEnv := p.env.Expand()
	metaCont := NewCompileTimeContinuation(p.template, metaEnv, p.evaluator)
	metaCont.SetInlineThreshold(p.inlineThreshold)
	err := metaCont.compileExpressionList(ctctx, rest)
	if err != nil {
		return werr.WrapForeignErrorf(err, "failed to compile meta")
	}
	return nil
}

func (p *CompileTimeContinuation) compileExpressionList(ctctx CompileTimeCallContext, expr *syntax.SyntaxPair) error {
	if !expr.IsList() {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list of expressions, got %T", expr)
	}
	tail, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(_ context.Context, _ int, hasNext bool, v syntax.SyntaxValue) error {
		ctctx0 := ctctx
		if hasNext {
			ctctx0 = ctctx.NotInTail()
		}
		err := p.CompileExpression(ctctx0, v)
		if err != nil {
			return werr.WrapForeignErrorf(err, "failed to compile expression list")
		}
		return nil
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "failed to compile expression list")
	}
	if !syntax.IsSyntaxEmptyList(tail) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected expression list, got %T", tail)
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
		return p.wrapCompilationError(
			werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", result.Error()))
	}
	// Compile the validated form
	err := p.compileValidated(ctctx, result.Expr)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	return nil
}

// validateQuotedLiteral recursively walks a quoted literal value to detect
// circular pair structures from datum labels (e.g., '#0=(a . #0#)).
// Returns an error if circular references are found.
func (p *CompileTimeContinuation) validateQuotedLiteral(v values.Value) (values.Value, error) {
	return p.validateQuotedLiteralWithVisited(v, nil)
}

func (p *CompileTimeContinuation) validateQuotedLiteralWithVisited(
	v values.Value, visited map[*values.Pair]bool,
) (values.Value, error) {
	switch val := v.(type) {
	case *values.Symbol:
		return val, nil
	case *values.Pair:
		if val == nil {
			return nil, nil
		}
		if visited == nil {
			visited = make(map[*values.Pair]bool)
		}
		if visited[val] {
			return nil, werr.WrapForeignErrorf(
				werr.ErrInvalidSyntax,
				"compile: circular datum label in quoted literal",
			)
		}
		visited[val] = true
		car, err := p.validateQuotedLiteralWithVisited(val.Car(), visited)
		if err != nil {
			return nil, err
		}
		cdr, err := p.validateQuotedLiteralWithVisited(val.Cdr(), visited)
		if err != nil {
			return nil, err
		}
		delete(visited, val)
		if car == val.Car() && cdr == val.Cdr() {
			return val, nil
		}
		return values.NewCons(car, cdr), nil
	case *values.Vector:
		if val == nil || len(*val) == 0 {
			return val, nil
		}
		changed := false
		newElements := make([]values.Value, len(*val))
		for i, elem := range *val {
			validated, err := p.validateQuotedLiteralWithVisited(elem, visited)
			if err != nil {
				return nil, err
			}
			newElements[i] = validated
			if validated != elem {
				changed = true
			}
		}
		if !changed {
			return val, nil
		}
		return values.NewVector(newElements...), nil
	default:
		return v, nil
	}
}

// CompileSelfEvaluating compiles a self-evaluating expression (literal).
func (p *CompileTimeContinuation) CompileSelfEvaluating(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if expr == nil {
		// Load void for nil expressions
		p.AppendOperations(
			machine.NewOperationLoadVoid(),
		)
		return nil
	}
	// Validate quoted literal for circular datum labels.
	// Use UnwrapAll() to fully unwrap syntax values (including vector elements)
	// so that equal? comparisons work correctly on literal vectors.
	val, err := p.validateQuotedLiteral(expr.UnwrapAll())
	if err != nil {
		return err
	}
	li := p.template.MaybeAppendLiteral(val)
	p.AppendOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(li),
	)
	return nil
}

// AppendOperations appends operations tagged with the current source from the source stack.
// Routes through the integer-dispatch code[] path: Wave 1-3 operations become
// direct instructions, everything else goes via machine.OpComplex to the sideTable.
func (p *CompileTimeContinuation) AppendOperations(ops ...machine.Operation) {
	p.template.AppendOperationsWithSource(p.currentSource(), ops...)
}

// emitPatchableSaveContinuation emits a SaveContinuation with a placeholder
// offset of 0. Returns the code[] index for later patching via
// patchSaveContinuationOffset.
func (p *CompileTimeContinuation) emitPatchableSaveContinuation() int {
	idx := p.template.CodeLen()
	p.AppendOperations(machine.NewOperationSaveContinuationOffsetImmediate(0))
	return idx
}

// patchSaveContinuationOffset patches a previously emitted SaveContinuation
// placeholder with the correct relative offset from the placeholder to the
// current position.
func (p *CompileTimeContinuation) patchSaveContinuationOffset(idx int) {
	offset := p.template.CodeLen() - idx
	p.template.PatchInstructionArg(idx, int32(offset))
}

// patchBranchTarget patches a previously emitted branch instruction (Branch,
// BranchOnFalseValue, or SaveContinuation) with the offset to targetIdx.
func (p *CompileTimeContinuation) patchBranchTarget(idx, targetIdx int) {
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

// wrapCompilationError attaches the current source location to a compilation error.
// If no source context is available, returns the error unchanged.
func (p *CompileTimeContinuation) wrapCompilationError(err error) error {
	src := p.currentSource()
	if src == nil {
		return err
	}
	return &SourcedError{Source: src, Cause: err}
}
