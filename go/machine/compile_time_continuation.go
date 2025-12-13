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
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"

	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/validate"
	"wile/values"
)

const (
	SchemeIncludePathEnv = "SCHEME_INCLUDE_PATH"
)

// CompileTimeContinuation is a continuation used during the compilation phase
type CompileTimeContinuation struct {
	// FIXME: only binding keys needed - no values
	env      *environment.EnvironmentFrame
	template *NativeTemplate
	// libraryCallback is called when a library is compiled (for LoadLibrary)
	libraryCallback func(*CompiledLibrary)
}

func NewCompiletimeContinuation(tpl *NativeTemplate, env *environment.EnvironmentFrame) *CompileTimeContinuation {
	q := &CompileTimeContinuation{
		env:      env,
		template: tpl,
	}
	return q
}

// SetLibraryCallback sets a callback function that will be called when a library
// is compiled via CompileDefineLibrary. This is used by LoadLibrary to capture
// the compiled library.
func (p *CompileTimeContinuation) SetLibraryCallback(cb func(*CompiledLibrary)) {
	p.libraryCallback = cb
}

func (p *CompileTimeContinuation) CompileSymbol(ctctx CompileTimeCallContext, expr *syntax.SyntaxSymbol) error {
	sym, ok := expr.Unwrap().(*values.Symbol)
	if !ok {
		return values.ErrNotASymbol
	}
	sym = p.env.InternSymbol(sym)

	// Get the scopes from the syntax symbol for hygiene checking
	symbolScopes := expr.Scopes()

	// If the symbol has no scopes (e.g., from user code, not from macro expansion),
	// use the regular binding resolution
	if symbolScopes == nil || len(symbolScopes) == 0 {
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

		i := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(
			NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
		)
		return nil
	}

	// Symbol has scopes (from macro expansion), use scope-aware binding resolution
	binding := p.env.GetBindingWithScopes(sym, symbolScopes)
	if binding == nil {
		// No binding found that matches the scopes
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such binding %q with compatible scopes", sym.Key)
	}

	// Check if it's a local binding
	li := p.env.GetLocalIndex(sym)
	if li != nil {
		// It's a local binding and scopes matched
		p.AppendOperations(
			NewOperationLoadLocalByLocalIndexImmediate(li),
		)
		return nil
	}

	// It must be a global binding
	gi := p.env.GetGlobalIndex(sym)
	if gi == nil {
		// This shouldn't happen if GetBindingWithScopes succeeded
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "internal error: binding found but no index for %q", sym.Key)
	}

	i := p.template.MaybeAppendLiteral(gi)
	p.AppendOperations(
		NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(i),
	)
	return nil
}

func (p *CompileTimeContinuation) CompileSyntaxPrimitive(ctctx CompileTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (bool, error) {
	symVal := sym.Unwrap().(*values.Symbol)
	symVal = p.env.InternSymbol(symVal)
	scopes := sym.Scopes()

	// Dynamic lookup in the compile environment.
	// Syntax compilers are bound as SyntaxCompiler values in env.Compile().
	// All syntax compilers are registered in syntax_compilers_registry.go.
	if pc := LookupSyntaxCompiler(p.env, symVal, scopes); pc != nil {
		err := pc.Compile(p, ctctx, expr)
		if err != nil {
			return true, err
		}
		return true, nil
	}

	// Not a primitive - caller should treat as procedure call.
	// Core forms (define, lambda, quote, quasiquote, if, set!, begin) are handled
	// by compileValidated* methods and never reach here.
	return false, values.ErrNotAPrimitive
}

func (p *CompileTimeContinuation) CompileMeta(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%T is not a pair", expr)
	}
	// Get the meta environment and compile expressions in it
	metaEnv := p.env.Meta()
	metaCont := NewCompiletimeContinuation(p.template, metaEnv)
	err := metaCont.compileExpressionList(ctctx, rest)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile meta")
	}
	return nil
}

func findFile(p *CompileTimeContinuation, ctctx CompileTimeCallContext, path string) (fs.File, string, error) {
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath == "" {
		return nil, "", values.WrapForeignErrorf(values.ErrInvalidSyntax, "environment variable %q not set", SchemeIncludePathEnv)
	}
	includePaths := filepath.SplitList(includePath)
	for i := range includePath {
		fn := filepath.Join(includePaths[i], path)
		f, err := os.Open(fn)
		// return the first found file
		if err == nil {
			return f, fn, nil
		}
	}
	return nil, "", nil
}

// CompileInclude compiles an include expression.
// It reads and compiles all forms from the specified files in order.
// Each form is expanded and compiled in the current environment.
func (p *CompileTimeContinuation) CompileInclude(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.compileIncludeImpl(ctctx, expr, false)
}

// compileIncludeImpl is the shared implementation for include and include-ci.
func (p *CompileTimeContinuation) compileIncludeImpl(ctctx CompileTimeCallContext, expr syntax.SyntaxValue, caseInsensitive bool) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	for !syntax.IsSyntaxEmptyList(rest) {
		// Get the file name
		car := rest.SyntaxCar()
		next, ok := car.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a value, but got %T", car)
		}
		fn, ok := next.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a string but got a %T", next)
		}

		// Find and open the file
		file, filePath, err := findFile(p, ctctx, fn.Value)
		if err != nil {
			return values.WrapForeignErrorf(err, "include: failed to find file %q", fn.Value)
		}
		if file == nil {
			return values.NewForeignErrorf("include: file not found: %q", fn.Value)
		}
		defer file.Close() //nolint:errcheck

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParserWithFile(p.env, reader, filePath)

		// Read and compile all forms from the file
		for {
			stx, readErr := fileParser.ReadSyntax(nil)
			if readErr != nil {
				if errors.Is(readErr, io.EOF) {
					break
				}
				return values.WrapForeignErrorf(readErr, "include: error reading %q", fn.Value)
			}

			// Expand the form
			ectx := NewExpandTimeCallContext()
			expanded, expandErr := NewExpanderTimeContinuation(p.env).ExpandExpression(ectx, stx)
			if expandErr != nil {
				return values.WrapForeignErrorf(expandErr, "include: error expanding form from %q", fn.Value)
			}

			// Compile the expanded form
			compileErr := p.CompileExpression(ctctx, expanded)
			if compileErr != nil {
				return values.WrapForeignErrorf(compileErr, "include: error compiling form from %q", fn.Value)
			}
		}

		// Move to next filename
		rest, ok = rest.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list, got %T", rest)
		}
	}
	return nil
}

// CompileIncludeCi compiles an include-ci expression.
// It reads and compiles all forms from the specified files in order,
// treating symbols as case-insensitive (folded to lowercase).
func (p *CompileTimeContinuation) CompileIncludeCi(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// TODO: implement case-insensitive parsing by creating parser with caseInsensitive=true
	// For now, just use the regular include implementation
	return p.compileIncludeImpl(ctctx, expr, true)
}

func (p *CompileTimeContinuation) compileLambdaParameterList(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) (*NativeTemplate, *environment.EnvironmentFrame, error) {
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	tpl := NewNativeTemplate(0, 0, false)
	tail, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
		param, ok := v.Unwrap().(*values.Symbol)
		if !ok {
			return values.ErrInvalidSyntax
		}
		param = p.env.InternSymbol(param)
		// Get scopes from the syntax value
		var scopes []*syntax.Scope
		if syntaxSym, ok := v.(*syntax.SyntaxSymbol); ok {
			scopes = syntaxSym.Scopes()
		} else if syntaxObj, ok := v.(*syntax.SyntaxObject); ok {
			scopes = syntaxObj.Scopes()
		}
		// Create binding with scopes
		_, ok = lenv.CreateLocalBinding(param, environment.BindingTypeVariable)
		if !ok {
			return values.WrapForeignErrorf(values.ErrDuplicateBinding, "duplicate parameter %q in lambda", param.Key)
		}
		// Update the binding with scopes if present (get the last binding added)
		if scopes != nil && len(scopes) > 0 {
			bindings := lenv.Bindings()
			if len(bindings) > 0 {
				binding := bindings[len(bindings)-1]
				if binding != nil {
					binding.SetScopes(scopes)
				}
			}
		}
		tpl.parameterCount++
		return nil
	})
	if err != nil {
		return nil, nil, values.WrapForeignErrorf(err, "failed to compile lambda parameter list")
	}
	if syntax.IsSyntaxEmptyList(tail) {
		env := environment.NewEnvironmentFrameWithParent(lenv, p.env)
		return tpl, env, nil
	}
	last, ok := tail.Unwrap().(*values.Symbol)
	if !ok {
		return nil, nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "expected a symbol for variadic parameter, got %T", tail)
	}
	last = p.env.InternSymbol(last)
	// Get scopes from the tail syntax value
	var scopes []*syntax.Scope
	if syntaxSym, ok := tail.(*syntax.SyntaxSymbol); ok {
		scopes = syntaxSym.Scopes()
	} else if syntaxObj, ok := tail.(*syntax.SyntaxObject); ok {
		scopes = syntaxObj.Scopes()
	}
	_, ok = lenv.CreateLocalBinding(last, environment.BindingTypeVariable)
	if !ok {
		return nil, nil, values.ErrDuplicateBinding
	}
	// Update the binding with scopes if present (get the last binding added)
	if scopes != nil && len(scopes) > 0 {
		bindings := lenv.Bindings()
		if len(bindings) > 0 {
			binding := bindings[len(bindings)-1]
			if binding != nil {
				binding.SetScopes(scopes)
			}
		}
	}
	tpl.parameterCount++
	tpl.isVariadic = true
	return tpl, env, nil
}

func (p *CompileTimeContinuation) compileProcedureArgumentList(ctctx CompileTimeCallContext, args *syntax.SyntaxPair) error {
	tail, err := syntax.SyntaxForEach(ctctx.ctx, args, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
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
	tail, err := syntax.SyntaxForEach(ctctx.ctx, expr, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
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
		operationSaveContinuationIndex = p.template.operations.Length()
		p.AppendOperations(
			NewOperationSaveContinuationOffsetImmediate(0),
		)
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
	err = p.compileProcedureArgumentList(ctctx, expr.(*syntax.SyntaxPair))
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression list")
	}
	p.AppendOperations(
		NewOperationPull(),
		NewOperationApply(),
	)

	if !ctctx.inTail {
		// Patch the SaveContinuation offset for non-tail calls
		l := p.template.operations.Length()
		p.template.operations[operationSaveContinuationIndex] = NewOperationSaveContinuationOffsetImmediate(l - operationSaveContinuationIndex)
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
	initial, ok := stx0pr.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "expected a syntax object for procedure call, got %T", stx0pr.Car())
	}
	stx1cdr, ok := stx0pr.SyntaxCdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", stx1cdr)
	}
	stx0pr, ok = stx1cdr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", stx1cdr.Unwrap())
	}
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

// compileQuasiquoteDatum compiles a quasiquoted datum at the given nesting depth.
//
// depth=1 means we're inside one level of quasiquote (the common case).
// depth=2 means nested quasiquote `(a `(b ,x)), etc.
// depth=0 would mean we should evaluate (but we start at 1, so this is the trigger).
func (p *CompileTimeContinuation) compileQuasiquoteDatum(ctctx CompileTimeCallContext, datum syntax.SyntaxValue, depth int) error {
	// Optimization: if no runtime evaluation needed, emit as literal
	if !p.quasiquoteNeedsRuntime(datum, depth) {
		// Intern symbols to ensure eq? identity per R7RS 6.5
		val := p.internSymbolsInValue(datum.UnwrapAll())
		li := p.template.MaybeAppendLiteral(val)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Transform to equivalent Scheme code and compile
	expanded := p.expandQuasiquote(datum, depth)
	return p.CompileExpression(ctctx, expanded)
}

// buildQuasiquoteSyntaxList creates a proper list from syntax elements.
func (p *CompileTimeContinuation) buildQuasiquoteSyntaxList(srcCtx *syntax.SourceContext, elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
	for i := len(elems) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
	}
	return result
}

// wrapDatumAsSyntax wraps a values.Value as syntax.SyntaxValue.
func (p *CompileTimeContinuation) wrapDatumAsSyntax(srcCtx *syntax.SourceContext, v values.Value) syntax.SyntaxValue {
	if values.IsEmptyList(v) {
		return syntax.NewSyntaxEmptyList(srcCtx)
	}
	switch val := v.(type) {
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(val.Key, srcCtx)
	case *values.Pair:
		car := p.wrapDatumAsSyntax(srcCtx, val.Car())
		cdr := p.wrapDatumAsSyntax(srcCtx, val.Cdr())
		return syntax.NewSyntaxCons(car, cdr, srcCtx)
	case syntax.SyntaxValue:
		return val
	default:
		return syntax.NewSyntaxObject(v, srcCtx)
	}
}

// expandQuasiquote transforms quasiquoted syntax into equivalent Scheme code.
// At depth=1, unquotes are evaluated. At depth>1, they produce literal unquote forms.
//
// Key behaviors:
//   - unquote at d=1: return the expression directly (evaluate it)
//   - unquote at d>1: process arg at d-1, wrap in (list 'unquote <result>)
//   - unquote-splicing at d=1: handled specially by expandQuasiquoteList
//   - unquote-splicing at d>1: process arg at d-1, wrap in (list 'unquote-splicing <result>)
//   - quasiquote: process body at d+1, wrap in (list 'quasiquote <result>)
//   - lists: generate (list ...) or (append ...) for runtime construction
//   - atoms: quote them
func (p *CompileTimeContinuation) expandQuasiquote(stx syntax.SyntaxValue, depth int) syntax.SyntaxValue {
	srcCtx := stx.SourceContext()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)
		}

		if carSymName, ok := p.getSymbolName(v.SyntaxCar()); ok {
			switch carSymName {
			case "unquote":
				if depth == 1 {
					if v.Length() == 2 {
						cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
						return cdr.SyntaxCar().(syntax.SyntaxValue)
					}
				}
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar().(syntax.SyntaxValue)
					processedArg := p.expandQuasiquote(arg, depth-1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("unquote", srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

			case "unquote-splicing":
				if depth > 1 && v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar().(syntax.SyntaxValue)
					processedArg := p.expandQuasiquote(arg, depth-1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("unquote-splicing", srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

			case "quasiquote":
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar().(syntax.SyntaxValue)
					processedBody := p.expandQuasiquote(body, depth+1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("quasiquote", srcCtx),
						),
						processedBody,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)
			}
		}

		// Regular list - check for unquote-splicing at depth 1
		return p.expandQuasiquoteList(v, depth)

	case *syntax.SyntaxSymbol:
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

	case *syntax.SyntaxVector:
		// Vectors: expand elements and wrap in (list->vector (list ...))
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))
		for _, elem := range v.Values {
			elems = append(elems, p.expandQuasiquote(elem, depth))
		}
		listExpr := p.buildQuasiquoteSyntaxList(srcCtx, elems...)
		return p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("list->vector", srcCtx),
			listExpr,
		)

	default:
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, stx)
	}
}

// expandQuasiquoteList handles list expansion, detecting unquote-splicing.
func (p *CompileTimeContinuation) expandQuasiquoteList(pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Check if any element is ,@ at depth 1
	hasSplice := false
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if ok {
			carPair, ok := carSyntax.(*syntax.SyntaxPair)
			if ok {
				carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
				if ok {
					if carSymName == "unquote-splicing" && depth == 1 {
						hasSplice = true
						break
					}
				}
			}
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	if !hasSplice {
		// Simple case: (list elem1 elem2 ...)
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))

		current := pair
		for !values.IsEmptyList(current) {
			car := current.SyntaxCar()
			if carSyntax, ok := car.(syntax.SyntaxValue); ok {
				elems = append(elems, p.expandQuasiquote(carSyntax, depth))
			} else {
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				litStx := p.wrapDatumAsSyntax(srcCtx, car)
				elems = append(elems, p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, litStx))
			}
			cdr := current.SyntaxCdr()
			if syntax.IsSyntaxEmptyList(cdr) {
				break
			}
			if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
				current = nextPair
			} else {
				// Improper list - handle dotted tail
				// Generate (cons elem1 (cons elem2 ... tail))
				return p.expandQuasiquoteImproperList(pair, depth)
			}
		}
		return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
	}

	// Has splicing: use (append seg1 seg2 ...)
	return p.expandQuasiquoteListWithSplice(pair, depth)
}

// expandQuasiquoteImproperList handles improper (dotted) lists.
func (p *CompileTimeContinuation) expandQuasiquoteImproperList(pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Collect all elements and the tail
	var elements []syntax.SyntaxValue
	var tail values.Value

	current := pair
	for {
		car := current.SyntaxCdr()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			elements = append(elements, p.expandQuasiquote(carSyntax, depth))
		} else {
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			litStx := p.wrapDatumAsSyntax(srcCtx, car)
			elements = append(elements, p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, litStx))
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			tail = values.EmptyList
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			// Found the improper tail
			if tailSyntax, ok := cdr.(syntax.SyntaxValue); ok {
				tail = tailSyntax
			} else {
				tail = cdr
			}
			break
		}
	}

	// Build nested cons: (cons elem1 (cons elem2 ... tail))
	var result syntax.SyntaxValue
	if tailSyntax, ok := tail.(syntax.SyntaxValue); ok {
		result = p.expandQuasiquote(tailSyntax, depth)
	} else {
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		litStx := p.wrapDatumAsSyntax(srcCtx, tail)
		result = p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, litStx)
	}

	for i := len(elements) - 1; i >= 0; i-- {
		result = p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("cons", srcCtx),
			elements[i],
			result,
		)
	}
	return result
}

// expandQuasiquoteListWithSplice handles lists containing unquote-splicing.
func (p *CompileTimeContinuation) expandQuasiquoteListWithSplice(pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Segment types
	type segmentType int
	const (
		segNormal segmentType = iota
		segSplice
	)

	type segment struct {
		typ   segmentType
		elems []syntax.SyntaxValue // for normal segments
		expr  syntax.SyntaxValue   // for splice segments
	}

	var segments []segment
	var currentElems []syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			segments = append(segments, segment{typ: segNormal, elems: currentElems})
			currentElems = nil
		}
	}

	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if !ok {
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			litStx := p.wrapDatumAsSyntax(srcCtx, car)
			currentElems = append(currentElems, p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, litStx))
			goto next
		}

		if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok {
				if carSymName == "unquote-splicing" && depth == 1 {
					flushNormal()
					if carPair.Length() != 2 {
						// Malformed - treat as normal
						currentElems = append(currentElems, p.expandQuasiquote(carSyntax, depth))
					} else {
						cdrPair := carPair.SyntaxCdr().(*syntax.SyntaxPair)
						expr := cdrPair.SyntaxCar().(syntax.SyntaxValue)
						segments = append(segments, segment{typ: segSplice, expr: expr})
					}
					goto next
				}
			}
		}

		currentElems = append(currentElems, p.expandQuasiquote(carSyntax, depth))

	next:
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	flushNormal()

	// Build (append seg1 seg2 ...)
	var appendArgs []syntax.SyntaxValue
	appendArgs = append(appendArgs, syntax.NewSyntaxSymbol("append", srcCtx))

	for _, seg := range segments {
		switch seg.typ {
		case segNormal:
			// Wrap in (list ...)
			listArgs := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
			listArgs = append(listArgs, seg.elems...)
			appendArgs = append(appendArgs, p.buildQuasiquoteSyntaxList(srcCtx, listArgs...))
		case segSplice:
			appendArgs = append(appendArgs, seg.expr)
		}
	}

	return p.buildQuasiquoteSyntaxList(srcCtx, appendArgs...)
}

// quasiquoteNeedsRuntime checks if a syntax value contains unquotes that would
// be evaluated at the given depth. This is used to determine whether we can
// emit the quasiquoted form as a compile-time literal or need runtime list construction.
//
// Returns true if the form contains any unquote/unquote-splicing that reaches depth 1.
// For nested forms, it recursively adjusts the depth:
//   - unquote/unquote-splicing at depth 1 → needs runtime (returns true)
//   - unquote/unquote-splicing at depth > 1 → check argument at depth-1
//   - quasiquote → check body at depth+1
func (p *CompileTimeContinuation) quasiquoteNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			return false
		}
		// Check if this is (unquote ...) or (unquote-splicing ...) at depth 1
		if carSymName, ok := p.getSymbolName(v.SyntaxCar()); ok {
			switch carSymName {
			case "unquote", "unquote-splicing":
				if depth == 1 {
					return true
				}
				// Nested unquote at depth > 1 - check if the argument needs runtime
				// For ,,x at depth 2: the inner ,x is at depth 1 and needs eval
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					if argSyntax, ok := arg.(syntax.SyntaxValue); ok {
						return p.quasiquoteNeedsRuntime(argSyntax, depth-1)
					}
				}
				return false
			case "quasiquote":
				// Nested quasiquote increases depth
				return p.quasiquoteNeedsRuntimeList(v, depth+1)
			}
		}
		// Check elements
		return p.quasiquoteNeedsRuntimeList(v, depth)

	case *syntax.SyntaxVector:
		// Check vector elements
		for _, elem := range v.Values {
			if p.quasiquoteNeedsRuntime(elem, depth) {
				return true
			}
		}
		return false

	default:
		return false
	}
}

func (p *CompileTimeContinuation) quasiquoteNeedsRuntimeList(pair *syntax.SyntaxPair, depth int) bool {
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			if p.quasiquoteNeedsRuntime(carSyntax, depth) {
				return true
			}
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			break
		}
		current = nextPair
	}
	return false
}

// getSymbolName returns the symbol name if the value is a symbol
func (p *CompileTimeContinuation) getSymbolName(v syntax.SyntaxValue) (string, bool) {
	switch s := v.(type) {
	case *syntax.SyntaxSymbol:
		if sym, ok := s.Unwrap().(*values.Symbol); ok {
			return sym.Key, true
		}
	}
	return "", false
}

// CompileUnquote errors - unquote outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquote(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unquote: not in quasiquote context")
}

// CompileUnquoteSplicing errors - unquote-splicing outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquoteSplicing(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unquote-splicing: not in quasiquote context")
}

func (p *CompileTimeContinuation) CompileExpression(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// Validate the expression first
	result := validate.ValidateExpression(nil, expr)
	if !result.Ok() {
		return values.NewForeignError(result.Error())
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
		if val == nil || val == values.EmptyList {
			return val
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

func (p *CompileTimeContinuation) CompileSelfEvaluating(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if expr == nil {
		// Load void for nil expressions
		p.AppendOperations(
			NewOperationLoadVoid(),
		)
		return nil
	}
	// Intern symbols to ensure eq? identity per R7RS 6.5
	val := p.internSymbolsInValue(expr.Unwrap())
	li := p.template.MaybeAppendLiteral(val)
	p.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(li),
	)
	return nil
}

func (p *CompileTimeContinuation) AppendOperations(ops ...Operation) {
	p.template.operations = append(p.template.operations, ops...)
}

// recordSource records source location mapping for operations emitted between
// startPC and the current PC. This enables stack traces and debugging.
func (p *CompileTimeContinuation) recordSource(startPC int, source *syntax.SourceContext) {
	if source != nil && p.template.sourceMap != nil {
		endPC := len(p.template.operations)
		p.template.sourceMap.Add(startPC, endPC, source)
	}
}

// CompileDefineLibrary handles (define-library (lib-name) <library-declaration> ...).
//
// R7RS library syntax:
//
//	(define-library <library-name>
//	  <library-declaration> ...)
//
//	<library-declaration> =
//	  | (export <export-spec> ...)
//	  | (import <import-set> ...)
//	  | (begin <command-or-definition> ...)
//	  | (include <filename> ...)
//	  | (include-ci <filename> ...)
//
// This creates an isolated environment for the library, processes declarations
// in order, and registers the compiled library in the registry.
func (p *CompileTimeContinuation) CompileDefineLibrary(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is ((lib-name) <declaration> ...) - args after 'define-library' keyword
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected library name and declarations")
	}

	// Parse library name: (lib-name) is a list of identifiers
	libNameExpr, ok := rest.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "define-library: expected library name")
	}
	libName, err := parseLibraryName(libNameExpr)
	if err != nil {
		return values.WrapForeignErrorf(err, "define-library: invalid library name")
	}

	// Create isolated library environment with primitives
	var libEnv *environment.EnvironmentFrame
	if LibraryEnvFactory != nil {
		libEnv, err = LibraryEnvFactory()
		if err != nil {
			return values.WrapForeignErrorf(err, "define-library: could not create library environment")
		}
		// Share the library registry with the new environment so nested imports work
		libEnv.SetLibraryRegistry(p.env.LibraryRegistry())
	} else {
		// Fallback for tests that don't set up the factory
		libEnv = environment.NewTopLevelEnvironmentFrame()
	}
	lib := NewCompiledLibrary(libName, libEnv)

	// Process library declarations
	declsExpr, ok := rest.SyntaxCdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected declarations")
	}

	// Handle empty declarations list (just (define-library (name)))
	if syntax.IsSyntaxEmptyList(declsExpr) {
		// Empty library is valid - just call the callback
		lib.Template = NewNativeTemplate(0, 0, false)
		if p.libraryCallback != nil {
			p.libraryCallback(lib)
		}
		return nil
	}

	decls, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected list of declarations")
	}

	// Create a compiler for the library environment
	libTemplate := NewNativeTemplate(0, 0, false)
	libCompiler := NewCompiletimeContinuation(libTemplate, libEnv)

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, decls, func(ctx context.Context, i int, hasNext bool, decl syntax.SyntaxValue) error {
		return libCompiler.processLibraryDeclaration(ctctx, lib, decl)
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "define-library: error processing declarations")
	}

	// Store the compiled template in the library
	lib.Template = libTemplate

	// Call the library callback if set (used by LoadLibrary)
	if p.libraryCallback != nil {
		p.libraryCallback(lib)
	}

	// Library compilation is complete - no runtime operations in the main template
	return nil
}

// processLibraryDeclaration handles a single library declaration.
func (p *CompileTimeContinuation) processLibraryDeclaration(ctctx CompileTimeCallContext, lib *CompiledLibrary, decl syntax.SyntaxValue) error {
	declPair, ok := decl.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "library declaration must be a list")
	}

	// Get the declaration keyword
	keywordExpr, ok := declPair.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "library declaration must start with keyword")
	}

	keywordSym, ok := keywordExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "library declaration must start with symbol")
	}

	keyword := keywordSym.Unwrap().(*values.Symbol).Key

	// Get the rest of the declaration (arguments)
	argsExpr, ok := declPair.SyntaxCdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "library declaration missing arguments")
	}

	switch keyword {
	case "export":
		return p.processLibraryExport(lib, argsExpr)
	case "import":
		return p.processLibraryImport(ctctx, lib, argsExpr)
	case "begin":
		// Compile the begin body in the library environment
		beginPair, ok := argsExpr.(*syntax.SyntaxPair)
		if !ok {
			if syntax.IsSyntaxEmptyList(argsExpr) {
				return nil // empty begin is valid
			}
			return values.WrapForeignErrorf(values.ErrNotAPair, "begin: expected list of expressions")
		}
		return p.compileExpressionList(ctctx, beginPair)
	case "include":
		return p.CompileInclude(ctctx, argsExpr)
	case "include-ci":
		return p.CompileIncludeCi(ctctx, argsExpr)
	case "include-library-declarations":
		return p.processIncludeLibraryDeclarations(ctctx, lib, argsExpr)
	case "cond-expand":
		return p.processCondExpand(ctctx, lib, argsExpr)
	default:
		return values.NewForeignErrorf("unknown library declaration: %s", keyword)
	}
}

// processLibraryExport handles (export <export-spec> ...) within a library.
func (p *CompileTimeContinuation) processLibraryExport(lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty export is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "export: expected list of export specs")
	}

	_, err := syntax.SyntaxForEach(nil, argsPair, func(ctx context.Context, i int, hasNext bool, spec syntax.SyntaxValue) error {
		return parseExportSpec(lib, spec)
	})
	return err
}

// parseExportSpec parses a single export spec and adds it to the library.
// Export specs can be:
//   - <identifier>              : export with same internal and external name
//   - (rename <internal> <external>) : export with different names
func parseExportSpec(lib *CompiledLibrary, spec syntax.SyntaxValue) error {
	switch s := spec.(type) {
	case *syntax.SyntaxSymbol:
		// Simple export: symbol name
		name := s.Unwrap().(*values.Symbol).Key
		lib.AddExport(name, name)
		return nil

	case *syntax.SyntaxPair:
		// Could be (rename internal external)
		carExpr, ok := s.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "export: invalid spec")
		}

		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export: expected symbol")
		}

		if carSym.Unwrap().(*values.Symbol).Key == "rename" {
			// (rename internal external)
			cdrExpr, ok := s.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected internal and external names")
			}

			internalExpr, ok := cdrExpr.SyntaxCar().(syntax.SyntaxValue)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "export rename: expected internal name")
			}
			internalSym, ok := internalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export rename: internal name must be symbol")
			}

			cdrCdr, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected external name")
			}

			externalExpr, ok := cdrCdr.SyntaxCar().(syntax.SyntaxValue)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "export rename: expected external name")
			}
			externalSym, ok := externalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export rename: external name must be symbol")
			}

			internalName := internalSym.Unwrap().(*values.Symbol).Key
			externalName := externalSym.Unwrap().(*values.Symbol).Key
			lib.AddExport(externalName, internalName)
			return nil
		}

		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "export: invalid spec form")

	default:
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "export: expected symbol or rename form")
	}
}

// processLibraryImport handles (import <import-set> ...) within a library.
func (p *CompileTimeContinuation) processLibraryImport(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, i int, hasNext bool, importSetExpr syntax.SyntaxValue) error {
		importSet, err := parseImportSet(importSetExpr)
		if err != nil {
			return err
		}

		// Load the library
		// Note: p.env is the library's environment, which has the registry via SetLibraryRegistry
		importedLib, err := LoadLibrary(ctx, importSet.LibraryName, p.env)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers to get final bindings
		bindings, err := importSet.ApplyToExports(importedLib)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		// Bind the imported names in the library's environment (lib.Env)
		for localName, externalName := range bindings {
			internalName := importedLib.GetInternalName(externalName)
			if internalName == "" {
				internalName = externalName
			}

			// Get the binding from the imported library's environment
			// First check the runtime environment, then the expand environment for syntax bindings
			libSym := importedLib.Env.InternSymbol(values.NewSymbol(internalName))
			importedBinding := importedLib.Env.GetBinding(libSym)
			if importedBinding == nil {
				// Syntax bindings (define-syntax) are stored in the expand environment
				expandEnv := importedLib.Env.Expand()
				if expandEnv != nil {
					importedBinding = expandEnv.GetBinding(libSym)
				}
			}
			if importedBinding == nil {
				return values.NewForeignErrorf("import: %s exports %q but binding not found",
					importSet.LibraryName.SchemeString(), internalName)
			}

			// Create binding in the importing library's environment
			localSym := lib.Env.InternSymbol(values.NewSymbol(localName))
			_, _ = lib.Env.MaybeCreateOwnGlobalBinding(localSym, importedBinding.BindingType())
			globalIdx := lib.Env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				if err := lib.Env.SetOwnGlobalValue(globalIdx, importedBinding.Value()); err != nil {
					return values.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
				}
			}

			// If it's a syntax binding, also copy to expand phase
			if importedBinding.BindingType() == environment.BindingTypeSyntax {
				expandEnv := lib.Env.Expand()
				_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
				expandIdx := expandEnv.GetGlobalIndex(localSym)
				if expandIdx != nil {
					_ = expandEnv.SetOwnGlobalValue(expandIdx, importedBinding.Value())
				}
			}
		}

		return nil
	})
	return err
}

// parseImportSet parses an import set with optional modifiers.
// Import sets can be:
//   - (<library-name>)              : import all exports
//   - (only <import-set> <id> ...)  : import only specified identifiers
//   - (except <import-set> <id> ...): import all except specified
//   - (prefix <import-set> <prefix>): add prefix to all imported names
//   - (rename <import-set> (<old> <new>) ...): rename specific imports
func parseImportSet(expr syntax.SyntaxValue) (*ImportSet, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "import set must be a list")
	}

	// Check if first element is a modifier keyword
	carExpr, ok := pair.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "import set: invalid first element")
	}

	if carSym, ok := carExpr.(*syntax.SyntaxSymbol); ok {
		keyword := carSym.Unwrap().(*values.Symbol).Key

		switch keyword {
		case "only":
			return parseImportSetOnly(pair)
		case "except":
			return parseImportSetExcept(pair)
		case "prefix":
			return parseImportSetPrefix(pair)
		case "rename":
			return parseImportSetRename(pair)
		}
	}

	// Not a modifier, must be a library name
	libName, err := parseLibraryName(expr)
	if err != nil {
		return nil, err
	}
	return NewImportSet(libName), nil
}

// parseImportSetOnly parses (only <import-set> <id> ...)
func parseImportSetOnly(pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "only: expected import-set")
	}
	importSet, err := parseImportSet(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr, ok := cdrExpr.Cdr().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected identifiers")
	}

	ids, err := parseIdentifierList(idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Only = ids
	return importSet, nil
}

// parseImportSetExcept parses (except <import-set> <id> ...)
func parseImportSetExcept(pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "except: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "except: expected import-set")
	}
	importSet, err := parseImportSet(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get identifiers
	idsExpr, ok := cdrExpr.Cdr().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "except: expected identifiers")
	}

	ids, err := parseIdentifierList(idsExpr)
	if err != nil {
		return nil, err
	}

	importSet.Except = ids
	return importSet, nil
}

// parseImportSetPrefix parses (prefix <import-set> <prefix>)
func parseImportSetPrefix(pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected import-set and prefix")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "prefix: expected import-set")
	}
	importSet, err := parseImportSet(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get prefix
	prefixPair, ok := cdrExpr.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected prefix identifier")
	}

	prefixExpr, ok := prefixPair.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "prefix: expected prefix identifier")
	}

	prefixSym, ok := prefixExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "prefix: prefix must be a symbol")
	}

	importSet.Prefix = prefixSym.Unwrap().(*values.Symbol).Key
	return importSet, nil
}

// parseImportSetRename parses (rename <import-set> (<old> <new>) ...)
func parseImportSetRename(pair *syntax.SyntaxPair) (*ImportSet, error) {
	cdrExpr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected import-set and rename pairs")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.SyntaxCar().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "rename: expected import-set")
	}
	importSet, err := parseImportSet(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get rename pairs
	renamesExpr, ok := cdrExpr.Cdr().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected rename pairs")
	}

	if syntax.IsSyntaxEmptyList(renamesExpr) {
		return importSet, nil
	}

	renamesPair, ok := renamesExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected list of rename pairs")
	}

	_, err = syntax.SyntaxForEach(nil, renamesPair, func(ctx context.Context, i int, hasNext bool, renamePairExpr syntax.SyntaxValue) error {
		renamePair, ok := renamePairExpr.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected (old new) pair")
		}

		oldExpr, ok := renamePair.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "rename: expected old name")
		}
		oldSym, ok := oldExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "rename: old name must be symbol")
		}

		newPair, ok := renamePair.SyntaxCdr().(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected new name")
		}

		newExpr, ok := newPair.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "rename: expected new name")
		}
		newSym, ok := newExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "rename: new name must be symbol")
		}

		oldName := oldSym.Unwrap().(*values.Symbol).Key
		newName := newSym.Unwrap().(*values.Symbol).Key
		importSet.Renames[oldName] = newName
		return nil
	})

	return importSet, err
}

// parseIdentifierList parses a list of identifiers into a string slice.
func parseIdentifierList(expr syntax.SyntaxValue) ([]string, error) {
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "expected list of identifiers")
	}

	var ids []string
	_, err := syntax.SyntaxForEach(nil, pair, func(ctx context.Context, i int, hasNext bool, idExpr syntax.SyntaxValue) error {
		idSym, ok := idExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "expected identifier symbol")
		}
		ids = append(ids, idSym.Unwrap().(*values.Symbol).Key)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return ids, nil
}

// parseLibraryName extracts a LibraryName from a syntax expression like (scheme base).
func parseLibraryName(expr syntax.SyntaxValue) (LibraryName, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return LibraryName{}, values.WrapForeignErrorf(values.ErrNotAPair, "library name must be a list")
	}

	var parts []string
	_, err := syntax.SyntaxForEach(nil, pair, func(ctx context.Context, i int, hasNext bool, partExpr syntax.SyntaxValue) error {
		if sym, ok := partExpr.(*syntax.SyntaxSymbol); ok {
			parts = append(parts, sym.Unwrap().(*values.Symbol).Key)
			return nil
		}
		// Could be a number (for versioned library names)
		if num, ok := partExpr.Unwrap().(*values.Integer); ok {
			parts = append(parts, fmt.Sprintf("%d", num.Value))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrInvalidSyntax, "library name part must be identifier or integer")
	})
	if err != nil {
		return LibraryName{}, err
	}
	if len(parts) == 0 {
		return LibraryName{}, values.NewForeignErrorf("library name cannot be empty")
	}
	return NewLibraryName(parts...), nil
}

// CompileImport handles top-level (import <import-set> ...).
//
// This is for top-level imports outside of a library definition.
// It loads the specified libraries and binds their exports in the current environment.
func (p *CompileTimeContinuation) CompileImport(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is (<import-set> ...) - args after 'import' keyword
	if syntax.IsSyntaxEmptyList(expr) {
		return nil // empty import is valid
	}

	importSets, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	v, err := syntax.SyntaxForEach(ctctx.ctx, importSets, func(ctx context.Context, i int, hasNext bool, importSetExpr syntax.SyntaxValue) error {
		importSet, err := parseImportSet(importSetExpr)
		if err != nil {
			return err
		}

		// Load the library
		lib, err := LoadLibrary(ctx, importSet.LibraryName, p.env)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: failed to load library %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply import modifiers (only, except, prefix, rename) to get final bindings
		bindings, err := importSet.ApplyToExports(lib)
		if err != nil {
			return values.WrapForeignErrorf(err, "import: error applying modifiers for %s",
				importSet.LibraryName.SchemeString())
		}

		// Bind the imported names in the current environment
		for localName, externalName := range bindings {
			internalName := lib.GetInternalName(externalName)
			if internalName == "" {
				internalName = externalName
			}

			// Get the binding from the library's environment
			// First check the runtime environment, then the expand environment for syntax bindings
			libSym := lib.Env.InternSymbol(values.NewSymbol(internalName))
			libBinding := lib.Env.GetBinding(libSym)
			if libBinding == nil {
				// Syntax bindings (define-syntax) are stored in the expand environment
				expandEnv := lib.Env.Expand()
				if expandEnv != nil {
					libBinding = expandEnv.GetBinding(libSym)
				}
			}
			if libBinding == nil {
				return values.NewForeignErrorf("import: %s exports %q but binding not found in library",
					importSet.LibraryName.SchemeString(), internalName)
			}

			// Create binding in the importing environment
			localSym := p.env.InternSymbol(values.NewSymbol(localName))
			_, created := p.env.MaybeCreateOwnGlobalBinding(localSym, libBinding.BindingType())
			if !created {
				// Binding already exists - update it
				// (This allows re-importing in REPL)
			}
			globalIdx := p.env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				if err := p.env.SetOwnGlobalValue(globalIdx, libBinding.Value()); err != nil {
					return values.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
				}
			}

			// If it's a syntax binding, also copy to expand phase
			if libBinding.BindingType() == environment.BindingTypeSyntax {
				expandEnv := p.env.Expand()
				_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
				expandIdx := expandEnv.GetGlobalIndex(localSym)
				if expandIdx != nil {
					_ = expandEnv.SetOwnGlobalValue(expandIdx, libBinding.Value())
				}
			}
		}

		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "import: error processing import sets")
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "import: unexpected return value")
	}
	return nil
}

// CompileExport handles top-level (export <export-spec> ...).
//
// This is only valid within a library definition. At top-level, it's an error.
func (p *CompileTimeContinuation) CompileExport(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignErrorf("export: only valid within define-library")
}

// CompileDefineSyntax handles (define-syntax keyword transformer-expr).
//
// This is the compile-time handler for R7RS define-syntax. Unlike most
// definitions, define-syntax is processed entirely at compile time:
//
//  1. Parse the form: (define-syntax keyword (syntax-rules ...))
//  2. Compile the syntax-rules transformer to a MachineClosure
//  3. Store the closure in the environment with BindingTypeSyntax
//  4. Emit NO runtime operations (the binding is already established)
//
// The BindingTypeSyntax marker is crucial: when the expander encounters
// a symbol, it checks if that symbol is bound to a syntax transformer.
// If so, it invokes the transformer closure to expand the macro.
//
// This is how derived expressions like 'let' work: they're defined as
// macros using define-syntax, and expand to lambda expressions:
//
//	(define-syntax let
//	  (syntax-rules ()
//	    ((let ((name val) ...) body)
//	     ((lambda (name ...) body) val ...))))
//
// Reference: R7RS Section 5.4 (Syntax definitions)
func (p *CompileTimeContinuation) CompileDefineSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if p.env == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: nil environment")
	}
	if p.template == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: nil template")
	}
	// expr is (keyword transformer-expr) - the args after 'define-syntax' has been stripped
	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-syntax: expected keyword and transformer")
	}
	// Get the keyword to bind
	keywordStx := argsPair.SyntaxCar()
	if keywordStx == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing keyword")
	}
	keywordSym, ok := keywordStx.(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol")
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	// Get the transformer expression
	transformerCdr := argsPair.Cdr()
	if transformerCdr == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing transformer expression")
	}
	transformerPair, ok := transformerCdr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-syntax: expected transformer expression")
	}
	transformerExpr := transformerPair.SyntaxCar()
	if transformerExpr == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing transformer expression")
	}
	// Check if transformer is a syntax-rules form
	transformerPairExpr, ok := transformerExpr.(*syntax.SyntaxPair)
	if !ok {
		// For non-syntax-rules transformers, we would need to compile and evaluate
		// the transformer expression at compile time. For now, just support syntax-rules
		return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
	}
	car := transformerPairExpr.SyntaxCar()
	if car == nil {
		// For non-syntax-rules transformers, we would need to compile and evaluate
		// the transformer expression at compile time. For now, just support syntax-rules
		return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
	}
	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		// For non-syntax-rules transformers, we would need to compile and evaluate
		// the transformer expression at compile time. For now, just support syntax-rules
		return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
	}
	symVal := sym.Unwrap()
	if symVal == nil {
		// For non-syntax-rules transformers, we would need to compile and evaluate
		// the transformer expression at compile time. For now, just support syntax-rules
		return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
	}
	symbol, ok := symVal.(*values.Symbol)
	if ok && symbol.Key == "syntax-rules" {
		// Compile syntax-rules directly
		closure, err := CompileSyntaxRules(ctctx.ctx, p.env, transformerPairExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "could not compile syntax-rules transformer")
		}

		// Store the transformer in the expand phase environment with BindingTypeSyntax
		// R7RS requires syntax bindings to live in the expand phase, separate from runtime bindings
		expandEnv := p.env.Expand()
		globalIndex, created := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
		if !created {
			// Update existing binding
			globalIndex = expandEnv.GetGlobalIndex(keyword)
		}
		if globalIndex != nil {
			err = expandEnv.SetOwnGlobalValue(globalIndex, closure)
			if err != nil {
				return err
			}
		}

		// define-syntax is compile-time only, emit no runtime operations
		return nil
	}
	// Check if transformer is a lambda (procedural macro)
	symbol, ok = symVal.(*values.Symbol)
	if ok && symbol.Key == "lambda" {
		// Compile and evaluate the lambda at compile time to get a transformer closure
		closure, err := p.compileAndEvalTransformer(transformerPairExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "could not compile lambda transformer")
		}

		// Store the transformer in the expand phase environment with BindingTypeSyntax
		expandEnv := p.env.Expand()
		globalIndex, created := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax)
		if !created {
			globalIndex = expandEnv.GetGlobalIndex(keyword)
		}
		if globalIndex == nil {
			return nil
		}
		err = expandEnv.SetOwnGlobalValue(globalIndex, closure)
		if err != nil {
			return err
		}

		return nil
	}
	// For non-syntax-rules transformers, we would need to compile and evaluate
	// the transformer expression at compile time. For now, just support syntax-rules
	return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
}

// compileAndEvalTransformer compiles a lambda expression and evaluates it at
// compile time to produce a closure that can be used as a syntax transformer.
func (p *CompileTimeContinuation) compileAndEvalTransformer(transformerExpr syntax.SyntaxValue) (*MachineClosure, error) {
	// Create a fresh template for the transformer
	tpl := NewNativeTemplate(0, 0, false)

	// Use the expand phase environment for compiling the transformer
	// since transformers operate at the expand phase
	expandEnv := p.env.Expand()

	// Expand the transformer expression first
	ectx := NewExpandTimeCallContext()
	expandedExpr, err := NewExpanderTimeContinuation(expandEnv).ExpandExpression(ectx, transformerExpr)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error expanding transformer")
	}

	// Compile the transformer lambda to bytecode
	// Use inTail=false and inExpression=true for compile-time evaluation
	cctx := NewCompileTimeCallContext(false, true, expandEnv)
	compiler := NewCompiletimeContinuation(tpl, expandEnv)
	err = compiler.CompileExpression(cctx, expandedExpr)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error compiling transformer")
	}

	// Execute the compiled template at compile time to get the closure
	cont := NewMachineContinuation(nil, tpl, expandEnv)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error evaluating transformer")
	}

	// The result should be a closure
	result := mc.GetValue()
	closure, ok := result.(*MachineClosure)
	if !ok {
		return nil, values.NewForeignErrorf("define-syntax: transformer must be a procedure, got %T", result)
	}

	return closure, nil
}

// CompileCondExpand compiles a cond-expand expression.
// cond-expand is evaluated at compile-time and expands to the body of the first
// clause whose feature requirement is satisfied.
//
// Syntax: (cond-expand <clause> ...)
// where <clause> is (<feature-requirement> <expression> ...)
//
// Example:
//
//	(cond-expand
//	  (r7rs (display "R7RS"))
//	  (else (display "other")))
func (p *CompileTimeContinuation) CompileCondExpand(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(expr) {
		return values.NewForeignErrorf("cond-expand: no clauses")
	}

	argsPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of clauses")
	}

	// Get the library registry for checking library availability
	var registry *LibraryRegistry
	if regAny := p.env.LibraryRegistry(); regAny != nil {
		registry, _ = regAny.(*LibraryRegistry)
	}

	// Find the first matching clause
	var matchedClause syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, i int, hasNext bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr, ok := clausePair.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "cond-expand: invalid feature requirement")
		}

		// Parse and evaluate the feature requirement
		req, err := parseFeatureRequirement(reqExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "cond-expand: invalid feature requirement")
		}

		if req.IsSatisfied(registry) {
			matchedClause = clausePair
		}

		return nil
	})
	if err != nil {
		return err
	}

	if matchedClause == nil {
		return values.NewForeignErrorf("cond-expand: no matching clause")
	}

	// Compile the expressions in the matched clause
	matchedPair := matchedClause.(*syntax.SyntaxPair)
	bodyExpr, ok := matchedPair.Cdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "cond-expand: invalid body")
	}

	if syntax.IsSyntaxEmptyList(bodyExpr) {
		// Empty body - emit void
		voidIdx := p.template.MaybeAppendLiteral(values.Void)
		p.template.AppendOperations(
			NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx),
		)
		return nil
	}

	bodyPair, ok := bodyExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of expressions")
	}

	// Expand and compile each body expression
	// (since cond-expand is not expanded, we must expand the body here)
	_, err = syntax.SyntaxForEach(ctctx.ctx, bodyPair, func(ctx context.Context, i int, hasNext bool, expr syntax.SyntaxValue) error {
		// Expand the expression
		ectx := NewExpandTimeCallContext()
		expanded, expandErr := NewExpanderTimeContinuation(p.env).ExpandExpression(ectx, expr)
		if expandErr != nil {
			return values.WrapForeignErrorf(expandErr, "cond-expand: error expanding body expression")
		}

		// Compile the expanded expression
		// Use the appropriate context for tail position (only last expression is in tail position)
		bodyCtx := ctctx
		if hasNext {
			bodyCtx = ctctx.NotInTail()
		}
		return p.CompileExpression(bodyCtx, expanded)
	})
	return err
}

// processCondExpand handles (cond-expand <clause> ...) within a library.
// Each clause is (<feature-requirement> <library-declaration> ...)
// The first clause whose feature requirement is satisfied has its declarations processed.
func (p *CompileTimeContinuation) processCondExpand(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return values.NewForeignErrorf("cond-expand: no clauses")
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of clauses")
	}

	// Get the library registry for checking library availability
	var registry *LibraryRegistry
	if regAny := p.env.LibraryRegistry(); regAny != nil {
		registry, _ = regAny.(*LibraryRegistry)
	}

	// Iterate through clauses
	var matchedClause syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, i int, hasNext bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr, ok := clausePair.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "cond-expand: invalid feature requirement")
		}

		// Parse and evaluate the feature requirement
		req, err := parseFeatureRequirement(reqExpr)
		if err != nil {
			return values.WrapForeignErrorf(err, "cond-expand: invalid feature requirement")
		}

		if req.IsSatisfied(registry) {
			matchedClause = clausePair
		}

		return nil
	})
	if err != nil {
		return err
	}

	if matchedClause == nil {
		return values.NewForeignErrorf("cond-expand: no matching clause")
	}

	// Process the declarations in the matched clause
	matchedPair := matchedClause.(*syntax.SyntaxPair)
	declsExpr, ok := matchedPair.Cdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "cond-expand: invalid declarations")
	}

	if syntax.IsSyntaxEmptyList(declsExpr) {
		return nil // Empty clause body is valid
	}

	declsPair, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: expected list of declarations")
	}

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, declsPair, func(ctx context.Context, i int, hasNext bool, decl syntax.SyntaxValue) error {
		return p.processLibraryDeclaration(ctctx, lib, decl)
	})
	return err
}

// parseFeatureRequirement parses a feature requirement expression.
// Feature requirements can be:
//   - <identifier> - simple feature check
//   - (library <library-name>) - library availability check
//   - (and <req> ...) - all requirements must be satisfied
//   - (or <req> ...) - at least one must be satisfied
//   - (not <req>) - must NOT be satisfied
//   - else - always satisfied (only valid as the last clause)
func parseFeatureRequirement(expr syntax.SyntaxValue) (FeatureRequirement, error) {
	switch v := expr.(type) {
	case *syntax.SyntaxSymbol:
		name := v.Unwrap().(*values.Symbol).Key
		if name == "else" {
			return NewElseRequirement(), nil
		}
		return NewFeatureIdentifier(name), nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			return nil, values.NewForeignErrorf("empty feature requirement")
		}

		carExpr, ok := v.SyntaxCar().(syntax.SyntaxValue)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "invalid feature requirement")
		}

		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "feature requirement must start with symbol")
		}

		keyword := carSym.Unwrap().(*values.Symbol).Key
		argsExpr, ok := v.Cdr().(syntax.SyntaxValue)
		if !ok {
			return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "invalid feature requirement arguments")
		}

		switch keyword {
		case "library":
			// (library <library-name>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, values.NewForeignErrorf("library: expected library name")
			}
			libNameExpr, ok := argsPair.SyntaxCar().(syntax.SyntaxValue)
			if !ok {
				return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "library: invalid library name")
			}
			libName, err := parseLibraryName(libNameExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "library: invalid library name")
			}
			return NewLibraryRequirement(libName), nil

		case "and":
			// (and <req> ...)
			reqs, err := parseFeatureRequirementList(argsExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "and: invalid requirements")
			}
			return NewAndRequirement(reqs...), nil

		case "or":
			// (or <req> ...)
			reqs, err := parseFeatureRequirementList(argsExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "or: invalid requirements")
			}
			return NewOrRequirement(reqs...), nil

		case "not":
			// (not <req>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, values.NewForeignErrorf("not: expected one requirement")
			}
			reqExpr, ok := argsPair.SyntaxCar().(syntax.SyntaxValue)
			if !ok {
				return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "not: invalid requirement")
			}
			req, err := parseFeatureRequirement(reqExpr)
			if err != nil {
				return nil, values.WrapForeignErrorf(err, "not: invalid requirement")
			}
			return NewNotRequirement(req), nil

		default:
			return nil, values.NewForeignErrorf("unknown feature requirement keyword: %s", keyword)
		}

	default:
		return nil, values.NewForeignErrorf("invalid feature requirement type: %T", expr)
	}
}

// parseFeatureRequirementList parses a list of feature requirements.
func parseFeatureRequirementList(expr syntax.SyntaxValue) ([]FeatureRequirement, error) {
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "expected list of requirements")
	}

	var reqs []FeatureRequirement
	_, err := syntax.SyntaxForEach(nil, pair, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
		req, err := parseFeatureRequirement(v)
		if err != nil {
			return err
		}
		reqs = append(reqs, req)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return reqs, nil
}

// processIncludeLibraryDeclarations handles (include-library-declarations <string> ...) within a library.
// It reads each file and processes its contents as library declarations.
func (p *CompileTimeContinuation) processIncludeLibraryDeclarations(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // Empty is valid (no-op)
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "include-library-declarations: expected list of filenames")
	}

	// Process each filename
	_, err := syntax.SyntaxForEach(ctctx.ctx, argsPair, func(ctx context.Context, i int, hasNext bool, v syntax.SyntaxValue) error {
		fn, ok := v.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "include-library-declarations: expected string filename")
		}

		// Find and open the file
		file, filePath, err := findFile(p, ctctx, fn.Value)
		if err != nil {
			return values.WrapForeignErrorf(err, "include-library-declarations: failed to find file %q", fn.Value)
		}
		if file == nil {
			return values.NewForeignErrorf("include-library-declarations: file not found: %q", fn.Value)
		}
		defer file.Close() //nolint:errcheck

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParserWithFile(p.env, reader, filePath)

		// Read and process all forms from the file as library declarations
		for {
			stx, readErr := fileParser.ReadSyntax(nil)
			if readErr != nil {
				if errors.Is(readErr, io.EOF) {
					break
				}
				return values.WrapForeignErrorf(readErr, "include-library-declarations: error reading %q", fn.Value)
			}

			// Process the form as a library declaration
			if err := p.processLibraryDeclaration(ctctx, lib, stx); err != nil {
				return values.WrapForeignErrorf(err, "include-library-declarations: error processing declaration from %q", fn.Value)
			}
		}

		return nil
	})
	return err
}
