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

	"skeme/environment"
	"skeme/parser"
	"skeme/syntax"
	"skeme/validate"
	"skeme/values"
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

func (p *CompileTimeContinuation) CompileSymbol(ccnt CompileTimeCallContext, expr *syntax.SyntaxSymbol) error {
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

func (p *CompileTimeContinuation) CompileSyntaxPrimitive(ccnt CompileTimeCallContext, sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (bool, error) {
	symVal := sym.Unwrap().(*values.Symbol)
	symVal = p.env.InternSymbol(symVal)
	scopes := sym.Scopes()

	// First, try dynamic lookup in the compile environment.
	// This is the new data-driven approach where primitive compilers
	// are bound as PrimitiveCompiler values in env.Compile().
	if pc := LookupPrimitiveCompiler(p.env, symVal, scopes); pc != nil {
		err := pc.Compile(p, ccnt, expr)
		if err != nil {
			return true, err
		}
		return true, nil
	}

	// Fallback to switch statement for passthrough forms that aren't in the registry.
	// Core forms (define, lambda, quote, quasiquote, if, set!, begin) are handled
	// by compileValidated* methods and should never reach here.
	var err error
	switch symVal.Key {
	case "meta":
		err = p.CompileMeta(ccnt, expr)
	case "include":
		err = p.CompileInclude(ccnt, expr)
	case "include-ci":
		err = p.CompileIncludeCi(ccnt, expr)
	case "define-syntax":
		err = p.CompileDefineSyntax(ccnt, expr)
	case "define-library":
		err = p.CompileDefineLibrary(ccnt, expr)
	case "library":
		// R6RS-style library - treat as define-library
		err = p.CompileDefineLibrary(ccnt, expr)
	case "import":
		err = p.CompileImport(ccnt, expr)
	case "export":
		err = p.CompileExport(ccnt, expr)
	default:
		return false, values.ErrNotAPrimitive
	}
	if err != nil {
		return true, err
	}
	return true, nil
}

func (p *CompileTimeContinuation) CompileMeta(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%T is not a pair", expr)
	}
	// Get the meta environment and compile expressions in it
	metaEnv := p.env.Meta()
	metaCont := NewCompiletimeContinuation(p.template, metaEnv)
	err := metaCont.compileExpressionList(ccnt, rest)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile meta")
	}
	return nil
}

func findFile(p *CompileTimeContinuation, ccnt CompileTimeCallContext, path string) (fs.File, error) {
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath == "" {
		return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "environment variable %q not set", SchemeIncludePathEnv)
	}
	includePaths := filepath.SplitList(includePath)
	for i := range includePath {
		fn := filepath.Join(includePaths[i], path)
		f, err := os.Open(fn)
		// return the first found file
		if err == nil {
			return f, nil
		}
	}
	return nil, nil
}

// CompileInclude compiles an include expression.
// It reads and compiles all forms from the specified files in order.
// Each form is expanded and compiled in the current environment.
func (p *CompileTimeContinuation) CompileInclude(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.compileIncludeImpl(ccnt, expr, false)
}

// compileIncludeImpl is the shared implementation for include and include-ci.
func (p *CompileTimeContinuation) compileIncludeImpl(ccnt CompileTimeCallContext, expr syntax.SyntaxValue, caseInsensitive bool) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	for !syntax.IsSyntaxEmptyList(rest) {
		// Get the file name
		car := rest.Car()
		next, ok := car.(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a value, but got %T", car)
		}
		fn, ok := next.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a string but got a %T", next)
		}

		// Find and open the file
		file, err := findFile(p, ccnt, fn.Value)
		if err != nil {
			return values.WrapForeignErrorf(err, "include: failed to find file %q", fn.Value)
		}
		if file == nil {
			return values.NewForeignErrorf("include: file not found: %q", fn.Value)
		}
		defer file.Close()

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParser(p.env, reader)

		// Read and compile all forms from the file
		for {
			stx, readErr := fileParser.ReadSyntax()
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
			compileErr := p.CompileExpression(ccnt, expanded)
			if compileErr != nil {
				return values.WrapForeignErrorf(compileErr, "include: error compiling form from %q", fn.Value)
			}
		}

		// Move to next filename
		rest, ok = rest.Cdr().(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list, got %T", rest)
		}
	}
	return nil
}

// CompileIncludeCi compiles an include-ci expression.
// It reads and compiles all forms from the specified files in order,
// treating symbols as case-insensitive (folded to lowercase).
func (p *CompileTimeContinuation) CompileIncludeCi(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// TODO: implement case-insensitive parsing by creating parser with caseInsensitive=true
	// For now, just use the regular include implementation
	return p.compileIncludeImpl(ccnt, expr, true)
}

func (p *CompileTimeContinuation) compileLambdaParameterList(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) (*NativeTemplate, *environment.EnvironmentFrame, error) {
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	tpl := NewNativeTemplate(0, 0, false)
	tail, err := syntax.SyntaxForEach(expr, func(i int, hasNext bool, v syntax.SyntaxValue) error {
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
	if values.IsEmptyList(tail) {
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

func (p *CompileTimeContinuation) compileProcedureArgumentList(ccnt CompileTimeCallContext, args *syntax.SyntaxPair) error {
	tail, err := syntax.SyntaxForEach(args, func(i int, hasNext bool, v syntax.SyntaxValue) error {
		err := p.CompileExpression(ccnt.NotInTail(), v)
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

func (p *CompileTimeContinuation) compileExpressionList(ccnt CompileTimeCallContext, expr *syntax.SyntaxPair) error {
	if !expr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list of expressions, got %T", expr)
	}
	tail, err := syntax.SyntaxForEach(expr, func(i int, hasNext bool, v syntax.SyntaxValue) error {
		ccnt0 := ccnt
		if hasNext {
			ccnt0 = ccnt.NotInTail()
		}
		err := p.CompileExpression(ccnt0, v)
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
// Tail Call Optimization: When ccnt.inTail is true, we skip SaveContinuation.
// This allows the called function's RestoreContinuation to return directly
// to our caller's continuation, implementing proper tail call optimization
// per R7RS Section 3.5.
func (p *CompileTimeContinuation) CompileProcedureCall(ccnt CompileTimeCallContext, initial syntax.SyntaxValue, expr syntax.SyntaxValue) error {
	var operationSaveContinuationIndex int
	if !ccnt.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.template.operations.Length()
		p.AppendOperations(
			NewOperationSaveContinuationOffsetImmediate(0),
		)
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	err := p.CompileExpression(ccnt.NotInTail(), initial)
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression")
	}
	p.AppendOperations(
		NewOperationPush(),
	)
	// compile as a procedure call
	err = p.compileProcedureArgumentList(ccnt, expr.(*syntax.SyntaxPair))
	if err != nil {
		return values.WrapForeignErrorf(err, "failed to compile expression list")
	}
	p.AppendOperations(
		NewOperationPull(),
		NewOperationApply(),
	)

	if !ccnt.inTail {
		// Patch the SaveContinuation offset for non-tail calls
		l := p.template.operations.Length()
		p.template.operations[operationSaveContinuationIndex] = NewOperationSaveContinuationOffsetImmediate(l - operationSaveContinuationIndex)
	}
	return nil
}

// CompilePrimitiveOrProcedureCall compiles either a primitive or a procedure call.
// It first checks if the initial element is a syntax symbol that corresponds to a primitive.
// If so, it compiles the primitive. If not, it treats it as a procedure call.
func (p *CompileTimeContinuation) CompilePrimitiveOrProcedureCall(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	stx0pr, ok := expr.Unwrap().(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", expr)
	}
	initial, ok := stx0pr.Car().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "expected a syntax object for procedure call, got %T", stx0pr.Car())
	}
	stx1cdr, ok := stx0pr.Cdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", stx1cdr)
	}
	stx0pr, ok = stx1cdr.Unwrap().(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair for procedure call, got %T", stx1cdr.Unwrap())
	}
	switch v := initial.(type) {
	case *syntax.SyntaxSymbol:
		ok, err := p.CompileSyntaxPrimitive(ccnt, v, stx1cdr)
		if !ok {
			return p.CompileProcedureCall(ccnt, v, stx1cdr)
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "failed to compile primitive or call")
		}
	case *syntax.SyntaxPair:
		err := p.CompileProcedureCall(ccnt, v, stx1cdr)
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
func (p *CompileTimeContinuation) compileQuasiquoteDatum(ccnt CompileTimeCallContext, datum syntax.SyntaxValue, depth int) error {
	switch d := datum.(type) {
	case *syntax.SyntaxPair:
		return p.compileQuasiquotePair(ccnt, d, depth)
	default:
		// Self-evaluating or symbol - just quote it as a literal
		li := p.template.MaybeAppendLiteral(datum.Unwrap())
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}
}

// compileQuasiquotePair handles pairs in quasiquote context.
//
// This is the core dispatch function that detects special keywords (unquote,
// unquote-splicing, quasiquote) and routes to the appropriate handler based
// on the current depth.
//
// IMPORTANT: For nested forms, we extract the body/argument and process it
// at the adjusted depth, rather than recursing on the entire form. This
// prevents infinite recursion.
func (p *CompileTimeContinuation) compileQuasiquotePair(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair, depth int) error {
	car := pair.Car()
	if carSym, ok := p.getSymbolName(car); ok {
		switch carSym {
		case "unquote":
			if depth == 1 {
				// At depth 1, unquote triggers evaluation
				return p.compileUnquoteExpr(ccnt, pair)
			}
			// Nested unquote at depth > 1: extract argument, process at depth-1
			// Example: for `(a `(b ,,x) c), the inner ,x is processed at depth 1
			if pair.Length() == 2 {
				cdr := pair.Cdr().(*syntax.SyntaxPair)
				arg := cdr.Car().(syntax.SyntaxValue)
				return p.compileQuasiquoteNestedUnquote(ccnt, arg, depth-1, "unquote")
			}
			return p.compileQuasiquoteListElements(ccnt, pair, depth-1)

		case "unquote-splicing":
			if depth == 1 {
				// unquote-splicing at depth 1 is only valid within a list context
				return values.NewForeignError("unquote-splicing: not in list context")
			}
			// Nested unquote-splicing at depth > 1: same pattern as unquote
			if pair.Length() == 2 {
				cdr := pair.Cdr().(*syntax.SyntaxPair)
				arg := cdr.Car().(syntax.SyntaxValue)
				return p.compileQuasiquoteNestedUnquote(ccnt, arg, depth-1, "unquote-splicing")
			}
			return p.compileQuasiquoteListElements(ccnt, pair, depth-1)

		case "quasiquote":
			// Nested quasiquote: extract body, process at depth+1
			// Example: for `(a `(b ,x) c), (b ,x) is processed at depth 2
			if pair.Length() == 2 {
				cdr := pair.Cdr().(*syntax.SyntaxPair)
				body := cdr.Car().(syntax.SyntaxValue)
				return p.compileQuasiquoteNestedQuasiquote(ccnt, body, depth+1)
			}
			return p.compileQuasiquoteListElements(ccnt, pair, depth+1)
		}
	}

	// Regular list - check for unquote-splicing in elements
	return p.compileQuasiquoteListElements(ccnt, pair, depth)
}

// compileUnquoteExpr compiles (unquote expr) at depth 1, which means we
// actually evaluate the expression. This is the "base case" for unquote.
func (p *CompileTimeContinuation) compileUnquoteExpr(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair) error {
	if pair.Length() != 2 {
		return values.NewForeignError("unquote: expected exactly one argument")
	}
	cdr := pair.Cdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return values.NewForeignError("unquote: malformed syntax")
	}
	expr, ok := cdrPair.Car().(syntax.SyntaxValue)
	if !ok {
		return values.NewForeignError("unquote: expected syntax value")
	}
	return p.CompileExpression(ccnt, expr)
}

// compileQuasiquoteNestedUnquote handles nested unquote/unquote-splicing at depth > 1.
//
// When we encounter (unquote arg) or (unquote-splicing arg) at depth > 1, we:
//  1. Process the argument at newDepth (which is depth-1)
//  2. Wrap the result in (list 'unquote <processed>) or (list 'unquote-splicing <processed>)
//
// Example: For `(a `(b ,,x) c) with x=5:
//   - Outer quasiquote starts at depth 1
//   - Inner quasiquote increases to depth 2
//   - First unquote (the outer ,) decreases to depth 1
//   - Second unquote (the inner ,) is processed at depth 1, evaluating x to 5
//   - Result: (a (quasiquote (b (unquote 5))) c)
func (p *CompileTimeContinuation) compileQuasiquoteNestedUnquote(ccnt CompileTimeCallContext, arg syntax.SyntaxValue, newDepth int, keyword string) error {
	srcCtx := arg.SourceContext()

	// Helper to build a syntax list
	buildSyntaxList := func(elems []syntax.SyntaxValue) syntax.SyntaxValue {
		var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
		for i := len(elems) - 1; i >= 0; i-- {
			result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
		}
		return result
	}

	// Check if the argument needs runtime evaluation at the new depth
	if p.quasiquoteNeedsRuntime(arg, newDepth) {
		// The argument contains unquotes that need evaluation at newDepth.
		// Generate (keyword arg) as syntax and process via compileQuasiquoteComplexListFromElements.
		return p.compileQuasiquoteComplexListFromElements(ccnt,
			syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol(keyword, srcCtx),
				syntax.NewSyntaxCons(arg, syntax.NewSyntaxEmptyList(srcCtx), srcCtx),
				srcCtx),
			newDepth+1) // +1 because we're wrapping in unquote
	}

	// Argument doesn't need runtime evaluation - emit as compile-time literal
	keywordSym := syntax.NewSyntaxSymbol(keyword, srcCtx)
	wrapped := buildSyntaxList([]syntax.SyntaxValue{keywordSym, arg})
	li := p.template.MaybeAppendLiteral(wrapped.UnwrapAll())
	p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
	return nil
}

// compileQuasiquoteNestedQuasiquote handles nested quasiquote at any depth.
//
// When we encounter (quasiquote body) inside another quasiquote, we:
//  1. Process the body at newDepth (which is depth+1)
//  2. Wrap the result in (list 'quasiquote <processed>)
//
// Example: For `(a `(b ,x) c):
//   - Outer quasiquote starts at depth 1
//   - Inner quasiquote increases to depth 2
//   - The ,x at depth 2 does NOT evaluate (would need ,,x to reach depth 0)
//   - Result: (a (quasiquote (b (unquote x))) c)
func (p *CompileTimeContinuation) compileQuasiquoteNestedQuasiquote(ccnt CompileTimeCallContext, body syntax.SyntaxValue, newDepth int) error {
	srcCtx := body.SourceContext()

	// Helper to build a syntax list
	buildSyntaxList := func(elems []syntax.SyntaxValue) syntax.SyntaxValue {
		var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
		for i := len(elems) - 1; i >= 0; i-- {
			result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
		}
		return result
	}

	// Check if the body needs runtime evaluation at the new depth
	if p.quasiquoteNeedsRuntime(body, newDepth) {
		// Generate (list 'quasiquote <processed-body>) and compile it
		qqSym := syntax.NewSyntaxSymbol("quasiquote", srcCtx)
		wrapped := syntax.NewSyntaxCons(
			qqSym,
			syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(srcCtx), srcCtx),
			srcCtx)
		return p.compileQuasiquoteComplexListFromElements(ccnt, wrapped, newDepth-1) // -1 because we're inside quasiquote
	}

	// Body doesn't need runtime - quote the whole thing as literal
	qqSym := syntax.NewSyntaxSymbol("quasiquote", srcCtx)
	wrapped := buildSyntaxList([]syntax.SyntaxValue{qqSym, body})
	li := p.template.MaybeAppendLiteral(wrapped.UnwrapAll())
	p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
	return nil
}

// compileQuasiquoteListElements compiles a list with potential unquote-splicing
func (p *CompileTimeContinuation) compileQuasiquoteListElements(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair, depth int) error {
	// Collect segments: either quoted literals or expressions to evaluate
	// A segment can be:
	// - A literal element (quote it)
	// - An unquote expression (evaluate it, wrap in list)
	// - An unquote-splicing expression (evaluate it, append directly)

	type segment struct {
		isSplice bool
		// For literals, we accumulate them
		literals []values.Value
		// For expressions, we compile them
		isExpr bool
	}

	var segments []segment
	var currentLiterals []values.Value

	// Helper to flush literals
	flushLiterals := func() {
		if len(currentLiterals) > 0 {
			segments = append(segments, segment{literals: currentLiterals})
			currentLiterals = nil
		}
	}

	// Walk the list
	current := pair
	for {
		if values.IsEmptyList(current) {
			break
		}

		car := current.Car()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if !ok {
			// Not a syntax value, treat as literal
			currentLiterals = append(currentLiterals, car)
		} else if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
			// Check if it's (unquote x) or (unquote-splicing x)
			if carSymName, ok := p.getSymbolName(carPair.Car()); ok {
				if carSymName == "unquote" && depth == 1 {
					flushLiterals()
					segments = append(segments, segment{isExpr: true, isSplice: false})
					// Compile the unquote expression - will be handled below
				} else if carSymName == "unquote-splicing" && depth == 1 {
					flushLiterals()
					segments = append(segments, segment{isExpr: true, isSplice: true})
				} else {
					// Nested or other form - recursively process
					currentLiterals = append(currentLiterals, nil) // placeholder
				}
			} else {
				currentLiterals = append(currentLiterals, nil) // placeholder
			}
		} else {
			currentLiterals = append(currentLiterals, carSyntax.Unwrap())
		}

		// Move to next element
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			// Improper list - handle the tail
			currentLiterals = append(currentLiterals, cdr)
			break
		}
		current = nextPair
	}

	flushLiterals()

	// For now, use a simpler approach: compile the whole thing recursively
	// and build with cons/list/append at runtime

	// Simple case: no unquote/unquote-splicing at this level
	hasUnquote := false
	hasSplice := false

	current = pair
	for !values.IsEmptyList(current) {
		car := current.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
				if carSymName, ok := p.getSymbolName(carPair.Car()); ok {
					if carSymName == "unquote" && depth == 1 {
						hasUnquote = true
					} else if carSymName == "unquote-splicing" && depth == 1 {
						hasSplice = true
					}
				}
			}
		}
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			break
		}
	}

	if !hasUnquote && !hasSplice {
		// No unquoting at this level - recursively process each element and build list
		return p.compileQuasiquoteSimpleList(ccnt, pair, depth)
	}

	// Has unquote or splice - need to use list/append
	return p.compileQuasiquoteComplexList(ccnt, pair, depth)
}

// compileQuasiquoteSimpleList compiles a list with no unquote/splice at current level
// Each element may still contain nested unquotes, so we process recursively.
func (p *CompileTimeContinuation) compileQuasiquoteSimpleList(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair, depth int) error {
	// Check if we need any runtime evaluation by recursively checking for unquotes
	needsRuntime := p.quasiquoteNeedsRuntime(pair, depth)

	if !needsRuntime {
		// Pure literal - emit as compile-time constant
		li := p.template.MaybeAppendLiteral(pair.UnwrapAll())
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Some nested element needs runtime evaluation
	// Build the list using (list elem1 elem2 ...) at runtime
	// Each element is recursively processed through quasiquote
	srcCtx := pair.SourceContext()

	// Helper to build a syntax list
	buildSyntaxList := func(elems []syntax.SyntaxValue) syntax.SyntaxValue {
		var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
		for i := len(elems) - 1; i >= 0; i-- {
			result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
		}
		return result
	}

	// Collect elements, recursively processing each through quasiquote
	var listArgs []syntax.SyntaxValue
	listArgs = append(listArgs, syntax.NewSyntaxSymbol("list", srcCtx))

	current := pair
	for !values.IsEmptyList(current) {
		car := current.Car()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if !ok {
			// Not syntax, quote it
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			litStx := syntax.NewSyntaxObject(car, srcCtx)
			listArgs = append(listArgs, buildSyntaxList([]syntax.SyntaxValue{quoteSym, litStx}))
		} else {
			// Wrap in quasiquote processing
			// We generate (qq-expand elem depth) where qq-expand is our recursive processor
			// But actually, we just need to check if this element has unquotes
			if p.quasiquoteNeedsRuntime(carSyntax, depth) {
				// This element needs runtime processing - emit a recursive quasiquote call
				// by directly compiling it as a quasiquote datum and wrapping it
				// Actually, we need to generate code that processes this element.
				// The cleanest way is to call compileQuasiquoteDatum inline.
				// But that emits to the current template, not builds syntax.
				// Let's use a different approach: generate the equivalent Scheme code.

				// For a pair element, wrap it in a nested (quasiquote ...) at compile time
				// so the recursive call handles it. But this would double-nest the quasiquote!
				// Actually, we're already inside a quasiquote context.
				// We need to inline the quasiquote processing here.

				// Simplest: just call the complex list compiler which handles this properly
				return p.compileQuasiquoteComplexListFromElements(ccnt, pair, depth)
			} else {
				// Pure literal element, quote it
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				listArgs = append(listArgs, buildSyntaxList([]syntax.SyntaxValue{quoteSym, carSyntax}))
			}
		}

		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			// Improper list - handle tail
			break
		}
		current = nextPair
	}

	// Build and compile (list arg1 arg2 ...)
	stx := buildSyntaxList(listArgs)
	return p.CompileExpression(ccnt, stx)
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
		if values.IsEmptyList(v) {
			return false
		}
		// Check if this is (unquote ...) or (unquote-splicing ...) at depth 1
		if carSymName, ok := p.getSymbolName(v.Car()); ok {
			switch carSymName {
			case "unquote", "unquote-splicing":
				if depth == 1 {
					return true
				}
				// Nested unquote at depth > 1 - check if the argument needs runtime
				// For ,,x at depth 2: the inner ,x is at depth 1 and needs eval
				if v.Length() == 2 {
					cdr := v.Cdr().(*syntax.SyntaxPair)
					arg := cdr.Car()
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
	for !values.IsEmptyList(current) {
		car := current.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			if p.quasiquoteNeedsRuntime(carSyntax, depth) {
				return true
			}
		}
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
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

// compileQuasiquoteComplexListFromElements handles lists where some elements need
// runtime evaluation. It generates a (list ...) expression where each element
// is processed through quasiquote transformation.
//
// The key helper here is genQuasiquoteExpr which transforms a syntax value at a
// given depth into the equivalent Scheme expression that will produce the correct
// result at runtime.
func (p *CompileTimeContinuation) compileQuasiquoteComplexListFromElements(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair, depth int) error {
	srcCtx := pair.SourceContext()

	// Helper to build a syntax list from elements
	buildSyntaxList := func(elems []syntax.SyntaxValue) syntax.SyntaxValue {
		var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
		for i := len(elems) - 1; i >= 0; i-- {
			result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
		}
		return result
	}

	// Helper to wrap a datum value in syntax (for quoting non-syntax values)
	var wrapDatum func(v values.Value) syntax.SyntaxValue
	wrapDatum = func(v values.Value) syntax.SyntaxValue {
		if values.IsEmptyList(v) {
			return syntax.NewSyntaxEmptyList(srcCtx)
		}
		switch val := v.(type) {
		case *values.Symbol:
			return syntax.NewSyntaxSymbol(val.Key, srcCtx)
		case *values.Pair:
			car := wrapDatum(val.Car())
			cdr := wrapDatum(val.Cdr())
			return syntax.NewSyntaxCons(car, cdr, srcCtx)
		default:
			return syntax.NewSyntaxObject(v, srcCtx)
		}
	}

	// genQuasiquoteExpr transforms a syntax value at depth d into the Scheme
	// expression that produces the quasiquote result.
	//
	// Key behaviors:
	//   - unquote at d=1: return the expression directly (evaluate it)
	//   - unquote at d>1: process arg at d-1, wrap in (list 'unquote <result>)
	//   - quasiquote: process body at d+1, wrap in (list 'quasiquote <result>)
	//   - lists: generate (list <processed-elem1> <processed-elem2> ...)
	//   - atoms: quote them
	//
	// IMPORTANT: For nested unquote/quasiquote, we extract the body and process
	// it at the adjusted depth. We do NOT recurse on the entire form, which
	// would cause infinite recursion.
	var genQuasiquoteExpr func(stx syntax.SyntaxValue, d int) syntax.SyntaxValue
	genQuasiquoteExpr = func(stx syntax.SyntaxValue, d int) syntax.SyntaxValue {
		switch v := stx.(type) {
		case *syntax.SyntaxPair:
			if values.IsEmptyList(v) {
				// Empty list - quote it
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return buildSyntaxList([]syntax.SyntaxValue{quoteSym, v})
			}
			// Check if this is (unquote ...) or (unquote-splicing ...)
			if carSymName, ok := p.getSymbolName(v.Car()); ok {
				switch carSymName {
				case "unquote":
					if d == 1 {
						// Return the unquoted expression directly
						if v.Length() == 2 {
							cdr := v.Cdr().(*syntax.SyntaxPair)
							return cdr.Car().(syntax.SyntaxValue)
						}
					}
					// Nested unquote at depth > 1 - process the argument at d-1
					// For `(a `(b ,,x) c)`: the inner `,x` should be evaluated
					// and wrapped back in (list 'unquote <evaluated>)
					if v.Length() == 2 {
						cdr := v.Cdr().(*syntax.SyntaxPair)
						arg := cdr.Car().(syntax.SyntaxValue)
						processedArg := genQuasiquoteExpr(arg, d-1)
						unquoteSym := syntax.NewSyntaxSymbol("unquote", srcCtx)
						return buildSyntaxList([]syntax.SyntaxValue{
							syntax.NewSyntaxSymbol("list", srcCtx),
							buildSyntaxList([]syntax.SyntaxValue{
								syntax.NewSyntaxSymbol("quote", srcCtx),
								unquoteSym,
							}),
							processedArg,
						})
					}
					// Malformed, just quote it
					quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
					return buildSyntaxList([]syntax.SyntaxValue{quoteSym, v})

				case "unquote-splicing":
					if d > 1 {
						// Nested unquote-splicing - process the argument at d-1
						if v.Length() == 2 {
							cdr := v.Cdr().(*syntax.SyntaxPair)
							arg := cdr.Car().(syntax.SyntaxValue)
							processedArg := genQuasiquoteExpr(arg, d-1)
							usSym := syntax.NewSyntaxSymbol("unquote-splicing", srcCtx)
							return buildSyntaxList([]syntax.SyntaxValue{
								syntax.NewSyntaxSymbol("list", srcCtx),
								buildSyntaxList([]syntax.SyntaxValue{
									syntax.NewSyntaxSymbol("quote", srcCtx),
									usSym,
								}),
								processedArg,
							})
						}
					}
					// At depth 1 or malformed - quote it
					quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
					return buildSyntaxList([]syntax.SyntaxValue{quoteSym, v})

				case "quasiquote":
					// Nested quasiquote - process the body at depth+1
					// For `(a `(b ,x) c): process (b ,x) at depth+1, wrap result in (list 'quasiquote ...)
					if v.Length() == 2 {
						cdr := v.Cdr().(*syntax.SyntaxPair)
						body := cdr.Car().(syntax.SyntaxValue)
						processedBody := genQuasiquoteExpr(body, d+1)
						qqSym := syntax.NewSyntaxSymbol("quasiquote", srcCtx)
						return buildSyntaxList([]syntax.SyntaxValue{
							syntax.NewSyntaxSymbol("list", srcCtx),
							buildSyntaxList([]syntax.SyntaxValue{
								syntax.NewSyntaxSymbol("quote", srcCtx),
								qqSym,
							}),
							processedBody,
						})
					}
					// Malformed - quote it
					quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
					return buildSyntaxList([]syntax.SyntaxValue{quoteSym, v})
				}
			}

			// Regular list - build (list elem1 elem2 ...)
			var elems []syntax.SyntaxValue
			elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))
			current := v
			for !values.IsEmptyList(current) {
				car := current.Car()
				if carSyntax, ok := car.(syntax.SyntaxValue); ok {
					elems = append(elems, genQuasiquoteExpr(carSyntax, d))
				} else {
					// Non-syntax, quote it
					quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
					litStx := wrapDatum(car)
					elems = append(elems, buildSyntaxList([]syntax.SyntaxValue{quoteSym, litStx}))
				}
				cdr := current.Cdr()
				if values.IsEmptyList(cdr) {
					break
				}
				nextPair, ok := cdr.(*syntax.SyntaxPair)
				if !ok {
					break
				}
				current = nextPair
			}
			return buildSyntaxList(elems)

		case *syntax.SyntaxSymbol:
			// Symbol - quote it
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			return buildSyntaxList([]syntax.SyntaxValue{quoteSym, v})

		default:
			// Other values - quote them
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			return buildSyntaxList([]syntax.SyntaxValue{quoteSym, stx})
		}
	}

	// Generate (list elem1 elem2 ...) for the pair
	stx := genQuasiquoteExpr(pair, depth)
	return p.CompileExpression(ccnt, stx)
}

// compileQuasiquoteComplexList compiles a list with unquote/splice
func (p *CompileTimeContinuation) compileQuasiquoteComplexList(ccnt CompileTimeCallContext, pair *syntax.SyntaxPair, depth int) error {
	// Strategy: build segments and use append
	// Each segment is either:
	// - A list of quoted literals: (list 'a 'b 'c)
	// - An unquoted expression wrapped in list: (list ,x)
	// - A spliced expression: ,@x
	// Then: (append seg1 seg2 seg3 ...)

	type segmentType int
	const (
		segLiterals segmentType = iota
		segUnquote
		segSplice
	)

	type seg struct {
		typ      segmentType
		literals []values.Value
		expr     syntax.SyntaxValue
	}

	var segments []seg
	var currentLiterals []values.Value

	flushLiterals := func() {
		if len(currentLiterals) > 0 {
			segments = append(segments, seg{typ: segLiterals, literals: currentLiterals})
			currentLiterals = nil
		}
	}

	current := pair
	for !values.IsEmptyList(current) {
		car := current.Car()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if !ok {
			currentLiterals = append(currentLiterals, car)
			goto next
		}

		if carPair, ok := carSyntax.(*syntax.SyntaxPair); ok {
			if carSymName, ok := p.getSymbolName(carPair.Car()); ok {
				if carSymName == "unquote" && depth == 1 {
					flushLiterals()
					// Get the expression
					if carPair.Length() != 2 {
						return values.NewForeignError("unquote: expected exactly one argument")
					}
					cdrPair := carPair.Cdr().(*syntax.SyntaxPair)
					expr := cdrPair.Car().(syntax.SyntaxValue)
					segments = append(segments, seg{typ: segUnquote, expr: expr})
					goto next
				} else if carSymName == "unquote-splicing" && depth == 1 {
					flushLiterals()
					if carPair.Length() != 2 {
						return values.NewForeignError("unquote-splicing: expected exactly one argument")
					}
					cdrPair := carPair.Cdr().(*syntax.SyntaxPair)
					expr := cdrPair.Car().(syntax.SyntaxValue)
					segments = append(segments, seg{typ: segSplice, expr: expr})
					goto next
				}
			}
		}

		// Regular element - need to recursively process it
		// For simplicity, just unwrap it for now
		currentLiterals = append(currentLiterals, carSyntax.Unwrap())

	next:
		cdr := current.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		if nextPair, ok := cdr.(*syntax.SyntaxPair); ok {
			current = nextPair
		} else {
			// Improper list - add tail
			currentLiterals = append(currentLiterals, cdr)
			break
		}
	}

	flushLiterals()

	if len(segments) == 0 {
		li := p.template.MaybeAppendLiteral(values.EmptyList)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// If only 1 segment, compile it directly without append
	if len(segments) == 1 {
		s := segments[0]
		switch s.typ {
		case segLiterals:
			var lst values.Value = values.EmptyList
			for j := len(s.literals) - 1; j >= 0; j-- {
				lst = values.NewCons(s.literals[j], lst)
			}
			li := p.template.MaybeAppendLiteral(lst)
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))

		case segUnquote:
			// Compile expression and wrap in list: (list expr)
			// Must use SaveContinuation to provide return point for RestoreContinuation in list
			listSym := p.env.InternSymbol(values.NewSymbol("list"))
			listIdx := p.template.MaybeAppendLiteral(p.env.GetGlobalIndex(listSym))

			// SaveContinuation - offset will be patched
			saveContinuationIdx := p.template.operations.Length()
			p.AppendOperations(NewOperationSaveContinuationOffsetImmediate(0))

			p.AppendOperations(NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(listIdx))
			p.AppendOperations(NewOperationPush())
			if err := p.CompileExpression(ccnt.NotInTail(), s.expr); err != nil {
				return err
			}
			p.AppendOperations(NewOperationPush())
			p.AppendOperations(NewOperationPull())
			p.AppendOperations(NewOperationApply())

			// Patch SaveContinuation offset
			currentIdx := p.template.operations.Length()
			p.template.operations[saveContinuationIdx] = NewOperationSaveContinuationOffsetImmediate(currentIdx - saveContinuationIdx)

		case segSplice:
			// Just compile the expression
			if err := p.CompileExpression(ccnt, s.expr); err != nil {
				return err
			}
		}
		return nil
	}

	// Multiple segments: use append
	// Strategy: compile each segment, push result, then call append
	// For segUnquote we need to call (list expr), so we use SaveContinuation/RestoreContinuation
	// to call list as a subexpression and continue building the append args.
	//
	// Actually, simpler: we can use the calling convention properly.
	// For each segment:
	//   - segLiterals: load literal list, push
	//   - segUnquote: call (list expr) - result in value, push
	//   - segSplice: compile expr - result in value, push
	// Then load append, swap with args somehow...
	//
	// Problem: Push puts value on stack, but we need function at bottom.
	// Solution: Push all segment results first, then load append and apply.
	// But Apply pops function from value register, args from stack.
	// So: push all segment results, load append into value, apply.

	// Compile each segment, push result onto stack
	for _, s := range segments {
		switch s.typ {
		case segLiterals:
			var lst values.Value = values.EmptyList
			for j := len(s.literals) - 1; j >= 0; j-- {
				lst = values.NewCons(s.literals[j], lst)
			}
			li := p.template.MaybeAppendLiteral(lst)
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
			p.AppendOperations(NewOperationPush())

		case segUnquote:
			// Need to call (cons expr '()) to wrap the expression result in a list
			// Use SaveContinuation to properly isolate this call from the outer eval stack
			consSym := p.env.InternSymbol(values.NewSymbol("cons"))
			consIdx := p.template.MaybeAppendLiteral(p.env.GetGlobalIndex(consSym))
			emptyLi := p.template.MaybeAppendLiteral(values.EmptyList)

			// SaveContinuation creates a fresh eval stack for the nested call
			// The offset will be patched to jump past the Apply
			saveContinuationIdx := p.template.operations.Length()
			p.AppendOperations(NewOperationSaveContinuationOffsetImmediate(0)) // placeholder

			// Load cons and push
			p.AppendOperations(NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(consIdx))
			p.AppendOperations(NewOperationPush())

			// Compile expr and push
			if err := p.CompileExpression(ccnt.NotInTail(), s.expr); err != nil {
				return err
			}
			p.AppendOperations(NewOperationPush())

			// Push empty list
			p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(emptyLi))
			p.AppendOperations(NewOperationPush())

			// Pull cons and apply
			p.AppendOperations(NewOperationPull())
			p.AppendOperations(NewOperationApply())

			// Patch SaveContinuation offset to point here (after Apply)
			currentIdx := p.template.operations.Length()
			p.template.operations[saveContinuationIdx] = NewOperationSaveContinuationOffsetImmediate(currentIdx - saveContinuationIdx)

			// Now result is in value register (after RestoreContinuation from cons)
			// Push it for append
			p.AppendOperations(NewOperationPush())

		case segSplice:
			if err := p.CompileExpression(ccnt, s.expr); err != nil {
				return err
			}
			p.AppendOperations(NewOperationPush())
		}
	}

	// Transform quasiquote to equivalent (list ...) or (append ...) call at compile time.
	// This is the simplest approach - let the normal compiler handle the function calls.
	//
	// For `(a ,x c)` without splicing: generate (list 'a x 'c)
	// For `(a ,@xs c)` with splicing: generate (append (list 'a) xs (list 'c))

	hasSplice := false
	for _, s := range segments {
		if s.typ == segSplice {
			hasSplice = true
			break
		}
	}

	// Build the syntax tree for the equivalent expression
	var stx syntax.SyntaxValue
	srcCtx := pair.SourceContext()

	// Helper to build a syntax list from syntax values
	buildSyntaxList := func(elems []syntax.SyntaxValue) syntax.SyntaxValue {
		var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(srcCtx)
		for i := len(elems) - 1; i >= 0; i-- {
			result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
		}
		return result
	}

	// Helper to wrap a datum value in syntax (declared first for recursion)
	var wrapDatum func(v values.Value) syntax.SyntaxValue
	wrapDatum = func(v values.Value) syntax.SyntaxValue {
		// Check for empty list first
		if values.IsEmptyList(v) {
			return syntax.NewSyntaxEmptyList(srcCtx)
		}
		switch val := v.(type) {
		case *values.Symbol:
			return syntax.NewSyntaxSymbol(val.Key, srcCtx)
		case *values.Pair:
			// Recursively wrap pair elements
			car := wrapDatum(val.Car())
			cdr := wrapDatum(val.Cdr())
			return syntax.NewSyntaxCons(car, cdr, srcCtx)
		default:
			// For other values (numbers, strings, etc.), use SyntaxObject
			return syntax.NewSyntaxObject(v, srcCtx)
		}
	}

	// Helper to build (quote datum)
	quoteExpr := func(v values.Value) syntax.SyntaxValue {
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		litStx := wrapDatum(v)
		return buildSyntaxList([]syntax.SyntaxValue{quoteSym, litStx})
	}

	if !hasSplice {
		// No splicing - use (list elem1 elem2 ...)
		// Build: (list 'a x 'c) for `(a ,x c)
		listArgs := []syntax.SyntaxValue{}
		for _, s := range segments {
			switch s.typ {
			case segLiterals:
				for _, lit := range s.literals {
					// Quote each literal: 'a becomes (quote a)
					listArgs = append(listArgs, quoteExpr(lit))
				}
			case segUnquote:
				// Use the expression directly
				listArgs = append(listArgs, s.expr)
			}
		}
		// Build (list arg1 arg2 ...)
		listSym := syntax.NewSyntaxSymbol("list", srcCtx)
		allArgs := append([]syntax.SyntaxValue{listSym}, listArgs...)
		stx = buildSyntaxList(allArgs)

	} else {
		// Has splicing - use (append seg1 seg2 ...)
		// Each segment becomes either (list ...) for literals/unquote, or just the expr for splice
		appendArgs := []syntax.SyntaxValue{}
		for _, s := range segments {
			switch s.typ {
			case segLiterals:
				// Build (list 'a 'b ...) for this segment's literals
				listArgs := []syntax.SyntaxValue{}
				for _, lit := range s.literals {
					listArgs = append(listArgs, quoteExpr(lit))
				}
				listSym := syntax.NewSyntaxSymbol("list", srcCtx)
				allArgs := append([]syntax.SyntaxValue{listSym}, listArgs...)
				appendArgs = append(appendArgs, buildSyntaxList(allArgs))

			case segUnquote:
				// Build (list expr) to wrap single element in a list
				listSym := syntax.NewSyntaxSymbol("list", srcCtx)
				appendArgs = append(appendArgs, buildSyntaxList([]syntax.SyntaxValue{listSym, s.expr}))

			case segSplice:
				// Use the expression directly (it should evaluate to a list)
				appendArgs = append(appendArgs, s.expr)
			}
		}
		// Build (append seg1 seg2 ...)
		appendSym := syntax.NewSyntaxSymbol("append", srcCtx)
		allArgs := append([]syntax.SyntaxValue{appendSym}, appendArgs...)
		stx = buildSyntaxList(allArgs)
	}

	// Compile the generated syntax
	return p.CompileExpression(ccnt, stx)
}

// getSymbolName returns the symbol name if the value is a symbol
func (p *CompileTimeContinuation) getSymbolName(v interface{}) (string, bool) {
	switch s := v.(type) {
	case *syntax.SyntaxSymbol:
		if sym, ok := s.Unwrap().(*values.Symbol); ok {
			return sym.Key, true
		}
	case *syntax.SyntaxObject:
		if sym, ok := s.Unwrap().(*values.Symbol); ok {
			return sym.Key, true
		}
	case *values.Symbol:
		return s.Key, true
	}
	return "", false
}

// CompileUnquote errors - unquote outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquote(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unquote: not in quasiquote context")
}

// CompileUnquoteSplicing errors - unquote-splicing outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquoteSplicing(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return values.NewForeignError("unquote-splicing: not in quasiquote context")
}

func (p *CompileTimeContinuation) CompileExpression(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// Validate the expression first
	result := validate.ValidateExpression(expr)
	if !result.Ok() {
		return values.NewForeignError(result.Error())
	}
	// Compile the validated form
	return p.compileValidated(ccnt, result.Expr)
}

func (p *CompileTimeContinuation) CompileSelfEvaluating(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	if expr == nil {
		// Load void for nil expressions
		p.AppendOperations(
			NewOperationLoadVoid(),
		)
		return nil
	}
	li := p.template.MaybeAppendLiteral(expr.Unwrap())
	p.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(li),
	)
	return nil
}

func (p *CompileTimeContinuation) AppendOperations(ops ...Operation) {
	p.template.operations = append(p.template.operations, ops...)
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
func (p *CompileTimeContinuation) CompileDefineLibrary(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is ((lib-name) <declaration> ...) - args after 'define-library' keyword
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "define-library: expected library name and declarations")
	}

	// Parse library name: (lib-name) is a list of identifiers
	libNameExpr, ok := rest.Car().(syntax.SyntaxValue)
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
	declsExpr, ok := rest.Cdr().(syntax.SyntaxValue)
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
	_, err = syntax.SyntaxForEach(decls, func(i int, hasNext bool, decl syntax.SyntaxValue) error {
		return libCompiler.processLibraryDeclaration(ccnt, lib, decl)
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
func (p *CompileTimeContinuation) processLibraryDeclaration(ccnt CompileTimeCallContext, lib *CompiledLibrary, decl syntax.SyntaxValue) error {
	declPair, ok := decl.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "library declaration must be a list")
	}

	// Get the declaration keyword
	keywordExpr, ok := declPair.Car().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "library declaration must start with keyword")
	}

	keywordSym, ok := keywordExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "library declaration must start with symbol")
	}

	keyword := keywordSym.Unwrap().(*values.Symbol).Key

	// Get the rest of the declaration (arguments)
	argsExpr, ok := declPair.Cdr().(syntax.SyntaxValue)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "library declaration missing arguments")
	}

	switch keyword {
	case "export":
		return p.processLibraryExport(lib, argsExpr)
	case "import":
		return p.processLibraryImport(ccnt, lib, argsExpr)
	case "begin":
		// Compile the begin body in the library environment
		beginPair, ok := argsExpr.(*syntax.SyntaxPair)
		if !ok {
			if syntax.IsSyntaxEmptyList(argsExpr) {
				return nil // empty begin is valid
			}
			return values.WrapForeignErrorf(values.ErrNotAPair, "begin: expected list of expressions")
		}
		return p.compileExpressionList(ccnt, beginPair)
	case "include":
		return p.CompileInclude(ccnt, argsExpr)
	case "include-ci":
		return p.CompileIncludeCi(ccnt, argsExpr)
	case "include-library-declarations":
		return p.processIncludeLibraryDeclarations(ccnt, lib, argsExpr)
	case "cond-expand":
		return p.processCondExpand(ccnt, lib, argsExpr)
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

	_, err := syntax.SyntaxForEach(argsPair, func(i int, hasNext bool, spec syntax.SyntaxValue) error {
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
		carExpr, ok := s.Car().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "export: invalid spec")
		}

		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export: expected symbol")
		}

		if carSym.Unwrap().(*values.Symbol).Key == "rename" {
			// (rename internal external)
			cdrExpr, ok := s.Cdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected internal and external names")
			}

			internalExpr, ok := cdrExpr.Car().(syntax.SyntaxValue)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "export rename: expected internal name")
			}
			internalSym, ok := internalExpr.(*syntax.SyntaxSymbol)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "export rename: internal name must be symbol")
			}

			cdrCdr, ok := cdrExpr.Cdr().(*syntax.SyntaxPair)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAPair, "export rename: expected external name")
			}

			externalExpr, ok := cdrCdr.Car().(syntax.SyntaxValue)
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
func (p *CompileTimeContinuation) processLibraryImport(ccnt CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // empty import is valid
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	ctx := context.Background()
	_, err := syntax.SyntaxForEach(argsPair, func(i int, hasNext bool, importSetExpr syntax.SyntaxValue) error {
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
			_, _ = lib.Env.MaybeCreateGlobalBinding(localSym, importedBinding.BindingType())
			globalIdx := lib.Env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				if err := lib.Env.SetGlobalValue(globalIdx, importedBinding.Value()); err != nil {
					return values.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
				}
			}

			// If it's a syntax binding, also copy to expand phase
			if importedBinding.BindingType() == environment.BindingTypeSyntax {
				expandEnv := lib.Env.Expand()
				_, _ = expandEnv.MaybeCreateGlobalBinding(localSym, environment.BindingTypeSyntax)
				expandIdx := expandEnv.GetGlobalIndex(localSym)
				if expandIdx != nil {
					_ = expandEnv.SetGlobalValue(expandIdx, importedBinding.Value())
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
	carExpr, ok := pair.Car().(syntax.SyntaxValue)
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
	cdrExpr, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "only: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.Car().(syntax.SyntaxValue)
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
	cdrExpr, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "except: expected import-set and identifiers")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.Car().(syntax.SyntaxValue)
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
	nestedExpr, ok := cdrExpr.Car().(syntax.SyntaxValue)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxValue, "prefix: expected import-set")
	}
	importSet, err := parseImportSet(nestedExpr)
	if err != nil {
		return nil, err
	}

	// Get prefix
	prefixPair, ok := cdrExpr.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "prefix: expected prefix identifier")
	}

	prefixExpr, ok := prefixPair.Car().(syntax.SyntaxValue)
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
	cdrExpr, ok := pair.Cdr().(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected import-set and rename pairs")
	}

	// Get nested import set
	nestedExpr, ok := cdrExpr.Car().(syntax.SyntaxValue)
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

	_, err = syntax.SyntaxForEach(renamesPair, func(i int, hasNext bool, renamePairExpr syntax.SyntaxValue) error {
		renamePair, ok := renamePairExpr.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected (old new) pair")
		}

		oldExpr, ok := renamePair.Car().(syntax.SyntaxValue)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxValue, "rename: expected old name")
		}
		oldSym, ok := oldExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotASyntaxSymbol, "rename: old name must be symbol")
		}

		newPair, ok := renamePair.Cdr().(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "rename: expected new name")
		}

		newExpr, ok := newPair.Car().(syntax.SyntaxValue)
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
	_, err := syntax.SyntaxForEach(pair, func(i int, hasNext bool, idExpr syntax.SyntaxValue) error {
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
	_, err := syntax.SyntaxForEach(pair, func(i int, hasNext bool, partExpr syntax.SyntaxValue) error {
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
func (p *CompileTimeContinuation) CompileImport(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is (<import-set> ...) - args after 'import' keyword
	if syntax.IsSyntaxEmptyList(expr) {
		return nil // empty import is valid
	}

	importSets, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "import: expected list of import sets")
	}

	// Process each import set
	ctx := context.Background()
	_, err := syntax.SyntaxForEach(importSets, func(i int, hasNext bool, importSetExpr syntax.SyntaxValue) error {
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
			_, created := p.env.MaybeCreateGlobalBinding(localSym, libBinding.BindingType())
			if !created {
				// Binding already exists - update it
				// (This allows re-importing in REPL)
			}
			globalIdx := p.env.GetGlobalIndex(localSym)
			if globalIdx != nil {
				if err := p.env.SetGlobalValue(globalIdx, libBinding.Value()); err != nil {
					return values.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
				}
			}

			// If it's a syntax binding, also copy to expand phase
			if libBinding.BindingType() == environment.BindingTypeSyntax {
				expandEnv := p.env.Expand()
				_, _ = expandEnv.MaybeCreateGlobalBinding(localSym, environment.BindingTypeSyntax)
				expandIdx := expandEnv.GetGlobalIndex(localSym)
				if expandIdx != nil {
					_ = expandEnv.SetGlobalValue(expandIdx, libBinding.Value())
				}
			}
		}

		return nil
	})
	return err
}

// CompileExport handles top-level (export <export-spec> ...).
//
// This is only valid within a library definition. At top-level, it's an error.
func (p *CompileTimeContinuation) CompileExport(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
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
func (p *CompileTimeContinuation) CompileDefineSyntax(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
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
	keywordStx := argsPair.Car()
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

	transformerExpr := transformerPair.Car()
	if transformerExpr == nil {
		return values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: missing transformer expression")
	}

	// Check if transformer is a syntax-rules form
	if transformerPairExpr, ok := transformerExpr.(*syntax.SyntaxPair); ok {
		if car := transformerPairExpr.Car(); car != nil {
			if sym, ok := car.(*syntax.SyntaxSymbol); ok {
				if symVal := sym.Unwrap(); symVal != nil {
					if symbol, ok := symVal.(*values.Symbol); ok && symbol.Key == "syntax-rules" {
						// Compile syntax-rules directly
						closure, err := CompileSyntaxRules(p.env, transformerPairExpr)
						if err != nil {
							return values.WrapForeignErrorf(err, "could not compile syntax-rules transformer")
						}

						// Store the transformer in the expand phase environment with BindingTypeSyntax
						// R7RS requires syntax bindings to live in the expand phase, separate from runtime bindings
						expandEnv := p.env.Expand()
						globalIndex, created := expandEnv.MaybeCreateGlobalBinding(keyword, environment.BindingTypeSyntax)
						if !created {
							// Update existing binding
							globalIndex = expandEnv.GetGlobalIndex(keyword)
						}
						if globalIndex != nil {
							err = expandEnv.SetGlobalValue(globalIndex, closure)
							if err != nil {
								return err
							}
						}

						// define-syntax is compile-time only, emit no runtime operations
						return nil
					}
				}
			}
		}
	}

	// For non-syntax-rules transformers, we would need to compile and evaluate
	// the transformer expression at compile time. For now, just support syntax-rules
	return values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: only syntax-rules transformers are currently supported")
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
func (p *CompileTimeContinuation) CompileCondExpand(ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
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
	_, err := syntax.SyntaxForEach(argsPair, func(i int, hasNext bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr, ok := clausePair.Car().(syntax.SyntaxValue)
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
	_, err = syntax.SyntaxForEach(bodyPair, func(i int, hasNext bool, expr syntax.SyntaxValue) error {
		// Expand the expression
		ectx := NewExpandTimeCallContext()
		expanded, expandErr := NewExpanderTimeContinuation(p.env).ExpandExpression(ectx, expr)
		if expandErr != nil {
			return values.WrapForeignErrorf(expandErr, "cond-expand: error expanding body expression")
		}

		// Compile the expanded expression
		// Use the appropriate context for tail position (only last expression is in tail position)
		bodyCtx := ccnt
		if hasNext {
			bodyCtx = ccnt.NotInTail()
		}
		return p.CompileExpression(bodyCtx, expanded)
	})
	return err
}

// processCondExpand handles (cond-expand <clause> ...) within a library.
// Each clause is (<feature-requirement> <library-declaration> ...)
// The first clause whose feature requirement is satisfied has its declarations processed.
func (p *CompileTimeContinuation) processCondExpand(ccnt CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
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
	_, err := syntax.SyntaxForEach(argsPair, func(i int, hasNext bool, clause syntax.SyntaxValue) error {
		if matchedClause != nil {
			return nil // Already found a match
		}

		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "cond-expand: clause must be a list")
		}

		// Get the feature requirement (car of clause)
		reqExpr, ok := clausePair.Car().(syntax.SyntaxValue)
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
	_, err = syntax.SyntaxForEach(declsPair, func(i int, hasNext bool, decl syntax.SyntaxValue) error {
		return p.processLibraryDeclaration(ccnt, lib, decl)
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

		carExpr, ok := v.Car().(syntax.SyntaxValue)
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
			libNameExpr, ok := argsPair.Car().(syntax.SyntaxValue)
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
			reqExpr, ok := argsPair.Car().(syntax.SyntaxValue)
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
	_, err := syntax.SyntaxForEach(pair, func(i int, hasNext bool, v syntax.SyntaxValue) error {
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
func (p *CompileTimeContinuation) processIncludeLibraryDeclarations(ccnt CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return nil // Empty is valid (no-op)
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "include-library-declarations: expected list of filenames")
	}

	// Process each filename
	_, err := syntax.SyntaxForEach(argsPair, func(i int, hasNext bool, v syntax.SyntaxValue) error {
		fn, ok := v.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAString, "include-library-declarations: expected string filename")
		}

		// Find and open the file
		file, err := findFile(p, ccnt, fn.Value)
		if err != nil {
			return values.WrapForeignErrorf(err, "include-library-declarations: failed to find file %q", fn.Value)
		}
		if file == nil {
			return values.NewForeignErrorf("include-library-declarations: file not found: %q", fn.Value)
		}
		defer file.Close()

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParser(p.env, reader)

		// Read and process all forms from the file as library declarations
		for {
			stx, readErr := fileParser.ReadSyntax()
			if readErr != nil {
				if errors.Is(readErr, io.EOF) {
					break
				}
				return values.WrapForeignErrorf(readErr, "include-library-declarations: error reading %q", fn.Value)
			}

			// Process the form as a library declaration
			if err := p.processLibraryDeclaration(ccnt, lib, stx); err != nil {
				return values.WrapForeignErrorf(err, "include-library-declarations: error processing declaration from %q", fn.Value)
			}
		}

		return nil
	})
	return err
}
