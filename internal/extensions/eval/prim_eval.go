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

package eval

import (
	"bufio"
	"context"
	"errors"
	"io"
	"os"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimEval implements the (eval) primitive.
// Evaluates an expression in a given environment.
func PrimEval(mc *machine.MachineContext) error {
	expr := mc.Arg(0)
	envSpec := mc.Arg(1)

	// Get the environment frame from the TopLevelEnvironment
	topLevelEnv, ok := envSpec.(*environment.TopLevelEnvironment)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "eval: expected an environment specifier but got %T", envSpec)
	}

	env := topLevelEnv.Runtime()

	// Convert datum to syntax value
	sctx := syntax.NewZeroValueSourceContext()
	stx := schemeutil.DatumToSyntaxValue(mc.Context(), sctx, expr)

	// Expand the expression
	expanded, err := machine.NewExpanderTimeContinuation(mc.Context(), env).ExpandExpression(stx)
	if err != nil {
		return werr.WrapForeignErrorf(err, "eval: expansion error")
	}

	// Compile the expression
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(mc.Context(), false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return werr.WrapForeignErrorf(err, "eval: compilation error")
	}

	// Run the compiled code in a sub-context
	cont := machine.NewMachineContinuation(nil, tpl, env)
	sub := machine.NewMachineContext(mc.Context(), cont)
	sub.SetExceptionHandler(mc.ExceptionHandler())
	sub.SetMaxCallDepth(mc.MaxCallDepth())
	sub.SetThread(mc.Thread())
	err = sub.Run()
	if err != nil {
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}

// PrimLoad implements the (load) primitive.
// Loads and evaluates a Scheme source file.
func PrimLoad(mc *machine.MachineContext) error {
	filenameVal := mc.Arg(0)
	filename, err := helpers.RequireType[*values.String](filenameVal, werr.ErrNotAString, "load")
	if err != nil {
		return err
	}

	// Use the current top-level environment
	env := mc.EnvironmentFrame().TopLevel()

	// Resolve the file path
	stack := env.LoadPathStack()
	cwd, err := os.Getwd()
	if err != nil {
		return werr.WrapForeignErrorf(err, "load: cannot get current directory")
	}

	absPath, err := environment.ResolveFile(stack, filename.Value, []string{cwd})
	if err != nil {
		return werr.WrapForeignErrorf(err, "load")
	}

	err = security.Check(mc.Context(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return err
	}

	// Open the file
	f, err := os.Open(absPath)
	if err != nil {
		return werr.WrapForeignErrorf(err, "load: cannot open file %s", absPath)
	}
	defer f.Close() //nolint:errcheck

	// Push to stack after successful open, pop on exit
	err = stack.Push(absPath)
	if err != nil {
		return err
	}
	defer stack.Pop()

	// Create parser with file tracking for source locations
	rdr := bufio.NewReader(f)
	p := parser.NewParserWithFile(env, true, rdr, absPath)

	// Read and evaluate each expression
	var lastValue = values.Void
	for {
		stx, err := p.ReadSyntax(mc.Context())
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return werr.WrapForeignErrorf(err, "load: parse error in %s", filename.Value)
		}

		// Expand the expression
		expanded, err := machine.NewExpanderTimeContinuation(mc.Context(), env).ExpandExpression(stx)
		if err != nil {
			return werr.WrapForeignErrorf(err, "load: expansion error in %s", filename.Value)
		}

		// Compile the expression
		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := machine.NewCompileTimeCallContext(mc.Context(), false, true)
		err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
		if err != nil {
			return werr.WrapForeignErrorf(err, "load: compilation error in %s", filename.Value)
		}

		// Run the compiled code
		cont := machine.NewMachineContinuation(nil, tpl, env)
		sub := machine.NewMachineContext(mc.Context(), cont)
		sub.SetExceptionHandler(mc.ExceptionHandler())
		sub.SetMaxCallDepth(mc.MaxCallDepth())
		sub.SetThread(mc.Thread())
		err = sub.Run()
		if err != nil {
			return werr.WrapForeignErrorf(err, "load: runtime error in %s", filename.Value)
		}

		lastValue = sub.GetValue()
	}

	mc.SetValue(lastValue)
	return nil
}

// PrimCurrentLoadPath implements the (current-load-path) primitive.
// Returns the absolute path of the file currently being loaded, or #f if
// no file is being loaded (e.g., REPL).
func PrimCurrentLoadPath(mc *machine.MachineContext) error {
	current := mc.EnvironmentFrame().TopLevelEnv().LoadPathStack().Current()
	if current == "" {
		mc.SetValue(values.FalseValue)
	} else {
		mc.SetValue(values.NewString(current))
	}
	return nil
}

// PrimCurrentLoadDirectory implements the (current-load-directory) primitive.
// Returns the directory of the file currently being loaded, or #f if
// no file is being loaded (e.g., REPL).
func PrimCurrentLoadDirectory(mc *machine.MachineContext) error {
	currentDir := mc.EnvironmentFrame().TopLevelEnv().LoadPathStack().CurrentDir()
	if currentDir == "" {
		mc.SetValue(values.FalseValue)
	} else {
		mc.SetValue(values.NewString(currentDir))
	}
	return nil
}

// PrimCurrentLoadDepth implements the (current-load-depth) primitive.
// Returns the current load stack depth (number of nested loads).
// Returns 0 when not inside a load call.
func PrimCurrentLoadDepth(mc *machine.MachineContext) error {
	depth := mc.EnvironmentFrame().TopLevelEnv().LoadPathStack().Depth()
	mc.SetValue(values.NewInteger(int64(depth)))
	return nil
}

// PrimSchemeReportEnvironment implements the (scheme-report-environment) primitive.
// Returns R5RS env.
func PrimSchemeReportEnvironment(mc *machine.MachineContext) error {
	version := mc.Arg(0)
	versionInt, err := helpers.RequireType[*values.Integer](version, werr.ErrNotAnInteger, "scheme-report-environment")
	if err != nil {
		return err
	}

	// R7RS specifies version 5 (for R5RS) or 7 (for R7RS)
	switch versionInt.Value {
	case 5, 7:
		// Create a new environment that is distinct from interaction-environment
		// but contains a snapshot of the current standard bindings.
		// R7RS §6.12: scheme-report-environment must be distinct from
		// interaction-environment and contain the R7RS standard bindings.
		callerTopLevel := mc.EnvironmentFrame().TopLevelEnv()
		newTopLevel := callerTopLevel.NewSchemeReportEnvironment()
		newTopLevel.Name = "scheme-report-environment"
		mc.SetValue(newTopLevel)
		return nil
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "scheme-report-environment: unsupported version, expected 5 or 7")
	}
}

// PrimNullEnvironment implements the null-environment primitive.
// Returns an empty R5RS environment with no bindings.
func PrimNullEnvironment(mc *machine.MachineContext) error {
	version := mc.Arg(0)
	versionInt, err := helpers.RequireType[*values.Integer](version, werr.ErrNotAnInteger, "null-environment")
	if err != nil {
		return err
	}

	// R7RS specifies version 5 (for R5RS)
	switch versionInt.Value {
	case 5, 7:
		// Create a new empty top-level environment with only syntax bindings.
		// Shares the caller's symbol interning for R7RS §6.5 symbol identity.
		callerTopLevel := mc.EnvironmentFrame().TopLevelEnv()
		newTopLevel := callerTopLevel.NewChildTopLevelEnvironment()
		newTopLevel.Name = "null-environment"
		mc.SetValue(newTopLevel)
		return nil
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "null-environment: unsupported version, expected 5 or 7")
	}
}

// PrimEnvironment implements the (environment) primitive.
// Constructs a new environment from import specifiers.
//
// Supports Racket-style phased imports:
//   - (environment '(scheme base))                    ; Phase 0 (runtime)
//   - (environment '(for-syntax (scheme base)))       ; Phase 1 (expand)
//   - (environment '(for-template (scheme base)))     ; Phase -1
//   - (environment '(for-meta 2 (scheme base)))       ; Phase 2
func PrimEnvironment(mc *machine.MachineContext) error {
	// Get variadic import specs (collected as a list in arg 0)
	argsVal := mc.Arg(0)

	// Create child top-level environment sharing the caller's symbol interning
	// and library registry for R7RS §6.5 symbol identity.
	callerTopLevel := mc.EnvironmentFrame().TopLevelEnv()
	callerEnv := mc.EnvironmentFrame().TopLevel()
	newTopLevel := callerTopLevel.NewChildTopLevelEnvironment()
	newTopLevel.Name = "environment"
	newEnv := newTopLevel.Runtime()

	// Handle empty arguments case
	if values.IsEmptyList(argsVal) {
		mc.SetValue(newTopLevel)
		return nil
	}

	args, ok := argsVal.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "environment: expected list of import specs, got %T", argsVal)
	}

	// Process each import spec
	v, err := args.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, specVal values.Value) error {
		// Parse the import set from datum
		importSet, err := machine.ParseImportSetFromDatum(mc.Context(), specVal)
		if err != nil {
			return werr.WrapForeignErrorf(err, "environment: invalid import spec")
		}

		// Load the library (uses callerEnv for registry access)
		lib, err := machine.LoadLibrary(mc.Context(), importSet.LibraryName, callerEnv)
		if err != nil {
			return werr.WrapForeignErrorf(err, "environment: failed to load %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply modifiers (only, except, prefix, rename)
		bindings, err := importSet.ApplyToExports(lib)
		if err != nil {
			return werr.WrapForeignErrorf(err, "environment: error in import set for %s",
				importSet.LibraryName.SchemeString())
		}

		// Copy bindings to new environment at the specified phase
		err = machine.CopyLibraryBindingsToEnvAtPhase(lib, bindings, newEnv, importSet.PhaseShift)
		if err != nil {
			return werr.WrapForeignErrorf(err, "environment: error copying bindings from %s",
				importSet.LibraryName.SchemeString())
		}

		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "environment: improper import spec list")
	}

	mc.SetValue(newTopLevel)
	return nil
}

// PrimExpand implements the expand primitive.
// Fully expands a syntax object and returns the expanded syntax.
// (expand stx) -> expanded-stx
func PrimExpand(mc *machine.MachineContext) error {
	stx := mc.Arg(0)

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxObject, "expand: expected syntax object")
	}

	// Check if we're in an expansion context
	expanderCtx := mc.ExpanderContext()
	if expanderCtx != nil {
		// In expansion phase - use current context
		expanded, err := expanderCtx.Expand(syntaxVal)
		if err != nil {
			return werr.WrapForeignErrorf(err, "expand: expansion failed")
		}
		mc.SetValue(expanded)
		return nil
	}

	// Not in expansion phase - create temporary expander
	env := mc.EnvironmentFrame()
	expander := machine.NewExpanderTimeContinuation(mc.Context(), env)
	expanded, err := expander.ExpandExpression(syntaxVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "expand: expansion failed")
	}
	mc.SetValue(expanded)
	return nil
}

// PrimExpandOnce implements the expand-once primitive.
// Performs a single step of macro expansion and returns both the
// expanded syntax and a boolean indicating whether expansion occurred.
// (expand-once stx) -> (values expanded-stx did-expand?)
func PrimExpandOnce(mc *machine.MachineContext) error {
	stx := mc.Arg(0)

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxObject, "expand-once: expected syntax object")
	}

	// Check if we're in an expansion context
	expanderCtx := mc.ExpanderContext()
	if expanderCtx != nil {
		// In expansion phase - use current context
		expanded, didExpand, err := expanderCtx.ExpandOnce(syntaxVal)
		if err != nil {
			return werr.WrapForeignErrorf(err, "expand-once: expansion failed")
		}
		mc.SetValues(expanded, values.BoolToBoolean(didExpand))
		return nil
	}

	// Not in expansion phase - create temporary expander
	env := mc.EnvironmentFrame()
	expander := machine.NewExpanderTimeContinuation(mc.Context(), env)
	expanded, didExpand, err := expander.ExpandOnce(syntaxVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "expand-once: expansion failed")
	}
	mc.SetValues(expanded, values.BoolToBoolean(didExpand))
	return nil
}

// PrimCompile implements the (compile) primitive.
// Compiles an expression and returns a zero-argument procedure (thunk)
// that, when called, executes the compiled code.
//
// (compile expr) -> procedure
//
// The expr can be either a syntax object or a datum (which will be
// converted to a syntax object automatically).
//
// This is the final phase hook, completing the pipeline:
//
//	expand -> compile -> (execute via calling the returned thunk)
func PrimCompile(mc *machine.MachineContext) error {
	expr := mc.Arg(0)

	// Accept either syntax object or datum
	var syntaxVal syntax.SyntaxValue
	sv, ok := expr.(syntax.SyntaxValue)
	if ok {
		syntaxVal = sv
	} else {
		// Convert datum to syntax value
		sctx := syntax.NewZeroValueSourceContext()
		syntaxVal = schemeutil.DatumToSyntaxValue(mc.Context(), sctx, expr)
	}

	// Get the environment for expansion and compilation
	env := mc.EnvironmentFrame()

	// Step 1: Expand the syntax object
	expanded, err := machine.NewExpanderTimeContinuation(mc.Context(), env).ExpandExpression(syntaxVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "compile: expansion failed")
	}

	// Step 2: Compile to bytecode template
	// Create a thunk template (0 params, 0 locals, not variadic)
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(mc.Context(), false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return werr.WrapForeignErrorf(err, "compile: compilation failed")
	}

	// Add return operation so the thunk properly returns its value
	// through the continuation chain when called
	tpl.AppendOperations(machine.NewOperationRestoreContinuation())

	// Step 3: Wrap in a closure (thunk)
	// The closure captures the current environment
	closure := machine.NewClosureWithTemplate(tpl, env)

	mc.SetValue(closure)
	return nil
}

// PrimSyntaxLocalValue implements the syntax-local-value primitive.
// Retrieves the compile-time value bound to an identifier in the expand phase.
// (syntax-local-value id) -> value
//
// This primitive can only be called during macro expansion (when an ExpanderContext
// is set on the MachineContext). It looks up the identifier in the expand phase
// environment, respecting hygiene scopes.
//
// If the binding is a CompileTimeValue, it returns the unwrapped value.
// This allows define-for-syntax bindings to be accessed from macro transformers.
func PrimSyntaxLocalValue(mc *machine.MachineContext) error {
	id := mc.Arg(0)

	syntaxSym, ok := id.(*syntax.SyntaxSymbol)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "syntax-local-value: expected identifier")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return werr.WrapForeignErrorf(werr.ErrNoCaptureContext, "syntax-local-value: not in expansion context")
	}

	// Look up in expand phase
	expandEnv := expanderCtx.Env().Expand()
	sym := syntaxSym.Datum()
	sym = expandEnv.InternSymbol(sym)

	binding := expandEnv.GetBindingWithScopes(sym, syntaxSym.Scopes())
	if binding == nil {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "syntax-local-value: no binding for %s", sym.Key)
	}

	val := binding.Value()

	// If it's a CompileTimeValue, unwrap it
	ctv, ok := val.(*values.CompileTimeValue)
	if ok {
		val = ctv.Unwrap()
	}

	mc.SetValue(val)
	return nil
}

// PrimMakeCompileTimeValue implements the make-compile-time-value primitive.
// Wraps a value for compile-time storage.
// (make-compile-time-value value) -> compile-time-value
//
// This creates a CompileTimeValue wrapper around the given value. CompileTimeValue
// is used to distinguish regular runtime values from values that should be stored
// in the expand phase and accessed during macro expansion.
//
// When syntax-local-value retrieves a CompileTimeValue, it automatically unwraps
// it to return the underlying value.
func PrimMakeCompileTimeValue(mc *machine.MachineContext) error {
	v := mc.Arg(0)

	ctv := values.NewCompileTimeValue(v)
	mc.SetValue(ctv)
	return nil
}

// PrimSyntaxLocalIntroduce implements the syntax-local-introduce primitive.
// Flips the introduction scope on a syntax object.
// (syntax-local-introduce stx) -> stx
//
// This primitive toggles the "introduction scope" on a syntax object.
// The introduction scope is added to identifiers introduced by a macro.
// By flipping it, you can make an introduced identifier behave as if it
// came from the macro use site (or vice versa).
//
// Use cases:
//   - Breaking hygiene intentionally (anaphoric macros)
//   - Making macro-introduced bindings visible at the use site
//   - Implementing advanced macro patterns like syntax-parameterize
//
// This primitive can only be called during macro expansion (when an
// ExpanderContext is set on the MachineContext with an introduction scope).
func PrimSyntaxLocalIntroduce(mc *machine.MachineContext) error {
	stx := mc.Arg(0)

	syntaxVal, ok := stx.(syntax.SyntaxValue)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxObject, "syntax-local-introduce: expected syntax object")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return werr.WrapForeignErrorf(werr.ErrNoCaptureContext, "syntax-local-introduce: not in expansion context")
	}

	introScope := expanderCtx.IntroductionScope()
	if introScope == nil {
		// No introduction scope set - return syntax unchanged
		// This can happen if called outside a macro transformer
		mc.SetValue(syntaxVal)
		return nil
	}

	// Flip the scope on the syntax object
	result := syntax.FlipScope(syntaxVal, introScope)
	mc.SetValue(result)
	return nil
}

// PrimSyntaxLocalIdentifierAsBinding implements the syntax-local-identifier-as-binding primitive.
// Marks an identifier as a binding site by adding the use-site scope.
// (syntax-local-identifier-as-binding id) -> id
//
// This primitive adds the "use-site scope" to an identifier, marking it as
// a binding site. This is used in binding forms (like let, lambda) to ensure
// proper hygiene when the binding form is implemented as a macro.
//
// When a macro introduces a binding form, the bound identifiers need the
// use-site scope to be properly distinguished from identifiers at the
// macro's definition site.
//
// Use cases:
//   - Implementing custom binding forms as macros
//   - Ensuring proper hygiene for macro-generated bindings
//   - Creating hygienic versions of anaphoric macros
//
// This primitive can only be called during macro expansion (when an
// ExpanderContext is set on the MachineContext with a use-site scope).
func PrimSyntaxLocalIdentifierAsBinding(mc *machine.MachineContext) error {
	id := mc.Arg(0)

	syntaxSym, ok := id.(*syntax.SyntaxSymbol)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "syntax-local-identifier-as-binding: expected identifier")
	}

	expanderCtx := mc.ExpanderContext()
	if expanderCtx == nil {
		return werr.WrapForeignErrorf(werr.ErrNoCaptureContext, "syntax-local-identifier-as-binding: not in expansion context")
	}

	useSiteScope := expanderCtx.UseSiteScope()
	if useSiteScope == nil {
		// No use-site scope set - return identifier unchanged
		mc.SetValue(syntaxSym)
		return nil
	}

	// Add the use-site scope to mark as binding
	result := syntax.AddScopeToSyntax(syntaxSym, useSiteScope)
	mc.SetValue(result)
	return nil
}
