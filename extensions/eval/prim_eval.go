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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ProfileFactory is the callback used by (environment '(wile <name>)) and
// (environment '(wile <name> <strictness>)) to construct a namespace for a named
// Wile profile. It is set by internal/bootstrap at init time (bootstrap cannot be
// imported here because bootstrap already imports this package). When nil,
// (wile <name>) specs return ErrWileProfilesNotRegistered.
//
// Both names cross as raw spellings and are validated on the far side: this
// package cannot name bootstrap's profile or strictness types, and splitting the
// validation would put half the vocabulary in each package. strictName is "" when
// the spec omitted it.
var ProfileFactory func(ctx context.Context, callerNS *environment.Namespace, profileName, strictName string) (*environment.Namespace, error)

// ErrWileProfilesNotRegistered is returned when tryWileProfile dispatches on
// a (wile <name>) spec but no ProfileFactory has been registered.
var ErrWileProfilesNotRegistered = werr.NewStaticError("wile profiles not registered")

// PrimEval implements the (eval) primitive.
// 1-arg form: (eval expr) — uses current namespace (interaction-environment).
// 2-arg form: (eval expr env) — uses the given environment.
//
// With ParamCount: 1 and IsVariadic: true, all args arrive as a rest
// list in mc.Arg(0).
func PrimEval(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "eval")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "<eval>",
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "eval: code evaluation denied")
	}
	argList, ok := mc.Arg(0).(values.Tuple)
	if !ok || argList.IsEmptyList() {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "eval: expected 1 or 2 arguments")
	}

	expr := argList.Car()

	// Determine the target namespace
	var topLevelEnv *environment.Namespace
	restTuple, _ := argList.Cdr().(values.Tuple)
	if restTuple != nil && !restTuple.IsEmptyList() {
		// 2-arg form: extract environment from second arg
		envSpec := restTuple.Car()
		topLevelEnv, ok = envSpec.(*environment.Namespace)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotANamespace, "eval: expected a namespace but got %T", envSpec)
		}
		// Reject extra arguments beyond the environment
		envRest, _ := restTuple.Cdr().(values.Tuple)
		if envRest != nil && !envRest.IsEmptyList() {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "eval: expected 1 or 2 arguments")
		}
	} else {
		// 1-arg form: use current namespace
		topLevelEnv = mc.EnvironmentFrame().Namespace()
	}

	env := topLevelEnv.Runtime()

	// Convert datum to syntax value, expand, and compile.
	sctx := syntax.NewZeroValueSourceContext()
	stx, err := schemeutil.DatumToSyntaxValue(mc.Context(), sctx, expr)
	if err != nil {
		return werr.WrapForeignErrorf(err, "eval")
	}

	tpl, err := compilation.ExpandAndCompile(mc.Context(), env, stx, nil, compilation.DefaultInlineThreshold, compilation.DefaultMaxExpandDepth)
	if err != nil {
		return werr.WrapForeignErrorf(err, "eval")
	}

	// Run the compiled code in a sub-context.
	// NewSubContextWithTemplate propagates all parent fields automatically
	// (exception handler, winding stack, barrier, escape continuation, etc.).
	sub := mc.NewSubContextWithTemplate(tpl, env)
	defer machine.ReleaseSubContext(sub)
	err = sub.RunWithinBoundary()
	if err != nil {
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}

// PrimLoad implements the (load) primitive.
// Loads and evaluates a Scheme source file.
//
// File resolution uses the same FileResolver as include, so load and
// include share the same search path priority:
//
//	current load directory > LibraryRegistry > SCHEME_INCLUDE_PATH > CWD
//
// The current load directory comes from the per-load-chain stack carried on the
// context, so concurrent loads on separate threads resolve independently.
func PrimLoad(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "load")
	if err != nil {
		return err
	}
	filenameVal := mc.Arg(0)
	filename, err := helpers.RequireType[*values.String](filenameVal, werr.ErrNotAString, "load")
	if err != nil {
		return err
	}

	// Loaded defines must land in the user-visible mutable global (becoming shadows of
	// sealed-base names under the immutable default), so use MutableRuntime().
	env := mc.EnvironmentFrame().MutableRuntime()

	// Resolve and open via the shared FileResolver (same as include).
	resolver := env.FileResolver()
	if resolver == nil {
		return werr.WrapForeignErrorf(werr.ErrFileNotFound, "load: no file resolver configured")
	}

	f, resolvedPath, err := resolver.ResolveAndOpen(mc.Context(), filename.Value)
	if err != nil {
		return werr.WrapForeignErrorf(err, "load")
	}
	defer f.Close() //nolint:errcheck

	// Push to stack after successful open, pop on exit. The stack is carried on
	// ctx and is per load CHAIN: an outer load installs one, a nested load pushes
	// onto the same object so its own relative resolutions see the inner
	// directory, and two threads loading concurrently never share one.
	//
	// It used to be a single stack hung off the Namespace, shared by every thread
	// in the engine. Two threads each loading a file that includes the same
	// basename from different directories interleaved their pushes and one
	// compiled the other's file — a source substitution, so -race cannot see it.
	loadCtx := mc.Context()
	stack := sourceload.LoadStackFromContext(loadCtx)
	if stack == nil {
		stack = sourceload.NewLoadStack()
		loadCtx = sourceload.WithLoadStack(loadCtx, stack)
	}
	stack.Push(resolvedPath)
	defer stack.Pop()

	// Create parser with file tracking for source locations
	rdr := bufio.NewReader(f)
	p := parser.NewParserWithFile(env, true, rdr, resolvedPath)

	// Read and evaluate each expression
	var lastValue = values.Void
	for {
		stx, err := p.ReadSyntax(loadCtx)
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return werr.WrapForeignErrorf(err, "load: parse error in %s", filename.Value)
		}

		tpl, err := compilation.ExpandAndCompile(loadCtx, env, stx, nil, compilation.DefaultInlineThreshold, compilation.DefaultMaxExpandDepth)
		if err != nil {
			return werr.WrapForeignErrorf(err, "load: in %s", filename.Value)
		}

		// Run the compiled code.
		// NewSubContextWithTemplate propagates all parent fields automatically.
		err = func() error {
			sub := mc.NewSubContextWithTemplate(tpl, env)
			defer machine.ReleaseSubContext(sub)
			// The sub-context inherits mc.ctx, which is the ctx BEFORE the stack was
			// installed, so a nested (load …) inside the loaded file would not see
			// the chain. Re-derive from sub.Context() and never from a fresh root:
			// a fresh root would drop cancellation for the whole extent of the load.
			sub.SetContext(sourceload.WithLoadStack(sub.Context(), stack))
			err := sub.RunWithinBoundary()
			if err != nil {
				return werr.WrapForeignErrorf(err, "load: runtime error in %s", filename.Value)
			}
			lastValue = sub.GetValue()
			return nil
		}()
		if err != nil {
			return err
		}
	}

	mc.SetValue(lastValue)
	return nil
}

// PrimCurrentLoadPath implements the (current-load-path) primitive.
// Returns the path of the file currently being loaded, or #f if
// no file is being loaded (e.g., REPL).
func PrimCurrentLoadPath(mc machine.CallContext) error {
	stack := sourceload.LoadStackFromContext(mc.Context())
	if stack == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.StringOrFalse(stack.Current()))
	return nil
}

// PrimCurrentLoadDirectory implements the (current-load-directory) primitive.
// Returns the directory of the file currently being loaded, or #f if
// no file is being loaded (e.g., REPL).
func PrimCurrentLoadDirectory(mc machine.CallContext) error {
	stack := sourceload.LoadStackFromContext(mc.Context())
	if stack == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.StringOrFalse(stack.CurrentDir()))
	return nil
}

// PrimCurrentLoadDepth implements the (current-load-depth) primitive.
// Returns the current load stack depth (number of nested loads).
// Returns 0 when not inside a load call.
func PrimCurrentLoadDepth(mc machine.CallContext) error {
	stack := sourceload.LoadStackFromContext(mc.Context())
	if stack == nil {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	mc.SetValue(values.NewInteger(int64(stack.Depth())))
	return nil
}

// PrimSchemeReportEnvironment implements the (scheme-report-environment) primitive.
// Returns R5RS env.
func PrimSchemeReportEnvironment(mc machine.CallContext) error {
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
		callerTopLevel := mc.EnvironmentFrame().Namespace()
		newTopLevel := callerTopLevel.NewSchemeReportNamespace()
		newTopLevel.Name = "scheme-report-environment"
		mc.SetValue(newTopLevel)
		return nil
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "scheme-report-environment: unsupported version, expected 5 or 7")
	}
}

// PrimNullEnvironment implements the null-environment primitive.
// Returns an empty R5RS environment with no bindings.
func PrimNullEnvironment(mc machine.CallContext) error {
	version := mc.Arg(0)
	versionInt, err := helpers.RequireType[*values.Integer](version, werr.ErrNotAnInteger, "null-environment")
	if err != nil {
		return err
	}

	// R7RS specifies version 5 (for R5RS)
	switch versionInt.Value {
	case 5, 7:
		// Create a new empty top-level environment. R7RS specifies syntax-only
		// bindings here; this returns a fully empty child instead.
		// Shares the caller's syntax-object interning (Namespace.InternSyntax
		// delegates to the parent) for R7RS §6.5 symbol identity.
		callerTopLevel := mc.EnvironmentFrame().Namespace()
		newTopLevel := callerTopLevel.NewChildNamespace()
		newTopLevel.Name = "null-environment"
		mc.SetValue(newTopLevel)
		return nil
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "null-environment: unsupported version, expected 5 or 7")
	}
}

// tryWileProfile checks whether argsVal is a single-element list whose only
// element is (wile <name>) or (wile <name> <strictness>). If so, it dispatches to
// bootstrap to construct a fresh namespace for that profile. Returns (ns, true,
// nil) on match, (nil, false, nil) if the spec is not a profile constructor
// (caller falls through to standard import-spec handling), or (nil, true, err) if
// the match succeeded but construction failed.
//
// The optional third element narrows the new environment's VISIBLE top level:
// 'core binds only the core surface, 'no-bindings binds nothing. It is a separate
// element rather than part of the profile name because strictness is orthogonal to
// the profile — the profile decides what is registered and remains the capability
// boundary, the level only decides how much of it is pre-bound.
//
//	(environment '(wile small))                ; the Small surface, pre-bound
//	(environment '(wile small core))           ; core only; import the rest
//	(environment '(wile small no-bindings))    ; nothing; import everything
func tryWileProfile(mc machine.CallContext, argsVal values.Value) (*environment.Namespace, bool, error) {
	args, ok := argsVal.(values.Tuple)
	if !ok {
		return nil, false, nil
	}
	first, ok := values.Single(args)
	if !ok {
		return nil, false, nil
	}
	spec, ok := first.(values.Tuple)
	if !ok || values.IsEmptyList(spec) {
		// The empty list SATISFIES values.Tuple, so the assertion above admits it
		// and spec.Car() then panics — uncatchably, since a primitive's panic
		// reaches the VM boundary as a *SchemeError rather than a condition.
		// (environment '()) is an ordinary malformed spec, i.e. a domain error, so
		// it must fall through to the clean diagnostic (environment '() '())
		// already produces rather than take the program down.
		return nil, false, nil
	}
	headSym, ok := spec.Car().(*values.Symbol)
	if !ok || headSym.Key != "wile" {
		return nil, false, nil
	}
	// At this point the head is 'wile. From here, malformed payload is an
	// error (return true), not a fallthrough.
	nameSym, restAfterName, err := values.UnconsTyped[*values.Symbol](
		spec.Cdr(), werr.ErrNotASymbol, "environment", "profile name after 'wile")
	if err != nil {
		return nil, true, err
	}
	// The strictness element is optional, so its absence is the empty list rather
	// than an error, and "" is how that absence travels downstream: ParseStrictLevel
	// maps it to StrictOff, so no caller special-cases it. That in-band encoding is
	// only safe because the present-but-empty spelling is refused below.
	strictName := ""
	if !values.IsEmptyList(restAfterName) {
		strictSym, restAfterStrict, strictErr := values.UnconsTyped[*values.Symbol](
			restAfterName, werr.ErrNotASymbol, "environment", "strictness after profile name")
		if strictErr != nil {
			return nil, true, strictErr
		}
		if !values.IsEmptyList(restAfterStrict) {
			return nil, true, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"environment: (wile %s ...) takes a profile name and an optional strictness", nameSym.Key)
		}
		// || is a legal zero-length symbol, so a PRESENT element can carry the
		// absent-marker. Refuse it while presence is still known, rather than let it
		// reach ParseStrictLevel as an absent argument: these levels only ever
		// narrow, so an unrecognized spelling must fail rather than silently select
		// the widest surface.
		if strictSym.Key == "" {
			return nil, true, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"environment: (wile %s ...) strictness must be a level name (want \"core\" or \"no-bindings\"), not the empty symbol", nameSym.Key)
		}
		strictName = strictSym.Key
	}

	if ProfileFactory == nil {
		return nil, true, werr.WrapForeignErrorf(ErrWileProfilesNotRegistered,
			"environment: (wile %s) requires bootstrap to register ProfileFactory", nameSym.Key)
	}

	callerNS := mc.EnvironmentFrame().Namespace()
	ns, err := ProfileFactory(mc.Context(), callerNS, nameSym.Key, strictName)
	if err != nil {
		return nil, true, werr.WrapForeignErrorf(err,
			"environment: failed to create wile profile %q", nameSym.Key)
	}
	ns.Name = "environment"
	em := callerNS.EnvMap()
	if em != nil {
		ns.SetEnvMap(em)
	}
	return ns, true, nil
}

// PrimEnvironment implements the (environment) primitive.
// Constructs a new environment from import specifiers.
//
// Supports Racket-style phased imports:
//   - (environment '(scheme base))                    ; Phase 0 (runtime)
//   - (environment '(for-syntax (scheme base)))       ; Phase 1 (expand)
//   - (environment '(for-template (scheme base)))     ; Phase -1
//   - (environment '(for-meta 2 (scheme base)))       ; Phase 2
func PrimEnvironment(mc machine.CallContext) error {
	// Get variadic import specs (collected as a list in arg 0)
	argsVal := mc.Arg(0)

	// Check for (wile <profile>) profile constructor as the sole spec.
	profileNS, handled, err := tryWileProfile(mc, argsVal)
	if err != nil {
		return err
	}
	if handled {
		mc.SetValue(profileNS)
		return nil
	}

	// Create child top-level environment sharing the caller's syntax-object
	// interning (Namespace.InternSyntax delegates to the parent) and library
	// registry for R7RS §6.5 symbol identity.
	callerTopLevel := mc.EnvironmentFrame().Namespace()
	// Import source = the mutable runtime (reaches the sealed base via its parent walk,
	// so resolution is preserved); TopLevel() now returns the sealed base alone.
	callerEnv := callerTopLevel.Runtime()
	newTopLevel := callerTopLevel.NewChildNamespace()
	newTopLevel.Name = "environment"
	newEnv := newTopLevel.Runtime()

	// Handle empty arguments case
	if values.IsEmptyList(argsVal) {
		mc.SetValue(newTopLevel)
		return nil
	}

	args, ok := argsVal.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "environment: expected list of import specs, got %T", argsVal)
	}

	// Process each import spec
	err = helpers.ForEachList(mc.Context(), args, "environment", func(_ context.Context, _ int, _ bool, specVal values.Value) error {
		return compilation.ImportSpecInto(mc.Context(), specVal, callerEnv, newEnv, machine.NewVMMacroEvaluator(), "environment")
	})
	if err != nil {
		return err
	}

	mc.SetValue(newTopLevel)
	return nil
}

// PrimExpand implements the expand primitive.
// Fully expands a syntax object and returns the expanded syntax.
// (expand stx) -> expanded-stx
func PrimExpand(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "expand")
	if err != nil {
		return err
	}
	// Gate at ENTRY, before any transformer body runs. expand and expand-once run
	// user-supplied transformer bodies, which is code the program assembled at
	// runtime — the capability `eval` names. Reusing code:eval rather than minting
	// a new action is deliberate: an authorizer that denies code:eval and permits
	// expand expresses no policy anyone wants, and a new action would silently
	// widen every existing authorizer's default arm.
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "<expand>",
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "expand: code evaluation denied")
	}
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
	expander := compilation.NewExpanderTimeContinuation(mc.Context(), env, machine.NewVMMacroEvaluator())
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
func PrimExpandOnce(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "expand-once")
	if err != nil {
		return err
	}
	// Gate at ENTRY, before any transformer body runs. expand and expand-once run
	// user-supplied transformer bodies, which is code the program assembled at
	// runtime — the capability `eval` names. Reusing code:eval rather than minting
	// a new action is deliberate: an authorizer that denies code:eval and permits
	// expand expresses no policy anyone wants, and a new action would silently
	// widen every existing authorizer's default arm.
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "<expand-once>",
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "expand-once: code evaluation denied")
	}
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
	expander := compilation.NewExpanderTimeContinuation(mc.Context(), env, machine.NewVMMacroEvaluator())
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
func PrimCompile(mc machine.CallContext) error {
	err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionEval,
		Target:   "<compile>",
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "compile: code evaluation denied")
	}

	expr := mc.Arg(0)

	// Accept either syntax object or datum
	var syntaxVal syntax.SyntaxValue
	sv, ok := expr.(syntax.SyntaxValue)
	if ok {
		syntaxVal = sv
	} else {
		// Convert datum to syntax value
		sctx := syntax.NewZeroValueSourceContext()
		converted, err := schemeutil.DatumToSyntaxValue(mc.Context(), sctx, expr)
		if err != nil {
			return werr.WrapForeignErrorf(err, "compile")
		}
		syntaxVal = converted
	}

	// Expand and compile against a top level, not the caller's frame.
	//
	// NOT mc.EnvironmentFrame(): inside a primitive that is the primitive's own
	// apply frame, which applyForeign draws from the env-frame pool. Capturing it
	// in the returned thunk left the closure pointing at a frame that was
	// released and wiped as soon as the enclosing evaluation finished, and later
	// handed back out to unrelated calls. It also compiled the expression against
	// the primitive's parameter frame rather than a top level, which is why
	// (compile '(define x 5)) emitted a local store and then failed at run time
	// with "no such local binding 1:0".
	//
	// A fresh child of the runtime frame, and this site is the only one in the
	// extension that needs one -- PrimEval (:92) and PrimLoad (:137) use the
	// runtime frame directly because they RUN immediately, while compile returns
	// a closure that outlives the call. The reason is structural:
	// the thunk's apply frame takes its globals and namespace from its PARENT,
	// so capturing the runtime frame directly hands the thunk the sealed base's
	// globals and a nil namespace.
	env := environment.NewEnvironmentFrameWithParent(nil, mc.EnvironmentFrame().MutableRuntime())

	tpl, err := compilation.ExpandAndCompile(mc.Context(), env, syntaxVal, nil, compilation.DefaultInlineThreshold, compilation.DefaultMaxExpandDepth)
	if err != nil {
		return werr.WrapForeignErrorf(err, "compile")
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
func PrimSyntaxLocalValue(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "syntax-local-value")
	if err != nil {
		return err
	}
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

	binding := expandEnv.GetBinding(sym, syntax.ScopesOf(syntaxSym.Scopes()))
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

// PrimSyntaxLocalValueImmediate implements (syntax-local-value/immediate id).
// Like syntax-local-value but does not chase rename-transformer chains.
// Wile does not currently have rename-transformers, so this behaves
// identically to syntax-local-value.
//
// When rename-transformers are implemented, this function must NOT chase
// the transformer chain — it should return the transformer object directly.
// At that point, PrimSyntaxLocalValue and PrimSyntaxLocalValueImmediate
// must diverge.
//
// Racket §12.4: syntax-local-value/immediate
func PrimSyntaxLocalValueImmediate(mc machine.CallContext) error {
	return PrimSyntaxLocalValue(mc)
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
func PrimMakeCompileTimeValue(mc machine.CallContext) error {
	v := mc.Arg(0)

	ctv := values.NewCompileTimeValue(v)
	mc.SetValue(ctv)
	return nil
}

// PrimSyntaxLocalIntroduce implements the syntax-local-introduce primitive.
// (syntax-local-introduce stx) -> error
//
// NOT IMPLEMENTED: this primitive is registered and callable but always fails
// with werr.ErrNotImplemented. The scope-flipping branch below is unreachable
// because ExpanderContext.IntroductionScope() is never set — see the comment at
// the nil check for why wiring it is not a one-liner.
//
// It is kept registered, rather than deleted, so that a program calling it gets
// a diagnosis instead of an unbound-variable error, and so the intended
// semantics stay documented in one place:
//
// Flipping the "introduction scope" on a syntax object makes a
// macro-introduced identifier behave as if it came from the macro use site (or
// vice versa) — the mechanism behind anaphoric macros, use-site-visible
// bindings, and syntax-parameterize.
func PrimSyntaxLocalIntroduce(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "syntax-local-introduce")
	if err != nil {
		return err
	}
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
		// Fail loudly. Nothing in production ever calls SetIntroductionScope, so this
		// scope is ALWAYS nil and this primitive was a silent no-op: it handed back
		// the syntax object unchanged and reported success, so a macro relying on it
		// to flip hygiene got a wrong answer that looked like a right one.
		//
		// Returning the input is the worst of the three options. Wiring the scope is
		// not a one-liner either: each of the three transformer entry points mints its
		// OWN intro scope per invocation (expander_time_continuation, the syntax-rules
		// transform op, and syntax-case), so "store one scope on the context and flip
		// it" cannot be correct — there is no single scope to store. Until the context
		// carries the invocation's actual scope, say so instead of lying.
		return werr.WrapForeignErrorf(werr.ErrNotImplemented,
			"syntax-local-introduce: no introduction scope is available on the expander "+
				"context; this primitive is not wired to the per-invocation intro scopes "+
				"and cannot flip hygiene")
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
func PrimSyntaxLocalIdentifierAsBinding(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "syntax-local-identifier-as-binding")
	if err != nil {
		return err
	}
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
