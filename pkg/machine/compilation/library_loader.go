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

// library_loader.go implements R7RS library loading.
//
// When an (import ...) is encountered, this module:
// 1. Finds the library file (.sld or .scm)
// 2. Parses the define-library form
// 3. Compiles the library to bytecode
// 4. Executes the library to populate its environment
// 5. Registers the library in the global registry
// 6. Returns the CompiledLibrary for import binding

import (
	"bufio"
	"context"
	"errors"
	"io"
	"path/filepath"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/security"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// LoadLibrary loads a library by name, compiling and executing it if not already loaded.
// Returns the CompiledLibrary which can be used to import bindings.
//
// The function:
// 1. Checks for circular dependencies
// 2. Checks if already loaded (returns cached library)
// 3. Resolves and opens the library file via FileResolver
// 4. Parses and compiles the define-library form
// 5. Executes the library to create runtime bindings
// 6. Registers the library in the registry
func LoadLibrary(ctx context.Context, name LibraryName, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	registryAny := env.LibraryRegistry()
	if registryAny == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: no library registry configured")
	}
	reg, ok := registryAny.(*LibraryRegistry)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: invalid library registry type")
	}

	// Cycle detection precedes the registry claim. A genuine import cycle (A→B→A)
	// is synchronous re-entry on this goroutine's own load chain, recorded in ctx
	// by withLoadChain below. Catching it here (before LookupClaimOrWait) is what
	// lets the registry treat a still-loading name as "wait", not "cycle": a
	// goroutine can never reach the wait path for a latch it installed itself.
	if loadChainContains(ctx, name) {
		return nil, werr.WrapForeignErrorf(werr.ErrCircularDependency,
			"circular dependency detected while loading %s", name.SchemeString())
	}

	// LoadLibrary is the only frame that holds both the CALLER's env and the
	// load, so the effective policy is computed once here and used twice: on the
	// cache hit below, and on the ctx installed for the miss. Computing it in one
	// place is what stops a hit and a miss from disagreeing about what is allowed.
	loadAuth := env.Namespace().EffectiveAuthorizer()

	// Claim the loading slot, or wait for a concurrent loader of the SAME library.
	// LookupClaimOrWait collapses lookup → claim into one locked decision so two
	// threads cannot both see "not loaded" and proceed to load+Register the same
	// library (a TOCTOU that would otherwise surface as ErrDuplicateBinding).
	for {
		cached, wait := reg.LookupClaimOrWait(name)
		if cached != nil {
			// A cache hit reaches neither the file resolver nor, therefore, the
			// authorizer, so an import that succeeded under a permissive policy
			// kept succeeding after the policy was tightened. Re-authorize the
			// recorded source path.
			//
			// An empty SourceFile is a SYNTHETIC library: extension libraries are
			// registered via NewCompiledLibrary and have no file. Handing "" to a
			// path-containment authorizer denies, so they are skipped rather than
			// refused for having no path. This arm sits inside the retry loop, so
			// it covers the first-lookup hit and the post-latch-wait hit alike.
			if cached.SourceFile == "" {
				return cached, nil
			}
			authErr := security.CheckWithAuthorizer(loadAuth, security.AccessRequest{
				Resource:     security.ResourceCode,
				Action:       security.ActionLoad,
				Target:       cached.SourceFile,
				TargetSource: sourceOf(cached.SourceFile),
			})
			if authErr != nil {
				return nil, werr.WrapForeignErrorf(authErr, "load-library: %s", name.SchemeString())
			}
			return cached, nil
		}
		if wait == nil {
			// Both nil: we claimed the loading slot.
			break
		}
		// Another goroutine is loading this library. Wait for it to finish, then
		// re-consult: success ⇒ cached; failure ⇒ neither cached nor loading, so
		// the next iteration re-claims and retries the load on this goroutine.
		//
		// LIMITATION: cross-goroutine mutual imports deadlock. If goroutine 1 holds
		// A and imports B while goroutine 2 holds B and imports A, each waits on the
		// other's latch — the load chain is per-goroutine, so loadChainContains does
		// not see the other's claim. There is no cross-goroutine wait-for cycle
		// detection; the only escape is ctx cancellation/deadline. Single-goroutine
		// cycles (the common case) are caught synchronously by loadChainContains
		// above. Mutually-importing libraries are themselves an R7RS program error.
		select {
		case <-wait:
		case <-ctx.Done():
			return nil, werr.WrapForeignErrorf(ctx.Err(),
				"load-library: %s: cancelled while awaiting concurrent load",
				name.SchemeString())
		}
	}
	// We own the loading slot; release it (closing the latch) on every exit path.
	defer reg.FinishLoading(name)

	// Record this library on the load chain so a nested import of it is detected
	// as a cycle above. The augmented context flows down the synchronous import
	// resolution; the parent frame retains its own shorter context.
	ctx = withLoadChain(ctx, name)

	// The resolvers read their authorizer off the env they captured at engine
	// construction — the ROOT env — and FileResolver.ResolveAndOpen takes no
	// authorizer parameter, so ctx is the only channel by which a caller with a
	// stricter policy reaches them. Same idiom as the load chain above.
	if loadAuth != nil {
		ctx = security.WithAuthorizer(ctx, loadAuth)
	}
	var lib *CompiledLibrary

	// Resolve and open via FileResolver (supports both OS and virtual FS).
	resolver := env.FileResolver()
	if resolver == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"load-library: no file resolver configured")
	}

	f, filePath, err := ResolveLibraryFile(ctx, resolver, name)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"load-library: %s", name.SchemeString())
	}
	defer f.Close() //nolint:errcheck

	lib, err = loadLibraryFromReader(ctx, f, filePath, name, env, evaluator)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error loading library %s from %s", name.SchemeString(), filePath)
	}

	err = reg.Register(lib)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error registering library %s", name.SchemeString())
	}

	return lib, nil
}

// sourceOf reports the security.AccessRequest.TargetSource for a recorded
// library path, so a cache hit re-asks the question the resolver asked on the
// miss rather than a different one.
//
// The path's own shape decides it, and that is not a guess about which resolver
// served the file: OS path containment is only meaningful for an ABSOLUTE path.
// A relative target handed to a path-confining authorizer is resolved against
// the PROCESS working directory, so its verdict is a coincidence of where the
// host happens to run — the defect W2-9 removed from the resolvers. Every
// host-filesystem resolution records an absolute path (both OSFileResolver arms
// absolutize before they authorize); a relative recording therefore names a file
// inside a virtual filesystem, which is exactly what SourceVirtualFS says.
func sourceOf(path string) string {
	if filepath.IsAbs(path) {
		return ""
	}
	return security.SourceVirtualFS
}

// propagateRegistryAcrossNamespace shares caller's library registry into child
// when the two live in different namespaces. Under the default factory child is
// a NewChildRuntime of caller's Namespace and already resolves to the same
// registry, so the copy is skipped: re-setting it writes the shared namespace's
// registry field redundantly and races sibling concurrent loads (the same
// idempotent-shared-write class as the ApplyDocs guard). Only a custom factory
// producing a distinct namespace needs the copy.
func propagateRegistryAcrossNamespace(child, caller *environment.EnvironmentFrame) {
	if child.Namespace() != caller.Namespace() {
		child.SetLibraryRegistry(caller.LibraryRegistry())
	}
}

// loadLibraryFromReader parses, compiles, and executes a library from an open reader.
func loadLibraryFromReader(ctx context.Context, r io.Reader, filePath string, expectedName LibraryName, callerEnv *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	// Track this library file on a per-load-chain stack carried on ctx, not the
	// single LoadStack shared on the root namespace: concurrent SRFI-18 thread
	// loads would otherwise corrupt that shared LIFO and resolve each other's
	// (include …) against the wrong directory ("file not found"). The first load
	// on a chain creates the stack; nested library loads and the includes within
	// each library reuse it through ctx. It is mutated only within this
	// synchronous, single-goroutine chain, so Push/Pop need no locking concern.
	stack := sourceload.LoadStackFromContext(ctx)
	if stack == nil {
		stack = sourceload.NewLoadStack()
		ctx = sourceload.WithLoadStack(ctx, stack)
	}
	stack.Push(filePath)
	defer stack.Pop()

	factory := callerEnv.Namespace().LibraryEnvFactory()
	if factory == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "LibraryEnvFactory not configured")
	}
	libEnv, err := factory(ctx, callerEnv, expectedName.Parts)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "could not create library environment")
	}

	// Share the registry only across a distinct (custom-factory) namespace.
	propagateRegistryAcrossNamespace(libEnv, callerEnv)

	reader := bufio.NewReader(r)
	p := parser.NewParserWithFile(libEnv, true, reader, filePath)

	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "library file is empty")
		}
		return nil, werr.WrapForeignErrorf(err, "could not parse library file")
	}

	if !isSyntaxFormWithKeyword(stx, "define-library") &&
		!isSyntaxFormWithKeyword(stx, "library") {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed,
			"expected define-library or library form, got %T", stx)
	}

	lib, err := compileAndExecuteLibrary(ctx, stx, expectedName, libEnv, filePath, evaluator)
	if err != nil {
		return nil, err
	}

	return lib, nil
}

// compileAndExecuteLibrary compiles a define-library form and executes it.
func compileAndExecuteLibrary(ctx context.Context, stx syntax.SyntaxValue, expectedName LibraryName, libEnv *environment.EnvironmentFrame, filePath string, evaluator machine.MacroEvaluator) (*CompiledLibrary, error) {
	// Create a template for the top-level compilation (will be empty after define-library)
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Expand the form. WithMaxExpandDepth is honored uniformly by
	// NewExpanderTimeContinuation, which reads the engine-configured bound from
	// the env's namespace — so both this structural expansion and the body's
	// compile-time macro re-expansion (through per-site expanders) obey it.
	expanded, err := NewExpanderTimeContinuation(ctx, libEnv, evaluator).ExpandExpression(stx)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "error expanding library"))
	}

	// Compile the form
	// Use inTail=false for top-level expressions
	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompileTimeContinuation(tpl, libEnv, evaluator)

	// Honor the engine's WithInlineThreshold for library code. Every in-process
	// child compiler re-threads the parent's threshold explicitly, but this
	// runtime-triggered compile reaches the library env through the namespace, so
	// read the engine-configured value from there. When unset (a namespace not
	// built by an Engine, e.g. a direct LoadLibrary in a unit test) fall back to
	// the compiler default, preserving prior behavior.
	inlineThreshold, ok := libEnv.Namespace().InlineThreshold()
	if !ok {
		inlineThreshold = DefaultInlineThreshold
	}
	compiler.SetInlineThreshold(inlineThreshold)

	// Set up to capture the compiled library
	var compiledLib *CompiledLibrary
	compiler.SetLibraryCallback(func(lib *CompiledLibrary) {
		compiledLib = lib
	})

	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "error compiling library"))
	}

	if compiledLib == nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "library was not produced by compilation"))
	}

	// Verify the library name matches what was expected
	if compiledLib.Name.Key() != expectedName.Key() {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(werr.ErrLibraryNameMismatch, "library name mismatch: expected %s, got %s",
			expectedName.SchemeString(), compiledLib.Name.SchemeString()))
	}

	// Record the source file for error messages
	compiledLib.SourceFile = filePath

	// Let the registry's compile observer see the template before any of it
	// runs: coverage instruments here, and a hook after EvalTemplate would
	// find every top-level library form already executed-but-unrecorded.
	fireCompileObserver(libEnv, compiledLib)

	// Execute the library's compiled template to populate bindings
	// The library's code (begin blocks, defines) is in compiledLib.Template
	if compiledLib.Template != nil && compiledLib.Template.CodeLen() > 0 {
		_, err = evaluator.EvalTemplate(ctx, compiledLib.Template, compiledLib.Env)
		if err != nil {
			return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "error executing library"))
		}
	}

	return compiledLib, nil
}
