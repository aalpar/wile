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
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// NamespaceMaxExpandDepth returns the engine's configured WithMaxExpandDepth
// bound as seen from env, else DefaultMaxExpandDepth. Nil-safe: frames with no
// namespace exist (test fixtures), and a namespace not built by an Engine reports
// unset. An explicit 0 (unlimited) is returned as 0. This is the reader for every
// site except the bootstrap pass (runBootstrapStx), which uses the default.
func NamespaceMaxExpandDepth(env *environment.EnvironmentFrame) int {
	ns := env.Namespace()
	if ns == nil {
		return DefaultMaxExpandDepth
	}
	n, ok := ns.MaxExpandDepth()
	if !ok {
		return DefaultMaxExpandDepth
	}
	return n
}

// NamespaceInlineThreshold is NamespaceMaxExpandDepth's counterpart for
// WithInlineThreshold, defaulting to DefaultInlineThreshold.
func NamespaceInlineThreshold(env *environment.EnvironmentFrame) int {
	ns := env.Namespace()
	if ns == nil {
		return DefaultInlineThreshold
	}
	n, ok := ns.InlineThreshold()
	if !ok {
		return DefaultInlineThreshold
	}
	return n
}

// ExpandAndCompile runs the expand+compile pipeline on a single syntax value,
// returning a ready-to-execute template.
//
// A single MacroEvaluator is shared across both phases. Configuration is read
// from env's namespace, as at every nested site: include/load resolve through
// env.FileResolver(), inlining follows NamespaceInlineThreshold (0 disables), and
// expander recursion is bounded by NamespaceMaxExpandDepth (0 disables), which
// prevents a fatal Go stack overflow on deeply nested syntax.
// Errors are wrapped with phase context ("expansion" or "compilation"); callers
// may add site-specific context on top.
//
// The returned template is NOT peephole-optimized; closure bodies inside it are.
// See ExpandAndCompileOptimized for when the top-level pass pays.
func ExpandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	return expandAndCompileScoped(ctx, env, stx, nil, NamespaceInlineThreshold(env), NamespaceMaxExpandDepth(env), nil)
}

// ExpandAndCompileOptimized is ExpandAndCompile followed by the peephole pass on
// the top-level template. Closure bodies are already optimized as they compile,
// so the pass only buys the top-level template's own fusions. Callers: the
// Engine's compile funnel (shared by Eval and Compile, whose CompiledCode can be
// Run repeatedly) and the compile primitive, whose thunk runs the template on
// every call; bootstrap loading takes the same pass through runBootstrapStx. The
// eval and load primitives run each template once and discard it, so they use
// ExpandAndCompile and skip the pass's allocations.
func ExpandAndCompileOptimized(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl, err := ExpandAndCompile(ctx, env, stx)
	if err != nil {
		return nil, err
	}
	tpl.Optimize()
	return tpl, nil
}

// expandAndCompileScoped is ExpandAndCompile with a library scope set on both
// halves of the pipeline. A transformer right-hand side compiled inside a
// define-library body needs it: the expander threads it to nested define-syntax
// (expander_body.go) and the compiler to CompileSyntaxRulesExpr and, from P0.3,
// CompileQuoteSyntax. nil is the ordinary top-level case.
//
// resolver, inlineThreshold and maxExpandDepth exist for the bootstrap pass
// alone: it compiles core sources through an embed resolver of their own and at
// the package defaults, without touching env's namespace, which a library env
// shares with the running engine. A nil resolver leaves the compiler on
// env.FileResolver(); every other caller passes the namespace readers.
//
// It is also the unit boundary for top-level slots. Everything the unit mints
// for a name it defines (the expander's predeclaration, the define's own
// declaration) is logged in one unitMints, and a unit that does not return its
// template retracts them, so a failed define leaves the name unbound rather
// than bound to #<void>. The deferred call covers a panic unwinding through
// here as well as an error return.
func expandAndCompileScoped(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, inlineThreshold int, maxExpandDepth int, libraryScope *syntax.Scope) (q *machine.NativeTemplate, rerr error) {
	evaluator := machine.NewVMMacroEvaluator()
	mints := &unitMints{store: env.GlobalEnvironment()}
	defer func() {
		if q == nil {
			mints.retract()
		}
	}()

	expander := NewExpanderTimeContinuation(ctx, env, evaluator)
	expander.libraryScope = libraryScope
	expander.mints = mints
	expander.SetMaxDepth(maxExpandDepth)
	expanded, err := expander.ExpandTopLevelExpression(stx)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "expansion"))
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompileTimeContinuation(tpl, env, evaluator)
	compiler.libraryScope = libraryScope
	compiler.mints = mints
	if resolver != nil {
		compiler.fileResolver = resolver
	}
	compiler.SetInlineThreshold(inlineThreshold)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "compilation"))
	}

	mints.store.NoteUnitCommit(env.PhaseLevel())
	return tpl, nil
}

// unitMints is one expand+compile unit's log of the top-level slots it
// created, kept so that a unit which fails can retract them.
//
// Which slots it may retract is decided by structure, not by inspecting
// compiled code. A slot belongs here only if this unit's own create answered
// created == true in the unit's own store, so it did not exist before the unit
// began; a redefinition reuses the old slot and is never logged, which is what
// keeps a failed redefine's old value. Code that loads such a slot can only
// have been compiled after it was minted. Code compiled by this unit lives in
// the template the failing unit never returns, so it never runs. Code compiled
// by ANOTHER unit in the window can hold it: a transformer that evaluates into
// the same environment commits a template while this unit is still expanding,
// and that template may cache the *Binding (OpLoadCachedBinding), which a
// retraction would orphan at #<void> while a later define of the name lands on
// a fresh slot. So each entry records the store's commit count at its slot's
// phase when it was minted, and is retracted only if no unit has committed at
// that phase since. Phases are hermetic for resolution, so a commit at another
// phase (every define-syntax right-hand side is one, at phase+1) cannot hold
// it and does not block. A by-index load cannot be orphaned: it misses, or
// heals onto a later slot (RetractGlobalBinding).
//
// Not covered: a unit compiling concurrently on another SRFI-18 thread that
// resolves the slot during the window and commits after this unit has
// retracted it. A cached load there reads the orphan's #<void>; a define of the
// same name there reuses the slot without logging it (created == false), and
// its pinned store then finds nothing at those coordinates and fails at run
// time with ErrBindingNotFound. Closing it needs the check and the delete to be
// one step against every resolver, which the store does not offer.
//
// nil is a valid log that records nothing: every compiler and expander outside
// a unit (library bodies, compile-time halves) carries one.
type unitMints struct {
	store *environment.GlobalEnvironmentFrame
	slots []mintedSlot
}

// mintedSlot is one unitMints entry: the pin and binding the create returned,
// and the store's commit count at the slot's phase at that moment.
type mintedSlot struct {
	gi      *environment.GlobalIndex
	binding *environment.Binding
	commits uint64
}

// note logs gi when created reports that the call creating it minted it here.
// A slot in another store (a library body's own) is not this unit's.
func (p *unitMints) note(gi *environment.GlobalIndex, created bool) {
	if p == nil || !created || gi == nil || gi.Env != p.store {
		return
	}
	b := p.store.GetOwnGlobalBinding(gi)
	if b == nil {
		return
	}
	p.slots = append(p.slots, mintedSlot{gi: gi, binding: b, commits: p.store.UnitCommits(gi.Phase())})
}

// retract removes every logged slot no other unit can be holding, newest
// first.
func (p *unitMints) retract() {
	for _, s := range slices.Backward(p.slots) {
		if p.store.UnitCommits(s.gi.Phase()) != s.commits {
			continue
		}
		p.store.RetractGlobalBinding(s.gi, s.binding)
	}
}
