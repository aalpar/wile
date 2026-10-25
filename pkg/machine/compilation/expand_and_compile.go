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
// than bound to #<void>. The unit is registered with the store for its whole
// span (BeginUnit/EndUnit), which is what lets a failing unit tell whether
// anyone else can hold its slots. The deferred call covers a panic unwinding
// through here as well as an error return, and retracts before EndUnit so the
// failing unit still counts itself in flight.
func expandAndCompileScoped(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, inlineThreshold int, maxExpandDepth int, libraryScope *syntax.Scope) (q *machine.NativeTemplate, rerr error) {
	evaluator := machine.NewVMMacroEvaluator()
	mints := &unitMints{store: env.GlobalEnvironment(), phase: env.PhaseLevel()}
	mints.store.BeginUnit(mints.phase)
	defer func() {
		if q == nil {
			mints.retract()
		}
		mints.store.EndUnit(mints.phase, q != nil)
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

	return tpl, nil
}

// unitMints is one expand+compile unit's log of the top-level slots it
// created, kept so that a unit which fails can retract them.
//
// Which slots it may retract is decided by structure, not by inspecting
// compiled code. A slot belongs here only if this unit's own create answered
// created == true in the unit's own store at the unit's own phase, so it did
// not exist before the unit began; a redefinition reuses the old slot and is
// never logged, which is what keeps a failed redefine's old value. Code that
// loads such a slot can only have been compiled after it was minted. Code
// compiled by this unit lives in the template the failing unit never returns,
// so it never runs. Code compiled by ANOTHER unit at the same phase can hold
// it: one that committed in the window (a transformer that evaluates into the
// same environment commits while this unit is still expanding), or one still
// in flight on another SRFI-18 thread. Each entry records the store's commit
// count when it was minted, and RetractGlobalBinding removes the slot only if
// that count is unchanged and this unit is the only one in flight, checked
// under the same lock hold as the removal; otherwise it keeps the slot as a
// placeholder. Phases are hermetic for resolution, so a unit at another phase
// (every define-syntax right-hand side is one, at phase+1) cannot hold it and
// does not block.
//
// nil is a valid log that records nothing: every compiler and expander outside
// a unit (library bodies, compile-time halves) carries one.
type unitMints struct {
	store *environment.GlobalEnvironmentFrame
	phase environment.Phase
	slots []mintedSlot
}

// mintedSlot is one unitMints entry: the pin and binding the create returned,
// and the store's commit count at the unit's phase at that moment.
type mintedSlot struct {
	gi      *environment.GlobalIndex
	binding *environment.Binding
	commits uint64
}

// note logs gi when created reports that the call creating it minted it here.
// A slot in another store (a library body's own) or at another phase is not
// this unit's: the in-flight and commit counts that guard it are kept per
// phase, and this unit is counted only at its own.
func (p *unitMints) note(gi *environment.GlobalIndex, created bool) {
	if p == nil || !created || gi == nil || gi.Env != p.store || gi.Phase() != p.phase {
		return
	}
	b := p.store.GetOwnGlobalBinding(gi)
	if b == nil {
		return
	}
	p.slots = append(p.slots, mintedSlot{gi: gi, binding: b, commits: p.store.UnitCommits(p.phase)})
}

// retract removes every logged slot no other unit can be holding, and reverts
// the rest to placeholders, newest first.
func (p *unitMints) retract() {
	for _, s := range slices.Backward(p.slots) {
		p.store.RetractGlobalBinding(s.gi, s.binding, s.commits)
	}
}
