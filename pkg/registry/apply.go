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

package registry

import (
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ApplyOption configures the behavior of Registry.Apply. Options are applied
// in order; later options override earlier ones.
type ApplyOption func(*applyConfig)

// applyConfig holds tunables for Apply. New knobs should extend this struct
// and be set via an ApplyOption constructor.
type applyConfig struct {
	contractEnforcement bool
	stableBase          bool
	runtimeTarget       *environment.EnvironmentFrame
}

// WithContractEnforcement installs a type-checking validator on each
// registered primitive whose spec declares ParamTypes. The validator runs
// after argument binding and before the implementation, rejecting
// mismatched types with a wrapped error. Disabled by default — validators
// cost nothing when not installed (ForeignClosure.validate stays nil).
func WithContractEnforcement() ApplyOption {
	return func(cfg *applyConfig) {
		cfg.contractEnforcement = true
	}
}

// WithStableBasePrimitives stamps every capture-safe primitive
// (!spec.InvokesProcedure: +, -, car, cons, assq, vector-ref, sqrt, …) Stable when
// it is bound ambiently, so the frame-reclaim classifier may trust calls to it as
// non-rebindable without an explicit (import (scheme base)). Imported primitives
// are already immutable; this closes the ambient-registration path, where
// Registry.Apply binds primitives directly into the base namespace without an
// Imported flag (Phase 2 finding #1).
//
// Scope is the capture-safe set, which must match CaptureSafe (stamped above from
// the same !spec.InvokesProcedure): the classifier trusts a primitive callee only
// when CaptureSafe AND Stable both hold, so a capture-safe primitive left
// un-stamped here would be CaptureSafe yet never trusted. Procedure-invoking
// primitives (apply, map, sort, eval, with-exception-handler, …) are
// InvokesProcedure:true, so they are neither CaptureSafe nor stamped Stable and
// stay R7RS-mutable even under the flag.
//
// Disabled by default. The engine appends it only under WithImmutableTopLevel(),
// where the set!/redefine enforcement (compile_validated.go) makes Stable a
// guarantee the classifier may rest a verdict on. The deviation it introduces —
// capture-safe primitives become non-rebindable — is exactly the opt-in
// optimization contract.
func WithStableBasePrimitives() ApplyOption {
	return func(cfg *applyConfig) {
		cfg.stableBase = true
	}
}

// WithRuntimeTarget routes PhaseRuntime primitive registration and global values
// into the given frame instead of env. Used by bootstrap to seat primitives in the
// immutable sealed base while expand-phase prims stay in env.Expand() and compile-time
// bindings stay in env.Compile(). Defaults to env (backward compatible — a flat library
// env passes its own frame, the engine root passes its sealed base).
func WithRuntimeTarget(frame *environment.EnvironmentFrame) ApplyOption {
	return func(c *applyConfig) {
		c.runtimeTarget = frame
	}
}

// Apply materializes registry contents into an environment, in order: compile-time
// bindings, runtime/expand-time primitives, global values, per-engine namespace
// initializers, then init functions.
func (p *Registry) Apply(ctx context.Context, env *environment.EnvironmentFrame, opts ...ApplyOption) error {
	var cfg applyConfig
	for _, opt := range opts {
		opt(&cfg)
	}

	p.mu.RLock()
	defer p.mu.RUnlock()

	// Register compile-time bindings first. DocOnly entries carry doc
	// strings but install no binding — they're emitted to bindingSpecs
	// by AddDocumentation, consumed only by ApplyDocs / SearchDoc.
	for _, spec := range p.bindingSpecs {
		if spec.DocOnly {
			continue
		}
		err := registerCompileTimeBinding(env, spec)
		if err != nil {
			return err
		}
	}

	// Register compile-only primitives (binding-only, no runtime value).
	// Skipped for primitives that also have PhaseRuntime — the runtime path
	// installs the binding via SetOwnGlobalValue.
	for _, reg := range p.primitives {
		if reg.Phases.Has(environment.PhaseCompile) && !reg.Phases.Has(environment.PhaseRuntime) {
			err := registerCompileTimeBinding(env, BindingSpec{Name: reg.Spec.Name})
			if err != nil {
				return err
			}
		}
	}

	// Register runtime and expand primitives. Both create a ForeignClosure; the phase
	// axis is iterated as data instead of replicating the loop body. Two frames matter
	// per phase:
	//   - bindingEnv: where the binding lives. PhaseRuntime → the sealed base when
	//     WithRuntimeTarget is set (immutable), else env. PhaseExpand → env.Expand().
	//   - closureEnv: what the ForeignClosure captures as its lexical env — the frame a
	//     foreign fn resolves user code against via mc.EnvironmentFrame(). This MUST be a
	//     frame that reaches user defines: the MUTABLE runtime (env), NOT the sealed base.
	//     Primitives like compile/expand/free-identifier=? resolve user-level names
	//     through this env; capturing the sealed base would hide every user define. Only
	//     the binding location is sealed for immutability — resolution stays merged.
	// For a flat library env (no carve) bindingEnv == closureEnv == env, so behavior is
	// unchanged. Compile-time bindings stay on env.Compile() (the carve is phase-0-only).
	runtimeEnv := env
	if cfg.runtimeTarget != nil {
		runtimeEnv = cfg.runtimeTarget
	}
	expandEnv := env.Expand()
	phaseTargets := []struct {
		phase      environment.Phase
		bindingEnv *environment.EnvironmentFrame
		closureEnv *environment.EnvironmentFrame
	}{
		{environment.PhaseRuntime, runtimeEnv, env},
		{environment.PhaseExpand, expandEnv, expandEnv},
	}
	// First-wins on a duplicate name+phase, matching FindPrimitive's first-match
	// lookup. Without the bound set this loop is last-wins: registerPhasePrimitive
	// ends in SetOwnGlobalValue, which overwrites, so a later duplicate would replace
	// both the closure and the CaptureSafe/Stable stamps while ,doc and every other
	// FindPrimitive caller kept describing the first. Same registry, two answers.
	//
	// Precedence is per phase: a name may legitimately be registered at runtime by one
	// spec and at expand by another, and those are not duplicates of each other.
	for _, pt := range phaseTargets {
		bound := make(values.StringSet, len(p.primitives))
		for _, reg := range p.primitives {
			if !reg.Phases.Has(pt.phase) {
				continue
			}
			_, dup := bound[reg.Spec.Name]
			if dup {
				continue
			}
			bound[reg.Spec.Name] = struct{}{}
			err := registerPhasePrimitive(pt.bindingEnv, pt.closureEnv, pt.phase, reg.Spec, cfg)
			if err != nil {
				return err
			}
		}
	}

	// Register global values into the runtime target (sealed base when carving).
	// Today's only callers are the math extension's pi/euler constants. None get the
	// Stable stamp, so a user (set! pi ...) stays permitted. The I/O port parameters
	// used to live here; they now bind via AddNamespaceInit (extensions/io/register.go,
	// registerPortParam) so each engine gets its own.
	for _, gv := range p.globalValues {
		err := registerGlobalValue(runtimeEnv, gv.Name, gv.Value)
		if err != nil {
			return err
		}
	}

	// Run per-engine namespace initializers with this engine's runtime frame.
	// Extensions use these to build per-Namespace state (e.g. the I/O port
	// parameters + caches) that must not be shared across engines.
	for _, fn := range p.namespaceInits {
		err := fn(runtimeEnv)
		if err != nil {
			return err
		}
	}

	// Run initialization functions
	for _, f := range p.initFuncs {
		err := f()
		if err != nil {
			return err
		}
	}

	return nil
}

//nolint:unparam // Returns error for consistency with other register functions
func registerCompileTimeBinding(env *environment.EnvironmentFrame, spec BindingSpec) error {
	compileEnv := env.Compile()
	sym := values.NewSymbol(spec.Name)
	compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive, nil)
	return nil
}

// registerPhasePrimitive installs a ForeignClosure for spec. The binding (and any
// Stable stamp) lives in bindingEnv; the closure captures closureEnv as its lexical env
// (the frame a foreign fn resolves user code against). These differ only for sealed
// runtime primitives: the binding is sealed (bindingEnv = sealed base) while the closure
// resolves through the mutable runtime (closureEnv = env) so compile/expand/identifier
// primitives still see user defines. The phase parameter is error-message context only.
// This is the single registration helper shared by Runtime and Expand phases — collapsed
// per Instance C of the dispatch-axis-as-data finding (plans/2026-05-08-dispatch-axis-as-data.md).
func registerPhasePrimitive(bindingEnv, closureEnv *environment.EnvironmentFrame, phase environment.Phase, spec PrimitiveSpec, cfg applyConfig) error {
	sym := values.NewSymbol(spec.Name)

	closure := machine.NewForeignClosure(
		closureEnv,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)
	closure.SetDoc(spec.Doc)
	// Every environment that applies this registry — the engine's sealed base and
	// each flat library env — mints its own closure here. The identity is what ties
	// those copies together, so a recognizer sees one primitive rather than one
	// object per environment.
	closure.SetIdentity(spec.Identity)
	if cfg.contractEnforcement {
		closure.SetValidator(BuildValidator(spec))
	}

	err := bindingEnv.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s at phase %s", spec.Name, phase)
	}

	// Address the binding just written, not the name's first slot. A GlobalIndex
	// built from the bare symbol resolves MATCH ANY, so the stamps below would
	// land on a hygiene-distinct binding of the same name if one existed. The
	// empty query resolves the ambient (unscoped) binding, which is the one
	// registration just defined.
	gi := bindingEnv.GetGlobalIndexWithScopes(sym, values.EmptyScopes())
	if gi == nil {
		return werr.WrapForeignErrorf(
			werr.ErrNoSuchBinding,
			"registerPhasePrimitive: binding for %s vanished after registration at phase %s",
			spec.Name, phase,
		)
	}

	// A nil binding here is an invariant violation — DefineOwnGlobal just
	// succeeded against this same name — so surface it rather than silently skipping
	// the stamps below.
	b := bindingEnv.GetGlobalBinding(gi)
	if b == nil {
		return werr.WrapForeignErrorf(
			werr.ErrNoSuchBinding,
			"registerPhasePrimitive: binding for %s vanished after registration at phase %s",
			spec.Name, phase,
		)
	}

	// CaptureSafe is a static capability — does this primitive invoke a Scheme
	// procedure? — stamped unconditionally from !spec.InvokesProcedure, independent
	// of the immutable-top-level flag (unlike Stable below). The frame-reclaim
	// classifier pairs it with IsStable() to trust a primitive callee. (No reader
	// until the classifier consults it; see validate's classifyCallee.)
	b.UpdateMeta(func(m *environment.BindingMeta) bool {
		m.CaptureSafe = !spec.InvokesProcedure
		return true
	})

	// Opt-in (WithStableBasePrimitives): mark the binding Stable so the
	// frame-reclaim classifier trusts it as non-rebindable, for every capture-safe
	// primitive — !spec.InvokesProcedure, the same capability stamped into
	// CaptureSafe above and read by the classifier's gate. The classifier trusts a
	// primitive callee only when CaptureSafe AND Stable both hold, so the two stamps
	// must cover the same set or a capture-safe primitive (e.g. assq) would be
	// CaptureSafe yet not Stable and never trusted. Stamping a procedure-invoking
	// primitive here would be pointless (it is not CaptureSafe) and is excluded. The
	// set!-gate (compile_validated.go) then makes the trust a guarantee.
	if cfg.stableBase && !spec.InvokesProcedure {
		b.UpdateMeta(func(m *environment.BindingMeta) bool {
			m.Stable = true
			return true
		})
	}
	return nil
}

func registerGlobalValue(env *environment.EnvironmentFrame, name string, value values.Value) error {
	sym := values.NewSymbol(name)
	err := env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, value)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering global value %s", name)
	}
	return nil
}

// ApplyDocs attaches documentation entries to existing bindings in the environment.
// It searches all phases for each documented name and sets the doc string on every
// matching binding. This is necessary because some names (e.g., special forms) have
// bindings in multiple phases (expand and compile), and the REPL's ,doc command may
// find any of them.
//
// Post-Phase-1: single walk over bindingSpecs (both real bindings with non-empty
// Doc and DocOnly entries land here). The earlier two-source merge — `docs` slice
// + bindingSpecs casted via DocEntry(spec) — collapsed when DocEntry was unified
// into BindingSpec.
func (p *Registry) ApplyDocs(env *environment.EnvironmentFrame) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	topLevel := env.Namespace()
	if topLevel == nil {
		return
	}
	phases := topLevel.Phases()

	for _, spec := range p.bindingSpecs {
		if spec.Doc == "" {
			continue
		}
		sym := values.NewSymbol(spec.Name)
		for _, phase := range phases.Phases() {
			phaseEnv := phases.Get(phase)
			if phaseEnv == nil {
				continue
			}
			bnd := phaseEnv.GetBinding(sym, values.AllScopes())
			if bnd != nil {
				bnd.UpdateMeta(func(m *environment.BindingMeta) bool {
					m.Doc = spec.Doc
					return true
				})
			}
		}
	}
}
