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

// ApplyOption configures the behavior of PrimitiveRegistry.Apply. Options are applied
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
// PrimitiveRegistry.Apply binds primitives directly into the base namespace without an
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
// into the given frame instead of env. frame is the sealed-write root view — used
// by bootstrap to seat primitives in the immutable sealed-write view. This option
// covers phase 0 only: expand-phase prims go to the phase-1 sealed-write view,
// which Apply derives from env itself, and compile-time bindings land ambient
// (registerCompileTimeBinding). Defaults to env when unset, but LoadBootstrapCore
// always sets it, for the engine root and every library env alike, each into its OWN
// phase-0 sealed-write view (SealedWriteViewAt(PhaseRuntime)); there is no
// shared "sealed base" and no library env skips the carve.
func WithRuntimeTarget(frame *environment.EnvironmentFrame) ApplyOption {
	return func(c *applyConfig) {
		c.runtimeTarget = frame
	}
}

// Apply materializes registry contents into an environment, in order: compile-time
// bindings, runtime/expand-time primitives, global values, per-engine namespace
// initializers, then init functions.
//
// env must be an owner root (the phase-0 entry of its own PhaseRegistry), as
// LoadBootstrapCore passes for the engine root and for every library env. Any
// other receiver makes SealedWriteViewAt fall back to that receiver's own mutable
// view, which lands every compile-time keyword at (0, mutable) instead of the
// ambient (ANY, sealed) coordinate; a later user define of the name would then
// reuse the keyword's slot rather than shadow it.
func (p *PrimitiveRegistry) Apply(ctx context.Context, env *environment.EnvironmentFrame, opts ...ApplyOption) error {
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

	// Register runtime and expand primitives. Both create a ForeignClosure; the phase
	// axis is iterated as data instead of replicating the loop body. Two frames matter
	// per phase:
	//   - bindingEnv: where the binding lives. PhaseRuntime → the owner's sealed-write
	//     view when WithRuntimeTarget is set (immutable; LoadBootstrapCore always sets
	//     it, for the engine root and every library env alike), else env. PhaseExpand →
	//     env.SealedWriteViewAt(PhaseExpand), the phase-1 half of the same carve: a
	//     top-level define-for-syntax of this name then creates a distinct (1, mutable)
	//     shadow instead of writing through this slot, exactly as a phase-0 define
	//     shadows a sealed primitive. RegisterPrimitiveExpanders takes the same target
	//     for the same reason (primitive_expanders_registry.go).
	//   - closureEnv: what the ForeignClosure captures as its lexical env — the frame a
	//     foreign fn resolves user code against via mc.EnvironmentFrame(). This MUST be a
	//     frame that reaches user defines: the MUTABLE runtime (env), NOT the sealed-write
	//     view. Primitives like compile/expand/free-identifier=? resolve user-level names
	//     through this env; capturing the sealed-write view would hide every user define.
	//     Only the binding location is sealed for immutability — resolution stays merged.
	// Compile-time bindings land at the ambient coordinate (registerCompileTimeBinding).
	// The expand phase always carves to its sealed-write view; the runtime phase does
	// so only when the caller supplies WithRuntimeTarget, which LoadBootstrapCore
	// always does and a bare reg.Apply does not.
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
		{environment.PhaseExpand, env.SealedWriteViewAt(environment.PhaseExpand), expandEnv},
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
		bound := values.NewStringSet(len(p.primitives))
		for _, reg := range p.primitives {
			if !reg.Phases.Has(pt.phase) {
				continue
			}
			dup := bound.ContainsOne(reg.Spec.Name)
			if dup {
				continue
			}
			bound.Set(reg.Spec.Name)
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

// registerCompileTimeBinding installs a compile-time-only name (an auxiliary
// keyword such as else or =>, or a special-form name carrying its docstring) as
// a valueless BindingTypePrimitive binding at the owner's AMBIENT coordinate: the
// (ANY, sealed) write that only the phase-0 sealed-write view produces
// (EnvironmentFrame.writeCoordinates). Ambient is what these names are: fixed,
// owner-wide, reachable from a frame at every level as the ranked probe's T3, and
// shadowed by a same-phase user define through the same T1 > T2 > T3 order that
// lets user code shadow car. RegisterSyntaxCompilers writes the syntax compilers
// through the same view, so a name in both tables (define-syntax, import, …) is
// ONE binding: created here, its compiler value written in afterwards.
//
// The value path refuses these on sight: refuseCompileTimeMeaning keys on
// BindingType, so (display if) is "syntactic keyword used as a variable" rather
// than the phase-2 era's "no such binding".
//
// env must be an owner root (the phase-0 entry of its own PhaseRegistry), which
// every production Apply passes (LoadBootstrapCore). For any other receiver
// SealedWriteViewAt falls back to the receiver's own mutable view, and the keyword
// would land at (0, mutable), where a later user define of the name would reuse
// its slot.
//
//nolint:unparam // Returns error for consistency with other register functions
func registerCompileTimeBinding(env *environment.EnvironmentFrame, spec BindingSpec) error {
	ambient := env.SealedWriteViewAt(environment.PhaseRuntime)
	sym := values.NewSymbol(spec.Name)
	ambient.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive, nil)
	return nil
}

// registerPhasePrimitive installs a ForeignClosure for spec. The binding (and any
// Stable stamp) lives in bindingEnv; the closure captures closureEnv as its lexical env
// (the frame a foreign fn resolves user code against). The two differ at BOTH phases
// whenever the binding is sealed: the binding goes to a sealed-write view while the
// closure resolves through the matching mutable frame (env at phase 0, env.Expand() at
// phase 1) so compile/expand/identifier primitives still see user defines. Phase 0 is
// sealed only under WithRuntimeTarget; phase 1 always is. The phase parameter is
// error-message context only.
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
	// Every owner that applies this registry — the engine root and each library
	// env — mints its own closure here, into its own sealed-write view. The
	// identity is what ties those copies together, so a recognizer sees one
	// primitive rather than one object per environment.
	closure.SetIdentity(spec.Identity)
	if cfg.contractEnforcement {
		closure.SetValidator(BuildValidator(spec))
	}

	// The index is the one the write itself used — the slot the create PINNED at
	// this view's coordinates — so the stamps below address the binding just
	// written, not the name's first slot and not whatever a ranked read would
	// answer. Both of those go wrong here: a bare-symbol index resolves MATCH ANY
	// and would stamp a hygiene-distinct binding of the same name if one existed,
	// and a ranked read from the sealed-write view would prefer a same-named
	// MUTABLE entry.
	gi, err := bindingEnv.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s at phase %s", spec.Name, phase)
	}

	// A nil binding here is an invariant violation — DefineOwnGlobal just
	// succeeded against this same name — so surface it rather than silently skipping
	// the stamps below.
	b := bindingEnv.GlobalEnvironment().GetOwnGlobalBinding(gi)
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
	//
	// Stable asserts the writer set for this slot is closed, and the argument now
	// runs the same way at both phases — but it takes two steps, not one, because
	// "bindingEnv is a sealed-write view" alone does not close the set.
	//
	//  1. USER code never addresses these coordinates. A define lands at
	//     (0, mutable) and a define-for-syntax at (1, mutable); both are other
	//     coordinates, hence shadows. Before the phase-1 carve the expand row wrote
	//     through env.Expand(), so this step was simply false there: a top-level
	//     define-for-syntax superseded this very slot with the stamp still on it.
	//  2. BOOTSTRAP Scheme does address them — a registry macro/procedure source
	//     compiles with env == the owner's phase-0 seal, so its define-syntax and
	//     define-for-syntax write one phase up, into the (1, sealed) view this row
	//     targets. Those sites go through createPhaseBindingUnlessStable
	//     (compile_define_syntax.go), which refuses a Stable slot, so a registry
	//     that supplies both an expand-phase primitive and a bootstrap macro of one
	//     name fails engine construction instead of overwriting the primitive.
	//
	// Phase 0 additionally needs the caller: LoadBootstrapCore pairs
	// WithStableBasePrimitives with WithRuntimeTarget, and without the latter the
	// phase-0 row binds into the MUTABLE env and step 1 asserts something false
	// there.
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
	_, err := env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, value)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering global value %s", name)
	}
	return nil
}

// ApplyDocs attaches documentation entries to existing bindings in the environment.
// It searches every instantiated phase view for each documented name and sets the
// doc string on every matching binding it finds.
//
// One name can be several bindings, so stopping at the first would leave the
// rest undocumented and the REPL's ,doc command may reach any of them. What
// spreads a name across views is the phase axis, not the keyword relocation: a
// special-form name like define or syntax-case is BOTH the ambient keyword every
// phase view reaches as T3 and a distinct phase-1 primitive expander
// (RegisterPrimitiveExpanders), two live slots for one name. Bootstrap macros
// bind at phase 1 the same way.
//
// Post-Phase-1: single walk over bindingSpecs (both real bindings with non-empty
// Doc and DocOnly entries land here). The earlier two-source merge (`docs` slice
// plus bindingSpecs casted via DocEntry(spec)) collapsed when DocEntry was
// unified into BindingSpec.
func (p *PrimitiveRegistry) ApplyDocs(env *environment.EnvironmentFrame) {
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
