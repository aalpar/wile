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

package bootstrap

import (
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/werr"
)

// LoadBootstrapCore runs the ordering-sensitive sequence that turns a bare frame plus a
// built registry into a usable base environment, and returns the runtime target frame
// the caller must use for any further sealed-base work.
//
// The five steps are order-dependent and each depends on the one before it:
//
//  1. Apply the registry, routing runtime primitives to the runtime target.
//  2. Register phase handlers (syntax compilers + primitive expanders).
//  3. Load bootstrap macros against the runtime target (the sealed base for a
//     namespace-owning env). Their define-syntax writes land in sealedExpandBase, so
//     they are immutable and a user define-syntax shadows in the mutable expand child.
//     These must precede the procedures, which are written in terms of them (let, and).
//  4. Load bootstrap procedures into the runtime target.
//  5. Load the LATE macros, which reference bootstrap procedures (unless -> not,
//     guard -> with-exception-handler). Loading them after step 4 is what makes their
//     free identifiers pin to the sealed base instead of taking a nil pin and
//     degrading to use-site resolution (R7RS 4.3.2).
//
// This exists because the sequence had been duplicated in two places — this package's
// initializeEnvironmentWithRegistry and pkg/wile's applyBaseEnvironment — that had to be
// kept in step by comment alone. On 2026-07-18 step 5 was added to only the first; unless
// and guard became undefined for the CLI and every embedder while every Go unit test
// stayed green, because the test helpers reach bootstrap through this package. A single
// definition makes that divergence unrepresentable rather than merely discouraged.
//
// What deliberately stays with the callers, because it genuinely differs between them:
// registry CONSTRUCTION (this package builds a fixed all-extensions registry; the Engine
// receives a filtered per-profile one), and post-steps (the Engine stamps inline HOFs,
// builds loop templates, and injects documentation). Errors are wrapped with terse
// context here and re-wrapped by each caller in its own idiom, so pkg/wile keeps its
// ErrEngineInit contract and this package keeps its plainer one.
func LoadBootstrapCore(ctx context.Context, env *environment.EnvironmentFrame, reg *registry.PrimitiveRegistry, opts ...registry.ApplyOption) (*environment.EnvironmentFrame, error) {
	// Runtime primitives and bootstrap procedures are phase-0 VALUES, so they route to
	// the phase-0 seal for a namespace-owning runtime env (engine root / profile child)
	// and to the frame itself for a flat library env. WithRuntimeTarget(self) is a
	// no-op, so the library path is unchanged. A fresh slice avoids racing the shared
	// opts backing array across concurrent library-env creation.
	runtimeTarget := env.SealedTargetAt(environment.PhaseRuntime, environment.SealKindValue)
	applyOpts := make([]registry.ApplyOption, 0, len(opts)+1)
	applyOpts = append(applyOpts, opts...)
	applyOpts = append(applyOpts, registry.WithRuntimeTarget(runtimeTarget))

	err := reg.Apply(ctx, env, applyOpts...)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "LoadBootstrapCore: apply registry")
	}

	err = compilation.RegisterAllPhaseHandlers(env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "LoadBootstrapCore: register phase handlers")
	}

	bootstrapResolver := compilation.NewEmbedFileResolver(core.BootstrapFS)
	// Bootstrap macros load against runtimeTarget (= the sealed base for a namespace-owning
	// env, the flat frame itself for a library env), symmetric with loadBootstrapProcedures
	// below. Compiling a define-syntax with env == sealedBase makes its NextPhase() write
	// land in sealedExpandBase (via the AtPhase receiver branch), so a bootstrap macro is
	// immutable and a later user (define-syntax foo …) shadows in the mutable expand child
	// instead of overwriting it in place. For a library env runtimeTarget == env, so this is
	// unchanged there.
	err = loadBootstrapMacros(ctx, runtimeTarget, reg.MacroSources(), bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "LoadBootstrapCore: load bootstrap macros")
	}

	err = loadBootstrapProcedures(ctx, runtimeTarget, reg.ProcedureSources(), bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "LoadBootstrapCore: load bootstrap procedures")
	}

	err = loadBootstrapMacros(ctx, runtimeTarget, []string{core.LateBootstrapMacroSource}, bootstrapResolver)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "LoadBootstrapCore: load late bootstrap macros")
	}

	return runtimeTarget, nil
}
