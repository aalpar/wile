# Engine Initialization Order Invariant

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Document and test the implicit 6-step initialization order in `NewEngine` so future refactors can't silently reorder dependent steps.

**Architecture:** Add an invariant block comment to `engine.go` documenting the dependency DAG, then write internal (package `wile`) negative tests that construct a namespace with steps omitted and verify the expected failures.

**Tech Stack:** Go, quicktest (`qt`), existing engine internals

---

## Background

`NewEngine` (`engine.go:122-204`) has a 6-step initialization sequence where each step depends on prior steps. The dependencies are:

```
config ──→ registry ──→ namespace ──→ bootstrap ──→ file resolver ──→ library system
                  └──────────────────→ bootstrap ─┘                       │
                                       bootstrap ←────────────────────────┘
```

Steps and their dependencies:

| Step | Function | Depends On |
|------|----------|------------|
| 1. Config | `engineConfig{}` + option apply | nothing |
| 2. Registry | `buildRegistry(cfg)` | config |
| 3. Namespace | `NewNamespace()` + `SetRegistry` | registry |
| 4. Bootstrap | `applyBaseEnvironment(...)` | registry, namespace (env) |
| 5. File resolver | `env.SetFileResolver(...)` | bootstrap (must come after, so bootstrap uses embed resolver) |
| 6. Library system | `setupLibrarySystem(...)` | file resolver, bootstrap (needs `define-library` macro) |

Currently enforced only by code sequencing. The `WithNamespace` path (line 140-153) trusts the caller bootstrapped correctly — no validation.

## Scope

Two deliverables:
1. Invariant comment block in `engine.go` before `NewEngine`
2. Negative tests in `engine_init_order_test.go` (internal, package `wile`)

Not in scope: adding runtime guards or refactoring initialization into a builder. The comment and tests are the safety net.

---

### Task 1: Add invariant comment

**Files:**
- Modify: `engine.go` (before `NewEngine`, after the existing doc comment)

**Step 1: Add the invariant comment**

Insert between the existing `NewEngine` doc comment and the `func` line. The comment documents the 6 steps, their dependencies, and what breaks if reordered:

```go
// Initialization Order Invariant
//
// NewEngine performs 6 initialization steps that MUST execute in this order.
// Each step depends on prior steps; reordering causes silent failures or panics.
//
//   1. Config         — build engineConfig from options
//   2. Registry       — buildRegistry(cfg): register core + extension primitives
//   3. Namespace      — NewNamespace() + SetRegistry + SetAuthorizer
//   4. Bootstrap      — applyBaseEnvironment: bind primitives, syntax compilers,
//                        expanders, bootstrap macros (uses EmbedFileResolver, NOT
//                        the runtime file resolver)
//   5. File resolver  — env.SetFileResolver: runtime include/load resolver.
//                        Must come AFTER bootstrap (step 4) so bootstrap uses its
//                        own EmbedFileResolver, not the runtime resolver.
//   6. Library system — setupLibrarySystem: search paths, extension libraries,
//                        library env factory. Requires file resolver (step 5)
//                        and bootstrap macros (step 4) for define-library parsing.
//
// The WithNamespace path (pre-built namespace) skips steps 2-5 and trusts that
// the caller bootstrapped correctly. NewNamespace() performs steps 2-4.
```

**Step 2: Verify build compiles**

Run: `go build ./...`
Expected: clean build

**Step 3: Commit**

```
add engine initialization order invariant comment

Document the 6-step dependency chain in NewEngine so future
refactors cannot silently reorder dependent steps.
```

---

### Task 2: Write negative test — unbootstrapped namespace

This test demonstrates the most important ordering constraint: bootstrap (step 4) must happen before use. It constructs a namespace with a registry but no bootstrap, passes it via `WithNamespace`, and verifies that:
- Core primitives work (they're in the registry, not bootstrap)
- Bootstrap-dependent macros fail (they require step 4)

**Files:**
- Create: `engine_init_order_test.go` (package `wile`)

**Step 1: Write the test**

```go
package wile

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/registry/core"

	qt "github.com/frankban/quicktest"
)

// TestInitOrder_UnbootstrappedNamespace verifies that skipping bootstrap
// (step 4) causes observable failures for macro-dependent code, even when
// the registry and namespace are correctly configured.
func TestInitOrder_UnbootstrappedNamespace(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Steps 1-3 only: config → registry → namespace. No bootstrap.
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	c.Assert(err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)

	eng, err := NewEngine(ctx, WithNamespace(ns))
	c.Assert(err, qt.IsNil)

	// Primitives bound by registry.Apply work — but registry.Apply was
	// never called on this namespace's env (bootstrap skipped), so even
	// core primitives like + are unbound.
	_, err = eng.Eval(ctx, eng.MustParse(ctx, "(if #t 1 2)"))
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("expected failure: unbootstrapped namespace has no bindings"))
}
```

**Step 2: Run the test to verify it catches the ordering violation**

Run: `go test -v -run TestInitOrder ./...`
Expected: PASS — the test expects an error and gets one.

**Step 3: Commit**

```
add negative test for engine initialization order

Verify that an unbootstrapped namespace (steps 2-3 only, no step 4)
produces observable failures, documenting the bootstrap dependency.
```

---

### Task 3: Write negative test — library system without bootstrap

This test verifies the step 4 → step 6 dependency: library loading requires bootstrap macros to parse `define-library` forms in `.sld` files.

**Files:**
- Modify: `engine_init_order_test.go`

**Step 1: Write the test**

```go
// TestInitOrder_LibraryWithoutBootstrap verifies that enabling the library
// system (step 6) on an unbootstrapped namespace fails, because
// define-library parsing requires bootstrap macros (step 4).
func TestInitOrder_LibraryWithoutBootstrap(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	c.Assert(err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetRegistry(reg)

	// WithNamespace skips bootstrap; WithLibraryPaths enables library system.
	_, err = NewEngine(ctx,
		WithNamespace(ns),
		WithLibraryPaths("."),
	)
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("expected failure: library system requires bootstrap macros"))
}
```

**Step 2: Run the test**

Run: `go test -v -run TestInitOrder ./...`
Expected: PASS — library setup fails on the unbootstrapped env.

> **Note:** If `NewEngine` succeeds (library setup doesn't eagerly parse anything), this test needs adjustment. In that case, change the assertion to verify that *importing* a library fails:
>
> ```go
> eng, err := NewEngine(ctx, WithNamespace(ns), WithLibraryPaths("."))
> // If engine creation succeeds, importing should fail
> if err == nil {
>     _, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
>     c.Assert(err, qt.IsNotNil,
>         qt.Commentf("expected failure: library import requires bootstrap"))
> }
> ```

**Step 3: Run `make lint && make covercheck`**

Expected: clean

**Step 4: Commit**

```
add library-without-bootstrap negative test

Verify that enabling library system on an unbootstrapped namespace
fails, documenting the bootstrap → library dependency.
```

---

## Verification

After all tasks:

```bash
go test -v -run TestInitOrder ./...
make lint
make covercheck
```

All three must pass.

## Open Questions

1. **Does `NewEngine` with `WithNamespace(unbootstrapped) + WithLibraryPaths` fail at engine creation or at first import?** Task 3 Step 2 determines this. The test adapts either way — the important thing is that failure is observable.

2. **Should `WithNamespace` validate that bootstrap was performed?** Out of scope for this task, but worth considering. A validation check (e.g., probe for a known bootstrap binding like `cond`) would catch misuse at construction time rather than at first eval. Could be a follow-up.
