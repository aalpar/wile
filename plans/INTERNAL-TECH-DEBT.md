# Internal Package Technical Debt Reduction

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate confirmed structural debt across `internal/` packages — duplicated logic, hardcoded tables, missing abstractions, sync hazards.

**Architecture:** Bottom-up by dependency order. Quick wins first (isolated changes, no cross-package impact), then parser/match refactoring (higher risk, needs careful test verification), then structural splits (file reorganization, no behavior changes).

**Tech Stack:** Go 1.23, project error conventions (`werr`), table-driven tests.

**Validated findings only.** One assessment finding (unguarded type assertion in `validate_case_lambda.go`) was verified as false — the code already has a proper guard at line 53. All remaining items below are confirmed against actual code.

---

## Phase 5: Extensions Cleanup (M effort, medium value)

### Task 5.1: Split prim_read_write.go

806 lines mixing text I/O, binary I/O, and port helpers. Split by concern.

**Files:**
- Modify: `internal/extensions/io/prim_read_write.go` (reduce to text read)
- Create: `internal/extensions/io/prim_write.go` (text write)
- Create: `internal/extensions/io/prim_binary.go` (byte-level I/O)
- Modify: `internal/extensions/io/register.go` (no changes needed if registration functions are stable)
- Test: existing tests must pass unchanged

**Step 1: Plan the split**

Read `prim_read_write.go` and group functions:
- **Text read** (~250 lines): PrimRead, PrimReadToken, PrimReadSyntax, PrimReadChar, PrimPeekChar, PrimCharReadyQ, PrimReadLine, PrimReadString
- **Text write** (~200 lines): PrimWrite, PrimDisplay, PrimWriteSimple, PrimWriteShared, PrimWriteChar, PrimWriteString, PrimNewline, PrimFlushOutputPort
- **Binary I/O** (~200 lines): PrimReadU8, PrimPeekU8, PrimU8ReadyQ, PrimWriteU8, PrimReadBytevector, PrimReadBytevectorBang, PrimWriteBytevector
- **Shared helpers** (keep in original or new helpers file): extractPort, getOptionalInputPort, getOptionalOutputPort, getOptionalTextualOutputPort, getRequiredBinaryInputPort, getRequiredBinaryOutputPort, fmtPrefix

**Step 2: Execute the split (pure file move)**

**Step 3: Run all tests**

Run: `go test -v ./internal/extensions/io/...`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/extensions/io/
git commit -m "refactor(extensions/io): split prim_read_write.go by concern

No behavior changes. Text read, text write, and binary I/O
now in separate files for better navigation."
```

---

## Phase 6: Bootstrap Parameterization (S-M effort, medium value)

### Task 6.1: Parameterize extension loading in bootstrap

`internal/bootstrap/environment_tiny.go:55-66` hardcodes all extensions. The public API (`wile.Engine`) already supports `WithExtension()` for selective loading, but the bootstrap package doesn't expose this to internal callers.

**Files:**
- Modify: `internal/bootstrap/environment_tiny.go`
- Test: `internal/bootstrap/environment_tiny_test.go`

**Step 1: Read the Engine's extension loading code**

Before changing bootstrap, understand how `wile.Engine` already handles extension selection. Check `engine.go` for `WithExtension` and how it calls bootstrap. The goal is to align bootstrap with the existing public API pattern.

Run: `grep -n "WithExtension\|allExtensions\|NewTopLevel" engine.go`

**Step 2: Add an extensions parameter to the internal initializer**

```go
// initializeEnvironmentWithExtensions is the shared initialization sequence.
// If exts is nil, all extensions are loaded (backward compatible).
func initializeEnvironmentWithExtensions(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	exts []registry.Extension,
) (*registry.Registry, error) {
	if exts == nil {
		exts = allExtensions
	}
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error adding core to registry")
	}
	for _, ext := range exts {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err, "error adding extension %s to registry", ext.Name())
		}
	}
	// ... rest unchanged
}
```

**Step 3: Update existing callers to pass nil (backward compatible)**

`initializeEnvironmentWithRegistry` and `initializeEnvironment` should pass `nil` to get the default behavior. This is a pure refactoring — no caller behavior changes.

**Step 4: Run all bootstrap tests**

Run: `go test -v ./internal/bootstrap/...`
Expected: PASS

**Step 5: Run full test suite (bootstrap affects everything)**

Run: `make test`
Expected: PASS

**Step 6: Run lint**

Run: `make lint && make covercheck`
Expected: PASS

**Step 7: Commit**

```bash
git add internal/bootstrap/environment_tiny.go
git commit -m "refactor(bootstrap): parameterize extension loading

initializeEnvironmentWithExtensions accepts an explicit extension list.
nil means 'all extensions' (backward compatible). Enables future
selective extension loading for sandboxed environments."
```

---

## Phases NOT Included (Deferred)

These were identified in the assessment but are deferred for good reasons:

| Finding | Why Deferred |
|---------|-------------|
| Parser: unify readList + readLabeledList | High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol. |
| Match: extract opcode handlers from VM interpreter | 264-line switch is large but stable. Extraction adds indirection without clear benefit until new opcodes are needed. |
| Syntax: nine types with boilerplate | Go doesn't support sealed type hierarchies or default method implementations. The boilerplate is inherent to the language. Documenting the invariant is cheaper than refactoring. |
| Match: consolidate bytecode type files | Pure cosmetic. Low value. |
| Tokenizer: test file consolidation | Already tracked in TODO.md as "Tokenizer test file consolidation [Low, M]". |
| Extensions: standardize registration patterns | Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring. |
| Schemeutil: grab-bag reorganization | Moving functions risks import cycle issues. Needs careful dependency analysis. |

---

## Verification Checklist

After all phases:

```bash
make lint && make covercheck   # Must both pass
go test ./internal/...         # All internal tests pass
go test ./...                  # Full suite passes
```

No behavioral changes in any phase. Every commit should be independently revertible.
