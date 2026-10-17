# Documented Exceptions, Investigated & Rejected, FCA Assessment

> One of the files [`TODO.md`](../TODO.md) is split into (2026-10-17). `TODO.md` holds the
> status, ordering, conventions, Plan Index and Top Priority table; this file holds rows.
> Cite a row by its bold text, never by line number. `make indexlint` and `make planlint`
> read `TODO.md` and `todo/*.md` as one corpus.

## Documented Exceptions

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---
## Investigated & Rejected

These items were investigated and determined not to warrant changes:

- [x] **Promoted op table**: Table-driven dispatch regressed ~1.5% geo mean (15/16 Gabriel benchmarks slower). Go compiles contiguous-integer switches to jump tables; table-driven adds overhead. `memory/2026-04-05-structural-reduction.md`
- [x] **PrimitiveSpec dead fields (D1)**: Originally 5%/2% usage. Extension contracts Phase 1 populated both broadly. No longer dead.
- [x] **ForeignClosure redundant fields (D2)**: `doc` duplicates `PrimitiveSpec.Doc` but costs ~3.2KB total, set once, cannot diverge. Removing requires circular import workarounds.
- [x] **Namespace root/child state waste (D3)**: ~6 nil fields in children. Not worth splitting — zero-value costs nothing, children are rare.
- [x] **LocalIndex / BindingID overlap (D4)**: `LocalIndex` is relative (slot+depth), `BindingID` is absolute (frame pointer + slot). Both needed.
- [x] **Binding/BindingMeta (FCA)**: Clean lazy-initialization pattern. FCA false positive.
- [x] **PrimitiveRegistration/PrimitiveSpec (FCA)**: Orthogonal concerns properly separated. FCA false positive.
- [x] **CompileTimeCallContext (FCA)**: 2-field value type parameter, not coupling. FCA false positive.
- [x] **Opcode resource limits**: Per-category limits (match steps, expand steps, continuation copy depth). Existing mechanisms sufficient: `WithMaxCallDepth` bounds recursion, `WithMaxStackSize` bounds stack growth, `context.WithTimeout` checked every 1024 ops in VM and match loops. Deterministic per-category budgets are niche; timeout is an adequate proxy.
- [x] **Core macro expander in Scheme (psyntax / Racket style)**: declined 2026-09-04. Wile has no image format (the stdlib recompiles on every `wile.New()`), so a Scheme-hosted expander needs template serialization first or a second, Go, bootstrap expander; expansion would run as bytecode on the embedding product's headline metric, engine startup; `*Binding` handles held from Scheme need a frame-layout change in `pkg/environment`; coverage, debugger, and provenance would see expander frames. Not smaller either (psyntax ≈10k, Racket >30k lines vs ≈5.6k Go core). The extension benefit is already served by `WithDialect` + open `FormSpec.Compile` and the registered procedural surface. The middle path, a small Go kernel with every `syntax-*` form specified in Scheme, is the live design (`plans/2026-09-04-scheme-specified-syntax-forms-design.md`); it is not this proposal. `plans/2026-09-04-expander-in-scheme-assessment.md` §3.

---
## FCA Assessment

Detailed staff-engineer assessment of cross-boundary coupling. Actionable items extracted into Tier 1 and Tier 5 above.

Snapshot as filed; its line ranges are stale. The vmState copy-site finding is guarded since `834b2db7`
(`contDescriptor`, introduced by `9c0c3f72` and hardened by `834b2db7`: a descriptor over the five
surviving sites, four under the completeness ratchet and `opSaveWrap` asserted explicitly,
`pkg/machine/continuation_descriptor_oracle_test.go`), and `PopContinuation` was deleted in `91460090`.

<details>
<summary>Full FCA findings (click to expand)</summary>

**[Priority: High] — vmState field-addition has 6 unguarded copy sites**

Where:
- machine/machine_context_continuation.go:31-224 (Restore, RestoreAndRelease, PopContinuation, SaveContinuation)
- machine/machine_continuation.go:96-113 (NewMachineContinuationFromMachineContext)
- machine/machine_continuation.go:157-183 (Copy)
- machine/machine_context.go:93-110 (NewMachineContext)

What: Adding a field to vmState requires updating 6 functions across 3 files, each with different copy semantics (transfer, clone, skip, force-false). No compile-time guard ensures all sites are updated. The documentation table at vm_state.go:78-93 is the only safety net — and it's comments.

Why it matters: Every field added has to be reasoned about independently at each site. The envPooled column alone has four different behaviors. The marks field uses cloneMarks in some paths and direct assignment in others. Miss one site → silent state corruption.

---

**[Priority: High] — No two transfer operations agree on which fields to copy**

Where: machine/machine_context_continuation.go — all four operations

What: Save, Restore, RestoreAndRelease, Pop each copy a different subset of vmState. The non-uniformity isn't accidental — each deviation is a semantic decision documented only in comments. vmState is treated as three implicit partitions (always-transfer, conditionally-transfer, never-transfer) but there's no type-level encoding.

The deeper issue: The evals field alone has four distinct ownership modes across the four operations.

---

**[Priority: Medium] — Opcode extension requires 7 coordinated edits**

Where:
- machine/opcode.go (constant + table entry)
- machine/machine_context.go:305-329 (dispatch switch)
- machine/native_template.go:129+, 256+ (both conversion directions)
- machine/operation_*.go (new Operation type)
- machine/compilation/*.go (compiler emission)
- machine/peephole.go (if fused)

What: Adding a new opcode touches 7 mandatory sites. The bidirectional conversion switches must stay synchronized.

---

**[Priority: Medium] — LocalEnvironmentFrame pointer ambiguity**

Where: environment/local_environment_frame.go:29-33, environment/environment_frame.go:93-108

What: LocalEnvironmentFrame is embedded by value in EnvironmentFrame (for heap savings), but NewLocalEnvironment() returns *LocalEnvironmentFrame (heap-allocated). Same type, two ownership semantics.

---

**State of the Code**: Wile's machine package is well-documented and intentionally designed, but carries real evolution risk in vmState transfer operations. The CESK architecture is sound. The debt isn't in the abstraction — it's in hand-unrolled field copying where each of 6 functions implements a different subset of a 12-field copy with different ownership semantics, guarded only by a comment table.

</details>
