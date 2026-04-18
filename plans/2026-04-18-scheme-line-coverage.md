# Scheme-Side Line Coverage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Scheme-level code-coverage tool that records which `(file, line, col)` positions of user code executed during a `wile` run, and emits a Go-cover-compatible report (`go tool cover -html` consumes it directly).

**Architecture:** Each `NativeTemplate` gains an optional `executed []bool` array parallel to its `code` and `sourceRefs`. When `Engine.WithCoverage` is set, the engine instruments every reachable template by allocating `executed`; the VM's `Run()` dispatch loop writes `executed[pc] = true` on each step. At engine shutdown the collector walks tracked templates, dereferences each covered `pc` through `sourceRefs[pc]` → `sourceTable` → `SourceContext`, and aggregates `(file, line, startCol, endCol) → count`. A `coverage.WriteGoCover` emitter writes the Go cover v1 format; a `coverage.WriteSummary` emits human-readable per-line rollup (`N/M subexprs covered`).

**Tech Stack:** Go 1.24, existing `machine/NativeTemplate` + `internal/syntax/SourceContext`, new `coverage/` package, integration via existing `EngineOption` pattern in `options.go`, CLI via existing `go-flags` in `cmd/wile/main.go`. Output format: Go cover v1 (`mode: set`), consumable by `go tool cover -html -o report.html coverage.out`.

---

## File Structure

**Create:**
- `coverage/coverage.go` — `Collector` type + `Track` / `Entries` methods (~80 LOC)
- `coverage/gocover.go` — `WriteGoCover(w, c)` emitter (~40 LOC)
- `coverage/summary.go` — `WriteSummary(w, c)` per-line rollup (~50 LOC)
- `coverage/coverage_test.go` — unit tests for Collector (~150 LOC)
- `coverage/gocover_test.go` — unit tests for emitter (~80 LOC)
- `coverage/summary_test.go` — unit tests for summary (~60 LOC)
- `coverage_integration_test.go` — engine-level integration test (~100 LOC, at repo root)
- `plans/2026-04-18-scheme-line-coverage.md` — this plan

**Modify:**
- `machine/native_template.go` — add `executed []bool` field, `EnableCoverage`, `Executed`, `AppendInstruction*` lockstep
- `machine/native_template_test.go` — new test cases for coverage field
- `machine/machine_context.go:328-355` — add dispatch hook in `Run()`
- `machine/machine_context_run_coverage_test.go` — new file; hook tests
- `options.go` — add `WithCoverage(*coverage.Collector)` engine option
- `engine.go` — walk reachable templates after compile, call `collector.Track`
- `cmd/wile/main.go` — `--cover PATH` flag, write report at exit

**No changes:**
- `internal/syntax/` — already exposes `SourceContext{File, Start, End}` with `Line()`/`Column()` accessors
- `make covercheck` — unrelated (Go-side coverage gate)

**Package layering:** `coverage/` is a peer of `registry/` (imports `machine/`). No cycle.

---

## Design Decisions (locked in, do not second-guess)

1. **Coverage unit:** `(file, startLine, startCol, endLine, endCol)` taken from `SourceContext`. This is s-expression coverage — every sexpr has a distinct `SourceContext`. Line coverage falls out as "any col on this line was hit."
2. **Per-line rollup metric:** `N / M` where `M` = distinct cols on that line in compiled bytecode, `N` = distinct cols that executed. User asked for "how deep did we get" — `max(covered cols)` is also emitted as a summary scalar, but labeled `max_col_reached` (honest under branching).
3. **Hot-path cost:** one `if mc.template.executed != nil` branch + conditional `executed[pc] = true` store per dispatch. Predictable branch when `nil` (the default). No change to dispatch when coverage is off.
4. **Output format:** Go cover v1, `mode: set`. Each `SourceContext` emits one entry: `file:startLine.startCol,endLine.endCol 1 {0|1}`. Entries sorted lexicographically.
5. **Stdlib scope:** Default excludes paths matching `scheme/` prefix (the embedded stdlib). `--cover-stdlib` overrides.
6. **Template reachability:** Sub-templates are stored as `*NativeTemplate` values in the parent's `literals` pool (used by `OpMakeClosure`). The walk does a BFS from the top-level template, treating each `*NativeTemplate` literal as a child.
7. **Concurrency:** `coverage.Collector` is thread-safe (mutex guards `templates` slice). Per-template `executed` writes from `Run()` are NOT mutex-guarded — each template is owned by one goroutine at a time during its dispatch window (SRFI-18 threads get fresh sub-contexts but share templates; a race on a bool write is benign because we only ever transition `false → true`).
8. **Nil SourceContext:** Skipped during aggregation (synthetic infrastructure ops have no user source).
9. **Peephole fusion:** Known limitation — fused instructions may lose source attribution (see `machine/peephole_test.go:540`). Entries that appear in `sourceRefs` will cover; ones that don't, won't. Documented in `coverage/coverage.go` package doc.

---

## Task 1: Add `executed []bool` field to `NativeTemplate`

**Files:**
- Modify: `machine/native_template.go` (struct + `AppendInstruction`, `AppendInstructionWithSource`, `AppendOperationsWithSource` if it exists in that file, `Clone`, `invariantCheck`)
- Test: `machine/native_template_coverage_test.go` (new file)

- [ ] **Step 1: Write the failing test**

Create `machine/native_template_coverage_test.go`:

```go
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

package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNativeTemplate_CoverageDisabledByDefault(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	c.Assert(tpl.Executed(), qt.IsNil)
}

func TestNativeTemplate_EnableCoverageAllocatesParallelToCode(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	tpl.EnableCoverage()

	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 3)
	c.Assert(exec[0], qt.IsFalse)
	c.Assert(exec[1], qt.IsFalse)
	c.Assert(exec[2], qt.IsFalse)
}

func TestNativeTemplate_AppendAfterEnableKeepsLockstep(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.EnableCoverage()
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 2)
	c.Assert(len(tpl.Code()), qt.Equals, 2)
}

func TestNativeTemplate_EnableCoverageIdempotent(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.EnableCoverage()
	tpl.Executed()[0] = true

	tpl.EnableCoverage() // second call must not clobber

	c.Assert(tpl.Executed()[0], qt.IsTrue)
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestNativeTemplate_Coverage|TestNativeTemplate_Enable|TestNativeTemplate_Append' ./machine/`

Expected: FAIL with "tpl.Executed undefined" / "tpl.EnableCoverage undefined"

- [ ] **Step 3: Add the field to `NativeTemplate` struct**

In `machine/native_template.go`, after the `sideTable []InlinedOperation` line (around line 53), add:

```go
	// executed tracks per-PC execution when coverage is enabled.
	// Nil means coverage is off (the default). When non-nil, its length
	// is kept equal to len(code) via AppendInstruction/AppendInstructionWithSource.
	// Writes in the VM dispatch loop are benign-racy: a bool transitioning
	// false → true does not need synchronization.
	executed []bool
```

- [ ] **Step 4: Add accessor and enabler methods**

In `machine/native_template.go`, after the existing `CodeLen` method (around line 567), add:

```go
// EnableCoverage allocates the per-PC executed array (if not already allocated)
// so the VM dispatch loop will record executions. Length is kept parallel to
// code via AppendInstruction. Idempotent: safe to call multiple times; an
// existing array is preserved.
func (p *NativeTemplate) EnableCoverage() {
	if p.executed != nil {
		return
	}
	p.executed = make([]bool, len(p.code))
}

// Executed returns the per-PC executed array, or nil if coverage is disabled.
// Returned slice aliases internal state; callers must not resize it.
func (p *NativeTemplate) Executed() []bool {
	return p.executed
}

// IsCoverageEnabled reports whether coverage tracking is active on this template.
func (p *NativeTemplate) IsCoverageEnabled() bool {
	return p.executed != nil
}
```

- [ ] **Step 5: Keep `executed` in lockstep with `code`**

In `machine/native_template.go`, in `AppendInstructionWithSource` (around line 523), change:

```go
func (p *NativeTemplate) AppendInstructionWithSource(src *syntax.SourceContext, instr Instruction) {
	idx := p.internSource(src)
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, idx)
}
```

to:

```go
func (p *NativeTemplate) AppendInstructionWithSource(src *syntax.SourceContext, instr Instruction) {
	idx := p.internSource(src)
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, idx)
	if p.executed != nil {
		p.executed = append(p.executed, false)
	}
}
```

In the same file, in `AppendInstruction` (around line 530), change:

```go
func (p *NativeTemplate) AppendInstruction(instr Instruction) {
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, 0)
}
```

to:

```go
func (p *NativeTemplate) AppendInstruction(instr Instruction) {
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, 0)
	if p.executed != nil {
		p.executed = append(p.executed, false)
	}
}
```

Search for any *other* site that appends to `p.code` or `p.sourceRefs` directly (there is one around line 249 in `AppendOperationsWithSource`). Use:

```bash
grep -n "p.code = append\|p.sourceRefs = append" /Users/aalpar/projects/wile-workspace/wile/machine/native_template.go
```

For each match that is NOT inside `AppendInstruction` or `AppendInstructionWithSource`, add the same `if p.executed != nil { p.executed = append(p.executed, false) }` block after the `sourceRefs` append. All appends must grow `executed` by the same count as `code`.

- [ ] **Step 6: Update `Clone` to deep-copy the executed array**

Find the `Clone` method in `machine/native_template.go` (around line 651 where `sourceRefs` is cloned). After the `q.sourceRefs = slices.Clone(p.sourceRefs)` line, add:

```go
	q.executed = slices.Clone(p.executed)
```

`slices.Clone(nil)` returns `nil`, so disabled-coverage templates remain disabled after clone.

- [ ] **Step 7: Update `invariantCheck` to verify lockstep**

Find `invariantCheck` in `machine/native_template.go` (around line 633). After the existing `code`/`sourceRefs` length check, add:

```go
	if p.executed != nil && len(p.executed) != len(p.code) {
		return werr.WrapForeignErrorf(
			werr.ErrCorruptBytecode,
			"native_template: code/executed length invariant violated (len(code)=%d, len(executed)=%d)",
			len(p.code), len(p.executed),
		)
	}
```

- [ ] **Step 8: Run tests to verify they pass**

Run: `go test -v -run 'TestNativeTemplate_Coverage|TestNativeTemplate_Enable|TestNativeTemplate_Append' ./machine/`

Expected: PASS (4 tests).

- [ ] **Step 9: Run the full machine package tests to verify no regressions**

Run: `go test ./machine/...`

Expected: PASS. If any test fails with a `sourceRefs` / `executed` length mismatch, there is an `append` site in `native_template.go` you missed in Step 5 — re-run the grep and fix.

- [ ] **Step 10: Commit**

```bash
git add machine/native_template.go machine/native_template_coverage_test.go
git commit -m "feat(machine): add optional per-PC executed array to NativeTemplate

Adds machine.NativeTemplate.executed []bool, parallel to code and
sourceRefs. Nil when coverage is disabled (the default); allocated
via EnableCoverage. Kept in lockstep with code via AppendInstruction
and AppendInstructionWithSource. No effect on dispatch yet — wired
in a subsequent commit.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 2: Hook the VM dispatch loop

**Files:**
- Modify: `machine/machine_context.go` — add one line in `Run()` after `instr := ...`
- Test: `machine/machine_context_coverage_test.go` (new file)

- [ ] **Step 1: Write the failing test**

Create `machine/machine_context_coverage_test.go`:

```go
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

package machine

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/values"
)

func TestRun_CoverageOff_NoEffect(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendLiteral(values.NewIntegerValue(42))
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: 0})
	tpl.AppendInstruction(Instruction{Op: OpHalt})

	mc := NewMachineContext(context.Background(), nil, tpl)
	err := mc.Run()

	c.Assert(err, qt.IsNil)
	c.Assert(tpl.Executed(), qt.IsNil, qt.Commentf("coverage should remain disabled"))
}

func TestRun_CoverageOn_MarksExecutedPCs(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendLiteral(values.NewIntegerValue(42))
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: 0})
	tpl.AppendInstruction(Instruction{Op: OpHalt})
	tpl.EnableCoverage()

	mc := NewMachineContext(context.Background(), nil, tpl)
	err := mc.Run()

	c.Assert(err, qt.IsNil)
	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 2)
	c.Assert(exec[0], qt.IsTrue, qt.Commentf("OpLoadLiteral should be marked"))
	c.Assert(exec[1], qt.IsTrue, qt.Commentf("OpHalt should be marked"))
}

func TestRun_CoverageOn_UnreachedPCsStayFalse(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpHalt}) // pc=0 halts immediately
	tpl.AppendInstruction(Instruction{Op: OpPush}) // pc=1 unreached
	tpl.EnableCoverage()

	mc := NewMachineContext(context.Background(), nil, tpl)
	err := mc.Run()

	c.Assert(err, qt.IsNil)
	exec := tpl.Executed()
	c.Assert(exec[0], qt.IsTrue)
	c.Assert(exec[1], qt.IsFalse, qt.Commentf("unreached PC must remain false"))
}
```

Note: If `NewMachineContext` has a different signature or `AppendLiteral` / `OpHalt` / `OpLoadLiteral` have different names, look up the actual names:
```bash
grep -n "^func New\w*MachineContext\|^func.*AppendLiteral" /Users/aalpar/projects/wile-workspace/wile/machine/*.go | head -10
grep -n "OpHalt\|OpLoadLiteral" /Users/aalpar/projects/wile-workspace/wile/machine/opcode.go | head -5
```
Use the names you find.

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v -run TestRun_Coverage ./machine/`

Expected: FAIL (`exec[0]` is `false`, expected `true`).

- [ ] **Step 3: Add the dispatch hook in `Run()`**

In `machine/machine_context.go`, find the dispatch loop body — the line `instr := mc.template.code[mc.pc]` (around line 349). Immediately *after* the existing `mc.counters.opcodeHits` block (around line 353) and *before* `switch instr.Op {`, add:

```go
		if mc.template.executed != nil {
			mc.template.executed[mc.pc] = true
		}
```

The resulting block reads:

```go
		instr := mc.template.code[mc.pc]
		mc.counters.OpsExecuted++
		if mc.counters.opcodeHits != nil {
			mc.counters.opcodeHits[instr.Op]++
		}

		if mc.template.executed != nil {
			mc.template.executed[mc.pc] = true
		}

		switch instr.Op {
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `go test -v -run TestRun_Coverage ./machine/`

Expected: PASS (3 tests).

- [ ] **Step 5: Run the full machine package tests**

Run: `go test ./machine/...`

Expected: PASS. No regressions.

- [ ] **Step 6: Micro-benchmark to confirm hot-path cost is negligible when off**

Run the existing fib benchmark twice and compare:

```bash
cd /Users/aalpar/projects/wile-workspace/wile
go test -bench=BenchmarkFib -benchtime=5s -count=3 ./machine/ | tee /tmp/fib-before.txt
```

The commit is OK only if the `ns/op` numbers are within 2% of the pre-hook baseline (read from git history if needed). If regression is > 2%, reconsider the hook design before proceeding (no action needed in this task — just flag it).

- [ ] **Step 7: Commit**

```bash
git add machine/machine_context.go machine/machine_context_coverage_test.go
git commit -m "feat(machine): hook Run() dispatch to record executed PCs

When NativeTemplate.executed is non-nil (coverage enabled), the VM
dispatch loop sets executed[pc]=true on each step. Nil check costs
one predictable branch when coverage is off; no measurable impact
on hot-path throughput.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 3: Create `coverage` package with `Collector` type

**Files:**
- Create: `coverage/coverage.go`
- Create: `coverage/coverage_test.go`

- [ ] **Step 1: Write the failing test**

Create `coverage/coverage_test.go`:

```go
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

package coverage

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
)

func newTplWithSources(sources ...*syntax.SourceContext) *machine.NativeTemplate {
	tpl := machine.NewNativeTemplate(0, 0, false)
	for _, src := range sources {
		tpl.AppendInstructionWithSource(src, machine.Instruction{Op: machine.OpPush})
	}
	return tpl
}

func mkSrc(file string, sl, sc, el, ec int) *syntax.SourceContext {
	return &syntax.SourceContext{
		File:  file,
		Start: syntax.NewSourceIndexes(0, sc, sl),
		End:   syntax.NewSourceIndexes(0, ec, el),
	}
}

func TestCollector_TrackEnablesCoverage(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()

	col.Track(tpl)

	c.Assert(tpl.IsCoverageEnabled(), qt.IsTrue)
}

func TestCollector_EntriesEmptyWhenNothingExecuted(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()
	col.Track(tpl)

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 1)
	c.Assert(entries[0].Count, qt.Equals, 0)
}

func TestCollector_EntriesReflectExecutedPCs(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 1, 7, 1, 11),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true // first sexpr covered; second not

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 2)
	c.Assert(entries[0].Count, qt.Equals, 1)
	c.Assert(entries[0].StartCol, qt.Equals, 1)
	c.Assert(entries[1].Count, qt.Equals, 0)
	c.Assert(entries[1].StartCol, qt.Equals, 7)
}

func TestCollector_NilSourceContextSkipped(t *testing.T) {
	c := qt.New(t)
	tpl := machine.NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpPush}) // no source
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 0, qt.Commentf("instructions without sources produce no entries"))
}

func TestCollector_MultipleInstructionsSameSource_SingleEntryCountsOnce(t *testing.T) {
	c := qt.New(t)
	src := mkSrc("a.scm", 1, 1, 1, 5)
	tpl := newTplWithSources(src, src, src)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[2] = true

	entries := col.Entries()

	// mode=set: same SourceContext collapses to one entry with count=1
	c.Assert(entries, qt.HasLen, 1)
	c.Assert(entries[0].Count, qt.Equals, 1)
}

func TestCollector_EntriesSortedLexicographically(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("b.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 10, 1, 10, 5),
		mkSrc("a.scm", 2, 1, 2, 5),
	)
	col := NewCollector()
	col.Track(tpl)

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 3)
	c.Assert(entries[0].File, qt.Equals, "a.scm")
	c.Assert(entries[0].StartLine, qt.Equals, 2)
	c.Assert(entries[1].File, qt.Equals, "a.scm")
	c.Assert(entries[1].StartLine, qt.Equals, 10)
	c.Assert(entries[2].File, qt.Equals, "b.scm")
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v ./coverage/`

Expected: FAIL with "package ./coverage: no Go files" / undefined `NewCollector`.

- [ ] **Step 3: Create the Collector type**

Create `coverage/coverage.go`:

```go
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

// Package coverage collects per-s-expression coverage data from
// executing Wile Scheme code. Each coverage entry corresponds to a
// unique SourceContext (file + start/end line/column), aggregated
// across all compiled NativeTemplates.
//
// Known limitations:
//   - Instructions synthesized by the peephole optimizer may drop
//     their source attribution (see machine/peephole_test.go:540).
//     Such PCs execute but produce no Entry.
//   - Instructions with no source context (synthetic infrastructure
//     ops) are skipped.
package coverage

import (
	"sort"
	"sync"

	"github.com/aalpar/wile/machine"
)

// Entry is one covered (or not covered) s-expression.
type Entry struct {
	File      string
	StartLine int
	StartCol  int
	EndLine   int
	EndCol    int
	// Count is 0 if the s-expression did not execute, 1 if it did
	// (mode=set). Higher modes are a future extension.
	Count int
}

// Collector aggregates coverage data across a set of tracked templates.
// A zero Collector is not usable; construct with NewCollector.
type Collector struct {
	mu        sync.Mutex
	templates []*machine.NativeTemplate
}

// NewCollector returns a ready-to-use Collector.
func NewCollector() *Collector {
	q := &Collector{}
	return q
}

// Track enables coverage on a template and adds it to the collector's
// tracked set. Calling Track multiple times with the same template is
// a no-op (templates are deduplicated by pointer identity).
func (p *Collector) Track(tpl *machine.NativeTemplate) {
	if tpl == nil {
		return
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, existing := range p.templates {
		if existing == tpl {
			return
		}
	}
	tpl.EnableCoverage()
	p.templates = append(p.templates, tpl)
}

// Entries returns one Entry per unique SourceContext seen across all
// tracked templates. Entries are sorted lexicographically by
// (File, StartLine, StartCol, EndLine, EndCol). An s-expression is
// reported with Count=1 iff any instruction referring to its
// SourceContext executed (mode=set).
func (p *Collector) Entries() []Entry {
	p.mu.Lock()
	defer p.mu.Unlock()

	type key struct {
		file     string
		startLn  int
		startCol int
		endLn    int
		endCol   int
	}
	// hit[key] = true iff any PC referring to this source was executed.
	hit := make(map[key]bool)

	for _, tpl := range p.templates {
		exec := tpl.Executed()
		if exec == nil {
			continue
		}
		for pc := 0; pc < len(exec); pc++ {
			src := tpl.SourceAt(pc)
			if src == nil || src.File == "" {
				continue
			}
			k := key{
				file:     src.File,
				startLn:  src.Start.Line(),
				startCol: src.Start.Column(),
				endLn:    src.End.Line(),
				endCol:   src.End.Column(),
			}
			prev := hit[k]
			hit[k] = prev || exec[pc]
		}
	}

	q := make([]Entry, 0, len(hit))
	for k, covered := range hit {
		entry := Entry{
			File:      k.file,
			StartLine: k.startLn,
			StartCol:  k.startCol,
			EndLine:   k.endLn,
			EndCol:    k.endCol,
		}
		if covered {
			entry.Count = 1
		}
		q = append(q, entry)
	}

	sort.Slice(q, func(i, j int) bool {
		return lessEntry(q[i], q[j])
	})
	return q
}

// lessEntry defines the canonical sort order for Entries.
func lessEntry(a, b Entry) bool {
	if a.File != b.File {
		return a.File < b.File
	}
	if a.StartLine != b.StartLine {
		return a.StartLine < b.StartLine
	}
	if a.StartCol != b.StartCol {
		return a.StartCol < b.StartCol
	}
	if a.EndLine != b.EndLine {
		return a.EndLine < b.EndLine
	}
	return a.EndCol < b.EndCol
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `go test -v ./coverage/`

Expected: PASS (6 tests).

- [ ] **Step 5: Run golangci-lint on the new package**

Run: `golangci-lint run ./coverage/...`

Expected: no issues.

- [ ] **Step 6: Commit**

```bash
git add coverage/coverage.go coverage/coverage_test.go
git commit -m "feat(coverage): add Collector type for Scheme-side line coverage

Package coverage/ collects per-s-expression coverage from executed
NativeTemplates. Collector.Track attaches a template for tracking
(enables its per-PC executed array); Collector.Entries aggregates
unique SourceContexts across all tracked templates and emits sorted
Entry records with Count=0|1 (mode=set).

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 4: Add `WithCoverage` engine option

**Files:**
- Modify: `options.go` — add option
- Modify: `engine.go` — store collector, walk templates
- Test: `engine_coverage_test.go` (new file)

- [ ] **Step 1: Write the failing test**

Create `engine_coverage_test.go` at the repo root (same package `wile`):

```go
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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile"
	"github.com/aalpar/wile/coverage"
)

func TestWithCoverage_TopLevelExprTracked(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultipleWithSource("(+ 1 2)", "test.scm")
	c.Assert(err, qt.IsNil)

	entries := col.Entries()
	c.Assert(len(entries) > 0, qt.IsTrue, qt.Commentf("entries: %+v", entries))

	// At least one entry from test.scm must have Count=1.
	hit := false
	for _, e := range entries {
		if e.File == "test.scm" && e.Count == 1 {
			hit = true
			break
		}
	}
	c.Assert(hit, qt.IsTrue, qt.Commentf("entries: %+v", entries))
}

func TestWithCoverage_DeadBranchNotCovered(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	// The else branch is dead; its SourceContext must not show Count=1.
	_, err = eng.EvalMultipleWithSource("(if #t 111 222)", "t.scm")
	c.Assert(err, qt.IsNil)

	entries := col.Entries()

	var hit111, hit222 bool
	for _, e := range entries {
		if e.File != "t.scm" {
			continue
		}
		// 111 is at col 8-11; 222 at col 12-15 in a zero-indexed or one-indexed col.
		// Assert via Count on any entry whose SourceContext covers col range we care about.
		// This is approximate; the real assertion is "both branches produced entries,
		// one Count=1 and one Count=0".
		if e.Count == 1 {
			hit111 = true
		}
		if e.Count == 0 {
			hit222 = true
		}
	}
	c.Assert(hit111, qt.IsTrue, qt.Commentf("taken branch must have a Count=1 entry; entries: %+v", entries))
	c.Assert(hit222, qt.IsTrue, qt.Commentf("dead branch must have a Count=0 entry; entries: %+v", entries))
}

func TestWithCoverage_NestedLambdaTracked(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultipleWithSource(
		"(define (f x) (* x x)) (f 7)",
		"nested.scm",
	)
	c.Assert(err, qt.IsNil)

	entries := col.Entries()

	// Body of f (the * expression) must have Count=1.
	var hitBody bool
	for _, e := range entries {
		if e.File == "nested.scm" && e.Count == 1 {
			hitBody = true
		}
	}
	c.Assert(hitBody, qt.IsTrue, qt.Commentf("lambda body must be tracked; entries: %+v", entries))
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v -run TestWithCoverage ./...`

Expected: FAIL with "undefined: wile.WithCoverage".

- [ ] **Step 3: Add the option to `options.go`**

In `options.go`, find the `engineConfig` struct (around line 38). Add a field:

```go
	coverageCollector *coverage.Collector
```

Then add the import at the top of the file:

```go
	"github.com/aalpar/wile/coverage"
```

Then at the bottom of `options.go`, add:

```go
// WithCoverage enables Scheme-side line coverage collection. All
// templates compiled by the engine (top-level plus reachable
// sub-templates from closures) are registered with the collector,
// which aggregates per-SourceContext execution counts. See the
// coverage package for the reporting API.
//
// Has no effect on performance when not set (nil-check in dispatch
// loop). Pass nil to disable.
func WithCoverage(c *coverage.Collector) EngineOption {
	return func(cfg *engineConfig) {
		cfg.coverageCollector = c
	}
}
```

- [ ] **Step 4: Thread the collector through compile**

Find the main Engine compile entry point. Run:

```bash
grep -n "func (.*\*Engine) Compile\|func (.*\*Engine) EvalMultiple\|func (.*\*Engine) ParseWithSource\|func (.*\*Engine) Run" /Users/aalpar/projects/wile-workspace/wile/engine.go
```

For every compile path that returns a `*machine.NativeTemplate` (or a compiled artifact holding one), add, immediately before returning:

```go
	if e.cfg.coverageCollector != nil {
		trackTemplateTree(e.cfg.coverageCollector, tpl)
	}
```

where `tpl` is the result template. Use the actual variable name at each site.

- [ ] **Step 5: Implement the template-tree walker**

At the bottom of `engine.go`, add a helper:

```go
// trackTemplateTree registers tpl and every *machine.NativeTemplate
// reachable via its literals pool with the given collector. Walks
// breadth-first with a visited set to cut cycles (rare but possible
// via self-referencing closures).
func trackTemplateTree(col *coverage.Collector, root *machine.NativeTemplate) {
	if root == nil {
		return
	}
	visited := make(map[*machine.NativeTemplate]bool)
	queue := []*machine.NativeTemplate{root}
	for len(queue) > 0 {
		tpl := queue[0]
		queue = queue[1:]
		if visited[tpl] {
			continue
		}
		visited[tpl] = true
		col.Track(tpl)
		for _, lit := range tpl.Literals() {
			child, ok := lit.(*machine.NativeTemplate)
			if !ok {
				continue
			}
			if !visited[child] {
				queue = append(queue, child)
			}
		}
	}
}
```

Add the `machine` and `coverage` imports to `engine.go` if not already present.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `go test -v -run TestWithCoverage ./...`

Expected: PASS (3 tests).

If `TestWithCoverage_NestedLambdaTracked` fails with `hitBody=false`, the template-tree walker is missing sub-templates. Debug by adding:

```go
fmt.Printf("tracked %d templates\n", len(col.TrackedCount()))
```

(Add a `TrackedCount()` accessor to Collector temporarily.) If only 1 template is tracked, closure templates are NOT in the literals pool — check what type they appear as. Look at `operations_closure.go` and `OpMakeClosure.Apply` to find where the template is loaded from.

- [ ] **Step 7: Run the full test suite**

Run: `go test ./...`

Expected: PASS. No regressions.

- [ ] **Step 8: Commit**

```bash
git add options.go engine.go engine_coverage_test.go
git commit -m "feat(wile): add WithCoverage engine option

Adds wile.WithCoverage(*coverage.Collector) as an EngineOption.
When set, every NativeTemplate compiled by the engine — top-level
plus nested templates reachable via each template's literals pool
(closures, define'd functions, etc.) — is registered with the
collector for per-s-expression coverage tracking.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 5: Go cover format emitter

**Files:**
- Create: `coverage/gocover.go`
- Create: `coverage/gocover_test.go`

- [ ] **Step 1: Write the failing test**

Create `coverage/gocover_test.go`:

```go
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

package coverage

import (
	"bytes"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteGoCover_HeaderModeSet(t *testing.T) {
	c := qt.New(t)
	col := NewCollector()
	var buf bytes.Buffer

	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\n")
}

func TestWriteGoCover_OneEntry(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:1.1,1.5 1 1\n")
}

func TestWriteGoCover_CountZeroForUncovered(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 2, 1, 2, 10))
	col := NewCollector()
	col.Track(tpl)
	// no executions

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:2.1,2.10 1 0\n")
}

func TestWriteGoCover_ExcludesStdlibByDefault(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("myapp.scm", 1, 1, 1, 5),
		mkSrc("scheme/base.sld", 10, 1, 10, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.1,1.5 1 1\n")
}

func TestWriteGoCover_IncludeStdlibWhenRequested(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("myapp.scm", 1, 1, 1, 5),
		mkSrc("scheme/base.sld", 10, 1, 10, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true

	var buf bytes.Buffer
	err := WriteGoCoverIncludingStdlib(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.1,1.5 1 1\nscheme/base.sld:10.1,10.5 1 1\n")
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v -run TestWriteGoCover ./coverage/`

Expected: FAIL with "undefined: WriteGoCover".

- [ ] **Step 3: Implement the emitter**

Create `coverage/gocover.go`:

```go
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

package coverage

import (
	"fmt"
	"io"
	"strings"
)

// stdlibPrefixes are paths whose entries are excluded by default.
// They correspond to the embedded R7RS stdlib paths under stdlib/lib/.
var stdlibPrefixes = []string{
	"scheme/",
	"wile/",
	"srfi/",
}

// WriteGoCover writes the collector's entries in Go cover v1 format
// (mode: set), excluding stdlib paths. The output is consumable by
// `go tool cover -html -o report.html <file>`.
func WriteGoCover(w io.Writer, c *Collector) error {
	return writeGoCover(w, c, false)
}

// WriteGoCoverIncludingStdlib is like WriteGoCover but includes
// entries from embedded stdlib files.
func WriteGoCoverIncludingStdlib(w io.Writer, c *Collector) error {
	return writeGoCover(w, c, true)
}

func writeGoCover(w io.Writer, c *Collector, includeStdlib bool) error {
	_, err := fmt.Fprintln(w, "mode: set")
	if err != nil {
		return err
	}
	for _, e := range c.Entries() {
		if !includeStdlib && isStdlibPath(e.File) {
			continue
		}
		_, err = fmt.Fprintf(w, "%s:%d.%d,%d.%d 1 %d\n",
			e.File, e.StartLine, e.StartCol, e.EndLine, e.EndCol, e.Count)
		if err != nil {
			return err
		}
	}
	return nil
}

// isStdlibPath reports whether a file path is part of the embedded
// stdlib and should be excluded from user-facing reports by default.
func isStdlibPath(file string) bool {
	for _, prefix := range stdlibPrefixes {
		if strings.HasPrefix(file, prefix) {
			return true
		}
	}
	return false
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `go test -v -run TestWriteGoCover ./coverage/`

Expected: PASS (5 tests).

- [ ] **Step 5: Commit**

```bash
git add coverage/gocover.go coverage/gocover_test.go
git commit -m "feat(coverage): emit Go cover v1 format

WriteGoCover emits coverage data in the format consumed by
'go tool cover -html', enabling inline HTML reports over Scheme
source. Stdlib paths (scheme/, wile/, srfi/) are excluded by
default; WriteGoCoverIncludingStdlib is available for completeness.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 6: Per-line human-readable summary

**Files:**
- Create: `coverage/summary.go`
- Create: `coverage/summary_test.go`

- [ ] **Step 1: Write the failing test**

Create `coverage/summary_test.go`:

```go
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

package coverage

import (
	"bytes"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteSummary_PerLineRollup(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),   // sexpr A, line 1 col 1
		mkSrc("a.scm", 1, 7, 1, 11),  // sexpr B, line 1 col 7
		mkSrc("a.scm", 1, 13, 1, 17), // sexpr C, line 1 col 13
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true
	// sexpr C not executed

	var buf bytes.Buffer
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	// Output format: "a.scm:1  2/3 covered  max_col_reached=7"
	c.Assert(strings.Contains(out, "a.scm:1"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "2/3"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "max_col_reached=7"), qt.IsTrue, qt.Commentf("got: %s", out))
}

func TestWriteSummary_TotalFooter(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 1, 7, 1, 11),
		mkSrc("a.scm", 2, 1, 2, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true
	tpl.Executed()[2] = true

	var buf bytes.Buffer
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	c.Assert(strings.Contains(out, "TOTAL"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "3/3"), qt.IsTrue, qt.Commentf("got: %s", out))
}

func TestWriteSummary_ExcludesStdlib(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("myapp.scm", 1, 1, 1, 5),
		mkSrc("scheme/base.sld", 10, 1, 10, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true

	var buf bytes.Buffer
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	c.Assert(strings.Contains(out, "myapp.scm"), qt.IsTrue)
	c.Assert(strings.Contains(out, "scheme/base.sld"), qt.IsFalse)
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `go test -v -run TestWriteSummary ./coverage/`

Expected: FAIL with "undefined: WriteSummary".

- [ ] **Step 3: Implement the summary emitter**

Create `coverage/summary.go`:

```go
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

package coverage

import (
	"fmt"
	"io"
	"sort"
)

// lineStat aggregates per-line coverage.
type lineStat struct {
	file         string
	line         int
	total        int
	covered      int
	maxColReachd int
}

// WriteSummary writes a human-readable per-line coverage summary
// with a total footer. Each line emits:
//
//	<file>:<line>  <covered>/<total> covered  max_col_reached=<col>
//
// max_col_reached is the maximum start column of any covered sexpr
// on that line, under the interpretation "how deep did we get."
// (Honest under non-branching sequences like (begin a b c);
// approximate under branches — a high max_col_reached with low
// covered/total means execution skipped sexprs in the middle.)
//
// Stdlib paths are excluded.
func WriteSummary(w io.Writer, c *Collector) error {
	return writeSummary(w, c, false)
}

// WriteSummaryIncludingStdlib is like WriteSummary but includes
// entries from embedded stdlib files.
func WriteSummaryIncludingStdlib(w io.Writer, c *Collector) error {
	return writeSummary(w, c, true)
}

func writeSummary(w io.Writer, c *Collector, includeStdlib bool) error {
	type key struct {
		file string
		line int
	}
	stats := make(map[key]*lineStat)

	for _, e := range c.Entries() {
		if !includeStdlib && isStdlibPath(e.File) {
			continue
		}
		k := key{e.File, e.StartLine}
		s := stats[k]
		if s == nil {
			s = &lineStat{file: e.File, line: e.StartLine}
			stats[k] = s
		}
		s.total++
		if e.Count > 0 {
			s.covered++
			if e.StartCol > s.maxColReachd {
				s.maxColReachd = e.StartCol
			}
		}
	}

	keys := make([]key, 0, len(stats))
	for k := range stats {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].file != keys[j].file {
			return keys[i].file < keys[j].file
		}
		return keys[i].line < keys[j].line
	})

	var totalCovered, totalAll int
	for _, k := range keys {
		s := stats[k]
		totalCovered += s.covered
		totalAll += s.total
		_, err := fmt.Fprintf(w, "%s:%d  %d/%d covered  max_col_reached=%d\n",
			s.file, s.line, s.covered, s.total, s.maxColReachd)
		if err != nil {
			return err
		}
	}

	_, err := fmt.Fprintf(w, "TOTAL  %d/%d sexprs covered\n", totalCovered, totalAll)
	return err
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `go test -v -run TestWriteSummary ./coverage/`

Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add coverage/summary.go coverage/summary_test.go
git commit -m "feat(coverage): emit human-readable per-line summary

WriteSummary emits a per-line rollup 'N/M covered' plus
max_col_reached (the 'how deep did we get' scalar), with a TOTAL
footer. Honest about branching: high max_col_reached with low N/M
indicates skipped sub-expressions in the middle of a line.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 7: CLI integration with `--cover` flag

**Files:**
- Modify: `cmd/wile/main.go` — add flag, wire collector to engine, write report at exit
- Test: `cmd/wile/cover_integration_test.go` (new file) — end-to-end

- [ ] **Step 1: Write the failing test**

Create `cmd/wile/cover_integration_test.go`:

```go
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

package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestCLI_CoverFlag_WritesGoCoverFormat compiles the binary, runs a
// tiny Scheme file under --cover, and asserts the output has the
// Go-cover-v1 header and the expected file/line entry.
func TestCLI_CoverFlag_WritesGoCoverFormat(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()

	schemePath := filepath.Join(dir, "prog.scm")
	err := os.WriteFile(schemePath, []byte("(+ 1 2)\n"), 0o644)
	c.Assert(err, qt.IsNil)

	covPath := filepath.Join(dir, "cov.out")

	binPath := filepath.Join(dir, "wile")
	buildCmd := exec.Command("go", "build", "-o", binPath, ".")
	buildCmd.Dir = "." // cmd/wile
	buildOut, err := buildCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("build output: %s", buildOut))

	runCmd := exec.Command(binPath, "--cover", covPath, "--file", schemePath)
	runOut, err := runCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("run output: %s", runOut))

	data, err := os.ReadFile(covPath)
	c.Assert(err, qt.IsNil)
	content := string(data)

	c.Assert(strings.HasPrefix(content, "mode: set\n"), qt.IsTrue, qt.Commentf("got: %s", content))
	c.Assert(strings.Contains(content, schemePath), qt.IsTrue, qt.Commentf("got: %s", content))
	c.Assert(strings.Contains(content, " 1 1"), qt.IsTrue, qt.Commentf("executed sexpr should have Count=1; got: %s", content))
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd cmd/wile && go test -v -run TestCLI_CoverFlag ./`

Expected: FAIL (flag `--cover` not recognized, or binary builds but produces no file).

- [ ] **Step 3: Add the flag**

In `cmd/wile/main.go`, in the `Options` struct (around line 41), add:

```go
	Cover        string   `long:"cover" description:"Write Scheme-level coverage report to file (Go cover format)"`
	CoverStdlib  bool     `long:"cover-stdlib" description:"Include stdlib files in --cover output (default excludes scheme/, wile/, srfi/)"`
	CoverSummary string   `long:"cover-summary" description:"Write human-readable coverage summary to file"`
```

- [ ] **Step 4: Wire the collector into engine construction**

Find where `wile.NewEngine(...)` is called in `cmd/wile/main.go` (likely a helper like `setupEngine`). Before the call, add:

```go
	var coverageCollector *coverage.Collector
	if opts.Cover != "" || opts.CoverSummary != "" {
		coverageCollector = coverage.NewCollector()
	}
```

Add to the `wile.NewEngine` options list:

```go
	if coverageCollector != nil {
		engineOpts = append(engineOpts, wile.WithCoverage(coverageCollector))
	}
```

At the top of `cmd/wile/main.go` imports, add:

```go
	"github.com/aalpar/wile/coverage"
```

- [ ] **Step 5: Write the report at exit**

After the engine's main run call and before the binary returns, add:

```go
	if coverageCollector != nil {
		if opts.Cover != "" {
			err := writeCoverageFile(opts.Cover, coverageCollector, opts.CoverStdlib)
			if err != nil {
				Failf(err, "writing coverage file")
			}
		}
		if opts.CoverSummary != "" {
			err := writeSummaryFile(opts.CoverSummary, coverageCollector, opts.CoverStdlib)
			if err != nil {
				Failf(err, "writing coverage summary")
			}
		}
	}
```

Then add the two helpers (somewhere sensible in `cmd/wile/main.go`):

```go
func writeCoverageFile(path string, col *coverage.Collector, includeStdlib bool) error {
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("create %s: %w", path, err)
	}
	defer f.Close()
	if includeStdlib {
		return coverage.WriteGoCoverIncludingStdlib(f, col)
	}
	return coverage.WriteGoCover(f, col)
}

func writeSummaryFile(path string, col *coverage.Collector, includeStdlib bool) error {
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("create %s: %w", path, err)
	}
	defer f.Close()
	if includeStdlib {
		return coverage.WriteSummaryIncludingStdlib(f, col)
	}
	return coverage.WriteSummary(f, col)
}
```

NOTE: `fmt.Errorf` is forbidden in production Wile code (`ruleguard/rules.go` `noFmtErrorf`). Since `cmd/wile/main.go` is already exempt from this rule (check by searching for existing `fmt.Errorf` in the file — if present, it's allowed here), this is fine. If the ruleguard rejects it, replace with:

```go
	return werr.WrapForeignErrorf(err, "create %s", path)
```

and import `werr`. Verify with:

```bash
grep -n "fmt.Errorf" /Users/aalpar/projects/wile-workspace/wile/cmd/wile/main.go
```

If existing uses of `fmt.Errorf` are present, keep it consistent; otherwise use `werr`.

- [ ] **Step 6: Run the test to verify it passes**

Run: `cd cmd/wile && go test -v -run TestCLI_CoverFlag ./`

Expected: PASS.

If the test fails because the Scheme file path is absolute and the coverage file shows a different path, inspect the generated file content; the test's `strings.Contains(content, schemePath)` uses the same absolute path the CLI sees, so they should match.

- [ ] **Step 7: Manual smoke test**

```bash
cd /Users/aalpar/projects/wile-workspace/wile
make build
cat > /tmp/demo.scm <<'EOF'
(define (sq x) (* x x))
(if #t (sq 5) (sq 6))
EOF
./dist/darwin/arm64/wile --cover /tmp/cov.out --cover-summary /tmp/cov.txt --file /tmp/demo.scm
echo "--- cov.out ---"
cat /tmp/cov.out
echo "--- cov.txt ---"
cat /tmp/cov.txt
```

Expected `/tmp/cov.out` has `mode: set` header and entries for `/tmp/demo.scm`. Expected `/tmp/cov.txt` shows per-line rollup plus `TOTAL`. Specifically: `(sq 6)` in the else branch must have a `Count=0` entry (dead branch).

If `go tool cover -html=/tmp/cov.out -o /tmp/cov.html` succeeds and renders, that confirms format compatibility.

- [ ] **Step 8: Run the full test suite and linters**

```bash
cd /Users/aalpar/projects/wile-workspace/wile
make lint
go test ./...
```

Expected: both PASS.

- [ ] **Step 9: Commit**

```bash
git add cmd/wile/main.go cmd/wile/cover_integration_test.go
git commit -m "feat(cmd): add --cover, --cover-stdlib, --cover-summary flags

--cover PATH writes Scheme-level line coverage in Go cover v1 format
(consumable by 'go tool cover -html'). --cover-summary PATH writes
a human-readable per-line rollup with max_col_reached. --cover-stdlib
includes embedded stdlib paths (scheme/, wile/, srfi/), which are
excluded by default.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Task 8: Documentation

**Files:**
- Create: `docs/coverage/scheme-coverage.md` — user guide
- Modify: `README.md` — add one-line mention under a Coverage section (if such a section exists)
- Modify: `docs/CLAUDE.md` or relevant docs index to link the new page

- [ ] **Step 1: Write the user-facing documentation**

Create `docs/coverage/scheme-coverage.md`:

````markdown
# Scheme-Side Line Coverage

`wile --cover PATH` records which sub-expressions of your Scheme code
executed during a run, and writes a coverage report compatible with
`go tool cover -html`. For human-readable per-line output, use
`--cover-summary PATH`.

## Quick start

```bash
wile --cover cov.out --file myapp.scm
go tool cover -html=cov.out -o cov.html
open cov.html
```

## What is covered

Each `(file, start-line, start-col, end-line, end-col)` corresponding
to a compiled sub-expression (`SourceContext`) gets one entry.
Line-level coverage falls out as "any column on this line covered."

## Per-line summary output

`wile --cover-summary cov.txt --file myapp.scm` produces:

```
myapp.scm:12  3/5 covered  max_col_reached=27
myapp.scm:15  0/2 covered  max_col_reached=0
TOTAL  3/7 sexprs covered
```

- `N/M`: distinct sub-expressions covered vs. total on the line.
- `max_col_reached`: rightmost column of any covered sub-expression.
  Under straight sequential code (`(begin a b c)`), this scalar tells
  you "how deep into the line we got." Under branches (`if`/`cond`),
  a high `max_col_reached` with a low `N/M` means execution hit a
  later sub-expression but skipped one in the middle — a dead branch.

## Stdlib exclusion

By default, entries from the embedded stdlib (`scheme/`, `wile/`,
`srfi/` paths) are excluded. Pass `--cover-stdlib` to include them.

## Limitations

- **Peephole fusion** may drop source attribution from some
  synthesized instructions; they execute but produce no entry.
- **Coverage mode is `set`**, not `count` — entries are 0 or 1, not
  a hit count. A `count` mode is a plausible future extension.
- **Coverage is opt-in**. With no `--cover` flag, the VM dispatch
  loop runs its regular path with no coverage-related overhead.

## Embedding API

For users of the `wile` package:

```go
import (
    "github.com/aalpar/wile"
    "github.com/aalpar/wile/coverage"
)

col := coverage.NewCollector()
eng, _ := wile.NewEngine(ctx, wile.WithCoverage(col))
// ... run Scheme ...
_ = coverage.WriteGoCover(os.Stdout, col)
```
````

- [ ] **Step 2: Link from docs index**

Find the docs index. Run:

```bash
grep -n "reference\|docs/" /Users/aalpar/projects/wile-workspace/wile/CLAUDE.md | head -10
```

If there is a `docs/CLAUDE.md` or similar that lists docs, add:

```markdown
- `docs/coverage/scheme-coverage.md` — Scheme-level line coverage (--cover flag, coverage package)
```

If the project's root `CLAUDE.md` has a `## References` section listing docs, add a line there too.

- [ ] **Step 3: Commit**

```bash
git add docs/coverage/scheme-coverage.md CLAUDE.md docs/CLAUDE.md 2>/dev/null || git add docs/coverage/scheme-coverage.md
git commit -m "docs(coverage): add user guide for Scheme-side line coverage

Covers --cover, --cover-summary, --cover-stdlib flags, output
format, per-line summary interpretation (including the
max_col_reached scalar), and embedding via coverage.Collector.

Part of plans/2026-04-18-scheme-line-coverage.md."
```

---

## Final Verification

- [ ] **Step 1: Run the full build**

```bash
cd /Users/aalpar/projects/wile-workspace/wile
make lint
make covercheck
go test ./...
```

Expected: all PASS.

- [ ] **Step 2: Run the canonical benchmarks to confirm no regression**

```bash
make bench-gabriel
```

Compare the output to the pre-plan baseline (in git history, pre-`Task 2` commit). Expected: within noise (±2%).

- [ ] **Step 3: End-to-end sanity check**

```bash
cat > /tmp/cov-demo.scm <<'EOF'
(define (classify n)
  (cond ((negative? n) 'neg)
        ((zero? n) 'zero)
        (else 'pos)))

(classify 5)
(classify -1)
EOF

./dist/darwin/arm64/wile --cover /tmp/demo.out --cover-summary /tmp/demo.txt --file /tmp/cov-demo.scm
cat /tmp/demo.txt
go tool cover -html=/tmp/demo.out -o /tmp/demo.html
```

Expected:
- `zero?` branch is dead → an Entry with Count=0 for the `'zero` sexpr.
- `negative?` and `else` branches both hit.
- Summary shows per-line `N/M`, with the `cond` line reporting ≤ full coverage (because the middle clause was skipped).
- HTML report opens and color-codes covered vs. uncovered sexprs.

---

## Self-Review Notes

**Spec coverage check:**
- ✅ Per-instruction hook in Run(): Task 2.
- ✅ Go cover v1 output: Task 5.
- ✅ Per-line rollup with max_col_reached: Task 6.
- ✅ Stdlib exclusion by default: Tasks 5, 6.
- ✅ CLI integration: Task 7.
- ✅ Embedding API via `coverage.Collector`: Task 3.
- ✅ Template-tree walk for sub-templates: Task 4.
- ✅ Hot-path cost negligible when off: Task 2 step 6 (benchmark).
- ✅ User docs: Task 8.

**Placeholder scan:** None. Every step contains the actual code or command.

**Type consistency:**
- `Collector` methods: `Track`, `Entries` — used consistently.
- `Entry` fields: `File`, `StartLine`, `StartCol`, `EndLine`, `EndCol`, `Count` — used consistently across `WriteGoCover`, `WriteSummary`.
- `NativeTemplate` methods: `EnableCoverage`, `Executed`, `IsCoverageEnabled` — used consistently.
- CLI flags: `Cover`, `CoverStdlib`, `CoverSummary` — bound via `go-flags` struct tags, used consistently.

**Risk notes:**
- Task 2 Step 6: benchmark regression gate is informational, not blocking. If > 2% regression appears, a follow-up task should investigate (e.g., move the hook behind a template-level bool that short-circuits earlier).
- Task 4 Step 6: if closures don't appear in `literals`, the template-tree walker must be extended. Look in `operations_closure.go` and `OpMakeClosure.Apply` — that's the authoritative place where templates become closures.
- Task 7 Step 5: if the CLI already uses `fmt.Errorf` elsewhere, keep it; if not, use `werr.WrapForeignErrorf` for consistency with project conventions.
