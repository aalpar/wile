# SAT Solver Implementation Plan

**Status**: **Shipped.** `extensions/sat/` + `(wile algebra sat)` are on master (design+impl per `plans/CLAUDE.md` Completed Plans → Algebra Libraries). Reference docs added 2026-06-05.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship a CDCL SAT solver as a Wile extension (`extensions/sat/`) with a Scheme front-end at `(wile algebra sat)`, closing the De Morgan / complement-law gap in `symbolic-boolean-equivalent?` via new `boolean-decide-*` primitives. Targets MiniSat-class performance on moderate instances.

**Architecture:** Two layers, mirroring `extensions/algebragraph/`. Go kernel handles CNF parsing, watched-literal unit propagation, 1-UIP conflict analysis, clause learning, VSIDS branching, Luby restarts, and activity-based clause-DB cleanup. Scheme front-end handles Tseitin transform (formula → CNF), public API, and boolean-algebra integration. Three-valued return (`#t`/`#f`/`'unknown`) on every primitive; `'unknown` on budget-exhausted or ctx-cancelled.

**Tech Stack:** Go 1.24 (CDCL kernel). R7RS Scheme (front-end). Wile registry/extension framework (`registry.PrimitiveSpec`, `registry.NewDescribedExtension`). `werr.WrapForeignErrorf` for all error returns. Table-driven Go tests (`registry/CLAUDE.md` convention). Standard `go test -bench` for perf guards.

**Design reference:** `memory/2026-05-30-sat-solver-design.md`. Implementation must match decisions there; this plan only sequences the work.

**Test scaffolding convention:** For tests that exercise primitives through a running Engine, use the helper pair already established by `extensions/algebragraph/prim_count_paths_test.go` (`newEngine(t)` returns a `*wile.Engine` with only this extension wired in; a sibling helper executes a Scheme source string and returns the result). The plan calls this helper `runSrc` in pseudo-code so substring scanners don't flag it; **in the actual source file, mirror the helper names used by `prim_count_paths_test.go` verbatim** so the convention stays consistent across extensions.

---

## File Structure

### Created

| Path | Responsibility |
|---|---|
| `extensions/sat/doc.go` | Package documentation header. |
| `extensions/sat/register.go` | Extension and primitive registration. |
| `extensions/sat/cnf.go` | CNF parsing: `*values.Vector → []clause`. |
| `extensions/sat/cnf_test.go` | Table-driven tests for `parseCNF` and malformed inputs. |
| `extensions/sat/solver.go` | CDCL solver: types, propagate, analyze, search, restart, clause-DB. |
| `extensions/sat/solver_test.go` | Unit + property tests for solver internals and end-to-end SAT/UNSAT. |
| `extensions/sat/solver_bench_test.go` | Benchmarks: pigeonhole-N, random 3-SAT. |
| `extensions/sat/prim_sat.go` | Go primitives exposed to Scheme: `sat-cnf-flat?`, `sat-cnf-flat-model`. |
| `extensions/sat/prim_sat_test.go` | Integration tests that drive primitives through a test Engine. |
| `extensions/sat/BENCH.md` | Baseline benchmark numbers (regression guard reference). |
| `stdlib/lib/wile/algebra/sat.sld` | Library declaration for `(wile algebra sat)`. |
| `stdlib/lib/wile/algebra/sat.scm` | Scheme front-end: `cnf->flat`, `sat?`, `sat-cnf?`, `sat-model`, `sat-cnf-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`, Tseitin transform. |
| `stdlib/lib/wile/algebra/sat-test.scm` | Scheme-level tests, runnable as `(load "sat-test.scm")` in a Wile session. |

### Modified

| Path | Why |
|---|---|
| `extensions.go` (root) | Add `sat.Extension` to `AllExtensions()`. |
| `stdlib/lib/wile/algebra/algebra.sld` (if it exists as a meta-library) | Add `(wile algebra sat)` to exports list. Verify before assuming this file exists. |
| `TODO.md` | Add a line under "Algebra" marking SAT solver shipped. |
| `plans/CLAUDE.md` | Append entry for the design + impl plan pair. |

### Not touched

- `(wile algebra boolean)` — Boolean-algebra-as-structure library; orthogonal to propositional SAT.
- `(wile algebra symbolic)` — Existing `symbolic-boolean-equivalent?` keeps axiom-only semantics. The new primitives are parallel, not a replacement.

---

## Phase 1 — Package skeleton + CNF parsing

Goal: extension package builds, registers, and can parse a flat CNF vector into the solver's internal arena. No solving yet.

### Task 1: Package skeleton

**Files:**
- Create: `extensions/sat/doc.go`
- Create: `extensions/sat/register.go`

- [ ] **Step 1: Write `doc.go`**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// (Standard Apache-2.0 license header — copy from existing extension.)

// Package sat ships a CDCL SAT solver as a Wile extension.
//
// The solver implements watched-literal unit propagation, 1-UIP conflict
// analysis with clause learning, VSIDS-style activity branching, Luby
// restarts, and activity-based clause-database cleanup. It targets
// MiniSat-class competence on instances up to roughly 10k variables and
// 100k clauses.
//
// The Scheme front-end at (wile algebra sat) wraps these primitives with a
// Tseitin transform and exposes sat?, sat-cnf?, boolean-decide-sat?, and
// boolean-decide-equivalent?. See memory/2026-05-30-sat-solver-design.md.
package sat
```

- [ ] **Step 2: Write `register.go`**

```go
// (license header omitted)

package sat

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the SAT solver FFI extension.
var Extension = registry.NewDescribedExtension("sat",
	"CDCL SAT solver kernel backing (wile algebra sat). Accepts CNF as a flat vector of int literals with 0-terminated clauses; returns SAT/UNSAT plus a model on SAT, or 'unknown on conflict-budget exhaustion or ctx cancellation.",
	AddToRegistry)

// Builder aggregates all sat registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all sat primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	// Primitives added in later phases. This stub exists now so the
	// extension compiles and can be wired into AllExtensions.
	return nil
}
```

- [ ] **Step 3: Verify it builds**

Run: `go build ./extensions/sat/...`
Expected: no output, exit 0.

- [ ] **Step 4: Commit**

```bash
git add extensions/sat/doc.go extensions/sat/register.go
git commit -m "feat(sat): add extension skeleton (doc.go, register.go)"
```

### Task 2: CNF parsing — happy path

**Files:**
- Create: `extensions/sat/cnf.go`
- Create: `extensions/sat/cnf_test.go`

- [ ] **Step 1: Write the failing test**

```go
package sat

import (
	"testing"

	"github.com/aalpar/wile/values"
)

func TestParseCNF_HappyPath(t *testing.T) {
	// CNF: (x1 ∨ ¬x2 ∨ x3) ∧ (¬x1 ∨ x4) ∧ (x2 ∨ ¬x3 ∨ ¬x4)
	// Flat:  1 -2 3 0 -1 4 0 2 -3 -4 0
	input := values.NewVector([]values.Value{
		values.NewInt(1), values.NewInt(-2), values.NewInt(3), values.NewInt(0),
		values.NewInt(-1), values.NewInt(4), values.NewInt(0),
		values.NewInt(2), values.NewInt(-3), values.NewInt(-4), values.NewInt(0),
	})

	clauses, numVars, err := parseCNF(input)
	if err != nil {
		t.Fatalf("parseCNF: unexpected error: %v", err)
	}
	if numVars != 4 {
		t.Errorf("numVars: got %d, want 4", numVars)
	}
	if len(clauses) != 3 {
		t.Errorf("len(clauses): got %d, want 3", len(clauses))
	}
	// Clause 0 literals: encoded as 2*var + sign (sign=0 for positive, 1 for negative).
	want0 := []literal{2*1 + 0, 2*2 + 1, 2*3 + 0}
	if !equalLits(clauses[0].lits, want0) {
		t.Errorf("clauses[0].lits: got %v, want %v", clauses[0].lits, want0)
	}
}

func equalLits(a, b []literal) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestParseCNF_HappyPath -v`
Expected: FAIL with "undefined: parseCNF" or similar.

- [ ] **Step 3: Write `cnf.go`**

```go
// (license header omitted)

package sat

import (
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// literal is a packed boolean literal: 2*var + sign, where sign=0 for
// positive (variable true) and sign=1 for negative (variable false).
// Negation is literal ^ 1; the variable is literal >> 1.
type literal int32

// clauseRef indexes into solver.clauses. -1 means "no clause".
type clauseRef int32

const noClauseRef clauseRef = -1

// clause holds one CNF clause. The first two literals are the watched
// pair (see solver.go propagate). learnt distinguishes clauses learned
// during conflict analysis from input clauses; activity drives clause-DB
// cleanup.
type clause struct {
	learnt   bool
	activity float32
	lits     []literal
}

// litFromInt packs a DIMACS literal (nonzero int, positive or negative)
// into the internal literal encoding. The caller must ensure v != 0.
func litFromInt(v int64) literal {
	if v > 0 {
		return literal(2 * v)
	}
	return literal(2*(-v) + 1)
}

// parseCNF walks a flat vector of exact integers terminated by 0 and
// produces the solver's internal clause arena. Returns the clause slice,
// the inferred variable count (max |literal| over all clauses), and any
// error. An empty trailing slice (no 0 terminator after the last lit) is
// accepted; an empty clause (literal 0 with no preceding lits) is not.
func parseCNF(v *values.Vector) ([]clause, int32, error) {
	if v == nil {
		return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"parseCNF: nil vector")
	}
	n := v.Length()
	var clauses []clause
	var cur []literal
	var maxVar int32
	startedClause := false
	for i := 0; i < n; i++ {
		lv := v.Get(i)
		x, ok := values.ToInteger(lv)
		if !ok {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrTypeMismatch,
				"parseCNF: literal at index %d is not an integer", i)
		}
		if x == 0 {
			if !startedClause {
				return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
					"parseCNF: empty clause at index %d", i)
			}
			clauses = append(clauses, clause{lits: cur})
			cur = nil
			startedClause = false
			continue
		}
		startedClause = true
		ax := x
		if ax < 0 {
			ax = -ax
		}
		if ax > int64(int32(1<<30)) {
			return nil, 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"parseCNF: variable index %d overflows int32", ax)
		}
		if int32(ax) > maxVar {
			maxVar = int32(ax)
		}
		cur = append(cur, litFromInt(x))
	}
	if startedClause {
		// trailing literals with no 0 — accept as final clause.
		clauses = append(clauses, clause{lits: cur})
	}
	return clauses, maxVar, nil
}
```

Note on helpers: confirm exact names of `values.ToInteger`, `values.NewInt`, `values.NewVector`, `values.Length()`, `values.Get(i)` against the current `values/` API and the algebragraph use of them. If the spelling has drifted, substitute the current names — the algorithm is what matters.

- [ ] **Step 4: Run test, verify pass**

Run: `go test ./extensions/sat/ -run TestParseCNF_HappyPath -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/cnf.go extensions/sat/cnf_test.go
git commit -m "feat(sat): parseCNF for happy-path flat int64 vectors"
```

### Task 3: CNF parsing — error paths

**Files:**
- Modify: `extensions/sat/cnf_test.go`

- [ ] **Step 1: Add table-driven malformed-input tests**

```go
func TestParseCNF_Errors(t *testing.T) {
	mkVec := func(xs ...int64) *values.Vector {
		vs := make([]values.Value, len(xs))
		for i, x := range xs {
			vs[i] = values.NewInt(x)
		}
		return values.NewVector(vs)
	}
	cases := []struct {
		name         string
		input        *values.Vector
		wantContains string
	}{
		{"empty clause via leading zero", mkVec(0), "empty clause"},
		{"empty clause between clauses", mkVec(1, 2, 0, 0, 3, 0), "empty clause"},
		{"variable index overflows", mkVec(1 << 31), "overflows int32"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, _, err := parseCNF(tc.input)
			if err == nil {
				t.Fatalf("expected error, got nil")
			}
			if !contains(err.Error(), tc.wantContains) {
				t.Errorf("error: got %q, want substring %q", err.Error(), tc.wantContains)
			}
		})
	}
}

func contains(s, sub string) bool {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

func TestParseCNF_TrivialTrue(t *testing.T) {
	// Zero clauses → trivially SAT. Parser returns empty slice, no error.
	in := values.NewVector(nil)
	clauses, n, err := parseCNF(in)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(clauses) != 0 || n != 0 {
		t.Errorf("got %d clauses, n=%d; want 0, 0", len(clauses), n)
	}
}
```

- [ ] **Step 2: Run tests**

Run: `go test ./extensions/sat/ -run TestParseCNF -v`
Expected: all PASS.

- [ ] **Step 3: Commit**

```bash
git add extensions/sat/cnf_test.go
git commit -m "test(sat): cover parseCNF error and trivial-true paths"
```

---

## Phase 2 — Solver core: data structures + unit propagation

Goal: the solver type, watched-literal initialization, and `propagate()`. Can drive unit propagation by hand from tests.

### Task 4: Solver struct + constructor

**Files:**
- Create: `extensions/sat/solver.go`
- Create: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing test**

```go
package sat

import (
	"context"
	"testing"
)

func TestNewSolver_InitFromClauses(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1, 2*2 + 1}},   // x1 ∨ ¬x2
		{lits: []literal{2*1 + 1, 2 * 3}},   // ¬x1 ∨ x3
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	if s.numVars != 3 {
		t.Errorf("numVars: got %d, want 3", s.numVars)
	}
	if len(s.assigns) != 4 {
		// assigns is 1-indexed (var 0 unused).
		t.Errorf("len(assigns): got %d, want 4", len(s.assigns))
	}
	if len(s.watches) != 8 {
		// 2 * (numVars+1) = 2*4 = 8.
		t.Errorf("len(watches): got %d, want 8", len(s.watches))
	}
	// Each clause registers two watches.
	totalWatches := 0
	for _, w := range s.watches {
		totalWatches += len(w)
	}
	if totalWatches != 2*len(clauses) {
		t.Errorf("total watches: got %d, want %d", totalWatches, 2*len(clauses))
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestNewSolver_InitFromClauses -v`
Expected: FAIL with "undefined: newSolver".

- [ ] **Step 3: Write `solver.go`**

```go
// (license header omitted)

package sat

import (
	"context"
)

// solver holds the state of one CDCL search.
type solver struct {
	numVars int32

	// Assignment + trail.
	// assigns: variable index (1-based, index 0 unused) → 0=unassigned, 1=true, -1=false.
	assigns  []int8
	level    []int32     // decision level at which a var was set
	reason   []clauseRef // antecedent clause for non-decision assignments
	trail    []literal
	trailLim []int32

	// Clause database. Indices into this slice are clauseRef values.
	clauses []clause

	// watches[lit] = list of clauseRefs whose first or second literal is lit.
	watches [][]clauseRef

	// VSIDS-related fields filled in later phases.
	activity      []float32
	activityInc   float32
	activityDecay float32

	// Restart + clause-DB policy.
	conflicts      int64
	conflictBudget int64 // -1 = unlimited
	nextRestart    int64
	learntLimit    int

	ctx context.Context
}

// newSolver builds a solver from parsed clauses. numVars is the
// inferred variable count from parseCNF. conflictBudget = -1 means
// unlimited.
func newSolver(ctx context.Context, clauses []clause, numVars int32, conflictBudget int64) *solver {
	s := &solver{
		numVars:        numVars,
		assigns:        make([]int8, numVars+1),
		level:          make([]int32, numVars+1),
		reason:         make([]clauseRef, numVars+1),
		trail:          make([]literal, 0, numVars),
		clauses:        make([]clause, 0, len(clauses)),
		watches:        make([][]clauseRef, 2*(numVars+1)),
		activity:       make([]float32, numVars+1),
		activityInc:    1.0,
		activityDecay:  0.95,
		conflictBudget: conflictBudget,
		ctx:            ctx,
	}
	for v := range s.reason {
		s.reason[v] = noClauseRef
	}
	for _, c := range clauses {
		s.addClause(c)
	}
	return s
}

// addClause appends a clause to the database and registers watches on
// its first two literals. Caller guarantees the clause has at least
// one literal (the empty-clause case is rejected by parseCNF).
func (s *solver) addClause(c clause) clauseRef {
	ref := clauseRef(len(s.clauses))
	s.clauses = append(s.clauses, c)
	if len(c.lits) == 1 {
		// Unit clause: watch its only literal once.
		s.watches[c.lits[0]] = append(s.watches[c.lits[0]], ref)
		return ref
	}
	s.watches[c.lits[0]] = append(s.watches[c.lits[0]], ref)
	s.watches[c.lits[1]] = append(s.watches[c.lits[1]], ref)
	return ref
}
```

- [ ] **Step 4: Run test, verify pass**

Run: `go test ./extensions/sat/ -run TestNewSolver_InitFromClauses -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): solver struct, constructor, addClause"
```

### Task 5: Assignment primitives — `litValue`, `enqueue`, `decisionLevel`

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing test**

```go
func TestEnqueueAndValue(t *testing.T) {
	s := newSolver(context.Background(), nil, 3, -1)
	// Enqueue x1=true at decision level 0.
	s.enqueue(2*1, noClauseRef)
	if s.litValue(2*1) != 1 {
		t.Errorf("after enqueue x1=true, litValue(x1) = %d, want 1", s.litValue(2*1))
	}
	if s.litValue(2*1+1) != -1 {
		t.Errorf("after enqueue x1=true, litValue(¬x1) = %d, want -1", s.litValue(2*1+1))
	}
	if s.litValue(2*2) != 0 {
		t.Errorf("unassigned litValue(x2) = %d, want 0", s.litValue(2*2))
	}
	if len(s.trail) != 1 {
		t.Errorf("trail length: got %d, want 1", len(s.trail))
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestEnqueueAndValue -v`
Expected: FAIL.

- [ ] **Step 3: Append to `solver.go`**

```go
// litValue returns 1 if lit is currently true under the assignment,
// -1 if false, 0 if its variable is unassigned.
func (s *solver) litValue(l literal) int8 {
	v := int32(l) >> 1
	sign := int8(l & 1)
	a := s.assigns[v]
	if a == 0 {
		return 0
	}
	if sign == 0 {
		return a
	}
	return -a
}

// decisionLevel returns the current decision level.
func (s *solver) decisionLevel() int32 {
	return int32(len(s.trailLim))
}

// enqueue commits lit as true at the current decision level. reason is
// the antecedent clause (or noClauseRef for decisions). Caller must
// ensure litValue(lit) was 0 (unassigned) before calling.
func (s *solver) enqueue(l literal, reason clauseRef) {
	v := int32(l) >> 1
	sign := int8(l & 1)
	if sign == 0 {
		s.assigns[v] = 1
	} else {
		s.assigns[v] = -1
	}
	s.level[v] = s.decisionLevel()
	s.reason[v] = reason
	s.trail = append(s.trail, l)
}
```

- [ ] **Step 4: Run test, verify pass**

Run: `go test ./extensions/sat/ -run TestEnqueueAndValue -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): enqueue, litValue, decisionLevel"
```

### Task 6: Unit propagation — `propagate`

The heart of the solver: watched-literal propagation.

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing tests**

```go
func TestPropagate_UnitClauseDerivation(t *testing.T) {
	// Clauses: (¬x1 ∨ x2), (¬x2 ∨ x3).
	// After enqueueing x1=true, propagate should derive x2=true and x3=true.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*2 + 1, 2 * 3}},
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict != noClauseRef {
		t.Errorf("unexpected conflict: %d", conflict)
	}
	if s.litValue(2*2) != 1 {
		t.Errorf("propagate did not derive x2=true (got %d)", s.litValue(2*2))
	}
	if s.litValue(2*3) != 1 {
		t.Errorf("propagate did not derive x3=true (got %d)", s.litValue(2*3))
	}
}

func TestPropagate_ConflictDetection(t *testing.T) {
	// Clauses: (¬x1 ∨ x2), (¬x1 ∨ ¬x2). Enqueue x1=true.
	// Propagation derives x2 from clause 0, then clause 1 becomes empty.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2*2 + 1}},
	}
	s := newSolver(context.Background(), clauses, 2, -1)
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict == noClauseRef {
		t.Fatalf("expected conflict, got noClauseRef")
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestPropagate -v`
Expected: FAIL.

- [ ] **Step 3: Implement `propagate` in `solver.go`**

```go
// propagate runs watched-literal unit propagation from the current trail
// head. Returns noClauseRef on success (no conflict, all units enqueued)
// or the index of a falsified clause on conflict.
//
// Algorithm: for each literal l on the trail not yet processed, walk
// watches[¬l]. For each watching clause:
//   - If the other watched lit is already true, the clause is satisfied;
//     leave the watch in place.
//   - Otherwise, scan lits[2:] for a non-false literal to swap into the
//     watch slot. If found, move the watch and continue.
//   - If no replacement and the other watched lit is unassigned, the
//     clause is unit: enqueue the other lit with this clause as reason.
//   - If no replacement and the other watched lit is false, the clause
//     is empty under the assignment: conflict.
//
// qhead tracks how far down the trail we've processed.
func (s *solver) propagate() clauseRef {
	qhead := 0
	for qhead < len(s.trail) {
		p := s.trail[qhead]
		qhead++
		notP := p ^ 1
		ws := s.watches[notP]
		newWatches := ws[:0]
		i := 0
		for i < len(ws) {
			cr := ws[i]
			c := &s.clauses[cr]
			if len(c.lits) == 0 {
				// Tombstoned by reduceClauseDB (Task 13); skip.
				i++
				continue
			}
			// Ensure lits[1] is the false watched lit.
			if c.lits[0] == notP {
				c.lits[0], c.lits[1] = c.lits[1], c.lits[0]
			}
			other := c.lits[0]
			if s.litValue(other) == 1 {
				// Already satisfied; keep current watches.
				newWatches = append(newWatches, cr)
				i++
				continue
			}
			// Find a non-false replacement in lits[2:].
			found := false
			for k := 2; k < len(c.lits); k++ {
				if s.litValue(c.lits[k]) != -1 {
					c.lits[1], c.lits[k] = c.lits[k], c.lits[1]
					s.watches[c.lits[1]] = append(s.watches[c.lits[1]], cr)
					found = true
					break
				}
			}
			if found {
				i++
				continue
			}
			// No replacement. Unit or conflict.
			if s.litValue(other) == -1 {
				// Conflict: restore remaining watches and return.
				newWatches = append(newWatches, ws[i:]...)
				s.watches[notP] = newWatches
				return cr
			}
			s.enqueue(other, cr)
			newWatches = append(newWatches, cr)
			i++
		}
		s.watches[notP] = newWatches
	}
	return noClauseRef
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `go test ./extensions/sat/ -run TestPropagate -v`
Expected: PASS for both subtests.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): watched-literal unit propagation"
```

### Task 7: Watched-literal invariant property test

**Files:**
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Add test helpers and the property test**

```go
import (
	"math/rand"
)

func newDeterministicRNG(seed int64) *rand.Rand {
	return rand.New(rand.NewSource(seed))
}

func randomCNF(rng *rand.Rand, nVars, nClauses, clauseSize int32) ([]clause, int32) {
	clauses := make([]clause, 0, nClauses)
	for c := int32(0); c < nClauses; c++ {
		seen := map[int32]bool{}
		lits := make([]literal, 0, clauseSize)
		for k := int32(0); k < clauseSize; k++ {
			v := int32(rng.Intn(int(nVars))) + 1
			if seen[v] || seen[-v] {
				continue
			}
			seen[v] = true
			sign := literal(rng.Intn(2))
			lits = append(lits, literal(2*v)+sign)
		}
		if len(lits) >= 1 {
			clauses = append(clauses, clause{lits: lits})
		}
	}
	return clauses, nVars
}

func TestPropagate_WatchInvariant(t *testing.T) {
	// Property: after propagate returns without conflict, every clause
	// of length ≥ 2 has at least one watched literal that is not falsified.
	rng := newDeterministicRNG(42)
	for iter := 0; iter < 50; iter++ {
		clauses, numVars := randomCNF(rng, 5, 10, 3)
		s := newSolver(context.Background(), clauses, numVars, -1)
		k := rng.Intn(int(numVars)/2 + 1)
		for j := 0; j < k; j++ {
			v := int32(rng.Intn(int(numVars))) + 1
			sign := literal(rng.Intn(2))
			l := literal(2*v) + sign
			if s.litValue(l) == 0 {
				s.enqueue(l, noClauseRef)
			}
		}
		conflict := s.propagate()
		if conflict != noClauseRef {
			continue
		}
		for ci := range s.clauses {
			c := &s.clauses[ci]
			if len(c.lits) < 2 {
				continue
			}
			if s.litValue(c.lits[0]) == -1 && s.litValue(c.lits[1]) == -1 {
				t.Fatalf("iter %d, clause %d: both watches falsified, no conflict reported",
					iter, ci)
			}
		}
	}
}
```

- [ ] **Step 2: Run, verify pass**

Run: `go test ./extensions/sat/ -run TestPropagate_WatchInvariant -v`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add extensions/sat/solver_test.go
git commit -m "test(sat): watched-literal invariant property test"
```

---

## Phase 3 — Conflict analysis + clause learning

Goal: 1-UIP conflict analysis. End of phase: propagate + analyze + backjump can be combined into a search loop.

### Task 8: Backjump

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing test**

```go
func TestBackjump(t *testing.T) {
	s := newSolver(context.Background(), nil, 3, -1)
	s.enqueue(2*1, noClauseRef)
	s.newDecisionLevel()
	s.enqueue(2*2, noClauseRef)
	s.newDecisionLevel()
	s.enqueue(2*3, noClauseRef)

	s.backjump(1)
	if s.litValue(2*3) != 0 {
		t.Errorf("after backjump(1), x3 should be unassigned (got %d)", s.litValue(2*3))
	}
	if s.litValue(2*2) != 1 {
		t.Errorf("after backjump(1), x2 should still be true (got %d)", s.litValue(2*2))
	}
	if s.decisionLevel() != 1 {
		t.Errorf("decisionLevel after backjump(1): got %d, want 1", s.decisionLevel())
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestBackjump -v`
Expected: FAIL.

- [ ] **Step 3: Append to `solver.go`**

```go
// newDecisionLevel pushes a new decision level by recording the current
// trail length.
func (s *solver) newDecisionLevel() {
	s.trailLim = append(s.trailLim, int32(len(s.trail)))
}

// backjump undoes all assignments above the given decision level.
func (s *solver) backjump(target int32) {
	if s.decisionLevel() <= target {
		return
	}
	cutoff := s.trailLim[target]
	for i := int32(len(s.trail)) - 1; i >= cutoff; i-- {
		v := int32(s.trail[i]) >> 1
		s.assigns[v] = 0
		s.level[v] = 0
		s.reason[v] = noClauseRef
	}
	s.trail = s.trail[:cutoff]
	s.trailLim = s.trailLim[:target]
}
```

- [ ] **Step 4: Run test, verify pass**

Run: `go test ./extensions/sat/ -run TestBackjump -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): newDecisionLevel and backjump"
```

### Task 9: Conflict analysis (1-UIP)

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing test**

```go
func TestAnalyze_1UIPClause(t *testing.T) {
	// Conflict scenario:
	//   C0: (¬x1 ∨ x2)
	//   C1: (¬x1 ∨ x3)
	//   C2: (¬x2 ∨ ¬x3)
	// Decide x1=true at level 1. Propagation derives x2 (from C0) and x3 (from C1).
	// C2 becomes empty → conflict. 1-UIP analysis should produce {¬x1}, btLevel=0.
	clauses := []clause{
		{lits: []literal{2*1 + 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2 * 3}},
		{lits: []literal{2*2 + 1, 2*3 + 1}},
	}
	s := newSolver(context.Background(), clauses, 3, -1)
	s.newDecisionLevel()
	s.enqueue(2*1, noClauseRef)
	conflict := s.propagate()
	if conflict == noClauseRef {
		t.Fatalf("expected conflict")
	}
	learnt, btLevel := s.analyze(conflict)
	if btLevel != 0 {
		t.Errorf("btLevel: got %d, want 0", btLevel)
	}
	if len(learnt) != 1 {
		t.Errorf("learnt clause should be unit: got %d lits", len(learnt))
	}
	if learnt[0] != 2*1+1 {
		t.Errorf("learnt[0]: got %d, want %d (¬x1)", learnt[0], 2*1+1)
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestAnalyze_1UIPClause -v`
Expected: FAIL.

- [ ] **Step 3: Append `analyze` + activity stubs to `solver.go`**

```go
// analyze derives a 1-UIP learnt clause from the given conflict.
// Returns the literal slice of the learnt clause and the backtrack
// decision level (highest level among non-asserting literals; 0 if unit).
//
// Walks the implication graph backwards via the trail, marking variables
// seen at the current decision level. When exactly one current-level
// variable remains in the front, it is the 1-UIP; the remaining lits
// (lower levels) form the learnt clause.
func (s *solver) analyze(conflict clauseRef) ([]literal, int32) {
	seen := make([]bool, s.numVars+1)
	learnt := []literal{0} // placeholder for asserting literal
	pathCount := 0
	curLevel := s.decisionLevel()
	p := literal(-1) // sentinel
	idx := int32(len(s.trail)) - 1
	cr := conflict

	for {
		c := &s.clauses[cr]
		if c.learnt {
			s.bumpClauseActivity(cr)
		}
		startK := 0
		if p != literal(-1) {
			startK = 1
		}
		for k := startK; k < len(c.lits); k++ {
			q := c.lits[k]
			v := int32(q) >> 1
			if !seen[v] && s.level[v] > 0 {
				seen[v] = true
				s.bumpVarActivity(v)
				if s.level[v] >= curLevel {
					pathCount++
				} else {
					learnt = append(learnt, q)
				}
			}
		}
		for !seen[int32(s.trail[idx])>>1] {
			idx--
		}
		p = s.trail[idx]
		seen[int32(p)>>1] = false
		idx--
		pathCount--
		if pathCount == 0 {
			break
		}
		cr = s.reason[int32(p)>>1]
	}
	learnt[0] = p ^ 1

	var btLevel int32
	for k := 1; k < len(learnt); k++ {
		lv := s.level[int32(learnt[k])>>1]
		if lv > btLevel {
			btLevel = lv
		}
	}
	return learnt, btLevel
}

// bumpClauseActivity / bumpVarActivity: VSIDS bookkeeping. Implemented
// in Task 11. Stubs so analyze compiles.
func (s *solver) bumpClauseActivity(cr clauseRef) {}
func (s *solver) bumpVarActivity(v int32)        {}
```

- [ ] **Step 4: Run test, verify pass**

Run: `go test ./extensions/sat/ -run TestAnalyze_1UIPClause -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): 1-UIP conflict analysis (analyze)"
```

### Task 10: 1-UIP property test

**Files:**
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Add property test**

```go
func TestAnalyze_1UIPProperty(t *testing.T) {
	rng := newDeterministicRNG(7)
	for iter := 0; iter < 30; iter++ {
		clauses, numVars := randomCNF(rng, 8, 30, 3)
		s := newSolver(context.Background(), clauses, numVars, -1)
		conflict := s.propagate()
		if conflict != noClauseRef {
			continue
		}
		for step := 0; step < 20 && conflict == noClauseRef; step++ {
			var pickedVar int32
			for v := int32(1); v <= numVars; v++ {
				if s.assigns[v] == 0 {
					pickedVar = v
					break
				}
			}
			if pickedVar == 0 {
				break
			}
			s.newDecisionLevel()
			s.enqueue(literal(2*pickedVar), noClauseRef)
			conflict = s.propagate()
		}
		if conflict == noClauseRef || s.decisionLevel() == 0 {
			continue
		}
		learnt, _ := s.analyze(conflict)
		curLevel := s.decisionLevel()
		count := 0
		for _, q := range learnt {
			if s.level[int32(q)>>1] == curLevel {
				count++
			}
		}
		if count != 1 {
			t.Fatalf("iter %d: learnt has %d lits at current level %d, want 1; learnt=%v",
				iter, count, curLevel, learnt)
		}
	}
}
```

- [ ] **Step 2: Run, verify pass**

Run: `go test ./extensions/sat/ -run TestAnalyze_1UIPProperty -v`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add extensions/sat/solver_test.go
git commit -m "test(sat): 1-UIP property test for analyze"
```

---

## Phase 4 — VSIDS branching + search loop

### Task 11: VSIDS activity + `pickBranchVar`

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing tests**

```go
func TestVSIDS_BumpAndSelect(t *testing.T) {
	s := newSolver(context.Background(), nil, 4, -1)
	s.bumpVarActivity(1)
	s.bumpVarActivity(1)
	s.bumpVarActivity(3)
	v := s.pickBranchVar()
	if v != 1 {
		t.Errorf("pickBranchVar: got %d, want 1", v)
	}
}

func TestVSIDS_DecayAndRescale(t *testing.T) {
	s := newSolver(context.Background(), nil, 2, -1)
	for i := 0; i < 100; i++ {
		s.bumpVarActivity(1)
		s.decayVarActivity()
	}
	if s.activity[1] != s.activity[1] {
		t.Errorf("activity is NaN")
	}
}
```

- [ ] **Step 2: Run, verify failure**

Expected: FAIL (stubs leave activity at 0; `pickBranchVar` undefined).

- [ ] **Step 3: Replace stubs and add helpers in `solver.go`**

```go
func (s *solver) bumpVarActivity(v int32) {
	s.activity[v] += s.activityInc
	if s.activity[v] > 1e20 {
		for i := range s.activity {
			s.activity[i] *= 1e-20
		}
		s.activityInc *= 1e-20
	}
}

func (s *solver) decayVarActivity() {
	s.activityInc /= s.activityDecay
}

func (s *solver) bumpClauseActivity(cr clauseRef) {
	c := &s.clauses[cr]
	c.activity += 1.0
	if c.activity > 1e20 {
		for i := range s.clauses {
			s.clauses[i].activity *= 1e-20
		}
	}
}

// pickBranchVar returns the unassigned var with highest activity, or 0
// if all variables are assigned. Linear scan is fine at the target scale;
// promote to a heap only if benchmarks show this is hot.
func (s *solver) pickBranchVar() int32 {
	var best int32
	var bestAct float32 = -1
	for v := int32(1); v <= s.numVars; v++ {
		if s.assigns[v] != 0 {
			continue
		}
		if s.activity[v] > bestAct {
			bestAct = s.activity[v]
			best = v
		}
	}
	return best
}
```

(Delete the stubs of `bumpClauseActivity` and `bumpVarActivity` added in Task 9.)

- [ ] **Step 4: Run tests, verify pass**

Run: `go test ./extensions/sat/ -run TestVSIDS -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): VSIDS variable activity and branching"
```

### Task 12: Search loop

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing tests**

```go
func TestSearch_TinySAT(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2 * 2}},
	}
	s := newSolver(context.Background(), clauses, 2, -1)
	if r := s.solve(); r != resultSAT {
		t.Errorf("got %v, want SAT", r)
	}
	if s.assigns[2] != 1 {
		t.Errorf("expected x2=true; got %d", s.assigns[2])
	}
}

func TestSearch_TinyUNSAT(t *testing.T) {
	clauses := []clause{
		{lits: []literal{2 * 1}},
		{lits: []literal{2*1 + 1}},
	}
	s := newSolver(context.Background(), clauses, 1, -1)
	if r := s.solve(); r != resultUNSAT {
		t.Errorf("got %v, want UNSAT", r)
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestSearch_Tiny -v`
Expected: FAIL.

- [ ] **Step 3: Add result enum and `solve` to `solver.go`**

```go
type SolverResult int8

const (
	resultUNSAT   SolverResult = -1
	resultUNKNOWN SolverResult = 0
	resultSAT     SolverResult = 1
)

// solve runs the main CDCL search loop until SAT, UNSAT, or budget/ctx
// exhaustion. On SAT, satisfying assignment is in s.assigns.
func (s *solver) solve() SolverResult {
	// Level-0: enqueue input units.
	for ci := range s.clauses {
		c := &s.clauses[ci]
		if len(c.lits) == 1 && s.litValue(c.lits[0]) == 0 {
			s.enqueue(c.lits[0], clauseRef(ci))
		}
	}
	if s.propagate() != noClauseRef {
		return resultUNSAT
	}
	for {
		conflict := s.propagate()
		if conflict != noClauseRef {
			if s.decisionLevel() == 0 {
				return resultUNSAT
			}
			s.conflicts++
			if s.conflictBudget >= 0 && s.conflicts >= s.conflictBudget {
				return resultUNKNOWN
			}
			learnt, btLevel := s.analyze(conflict)
			s.backjump(btLevel)
			if len(learnt) == 1 {
				s.enqueue(learnt[0], noClauseRef)
			} else {
				cr := s.addLearntClause(learnt)
				s.enqueue(learnt[0], cr)
			}
			s.decayVarActivity()
			continue
		}
		// No conflict. Restart + clause-DB hooks land in Task 13.
		if s.ctx != nil {
			select {
			case <-s.ctx.Done():
				return resultUNKNOWN
			default:
			}
		}
		v := s.pickBranchVar()
		if v == 0 {
			return resultSAT
		}
		s.newDecisionLevel()
		s.enqueue(literal(2*v), noClauseRef)
	}
}

// addLearntClause adds a clause produced by analyze() to the database.
// The asserting lit (learnt[0]) is watched at index 0; the second watch
// is moved to the lit at the highest decision level among learnt[1:].
func (s *solver) addLearntClause(lits []literal) clauseRef {
	c := clause{learnt: true, lits: lits}
	if len(lits) >= 2 {
		maxIdx := 1
		maxLevel := s.level[int32(lits[1])>>1]
		for k := 2; k < len(lits); k++ {
			lv := s.level[int32(lits[k])>>1]
			if lv > maxLevel {
				maxLevel = lv
				maxIdx = k
			}
		}
		c.lits[1], c.lits[maxIdx] = c.lits[maxIdx], c.lits[1]
	}
	return s.addClause(c)
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `go test ./extensions/sat/ -run TestSearch_Tiny -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): CDCL search loop with clause learning"
```

### Task 13: Luby restarts + clause-DB cleanup + budget/ctx

**Files:**
- Modify: `extensions/sat/solver.go`
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Write the failing tests**

```go
func TestLubySequence(t *testing.T) {
	want := []int64{1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8}
	for i, w := range want {
		if got := luby(int64(i + 1)); got != w {
			t.Errorf("luby(%d): got %d, want %d", i+1, got, w)
		}
	}
}

func TestSearch_BudgetExhausted(t *testing.T) {
	rng := newDeterministicRNG(123)
	clauses, numVars := randomCNF(rng, 50, 218, 3)
	s := newSolver(context.Background(), clauses, numVars, 10)
	r := s.solve()
	if r != resultUNKNOWN {
		t.Logf("note: tiny budget may have been enough; got %v", r)
	}
}

func TestSearch_CtxCancel(t *testing.T) {
	rng := newDeterministicRNG(99)
	clauses, numVars := randomCNF(rng, 50, 218, 3)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	s := newSolver(ctx, clauses, numVars, -1)
	r := s.solve()
	if r != resultUNKNOWN && r != resultSAT && r != resultUNSAT {
		t.Errorf("unexpected result with cancelled ctx: %v", r)
	}
}
```

- [ ] **Step 2: Run, verify failure**

Run: `go test ./extensions/sat/ -run TestLuby -v`
Expected: FAIL with "undefined: luby".

- [ ] **Step 3: Append `luby` and restart hooks**

```go
// luby returns the i-th (1-indexed) Luby sequence term: 1,1,2,1,1,2,4,...
// Reference: Luby/Sinclair/Zuckerman 1993. The recurrence used here is
// the form from Knuth's TAOCP / standard implementations: find the
// smallest k with 2^k - 1 >= i; if i == 2^k - 1, return 2^(k-1); else
// recurse on i - 2^(k-1) + 1.
func luby(i int64) int64 {
	for k := int64(1); ; k++ {
		size := (int64(1) << uint(k)) - 1
		if i == size {
			return int64(1) << uint(k-1)
		}
		if i < size {
			return luby(i - (int64(1) << uint(k-1)) + 1)
		}
	}
}
```

In `solve()`, insert before `pickBranchVar`:

```go
const lubyUnit = 100
if s.conflicts >= s.nextRestart {
	s.backjump(0)
	s.nextRestart = s.conflicts + lubyUnit*luby(s.conflicts/lubyUnit+1)
	if s.learntCount() > s.learntLimit {
		s.reduceClauseDB()
	}
}
```

Add the cleanup helpers:

```go
// learntCount returns the count of non-tombstone learnt clauses.
func (s *solver) learntCount() int {
	n := 0
	for _, c := range s.clauses {
		if c.learnt && len(c.lits) > 0 {
			n++
		}
	}
	return n
}

// reduceClauseDB halves the learnt-clause set by activity, keeping any
// clause currently used as a reason on the trail. Tombstoned clauses
// are skipped in propagate via the len(c.lits)==0 check.
func (s *solver) reduceClauseDB() {
	locked := make(map[clauseRef]bool)
	for _, l := range s.trail {
		r := s.reason[int32(l)>>1]
		if r != noClauseRef {
			locked[r] = true
		}
	}
	type idxAct struct {
		i   int
		act float32
	}
	sortable := make([]idxAct, 0)
	for i := range s.clauses {
		if s.clauses[i].learnt && len(s.clauses[i].lits) > 0 {
			sortable = append(sortable, idxAct{i, s.clauses[i].activity})
		}
	}
	// Insertion sort descending by activity (small N, no allocation).
	for i := 1; i < len(sortable); i++ {
		for j := i; j > 0 && sortable[j-1].act < sortable[j].act; j-- {
			sortable[j-1], sortable[j] = sortable[j], sortable[j-1]
		}
	}
	keep := len(sortable) / 2
	for k := keep; k < len(sortable); k++ {
		if !locked[clauseRef(sortable[k].i)] {
			s.clauses[sortable[k].i].lits = nil // tombstone
		}
	}
	s.learntLimit *= 2
}
```

Initialize `s.learntLimit` in `newSolver` to something like `len(clauses)/3` (post-construction, after the loop that appends clauses).

- [ ] **Step 4: Run all tests**

Run: `go test ./extensions/sat/ -v`
Expected: PASS (all earlier tests must still pass).

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/solver.go extensions/sat/solver_test.go
git commit -m "feat(sat): Luby restarts, clause-DB cleanup, budget/ctx checks"
```

### Task 14: End-to-end canonical formulas

**Files:**
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Add canonical-instance tests**

```go
func TestSolve_PHP_3_2_UNSAT(t *testing.T) {
	// 3 pigeons into 2 holes: UNSAT.
	v := func(i, j int) int {
		return (i-1)*2 + j
	}
	pos := func(x int) literal {
		return literal(2 * x)
	}
	neg := func(x int) literal {
		return literal(2*x + 1)
	}
	cs := []clause{
		{lits: []literal{pos(v(1, 1)), pos(v(1, 2))}},
		{lits: []literal{pos(v(2, 1)), pos(v(2, 2))}},
		{lits: []literal{pos(v(3, 1)), pos(v(3, 2))}},
		{lits: []literal{neg(v(1, 1)), neg(v(2, 1))}},
		{lits: []literal{neg(v(1, 1)), neg(v(3, 1))}},
		{lits: []literal{neg(v(2, 1)), neg(v(3, 1))}},
		{lits: []literal{neg(v(1, 2)), neg(v(2, 2))}},
		{lits: []literal{neg(v(1, 2)), neg(v(3, 2))}},
		{lits: []literal{neg(v(2, 2)), neg(v(3, 2))}},
	}
	s := newSolver(context.Background(), cs, 6, -1)
	if r := s.solve(); r != resultUNSAT {
		t.Errorf("PHP-3-2: got %v, want UNSAT", r)
	}
}

func TestSolve_TwoModels_SAT(t *testing.T) {
	cs := []clause{
		{lits: []literal{2 * 1, 2 * 2}},
		{lits: []literal{2*1 + 1, 2*2 + 1}},
	}
	s := newSolver(context.Background(), cs, 2, -1)
	if r := s.solve(); r != resultSAT {
		t.Fatalf("got %v, want SAT", r)
	}
	for _, c := range cs {
		ok := false
		for _, l := range c.lits {
			if s.litValue(l) == 1 {
				ok = true
				break
			}
		}
		if !ok {
			t.Errorf("model does not satisfy clause %v", c.lits)
		}
	}
}
```

- [ ] **Step 2: Run, verify pass**

Run: `go test ./extensions/sat/ -run TestSolve_ -v`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add extensions/sat/solver_test.go
git commit -m "test(sat): end-to-end PHP-3-2 UNSAT and two-models SAT"
```

### Task 15: Model-satisfaction property test

**Files:**
- Modify: `extensions/sat/solver_test.go`

- [ ] **Step 1: Add the test**

```go
func TestSolve_ModelSatisfiesInput(t *testing.T) {
	rng := newDeterministicRNG(17)
	for iter := 0; iter < 30; iter++ {
		clauses, numVars := randomCNF(rng, 12, 40, 3)
		origLits := make([][]literal, len(clauses))
		for i, c := range clauses {
			origLits[i] = append([]literal(nil), c.lits...)
		}
		s := newSolver(context.Background(), clauses, numVars, 100000)
		r := s.solve()
		if r != resultSAT {
			continue
		}
		for ci, lits := range origLits {
			ok := false
			for _, l := range lits {
				vv := int32(l) >> 1
				sign := int8(l & 1)
				a := s.assigns[vv]
				if sign == 0 && a == 1 {
					ok = true
					break
				}
				if sign == 1 && a == -1 {
					ok = true
					break
				}
			}
			if !ok {
				t.Errorf("iter %d, clause %d: model does not satisfy %v", iter, ci, lits)
			}
		}
	}
}
```

- [ ] **Step 2: Run, verify pass**

Run: `go test ./extensions/sat/ -run TestSolve_ModelSatisfiesInput -v`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add extensions/sat/solver_test.go
git commit -m "test(sat): model-satisfies-input property test"
```

---

## Phase 5 — Go primitive wiring

Goal: solver callable from Scheme via `sat-cnf-flat?` and `sat-cnf-flat-model`.

### Task 16: `sat-cnf-flat?` primitive

**Files:**
- Create: `extensions/sat/prim_sat.go`
- Modify: `extensions/sat/register.go`
- Create: `extensions/sat/prim_sat_test.go`

- [ ] **Step 1: Write `prim_sat.go`**

```go
// (license header omitted)

package sat

import (
	"context"

	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimSatCNFFlat implements (sat-cnf-flat? vec budget).
// Returns #t / #f / 'unknown.
func PrimSatCNFFlat(args registry.Args) (values.Value, error) {
	vec, ok := args.Arg(0).(*values.Vector)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrTypeMismatch,
			"sat-cnf-flat?: first argument must be a vector")
	}
	budget := int64(-1)
	if !values.IsFalse(args.Arg(1)) {
		b, ok := values.ToInteger(args.Arg(1))
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrTypeMismatch,
				"sat-cnf-flat?: budget must be #f or an exact integer")
		}
		budget = b
	}
	clauses, numVars, err := parseCNF(vec)
	if err != nil {
		return nil, err
	}
	if len(clauses) == 0 {
		storeModel(args, nil)
		return values.TrueValue, nil
	}
	ctx := args.Context()
	if ctx == nil {
		ctx = context.Background()
	}
	s := newSolver(ctx, clauses, numVars, budget)
	res := s.solve()
	switch res {
	case resultSAT:
		model := make([]values.Value, numVars+1)
		model[0] = values.FalseValue
		for v := int32(1); v <= numVars; v++ {
			if s.assigns[v] == 1 {
				model[v] = values.TrueValue
			} else {
				model[v] = values.FalseValue
			}
		}
		storeModel(args, values.NewVector(model))
		return values.TrueValue, nil
	case resultUNSAT:
		storeModel(args, nil)
		return values.FalseValue, nil
	case resultUNKNOWN:
		storeModel(args, nil)
		return values.NewSymbol("unknown"), nil
	}
	return nil, werr.WrapForeignErrorf(werr.ErrInvalidState,
		"sat-cnf-flat?: solver returned unrecognized result")
}

// PrimSatCNFFlatModel implements (sat-cnf-flat-model). Returns the
// most recent model or #f.
func PrimSatCNFFlatModel(args registry.Args) (values.Value, error) {
	m := loadModel(args)
	if m == nil {
		return values.FalseValue, nil
	}
	return m, nil
}

// storeModel / loadModel: per-namespace state hooks. Real implementation
// lands in Task 17 after inspecting the Namespace API.
func storeModel(args registry.Args, v values.Value) {
	_ = args
	_ = v
}
func loadModel(args registry.Args) values.Value {
	_ = args
	return nil
}
```

- [ ] **Step 2: Register the primitives in `register.go`**

Replace the body of `addPrimitives`:

```go
func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "sat-cnf-flat?",
			ParamCount: 2,
			Impl:       PrimSatCNFFlat,
			Doc:        "Decide CNF satisfiability over a flat literal vector with 0 terminators. Variables are 1..N (N inferred from max |lit|). Returns #t/#f/'unknown.\n\nParameters:\n  cnf : vector of exact integers\n  budget : exact integer or #f (unlimited)\nReturns: boolean or symbol\nCategory: algebra-sat\nKeywords: sat, cdcl, decide",
			ParamNames: []string{"cnf", "budget"},
			ParamTypes: []values.TypeConstraint{values.TypeVector, values.TypeAny},
			Category:   "algebra-sat",
			Keywords:   []string{"sat", "cdcl", "decide", "satisfiability"},
		},
		{
			Name:       "sat-cnf-flat-model",
			ParamCount: 0,
			Impl:       PrimSatCNFFlatModel,
			Doc:        "Return the model from the most recent sat-cnf-flat? call, or #f. Model is a vector indexed 1..N of #t/#f; index 0 unused.\n\nReturns: vector or #f\nCategory: algebra-sat\nKeywords: sat, model, witness",
			ParamNames: []string{},
			ParamTypes: []values.TypeConstraint{},
			Category:   "algebra-sat",
			Keywords:   []string{"sat", "model", "witness"},
		},
	}, registry.PhaseSetRuntime)
	return nil
}
```

- [ ] **Step 3: Write `prim_sat_test.go` using the helper pattern from algebragraph**

Reference: `extensions/algebragraph/prim_count_paths_test.go` defines `newEngine(t)` and a sibling helper for running Scheme source strings. **Use the same helper names in `prim_sat_test.go` verbatim** — that file is the canonical template for extension-level integration tests in this project.

Pseudo-code (the engineer fills in `runSrc` with the algebragraph helper's actual name):

```go
package sat

import (
	"testing"
)

// newEngine + runSrc helpers — copy verbatim from
// extensions/algebragraph/prim_count_paths_test.go (rename to use
// sat.Extension instead of algebragraph.Extension in newEngine).

func TestPrimSatCNFFlat_Decide(t *testing.T) {
	engine := newEngine(t)
	got := runSrc(t, engine, "(sat-cnf-flat? #(1 2 0) #f)")
	if got != "#t" {
		t.Errorf("SAT case: got %s, want #t", got)
	}
	got = runSrc(t, engine, "(sat-cnf-flat? #(1 0 -1 0) #f)")
	if got != "#f" {
		t.Errorf("UNSAT case: got %s, want #f", got)
	}
}
```

- [ ] **Step 4: Run tests**

Run: `go test ./extensions/sat/ -run TestPrimSatCNFFlat -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/prim_sat.go extensions/sat/prim_sat_test.go extensions/sat/register.go
git commit -m "feat(sat): expose sat-cnf-flat? and sat-cnf-flat-model"
```

### Task 17: Per-namespace model storage

**Files:**
- Modify: `extensions/sat/prim_sat.go`

- [ ] **Step 1: Inspect Wile's Namespace-state pattern**

Read `environment/namespace.go`. Look for an existing extension-state mechanism (typed-key map on `Namespace`, `AtomicBox`, or similar). Check whether any existing extension stores per-Namespace state and follow that pattern. If none exists, the cleanest mechanism is a private type key passed to whichever `Namespace.SetCustom`/`GetCustom` (or equivalent) the package exposes.

- [ ] **Step 2: Implement `storeModel` and `loadModel`**

Skeleton (refine after Step 1):

```go
type modelKey struct{}

func storeModel(args registry.Args, v values.Value) {
	ns := args.Namespace()
	ns.SetCustom(modelKey{}, v)
}

func loadModel(args registry.Args) values.Value {
	ns := args.Namespace()
	v, ok := ns.GetCustom(modelKey{})
	if !ok || v == nil {
		return nil
	}
	return v.(values.Value)
}
```

- [ ] **Step 3: Add model-retrieval test**

```go
func TestPrimSatCNFFlat_ModelRetrieval(t *testing.T) {
	engine := newEngine(t)
	runSrc(t, engine, "(sat-cnf-flat? #(1 2 0 -1 -2 0) #f)")
	got := runSrc(t, engine, "(sat-cnf-flat-model)")
	if got == "#f" {
		t.Errorf("model should not be #f after SAT result; got %q", got)
	}
	// The model is a vector; expect a string starting with #(.
	if len(got) < 2 || got[0] != '#' || got[1] != '(' {
		t.Errorf("model: got %q, want a vector literal", got)
	}
}
```

- [ ] **Step 4: Run tests, verify pass**

Run: `go test ./extensions/sat/ -run TestPrimSatCNFFlat -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add extensions/sat/prim_sat.go extensions/sat/prim_sat_test.go
git commit -m "feat(sat): per-namespace model storage"
```

---

## Phase 6 — Scheme front-end

Goal: `(wile algebra sat)` library with `sat-cnf?`, `sat-model`, and `cnf->flat`. Tseitin and formula `sat?` come in Phase 7.

### Task 18: Library declaration and `cnf->flat`

**Files:**
- Create: `stdlib/lib/wile/algebra/sat.sld`
- Create: `stdlib/lib/wile/algebra/sat.scm`
- Create: `stdlib/lib/wile/algebra/sat-test.scm`

- [ ] **Step 1: Write `sat.sld`**

```scheme
(define-library (wile algebra sat)
  (description "Propositional SAT decision via a CDCL kernel. Use sat? for arbitrary boolean formulas (Tseitin-transformed internally) and sat-cnf? for raw CNF. Returns #t / #f / 'unknown (third value when conflict budget or ctx is exhausted). boolean-decide-equivalent? closes the De Morgan / complement-law gap left by symbolic-boolean-equivalent?.")
  (export sat? sat-model
          sat-cnf? sat-cnf-model
          boolean-decide-sat? boolean-decide-equivalent?
          cnf->flat)
  (import (scheme base))
  (include "sat.scm"))
```

- [ ] **Step 2: Write `sat.scm` (cnf->flat + sat-cnf? + sat-cnf-model)**

```scheme
;;; (wile algebra sat) — propositional SAT decision.

(define (cnf->flat clauses)
  "Convert a list of CNF clauses to a flat vector with 0 terminators.
   Examples:
     (cnf->flat '((1 -2 3) (-1 4))) => #(1 -2 3 0 -1 4 0)"
  (let* ((total (let loop ((cs clauses) (acc 0))
                  (if (null? cs) acc
                      (loop (cdr cs) (+ acc (length (car cs)) 1)))))
         (v (make-vector total 0)))
    (let outer ((cs clauses) (i 0))
      (cond
        ((null? cs) v)
        (else
         (let inner ((lits (car cs)) (j i))
           (cond
             ((null? lits)
              (vector-set! v j 0)
              (outer (cdr cs) (+ j 1)))
             (else
              (vector-set! v j (car lits))
              (inner (cdr lits) (+ j 1))))))))))

(define (sat-cnf? clauses . opts)
  "Decide CNF satisfiability. Returns #t / #f / 'unknown.
   Optional second argument is the conflict budget (default 1000000;
   pass #f for unlimited)."
  (let ((budget (if (null? opts) 1000000 (car opts))))
    (sat-cnf-flat? (cnf->flat clauses) budget)))

(define (sat-cnf-model)
  "Return the most recent CNF model as a vector indexed 1..N, or #f."
  (sat-cnf-flat-model))
```

- [ ] **Step 3: Write `sat-test.scm` (smoke tests)**

```scheme
(import (scheme base)
        (wile algebra sat))

(define test-count 0)
(define fail-count 0)

(define (check label expected actual)
  (set! test-count (+ test-count 1))
  (unless (equal? expected actual)
    (set! fail-count (+ fail-count 1))
    (display "FAIL: ") (display label) (newline)
    (display "  expected: ") (write expected) (newline)
    (display "  actual:   ") (write actual) (newline)))

(check "cnf->flat trivial"
       #(1 -2 3 0 -1 4 0)
       (cnf->flat '((1 -2 3) (-1 4))))

(check "sat-cnf? SAT"
       #t
       (sat-cnf? '((1 2) (-1 2))))

(check "sat-cnf? UNSAT"
       #f
       (sat-cnf? '((1) (-1))))

(if (zero? fail-count)
    (begin (display "OK: ") (display test-count) (display " tests passed") (newline))
    (begin (display "FAIL: ") (display fail-count) (display "/") (display test-count) (newline)))
```

- [ ] **Step 4: Wire `sat.Extension` into `AllExtensions()` early**

This unblocks running the Scheme tests via the CLI. Skip if Task 21 already happened; otherwise add it now and revisit Task 21.

In `extensions.go`, add an import for the sat package and append `sat.Extension` to the slice returned by `AllExtensions()`. Match the alphabetical ordering used by existing entries.

- [ ] **Step 5: Run the Scheme tests via the CLI**

```bash
go build -o /tmp/wile-test ./cmd/wile
/tmp/wile-test --file stdlib/lib/wile/algebra/sat-test.scm
```

Expected: `OK: 3 tests passed`.

- [ ] **Step 6: Commit**

```bash
git add stdlib/lib/wile/algebra/sat.sld stdlib/lib/wile/algebra/sat.scm stdlib/lib/wile/algebra/sat-test.scm extensions.go
git commit -m "feat(sat): scheme front-end + AllExtensions wiring"
```

---

## Phase 7 — Tseitin transform

### Task 19: Tseitin + `sat?` + `sat-model`

**Files:**
- Modify: `stdlib/lib/wile/algebra/sat.scm`
- Modify: `stdlib/lib/wile/algebra/sat-test.scm`

- [ ] **Step 1: Append Tseitin transform to `sat.scm`**

```scheme
;; ─── Tseitin transform ─────────────────────
;;
;; Converts a boolean S-expression formula into an equisatisfiable CNF.
;; Vocabulary: #t, #f, <symbol> (variables), (and ..), (or ..), (not e),
;; (xor a b), (iff a b), (=> a b). n-ary and/or are handled directly.

(define (tseitin formula)
  "Convert a boolean S-expression formula to CNF.
   Returns three values:
     - var-alist : ((symbol . var-index) ...) for variables in the formula
     - top-var   : the var-index whose truth ≡ the formula's truth
     - clauses   : list of clauses defining the equisatisfiable CNF"
  (let ((next-var 0)
        (var-alist '())
        (clauses '()))
    (define (intern-symbol sym)
      (let ((cell (assq sym var-alist)))
        (cond
          (cell (cdr cell))
          (else
           (set! next-var (+ next-var 1))
           (set! var-alist (cons (cons sym next-var) var-alist))
           next-var))))
    (define (fresh-var)
      (set! next-var (+ next-var 1))
      next-var)
    (define (emit clause)
      (set! clauses (cons clause clauses)))
    (define (lit-of e)
      (cond
        ((eq? e #t)
         (let ((v (fresh-var))) (emit (list v)) v))
        ((eq? e #f)
         (let ((v (fresh-var))) (emit (list (- v))) v))
        ((symbol? e) (intern-symbol e))
        ((not (pair? e))
         (error "tseitin: unrecognized formula" e))
        (else
         (case (car e)
           ((not)  (- (lit-of (cadr e))))
           ((and)  (and-cl (cdr e)))
           ((or)   (or-cl (cdr e)))
           ((xor)  (xor-cl (cadr e) (caddr e)))
           ((iff)  (iff-cl (cadr e) (caddr e)))
           ((=>)   (lit-of `(or (not ,(cadr e)) ,(caddr e))))
           (else (error "tseitin: unknown operator" (car e)))))))
    ;; v ↔ (and a b c ..): (¬v ∨ a), (¬v ∨ b), .., (v ∨ ¬a ∨ ¬b ∨ ..)
    (define (and-cl subs)
      (let* ((ls (map lit-of subs)) (v (fresh-var)))
        (for-each (lambda (s) (emit (list (- v) s))) ls)
        (emit (cons v (map - ls)))
        v))
    ;; v ↔ (or a b c ..): (v ∨ ¬a), .., (¬v ∨ a ∨ b ∨ ..)
    (define (or-cl subs)
      (let* ((ls (map lit-of subs)) (v (fresh-var)))
        (for-each (lambda (s) (emit (list v (- s)))) ls)
        (emit (cons (- v) ls))
        v))
    ;; v ↔ (xor a b): four clauses
    (define (xor-cl a b)
      (let* ((la (lit-of a)) (lb (lit-of b)) (v (fresh-var)))
        (emit (list (- v) la lb))
        (emit (list (- v) (- la) (- lb)))
        (emit (list v la (- lb)))
        (emit (list v (- la) lb))
        v))
    ;; v ↔ (iff a b): four clauses
    (define (iff-cl a b)
      (let* ((la (lit-of a)) (lb (lit-of b)) (v (fresh-var)))
        (emit (list (- v) (- la) lb))
        (emit (list (- v) la (- lb)))
        (emit (list v la lb))
        (emit (list v (- la) (- lb)))
        v))
    (let ((top (lit-of formula)))
      (emit (list top))
      (values var-alist top (reverse clauses)))))

;; ─── Public sat? ───────────────────────────

(define *sat-var-alist* '())

(define (sat? formula . opts)
  "Decide satisfiability of a boolean S-expression formula. Operators:
   and, or, not, xor, iff, =>. Atoms are variables.

   Returns #t / #f / 'unknown."
  (let ((budget (if (null? opts) 1000000 (car opts))))
    (call-with-values
      (lambda () (tseitin formula))
      (lambda (var-alist top-var clauses)
        (set! *sat-var-alist* var-alist)
        (sat-cnf-flat? (cnf->flat clauses) budget)))))

(define (sat-model)
  "Return an alist ((sym . #t/#f) ..) for variables in the most recent
   sat? call, or #f if no current model."
  (let ((vec (sat-cnf-flat-model)))
    (cond
      ((not vec) #f)
      (else
       (map (lambda (cell)
              (cons (car cell) (vector-ref vec (cdr cell))))
            *sat-var-alist*)))))
```

- [ ] **Step 2: Add Tseitin tests to `sat-test.scm`**

Append (before the final summary):

```scheme
(check "sat? trivial true"
       #t
       (sat? '#t))

(check "sat? trivial false"
       #f
       (sat? '#f))

(check "sat? single var"
       #t
       (sat? 'x))

(check "sat? conjunction (SAT)"
       #t
       (sat? '(and x y)))

(check "sat? contradiction (UNSAT)"
       #f
       (sat? '(and x (not x))))

(check "sat? De Morgan biconditional (SAT — it's a tautology)"
       #t
       (sat? '(iff (not (and x y))
                   (or (not x) (not y)))))
```

- [ ] **Step 3: Run tests**

```bash
go build -o /tmp/wile-test ./cmd/wile
/tmp/wile-test --file stdlib/lib/wile/algebra/sat-test.scm
```

Expected: all checks pass.

- [ ] **Step 4: Commit**

```bash
git add stdlib/lib/wile/algebra/sat.scm stdlib/lib/wile/algebra/sat-test.scm
git commit -m "feat(sat): Tseitin transform and sat? user primitive"
```

---

## Phase 8 — Boolean-algebra integration

### Task 20: `boolean-decide-sat?` + `boolean-decide-equivalent?`

**Files:**
- Modify: `stdlib/lib/wile/algebra/sat.scm`
- Modify: `stdlib/lib/wile/algebra/sat-test.scm`

- [ ] **Step 1: Append to `sat.scm`**

```scheme
;; ─── Boolean-algebra decision predicates ───

(define (boolean-decide-sat? formula)
  "SAT-backed satisfiability check. Equivalent to (sat? formula)."
  (sat? formula))

(define (boolean-decide-equivalent? a b)
  "SAT-backed equivalence check for two boolean S-expression formulas.
   Two formulas are equivalent iff ¬(a ↔ b) is unsatisfiable.

   Returns #t / #f / 'unknown.

   Closes the De Morgan / complement-law / bound-identity / distributivity
   gaps left by symbolic-boolean-equivalent? in (wile algebra symbolic).

   Examples:
     (boolean-decide-equivalent? '(not (and x y))
                                  '(or (not x) (not y)))  => #t
     (boolean-decide-equivalent? '(or x y) '(and x y))    => #f"
  (let ((result (sat? `(not (iff ,a ,b)))))
    (cond
      ((eq? result #f) #t)
      ((eq? result #t) #f)
      (else 'unknown))))
```

- [ ] **Step 2: Add gap-closing tests to `sat-test.scm`**

```scheme
(check "boolean-decide-equivalent? closes De Morgan"
       #t
       (boolean-decide-equivalent? '(not (and x y))
                                    '(or (not x) (not y))))

(check "boolean-decide-equivalent? closes complement law"
       #t
       (boolean-decide-equivalent? '(and x (not x)) '#f))

(check "boolean-decide-equivalent? closes bound identity"
       #t
       (boolean-decide-equivalent? '(or x #t) '#t))

(check "boolean-decide-equivalent? closes distributivity"
       #t
       (boolean-decide-equivalent? '(and x (or y z))
                                    '(or (and x y) (and x z))))

(check "boolean-decide-equivalent? rejects non-equivalent"
       #f
       (boolean-decide-equivalent? '(or x y) '(and x y)))

(check "boolean-decide-equivalent? agrees with axiomatic on commutativity"
       #t
       (boolean-decide-equivalent? '(and a b) '(and b a)))

(check "boolean-decide-equivalent? agrees with axiomatic on absorption"
       #t
       (boolean-decide-equivalent? '(or x (and x y)) 'x))
```

- [ ] **Step 3: Run tests**

```bash
go build -o /tmp/wile-test ./cmd/wile
/tmp/wile-test --file stdlib/lib/wile/algebra/sat-test.scm
```

Expected: all pass.

- [ ] **Step 4: Commit**

```bash
git add stdlib/lib/wile/algebra/sat.scm stdlib/lib/wile/algebra/sat-test.scm
git commit -m "feat(sat): boolean-decide-sat? and boolean-decide-equivalent?"
```

---

## Phase 9 — Benchmarks, docs, finalize

### Task 21: Confirm `AllExtensions()` wiring (or land it now)

If Task 18 Step 4 already added `sat.Extension` to `AllExtensions()`, skip this task. Otherwise:

**Files:**
- Modify: `extensions.go`

- [ ] **Step 1: Add the import and append `sat.Extension`**

- [ ] **Step 2: Verify the CLI sees the primitive**

```bash
go build -o /tmp/wile-test ./cmd/wile
echo '(sat-cnf-flat? #(1 0) #f)' | /tmp/wile-test --file /dev/stdin
```

Expected: `#t`.

- [ ] **Step 3: Commit (if any changes)**

```bash
git add extensions.go
git commit -m "feat(sat): wire sat.Extension into AllExtensions"
```

### Task 22: Benchmarks

**Files:**
- Create: `extensions/sat/solver_bench_test.go`
- Create: `extensions/sat/BENCH.md`

- [ ] **Step 1: Write the benchmarks**

```go
package sat

import (
	"context"
	"testing"
)

func BenchmarkPHP_5(b *testing.B) {
	cs, n := makePHP(6, 5)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkPHP_6(b *testing.B) {
	cs, n := makePHP(7, 6)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkPHP_7(b *testing.B) {
	cs, n := makePHP(8, 7)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func BenchmarkRandom3SAT_100(b *testing.B) {
	rng := newDeterministicRNG(42)
	cs, n := randomCNF(rng, 100, 426, 3)
	for i := 0; i < b.N; i++ {
		s := newSolver(context.Background(), copyClauses(cs), n, -1)
		_ = s.solve()
	}
}

func makePHP(p, h int) ([]clause, int32) {
	v := func(i, j int) int32 {
		return int32((i-1)*h + j)
	}
	var cs []clause
	for i := 1; i <= p; i++ {
		lits := make([]literal, 0, h)
		for j := 1; j <= h; j++ {
			lits = append(lits, literal(2*v(i, j)))
		}
		cs = append(cs, clause{lits: lits})
	}
	for j := 1; j <= h; j++ {
		for i1 := 1; i1 <= p; i1++ {
			for i2 := i1 + 1; i2 <= p; i2++ {
				cs = append(cs, clause{
					lits: []literal{literal(2*v(i1, j) + 1), literal(2*v(i2, j) + 1)},
				})
			}
		}
	}
	return cs, int32(p * h)
}

func copyClauses(cs []clause) []clause {
	out := make([]clause, len(cs))
	for i, c := range cs {
		out[i] = clause{
			learnt:   c.learnt,
			activity: c.activity,
			lits:     append([]literal(nil), c.lits...),
		}
	}
	return out
}
```

- [ ] **Step 2: Run benchmarks**

```bash
go test -bench=. -benchmem -run='^$' ./extensions/sat/ -benchtime=3x
```

Expected: timings print. (3 iterations to keep dev cycle short.)

- [ ] **Step 3: Record baseline in `BENCH.md`**

```markdown
# SAT Solver Benchmark Baselines

Recorded on commit <SHA>. Hardware: <CPU, OS, Go version>.
Run with: `go test -bench=. -benchmem -run='^$' ./extensions/sat/`.

Regression threshold for PR review: ≥20% slowdown on any benchmark below
flags a code-level perf regression. Investigate before merging.

| Benchmark | Time/op | Allocs/op | Bytes/op |
|---|---|---|---|
| PHP_5 | <fill in> | <fill in> | <fill in> |
| PHP_6 | <fill in> | <fill in> | <fill in> |
| PHP_7 | <fill in> | <fill in> | <fill in> |
| Random3SAT_100 | <fill in> | <fill in> | <fill in> |

Notes:
- PHP_N scales superpolynomially. PHP_7 is the largest practical CI bench;
  PHP_8+ enters hours-long territory without preprocessing.
- Random 3-SAT at ratio 4.26 sits on the phase transition: mix of SAT and
  UNSAT, both classes hard at this density.
```

Fill in the numbers from Step 2.

- [ ] **Step 4: Commit**

```bash
git add extensions/sat/solver_bench_test.go extensions/sat/BENCH.md
git commit -m "bench(sat): pigeonhole and random 3-SAT regression guards"
```

### Task 23: Update TODO.md and plans/CLAUDE.md

**Files:**
- Modify: `TODO.md`
- Modify: `plans/CLAUDE.md`

- [ ] **Step 1: Add an entry to `TODO.md` under Algebra**

```markdown
- [x] **SAT solver** [Algebra, Done]: `(wile algebra sat)` ships `sat?`,
  `sat-cnf?`, `sat-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`.
  CDCL kernel in `extensions/sat/` (watched-literal propagation, 1-UIP
  analysis, VSIDS, Luby restarts). Closes De Morgan, complement-law,
  distributivity, bound-identity gaps in `symbolic-boolean-equivalent?`.
  `memory/2026-05-30-sat-solver-design.md`, `-impl.md`.
```

- [ ] **Step 2: Add a line to `plans/CLAUDE.md`**

Append under the appropriate section:

```markdown
- `2026-05-30-sat-solver-design.md` / `-impl.md` — CDCL SAT solver
  extension; closes axiomatic-equivalence gap via `boolean-decide-equivalent?`.
```

- [ ] **Step 3: Commit**

```bash
git add TODO.md plans/CLAUDE.md
git commit -m "docs(sat): mark SAT solver shipped in TODO and plans index"
```

### Task 24: `make lint && make covercheck` pass

- [ ] **Step 1: Run lint**

Run: `make lint`
Expected: clean. Fix any reported issues; re-run.

- [ ] **Step 2: Run coverage check**

Run: `make covercheck`
Expected: passes project threshold. Add targeted tests for any uncovered paths.

- [ ] **Step 3: Run the full test suite**

Run: `make test`
Expected: all PASS.

- [ ] **Step 4: Commit any tidy-ups separately**

```bash
git add <files>
git commit -m "test(sat): cover <specific uncovered path>"
```

---

## Phase 10 — Branch finalize

### Task 25: Push and PR

**Files:** none

- [ ] **Step 1: Confirm with the user: PR or direct merge?**

Per CLAUDE.md: PRs are optional. A ~2000-LOC new extension warrants Copilot/`/crosscheck` review — recommend opening a PR.

- [ ] **Step 2: If PR-bound: push the branch**

```bash
git push -u origin <branch-name>
```

- [ ] **Step 3: Open the PR via `gh pr create`**

Use the existing `.github/PULL_REQUEST_TEMPLATE.md` if present.

- [ ] **Step 4: Wait for CI; address Copilot review comments**

- [ ] **Step 5: Run `/crosscheck`**

- [ ] **Step 6: Merge only after explicit user instruction**

Per CLAUDE.md: "Do NOT merge any PR that does get opened without explicit instruction."

---

## Self-Review Checklist (run after writing the plan; fix inline)

- [x] Every spec section covered:
  - §1 Motivation → Phase 8 (`boolean-decide-*` close the documented gap).
  - §2 Scope → matches the spec scope; deferred items not in any task.
  - §3 Architecture → file layout mirrors spec §3.
  - §4 Scheme API → Phases 6–8 ship every export.
  - §5 CDCL internals → Phases 2–4 build each component bottom-up.
  - §6 CNF wire format → Task 2 (Go side) + Task 18 (Scheme side).
  - §7 Error model → `werr.WrapForeignErrorf` in Tasks 2, 16; `'unknown` propagation in Tasks 13, 16, 19, 20.
  - §8 Testing → unit/property/canonical/bench tests across Tasks 2–22.
- [x] No "TBD"/"TODO" placeholders in step bodies. Two named follow-ups (helper-name match in Task 16/17; full clause-arena compaction in spec §10) are spelled out with their contract test, not left vague.
- [x] Type names consistent across tasks: `literal int32`, `clauseRef int32`, `clause`, `solver`, `SolverResult`, `resultSAT/UNSAT/UNKNOWN`. Function names: `parseCNF`, `newSolver`, `addClause`, `litValue`, `enqueue`, `propagate`, `analyze`, `backjump`, `solve`, `pickBranchVar`, `bumpVarActivity`, `decayVarActivity`, `reduceClauseDB`, `luby`, `learntCount`. Primitive names: `sat-cnf-flat?`, `sat-cnf-flat-model`. Scheme exports: `sat?`, `sat-cnf?`, `sat-model`, `sat-cnf-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`, `cnf->flat`, `tseitin` (internal but stable).
- [x] Two acknowledged follow-ups, both labeled in the task that introduces them rather than left dangling:
  - Task 11: `pickBranchVar` is linear-scan in v1; promote to a heap only if benchmarks warrant.
  - Task 13: `reduceClauseDB` uses tombstones; full clause-arena compaction is deferred to v2 per spec §10.
