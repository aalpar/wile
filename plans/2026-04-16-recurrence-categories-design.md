# Recurrence Categories Design

Three new benchmark categories testing recurrence-related reasoning.
Two use existing Wile algebra primitives; one requires a new matrix library.

## 1. Set Closure

**Category:** `set_closure`

**Problem:** Given universe U, closure rules (conditional implications), and
seed set S, compute cl(S) — the smallest superset of S closed under the rules.

**Difficulty gradient — rule complexity (not universe size):**

| Difficulty  | Universe | Rules | Rule type                                          |
|-------------|----------|-------|----------------------------------------------------|
| easy        | 8        | 1-2   | Simple implications: a ∈ S → b ∈ S                |
| medium      | 10       | 3-4   | Chained implications: a→b, b→c                    |
| hard        | 12       | 5-6   | Conjunctive triggers: {a,b} ⊆ S → c ∈ S          |
| extra-hard  | 12       | 8     | Mix conjunctive/disjunctive, multiple iterations   |
| super-hard  | 14       | 10+   | Deep cascades, distractor rules that never fire    |
| ultra-hard  | 14       | 12+   | Mutual dependencies, non-obvious closure           |

**Scheme implementation:** `powerset-lattice` + `fixpoint` with a transfer
function encoding the rules:

```scheme
(let ((L (powerset-lattice '(a b c ...))))
  (fixpoint L
    (lambda (s)
      (lattice-join L s
        (if (and (member 'a s) (member 'b s)) '(c d) '())
        ...))
    '(seed1 seed2)))
```

**Natural language format:**

> Given the universe {a, b, c, d, e, f, g, h}, the following closure
> rules apply:
> - If a is in the set, then b must also be in the set
> - If both c and d are in the set, then e must also be in the set
> - If e is in the set, then f and g must also be in the set
>
> Starting from {a, c, d}, compute the closure. Give the answer as a set.

**Answer type:** `set`

**Control failure mode:** Must iterate rules manually until stable.
Conjunctive triggers that fire only after earlier rules add elements
are where LLMs drop steps.

**Treatment advantage:** Direct `fixpoint` computation — one eval call.

---

## 2. Graph Reachability

**Category:** `graph_reachability`

**Problem:** Given directed graph G and starting node, find all reachable nodes.

**Difficulty gradient — structural variation:**

| Difficulty  | Nodes | Structure                         | Challenge                            |
|-------------|-------|-----------------------------------|--------------------------------------|
| easy        | 8     | Tree (unique paths)               | Follow edges, no revisiting          |
| medium      | 10    | DAG (multiple paths, no cycles)   | Merge paths without double-counting  |
| hard        | 12    | Sparse cyclic (2-3 cycles)        | Track visited set, avoid loops       |
| extra-hard  | 14    | Dense cyclic, multiple components | Some nodes unreachable               |
| super-hard  | 16    | Dense cyclic, high fan-out        | Combinatorial path explosion         |
| ultra-hard  | 16    | Dense cyclic + bidirectional clusters | Many paths revisit, deep traversal |

**Scheme implementation:** `graph-query-all` with Boolean semiring:

```scheme
(let ((ga (make-graph-analysis (boolean-semiring)
           '(("a" . (("b" . 1) ("c" . 1)))
             ("b" . (("d" . 1))) ...)
           #f)))
  (map car (graph-query-all ga "start")))
```

**Natural language format:**

> Consider a directed graph with nodes {a, b, c, d, e, f, g, h}
> and edges:
>   a -> b, a -> c
>   b -> d, b -> e
>   c -> e, c -> f
>   d -> g
>   f -> h
>   h -> c
>
> Starting from node a, which nodes are reachable (including a
> itself)? Give the answer as a set.

**Answer type:** `set`

**Control failure mode:** BFS/DFS mentally — track frontier and visited
set. Cycles cause loops if not tracked. Unreachable nodes (disconnected
components) test whether the LLM stops correctly.

**Treatment advantage:** `graph-query-all` gives all reachable nodes in
one call. Model just builds the adjacency alist.

---

## 3. Linear Algebra Basics

**Category:** `matrix_ops`

**Prerequisite:** New `(wile algebra matrix)` module. Separate work item.

Two operation sub-types, both scaled by matrix dimension.

### Matrix Multiplication

Compute A x B over a ring.

| Difficulty  | Dims       | Ring          | Operations    |
|-------------|------------|---------------|---------------|
| easy        | 2x2 x 2x2 | integers      | Single        |
| medium      | 3x3 x 3x3 | integers      | Single        |
| hard        | 3x3 x 3x3 | modular mod p | Single        |
| extra-hard  | 4x4 x 4x4 | integers      | Single        |
| super-hard  | 4x4 x 4x4 | modular mod p | Chain A*B*C   |
| ultra-hard  | 5x5 x 5x5 | modular mod p | Chain A*B*C   |

### Determinant

Compute det(A).

| Difficulty  | Dims | Ring          |
|-------------|------|---------------|
| easy        | 2x2  | integers      |
| medium      | 3x3  | integers      |
| hard        | 3x3  | modular mod p |
| extra-hard  | 4x4  | integers      |
| super-hard  | 4x4  | modular mod p |
| ultra-hard  | 5x5  | modular mod p |

**Anticipated Scheme API:**

```scheme
(matrix-times (modular-ring 97)
  (matrix '((3 7) (2 5)))
  (matrix '((1 4) (6 8))))

(matrix-determinant (integer-ring)
  (matrix '((1 2 3) (4 5 6) (7 8 9))))
```

**Natural language format (multiply):**

> Compute the matrix product A x B mod 97, where:
>
> A = | 3  7 |    B = | 1  4 |
>     | 2  5 |        | 6  8 |
>
> Give the answer as a matrix in row-major format:
> ((r1c1 r1c2) (r2c1 r2c2))

**Natural language format (determinant):**

> Compute the determinant of the following matrix:
>
> | 1  2  3 |
> | 4  5  6 |
> | 7  8  9 |
>
> Give only the numeric answer.

**Answer types:** `matrix` (new) for multiplication, `integer` for determinant.

**Minimum viable Wile library scope:**
- `matrix` — constructor from nested list
- `matrix-times` — parameterized by ring
- `matrix-determinant` — parameterized by ring
- `matrix-ref`, `matrix-rows`, `matrix-cols` — inspection

Inverse, transpose, system-solving deferred.

**Control failure mode:** N^3 multiply-accumulate operations, tracking
intermediate sums. Cofactor expansion at 4x4+ is error-prone.

**Treatment advantage:** Direct `matrix-times` / `matrix-determinant`.

---

## Implementation Dependencies

```
set_closure ─────────┐
                     ├── generate.py + evaluate.py changes
graph_reachability ──┘

matrix_ops ── blocked by ── (wile algebra matrix) module
```

Set closure and graph reachability can proceed immediately using
existing Wile algebra primitives. Matrix operations are blocked on
the new Wile library.

## Answer Type Additions

- `matrix` — new type for `normalize_answer` / `answers_match` in
  evaluate.py. Represented as nested tuples of integers, compared
  element-wise.
