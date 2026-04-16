# Set Closure & Graph Reachability Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add two new benchmark categories (`set_closure` and `graph_reachability`) to the algebra accuracy benchmark.

**Architecture:** Each category gets a generator function in `generate.py` following the existing pattern: produce `Problem` objects with `scheme_expression` (ground truth via Wile) and `natural_language` (LLM prompt). Both use `answer_type="set"`. The `build_scheme_script` function needs `(srfi 1)` added to its import header for `lset-union`. No changes to `evaluate.py` — the `set` answer type already exists.

**Tech Stack:** Python (generate.py), Wile Scheme (ground truth), existing `(wile algebra)` + `(srfi 1)` libraries.

---

### Task 1: Add (srfi 1) to build_scheme_script imports

**Files:**
- Modify: `algebra-accuracy/generate.py:438-441`

**Step 1: Make the change**

In `build_scheme_script`, add `(srfi 1)` to the import list:

```python
lines = [
    "(import (scheme base)",
    "        (scheme write)",
    "        (srfi 1)",
    "        (wile algebra))",
    "",
]
```

**Step 2: Verify existing categories still generate correctly**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories modular_arithmetic --difficulty easy --count 2 --output /dev/stdout 2>/dev/null | python -m json.tool | head -20`

Expected: Valid JSON output with 2 modular_arithmetic problems, answers populated.

**Step 3: Commit**

```
git add algebra-accuracy/generate.py
git commit -m "gen: add (srfi 1) import for lset-union in set closure"
```

---

### Task 2: Implement gen_set_closure

**Files:**
- Modify: `algebra-accuracy/generate.py` — add `gen_set_closure` function and helper, add to `GENERATORS` and `DEFAULT_COUNTS`

**Step 1: Add the closure rule generator and gen_set_closure**

Insert before the `# ── Main` section (around line 700):

```python
# ── Set Closure ─────────────────────────────────────────────────
#
# Compute the closure of a seed set under conditional implication
# rules. Rules have the form "if {triggers} ⊆ S then {consequents} ⊆ S".
#
# Difficulty levers:
#   easy       — 1-2 simple rules (single trigger, single consequent)
#   medium     — 3-4 rules, some chained (a→b, b→c)
#   hard       — 5-6 rules with conjunctive triggers ({a,b}→c)
#   extra-hard — 8 rules, mix of conjunctive, multiple iterations
#   super-hard — 10+ rules, deep cascades, distractor rules
#   ultra-hard — 12+ rules, mutual dependencies


def _gen_closure_rules(universe, n_rules, allow_conjunctive, rng):
    """Generate closure rules as (triggers, consequents) pairs.

    Each rule: if all triggers are in the set, add all consequents.
    Returns a list of (trigger_set, consequent_set) tuples and
    a seed set that is disjoint from any consequent (so closure
    is non-trivial).
    """
    elements = list(universe)
    rules = []
    # Track which elements appear as consequents so the seed avoids them
    all_consequents = set()

    for _ in range(n_rules):
        # Pick trigger size: 1 unless conjunctive allowed
        trigger_size = 1
        if allow_conjunctive:
            trigger_size = rng.choice([1, 1, 2, 2, 3])

        available_triggers = [e for e in elements if e not in all_consequents]
        if len(available_triggers) < trigger_size:
            break
        triggers = tuple(sorted(rng.sample(available_triggers, trigger_size)))

        # Pick 1-2 consequents that aren't triggers in this rule
        available_consequents = [e for e in elements if e not in triggers]
        if not available_consequents:
            break
        n_cons = rng.choice([1, 1, 2])
        n_cons = min(n_cons, len(available_consequents))
        consequents = tuple(sorted(rng.sample(available_consequents, n_cons)))
        all_consequents.update(consequents)
        rules.append((triggers, consequents))

    # Seed: pick 2-4 elements, must include at least one trigger
    # to start the chain, but no consequents
    seed_pool = [e for e in elements if e not in all_consequents]
    if not seed_pool:
        seed_pool = elements[:2]
    n_seed = min(rng.randint(2, 4), len(seed_pool))
    seed = sorted(rng.sample(seed_pool, n_seed))

    return rules, seed


def gen_set_closure(difficulty: str, n: int) -> List[Problem]:
    ELEMS = list("abcdefghijklmnop")
    params = {
        #            (universe_size, n_rules, allow_conjunctive)
        "easy":       (8,  2, False),
        "medium":     (10, 4, False),
        "hard":       (12, 6, True),
        "extra-hard": (12, 8, True),
        "super-hard": (14, 11, True),
        "ultra-hard": (14, 13, True),
    }
    problems = []

    for i in range(n):
        n_elems, n_rules, allow_conj = params[difficulty]
        universe = ELEMS[:n_elems]

        rules, seed = _gen_closure_rules(universe, n_rules, allow_conj, random)

        # Build Scheme expression
        elems_sch = " ".join(universe)

        # Each rule becomes a conditional lset-union operand
        rule_exprs = []
        for triggers, consequents in rules:
            cons_sch = " ".join(f"'{c}" for c in consequents)
            if len(triggers) == 1:
                cond = f"(memq '{triggers[0]} s)"
            else:
                cond = "(and " + " ".join(f"(memq '{t} s)" for t in triggers) + ")"
            rule_exprs.append(
                f"(if {cond} (list {cons_sch}) '())"
            )

        seed_sch = " ".join(f"'{e}" for e in seed)

        # lset-union is variadic: (lset-union eq? s rule1 rule2 ...)
        union_args = " ".join(rule_exprs)
        scheme = (
            f"(let ((L (powerset-lattice '({elems_sch}))))"
            f" (fixpoint L"
            f" (lambda (s) (lset-union eq? s {union_args}))"
            f" (list {seed_sch})))"
        )

        # Build natural language
        nl_lines = [
            f"Given the universe {{{', '.join(universe)}}}, "
            "the following closure rules apply:"
        ]
        for triggers, consequents in rules:
            if len(triggers) == 1:
                trig_nl = f"{triggers[0]} is in the set"
            else:
                trig_nl = "both " + " and ".join(triggers) + " are in the set"
            cons_nl = " and ".join(consequents)
            nl_lines.append(f"  - If {trig_nl}, then {cons_nl} must also be in the set")

        nl_lines.append(
            f"\nStarting from {{{', '.join(seed)}}}, "
            "compute the closure (the smallest superset of the starting "
            "set that satisfies all rules)."
        )

        problems.append(
            Problem(
                id=f"clos-{difficulty}-{i:03d}",
                category="set_closure",
                difficulty=difficulty,
                natural_language="\n".join(nl_lines)
                + "\nGive the answer as a set in {a, b, ...} notation.",
                scheme_expression=scheme,
                answer_type="set",
            )
        )
    return problems
```

**Step 2: Register in GENERATORS and DEFAULT_COUNTS**

Add to `GENERATORS` dict:

```python
"set_closure": gen_set_closure,
```

Add to `DEFAULT_COUNTS` dict:

```python
"set_closure": {"easy": 10, "medium": 10, "hard": 10, "extra-hard": 5, "super-hard": 5, "ultra-hard": 5},
```

**Step 3: Test generation**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories set_closure --difficulty easy --count 3 --output /tmp/test_closure.json`

Expected: "Generated 3 problems" + "Wrote 3 problems" on stderr, valid JSON at `/tmp/test_closure.json` with populated `answer` fields of type set.

Verify: `python -c "import json; data=json.load(open('/tmp/test_closure.json')); [print(f'{p[\"id\"]}: {p[\"answer\"]}') for p in data]"`

**Step 4: Test all difficulties**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories set_closure --count 2 --output /tmp/test_closure_all.json`

Expected: 12 problems (2 per difficulty), all with non-empty answers.

**Step 5: Commit**

```
git add algebra-accuracy/generate.py
git commit -m "gen: add set_closure category with fixpoint-based closure rules"
```

---

### Task 3: Implement gen_graph_reachability

**Files:**
- Modify: `algebra-accuracy/generate.py` — add graph generation helpers and `gen_graph_reachability`, register in `GENERATORS` and `DEFAULT_COUNTS`

**Step 1: Add graph generators and gen_graph_reachability**

Insert after the set_closure section:

```python
# ── Graph Reachability ──────────────────────────────────────────
#
# Given a directed graph and a starting node, find all reachable
# nodes via graph-query-all with the Boolean semiring.
#
# Difficulty levers (structural variation):
#   easy       — tree (unique paths, no cycles)
#   medium     — DAG (multiple paths, no cycles)
#   hard       — sparse cyclic graph (2-3 cycles)
#   extra-hard — dense cyclic, some unreachable nodes
#   super-hard — dense cyclic, high fan-out
#   ultra-hard — dense cyclic, bidirectional clusters


def _gen_tree(nodes, rng):
    """Generate a random tree as adjacency dict. Every node reachable from nodes[0]."""
    edges = {n: [] for n in nodes}
    # Shuffle non-root nodes and attach each to a random earlier node
    rest = list(nodes[1:])
    rng.shuffle(rest)
    available = [nodes[0]]
    for node in rest:
        parent = rng.choice(available)
        edges[parent].append(node)
        available.append(node)
    return edges, nodes[0]


def _gen_dag(nodes, extra_edges, rng):
    """Generate a DAG with a topological ordering. Start node is nodes[0]."""
    order = list(nodes)
    rng.shuffle(order)
    edges = {n: [] for n in nodes}
    # Ensure connectivity: each node (except first) has at least one parent
    for j in range(1, len(order)):
        parent = rng.choice(order[:j])
        edges[parent].append(order[j])
    # Add extra forward edges
    for _ in range(extra_edges):
        i = rng.randint(0, len(order) - 2)
        j = rng.randint(i + 1, len(order) - 1)
        if order[j] not in edges[order[i]]:
            edges[order[i]].append(order[j])
    return edges, order[0]


def _gen_cyclic(nodes, n_edges, n_unreachable, rng):
    """Generate a cyclic graph. n_unreachable nodes have no incoming edges from
    the reachable component."""
    reachable = list(nodes[:len(nodes) - n_unreachable])
    unreachable = list(nodes[len(nodes) - n_unreachable:])
    edges = {n: [] for n in nodes}

    # Ensure reachable nodes form a connected component from start
    start = reachable[0]
    rest = list(reachable[1:])
    rng.shuffle(rest)
    available = [start]
    for node in rest:
        parent = rng.choice(available)
        if node not in edges[parent]:
            edges[parent].append(node)
        available.append(node)

    # Add extra edges within reachable component (may create cycles)
    added = 0
    attempts = 0
    target_extra = n_edges - (len(reachable) - 1)
    while added < target_extra and attempts < target_extra * 5:
        attempts += 1
        src = rng.choice(reachable)
        dst = rng.choice(reachable)
        if src != dst and dst not in edges[src]:
            edges[src].append(dst)
            added += 1

    # Unreachable nodes get edges among themselves only
    for node in unreachable:
        if unreachable:
            targets = [u for u in unreachable if u != node]
            if targets:
                dst = rng.choice(targets)
                if dst not in edges[node]:
                    edges[node].append(dst)

    return edges, start, set(reachable)


def gen_graph_reachability(difficulty: str, n: int) -> List[Problem]:
    ELEMS = list("abcdefghijklmnopqrstuvwx")
    params = {
        #            (n_nodes, structure, extra_edges, n_unreachable)
        "easy":       (8,  "tree", 0, 0),
        "medium":     (10, "dag",  3, 0),
        "hard":       (12, "cyclic", 15, 0),
        "extra-hard": (14, "cyclic", 20, 3),
        "super-hard": (16, "cyclic", 30, 3),
        "ultra-hard": (16, "cyclic", 40, 4),
    }
    problems = []

    for i in range(n):
        n_nodes, structure, extra_edges, n_unreach = params[difficulty]
        nodes = ELEMS[:n_nodes]

        if structure == "tree":
            edges, start = _gen_tree(nodes, random)
            # All nodes reachable in a tree
        elif structure == "dag":
            edges, start = _gen_dag(nodes, extra_edges, random)
        else:
            edges, start, _ = _gen_cyclic(nodes, extra_edges, n_unreach, random)

        # Build Scheme expression using graph-query-all + boolean-semiring
        adj_entries = []
        for node in nodes:
            if edges[node]:
                neighbors = " ".join(
                    f"(cons '{dst} 1)" for dst in edges[node]
                )
                adj_entries.append(
                    f"(cons '{node} (list {neighbors}))"
                )
            else:
                adj_entries.append(f"(cons '{node} '())")

        adj_sch = " ".join(adj_entries)
        scheme = (
            f"(let ((ga (make-graph-analysis (boolean-semiring)"
            f" (list {adj_sch}) #f)))"
            f" (map car (graph-query-all ga '{start})))"
        )

        # Build natural language
        edge_lines = []
        for node in nodes:
            if edges[node]:
                targets = ", ".join(edges[node])
                edge_lines.append(f"  {node} -> {targets}")

        nl = (
            f"Consider a directed graph with nodes "
            f"{{{', '.join(nodes)}}} and edges:\n"
            + "\n".join(edge_lines)
            + f"\n\nStarting from node {start}, which nodes are "
            f"reachable (including {start} itself)?\n"
            f"Give the answer as a set in {{a, b, ...}} notation."
        )

        problems.append(
            Problem(
                id=f"reach-{difficulty}-{i:03d}",
                category="graph_reachability",
                difficulty=difficulty,
                natural_language=nl,
                scheme_expression=scheme,
                answer_type="set",
            )
        )
    return problems
```

**Step 2: Register in GENERATORS and DEFAULT_COUNTS**

Add to `GENERATORS` dict:

```python
"graph_reachability": gen_graph_reachability,
```

Add to `DEFAULT_COUNTS` dict:

```python
"graph_reachability": {"easy": 10, "medium": 10, "hard": 10, "extra-hard": 5, "super-hard": 5, "ultra-hard": 5},
```

**Step 3: Test generation — easy (tree)**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories graph_reachability --difficulty easy --count 3 --output /tmp/test_reach.json`

Expected: 3 problems with set answers, all 8 nodes reachable in every tree problem.

Verify: `python -c "import json; data=json.load(open('/tmp/test_reach.json')); [print(f'{p[\"id\"]}: {p[\"answer\"]}') for p in data]"`

**Step 4: Test generation — extra-hard (with unreachable nodes)**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories graph_reachability --difficulty extra-hard --count 3 --output /tmp/test_reach_eh.json`

Expected: 3 problems, answers should have fewer than 14 nodes (some unreachable).

Verify: `python -c "import json; data=json.load(open('/tmp/test_reach_eh.json')); [print(f'{p[\"id\"]}: {len(p[\"answer\"].strip(\"()\").split())} of 14 reachable') for p in data]"`

**Step 5: Test all difficulties**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories graph_reachability --count 2 --output /tmp/test_reach_all.json`

Expected: 12 problems (2 per difficulty), all with valid set answers.

**Step 6: Commit**

```
git add algebra-accuracy/generate.py
git commit -m "gen: add graph_reachability category with boolean semiring"
```

---

### Task 4: Integration test — generate full problem set

**Files:**
- None (testing only)

**Step 1: Generate both new categories together**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories set_closure graph_reachability --output /tmp/test_both.json`

Expected: 90 problems (45 per category), all answers populated.

**Step 2: Verify answer diversity**

```
python -c "
import json
from collections import Counter
data = json.load(open('/tmp/test_both.json'))
for cat in ['set_closure', 'graph_reachability']:
    probs = [p for p in data if p['category'] == cat]
    sizes = [len(p['answer'].strip('()').split()) for p in probs]
    print(f'{cat}: {len(probs)} problems, answer sizes: min={min(sizes)}, max={max(sizes)}, mean={sum(sizes)/len(sizes):.1f}')
"
```

Expected: Non-degenerate answer sizes — not all answers should be the full universe or empty set.

**Step 3: Verify both categories work in evaluate.py --rescore**

Run: `python algebra-accuracy/evaluate.py --rescore /tmp/test_both.json`

Expected: Summary table with `set_closure` and `graph_reachability` rows. Since there's no control/treatment data yet, accuracy columns show `n/a` — but the rescore should not crash on the new categories or answer types.

---

### Task 5: Generate the benchmark problem set

**Files:**
- Create: `algebra-accuracy/recurrence_problems.json`

**Step 1: Generate with fixed seed**

Run: `python algebra-accuracy/generate.py --wile /Users/aalpar/projects/wile-workspace/wile/wile --categories set_closure graph_reachability --seed 42 --output algebra-accuracy/recurrence_problems.json`

**Step 2: Spot-check a few problems**

```
python -c "
import json
data = json.load(open('algebra-accuracy/recurrence_problems.json'))
for cat in ['set_closure', 'graph_reachability']:
    for diff in ['easy', 'hard', 'ultra-hard']:
        p = next((x for x in data if x['category'] == cat and x['difficulty'] == diff), None)
        if p:
            print(f'--- {p[\"id\"]} ---')
            print(p['natural_language'][:200])
            print(f'answer: {p[\"answer\"]}')
            print()
"
```

Expected: Readable problem text, reasonable answers.

**Step 3: Commit**

```
git add algebra-accuracy/generate.py algebra-accuracy/recurrence_problems.json
git commit -m "gen: generate recurrence benchmark (set_closure + graph_reachability)"
```
