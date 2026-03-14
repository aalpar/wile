# Performance Plans

## Completed: Allocation Optimization (-42.1% Gabriel geo-mean)

**Date:** 2026-02-22

Three fixes reduced Gabriel benchmark geo-mean by 42.1% and ZebraPuzzle by 29.5%.

| Fix | Commit | Mechanism | Impact (incremental) |
|-----|--------|-----------|---------------------|
| Foreign closure noCopyApply | `713661d` | `computeNoCopyApply()` in `NewForeignClosure` — primitives skip env copy | -24.5% geo-mean |
| 2-arg numeric fast path | `6282c36` | `values.Single()` skips ForEach closure for 2-arg calls | -20.1% geo-mean |
| Pull backing array fix | `db452db` | `copy()` instead of reslice preserves pool capacity | -13.0% geo-mean |

Post-fix allocation profile (fib 10): total allocs reduced 43.6% (300M to 169M). Per-iteration: 4,618 to 1,335. CPU time per op: 169us to 88us (-48%). Subsequent PRs (rest-arg buffer, env frame pooling, Stack.Drain, inline continuation evals) reduced allocations further.

## Completed: Block-Allocated Pairs

**Status:** Implemented (PR #311)

Replaced per-element `NewCons` allocation in `values.List()` with single `make([]Pair, N)` block allocation. N cons cells become 1 heap object. Each `&block[i]` is a valid `*Pair` — semantically identical to individual allocation. Cache-contiguous list traversal. No API change.

## Completed: Unified Pool Manager (Pool[T])

**Status:** Implemented

Generic `Pool[T]` replaces three ad-hoc `sync.Pool` instances (stack, sub-context, continuation). Provides unified observability (`PoolSnapshot`: acquires, releases, misses, in-flight), drain/disable controls, and `PoolManager` for coordinated operations.

**Benchmark overhead:** +4-7% on call-heavy Gabriel benchmarks (4 extra atomic ops per pool cycle). Acceptable cost of observability; can be compiled out via build tag if needed.

## Completed: Hot-Path Allocation Reductions

Multiple PRs eliminated the remaining per-call allocations identified in the post-fix profile:

| Fix | PR/Commit | Mechanism |
|-----|-----------|-----------|
| Rest-arg buffer reuse | PR #333 | Reusable `PairBlock` on `MachineContext` for foreign variadic calls; -68% allocs, `values.List` eliminated from profile |
| EnvironmentFrame pooling | PR #325, #386 | `Pool[T]` for env frames; pooled frames reused via `copyForApplyInto` with CoW keys |
| Slim Binding struct | `environment/binding.go` | `BindingMeta` extracted behind pointer — runtime copies move 32 bytes instead of 56 |
| Binding copy memcpy | `environment/local_environment_frame.go` | `copy(dst.bindings, p.bindings)` — single `memmove` replaces field-by-field loop |
| Inline continuation evals | PR #387 | Skip stack pool round-trip for continuation evals |
| Stack.Drain | PR #396 | Zero-copy view of eval stack eliminates `PopAll` allocation in hot path |

## Current State: What's Already Optimized

| Optimization | Mechanism | Location |
|---|---|---|
| Continuation frames | `sync.Pool` via `Pool[T]` | `pool.go` |
| Eval stacks | `sync.Pool` via `Pool[T]` (cap 8) | `pool.go` |
| Sub-contexts | `sync.Pool` via `Pool[T]` | `pool.go` |
| Environment frames | `sync.Pool` via `Pool[T]` | `pool.go` |
| Macro contexts | `acquireMacroContext` | `pool.go` |
| No-copy Apply | `noCopyApply` flag skips env copy for leaf functions | `native_template.go` |
| No-copy foreign closures | `computeNoCopyApply()` in `NewForeignClosure` | `util.go` |
| Fused NewApplyFrame | Single alloc instead of CopyForApply + NewEnvironmentFrameWithParent | `environment_frame.go` |
| CoW keys map | Shared between copies, only cloned on mutation | `local_environment_frame.go` |
| RestoreAndRelease | Transfer evals ownership for normal returns (no copy) | `machine_context.go` |
| Contiguous bindings | `[]Binding` not `[]*Binding` — cache-friendly, one alloc | `local_environment_frame.go` |
| Slim Binding struct | `BindingMeta` behind pointer — 32 byte copies instead of 56 | `environment/binding.go` |
| Binding copy memcpy | `copy()` on `[]Binding` — single `memmove` | `local_environment_frame.go` |
| 2-arg numeric fast path | `values.Single()` skips ForEach closure for 2-arg calls | `registry/helpers/numeric.go` |
| Pull backing array fix | `copy()` instead of reslice preserves pool capacity | `stack.go`, `pool.go` |
| Block-allocated pairs | `values.List()` allocates `make([]Pair, N)` block | `values/utils.go` |
| Rest-arg buffer | Reusable `PairBlock` on `MachineContext` for foreign variadic calls | `machine_context.go` |
| Stack.Drain | Zero-copy view eliminates `PopAll` allocation | `stack.go` |
| Inline continuation evals | Skip stack pool round-trip | `machine_context.go` |

## Completed: Opcode Promotion (Phase 1+2)

**Date:** 2026-03-13

Promoted 11 hot primitives to dedicated opcodes (22 opcodes total: non-tail + tail each). See `plans/OPCODE-PROMOTION.md` for Larceny profiling data and panic audit.

| Phase | PR | Primitives | Opcodes |
|-------|-----|-----------|---------|
| 1 | #497 | `null?`, `pair?`, `car`, `cdr` | 8 |
| 2 | #498 | `+`, `-`, `<`, `<=`, `>`, `>=`, `=` (2-arg) | 14 |
| Fix | #495 | Pool `opcodeHits` init | — |

Combined Larceny benchmark improvements (vs pre-promotion master):

| Benchmark | Change | Dominant Ops |
|-----------|--------|-------------|
| sumfp | **-71%** | `+`, `>=` |
| ackermann | **-57%** | `=`, `-`, `+` |
| takl | **-43%** | `cdr`, `pair?`, `null?` |
| fib | **-40%** | `<=`, `-`, `+` |
| browse | **-32%** | `pair?`, `car`, `cdr` |
| tak | **-30%** | `<`, `-` |
| destruct | **-29%** | `cdr`, `null?`, `car` |
| deriv | **-17%** | `car`, `null?`, `cdr`, `pair?` |
| diviter | **-16%** | `<=`, `/`, `-` |

No defer/recover needed for arithmetic — the numeric tower cannot panic from valid Number inputs, and the hot path (`callForeignCached`) never had panic recovery.

## Remaining Optimization Opportunities

### Architectural Changes (Tier 3)

#### 8. Flat Closures (Display-Based Environments)

Current model: closures capture a linked list of EnvironmentFrame nodes. `NewApplyFrame` copies the leaf frame's bindings. Parent chain is shared.

**Alternative:** At compile time, analyze which free variables a closure references. Copy only those values into a flat array on the closure. Eliminates:
- Parent-chain walk for `Up > 0` lookups
- Copying entire local frame (only copy free variables)
- EnvironmentFrame allocation (closure *is* its environment)

This is what Chez Scheme, Larceny, and Gambit do.

**Trade-offs:**
- Requires compile-time free-variable analysis pass
- Changes closure representation fundamentally
- `set!` on closed-over variables requires boxing (heap-allocate mutable cell, close over the box)
- Significant compiler + VM changes

#### 9. Stack Frames Instead of Continuation Chains

Replace per-call `MachineContinuation` allocation with a contiguous stack of frames. Save/restore becomes pointer arithmetic.

**Trade-off:** Incompatible with first-class continuations in general. Hybrid approach: stack frames for normal path, materialize continuation objects only when `call/cc` is invoked (stack-to-heap copy).

#### 10. NaN-Boxing or Tagged Pointers

`values.Value` is a Go interface (16 bytes). Small integers, booleans, characters could be encoded in 64 bits. Eliminates interface overhead, reduces stack/binding sizes by 50%.

**Trade-off:** Massive change affecting every value operation. Go's type system makes this awkward (unsafe.Pointer gymnastics).

## Measurement

Run benchmarks: `make bench-gabriel` for the 16-benchmark Gabriel suite. ZebraPuzzle (`go test -bench=BenchmarkZebraPuzzle`) for backtracking stress test. Profile with `go test -bench=X -cpuprofile` and `go test -bench=X -memprofile` for allocation analysis.

---

# Fused Lexing and Parsing: The Flap Approach

**Status:** Research reference

Based on: *flap: A Deterministic Parser with Fused Lexing* (Yallop, Xie,
Krishnaswami — PLDI 2023), building on *A Typed, Algebraic Approach to Parsing*
(Krishnaswami & Yallop — PLDI 2019, Distinguished Paper).

## 1. The Core Problem

Traditional pipelines separate lexing and parsing into two phases:

```
source bytes -> Lexer -> Token stream -> Parser -> AST
                         ^
                   heap-allocated objects
                   carrying type tag + string slice
```

This separation is good for modularity but introduces overhead:

1. **Token materialization** — each token is allocated as a data structure
   (type tag, source slice, position metadata).
2. **Redundant branching** — the lexer *knows* it just recognized a `NUMBER`
   token, then immediately discards that knowledge. The parser later
   case-switches on the token tag to recover the same information.
3. **Cache pressure** — the token objects are short-lived heap allocations
   that pollute the cache between the point of lexical recognition and
   syntactic consumption.

Flap's insight: the lexer and parser can be *defined* separately (preserving
modularity) but *compiled* into a single fused pass (recovering performance).

## 2. Mathematical Foundation

### 2.1 Context-Free Expressions (CFEs)

CFEs are an algebraic presentation of context-free languages — the same
expressive power as context-free grammars, but written as expressions
rather than production rules.

**Syntax:**

```
g  ::=  _|_         empty language (matches nothing)
     |  e           empty string (matches "")
     |  t           terminal (matches single token t)
     |  a           variable (for recursion)
     |  g1 . g2     sequencing (g1 followed by g2)
     |  g1 v g2     alternation (g1 or g2)
     |  ua:t. g     least fixed point (recursion with type annotation)
```

These satisfy the equations of an idempotent semiring:

```
(v, _|_) forms a commutative monoid    g v _|_ = g,  g v g = g
(., e)   forms a monoid                g . e = e . g = g
.  distributes over v
_|_ annihilates .                      g . _|_ = _|_ . g = _|_
```

**Denotational semantics** — [g]y maps a CFE to the set of strings it
accepts, given an environment y for free variables:

```
[e]y         = {e}                                     singleton empty string
[t]y         = {t}                                     singleton one-token string
[_|_]y       = {}                                      empty language
[a]y         = y(a)                                    variable lookup
[g1 . g2]y   = { w . w' | w in [g1]y ^ w' in [g2]y }  concatenation
[g1 v g2]y   = [g1]y U [g2]y                           union
[ua. g]y     = fix(lL. [g](y, L/a))                    least fixed point

where fix(f) = U_i f^i({}),  f^0 = {},  f^(i+1) = f(f^i)
```

### 2.2 The Type System — Ensuring Determinism

The key contribution of the 2019 paper: a type system that *statically
rejects* ambiguous grammars. A well-typed CFE is guaranteed parseable in
linear time with single-token lookahead and no backtracking.

**A type t is a triple:**

```
t  in  { Null : 2;  First : P(S);  FLast : P(S) }

Null   : 2 (= {true, false})  -- can this expression match the empty string?
First  : P(S) (power set)     -- which tokens can start a string in this language?
FLast  : P(S)                 -- which tokens can follow the last token of a
                                  string in this language (before the boundary)
```

A type is an *overapproximation*: a language L satisfies type t if
(1) e in L implies Null = true, (2) the tokens that start strings in L <= First,
(3) the tokens that can follow the last token of a string in L <= FLast.

**Type combinators** — types compose structurally to track properties
through CFE constructors. The notation `b ? S` means `if b then S else {}`:

```
t1 . t2  =  { Null  = t1.Null ^ t2.Null
              First = t1.First U (t1.Null ? t2.First)
              FLast = t2.FLast U (t2.Null ? (t2.First U t1.FLast)) }

t1 v t2  =  { Null  = t1.Null v t2.Null
              First = t1.First U t2.First
              FLast = t1.FLast U t2.FLast }
```

**Separability (t1 * t2)** — the side condition for *sequencing*. Ensures
the boundary between g1 and g2 in `g1 . g2` is unambiguous:

```
t1 * t2  def=  FLast(t1) n First(t2) = {}       no follow/first conflict
             ^  -Null(t1)                        g1 must consume input
```

The `-Null(t1)` condition is critical: it prevents left recursion by
guaranteeing that the first sub-expression in a sequence always consumes
at least one token before the second sub-expression can be reached.

**Apartness (t1 # t2)** — the side condition for *alternation*. Ensures
the branches of `g1 v g2` are distinguishable by a single lookahead token:

```
t1 # t2  def=  First(t1) n First(t2) = {}       disjoint starts
             ^  -(Null(t1) ^ Null(t2))           at most one nullable
```

Note: apartness has only two conditions. The FLast/First disjointness
check belongs to *separability* (*), not apartness (#). A common
confusion.

**Key typing rules:**

```
G; D |- g1 : t1     G,D; . |- g2 : t2     t1 * t2
--------------------------------------------------- TSeq
           G; D |- g1 . g2 : t1 . t2

G; D |- g1 : t1     G; D |- g2 : t2     t1 # t2
--------------------------------------------------- TVee
           G; D |- g1 v g2 : t1 v t2

                 G; D, a:t |- g : t
              ----------------------- TFix
              G; D |- ua:t. g : t
```

The two contexts G (free) and D (guarded) prevent left recursion. In TSeq,
D is appended to G when typing g2 — since * guarantees g1 is non-nullable,
variables in D become available (they're "guarded" by at least one consumed
token). In TFix, a goes into D — it can only be used after consuming input.

**Theorem (Unambiguous Parse Derivations):** If |- g : t, then for every
string w in [g], there is a *unique* parse derivation. Proof by
lexicographic induction on (|w|, rank(g)). (From the 2019 paper.)

### 2.3 Greibach Normal Form (GNF)

Standard GNF: every production in a grammar starts with a terminal:

```
Standard GNF:   n -> t n1 n2 ... nk     (terminal first, then nonterminals)
                n -> e                    (nullable production)
```

This is a classical result — every CFG can be converted to an equivalent
GNF grammar. The significance for parsing: GNF grammars naturally consume
one terminal per step, making them the ideal form for fusion.

### 2.4 Deterministic GNF (DGNF) — The Novel Contribution

DGNF adds a determinism constraint on top of standard GNF.

**Definition 2 (DGNF)** (from the paper): A grammar G is in DGNF if all
productions are either of the form `n -> t n_bar` or `n -> e`, and moreover:

1. **Determinism:** For any nonterminal n and terminal t, if there are
   two distinct productions `(n -> t1 n_bar1) in G` and `(n -> t2 n_bar2) in G`,
   then `t1 != t2`.

2. **Guarded e-productions:** If `G |- n { t n1 n2 n_bar` and
   `(n1 -> e) in G`, then for any terminal t, either
   `(n1 -> t n_bar1) not_in G` or `(n2 -> t n_bar2) not_in G`.

The guarded e-production condition uses the *expansion relation*
(`G |- n { ...`), which captures what a nonterminal can expand to
step by step. The condition says: when parsing reaches a point where
n1 is followed by n2 and n1 has an e-production, then n1 and n2
cannot *both* start with the same terminal. This prevents ambiguity
about whether to take the e-production or consume the terminal.

```
Example (NOT in DGNF):        Why it fails:

n  -> a n1 n2                  After consuming 'a', n1 is followed by n2.
n1 -> c | e                   n1 has e-production, AND both n1 and n2
n2 -> c                        can start with 'c'. Ambiguous: should 'c'
                               be consumed by n1 or n2?
```

**Theorem 3.1 (Deterministic Parsing):** If G is a DGNF grammar, then
for any expansion `G |- n { w`, there is a *unique* derivation.

**Why DGNF matters:** Given nonterminal n and next input token t:
- If there's a production n -> t n_bar, take it (unique by condition 1).
- If there's n -> e and no n-production starts with t, take e.
- Otherwise, parse error.

No backtracking. No ambiguity. One branch per (nonterminal, terminal) pair.

### 2.5 Normalization: CFE -> DGNF

The normalization algorithm N[g] transforms any well-typed CFE into an
equivalent DGNF grammar. It is *compositional* — one rule per CFE
constructor, each defined in terms of the normalization of sub-expressions.

**The normalization function** N[g] returns `n => G` — a distinguished
start nonterminal n and a grammar G. Each rule allocates a fresh
nonterminal:

```
(epsilon)  N[e]       = n => { n -> e }
(token)    N[t]       = n => { n -> t }
(bot)      N[_|_]     = n => {}

(seq)      N[g1 . g2] = n => { n -> N1 n2 | (n1 -> N1) in G1 }
                              U G1 U G2
           where N[g1] = n1 => G1,  N[g2] = n2 => G2

(alt)      N[g1 v g2] = n => { n -> N1 | (n1 -> N1) in G1 }
                              U { n -> N2 | (n2 -> N2) in G2 }
                              U G1 U G2
           where N[g1] = n1 => G1,  N[g2] = n2 => G2

(var)      N[a]       = n => { n -> a }

(fix)      N[ua. g]   = a => { a -> N | (n -> N) in G }           (1)
                              U { n' -> N n_bar' | (n' -> a n_bar') in G
                                             ^ (n -> N) in G }    (2)
                              U G \ {n' -> a n_bar'}               (3)
           where N[g] = n => G
```

**How each rule works:**

- **(seq)** copies each production of n1 and appends n2. This puts
  productions into GNF shape (terminal first, then nonterminals).
  The separability condition * guarantees `-Null(t1)`, so n1 never
  has an e-production — the appended form `N1 n2` is always valid.

- **(alt)** merges productions from both branches under a new start
  symbol. Apartness (#) guarantees disjoint First sets, so no two
  productions share the same leading terminal.

- **(fix)** is the most subtle. It operates in three stages:
  (1) Copy productions of n (the body's start symbol) into a.
  (2) For any production starting with the placeholder a (i.e. `n' -> a n_bar'`),
     substitute a with its actual productions (expand `a` away).
  (3) Keep all remaining productions that don't start with a.

  The var rule creates `n -> a` placeholders during normalization of g.
  The fix rule resolves them once a's productions are known. The typing
  rules guarantee this terminates: a goes into the guarded context D,
  so it cannot appear at the head of its own start symbol's productions
  (which would create `a -> a`, an infinite loop).

**Key metatheory:**

- **Theorem 3.3 (Well-definedness):** If G; D |- g : t, then N[g]
  succeeds for some G and n.

- **Corollary 3.5:** Normalizing any *closed* well-typed expression
  produces only `n -> e` and `n -> t n_bar` forms (no leftover `a n_bar` forms).

- **Theorem 3.7 (N[g] produces DGNF):** If .; . |- g : t, then
  N[g] returns `n => D` where D is a DGNF grammar.

- **Theorem 3.8 (Soundness):** Given .; . |- g : t and N[g] returns
  `n => G`, we have `w in [g].` iff `G |- n { w` for any string w.

The type system does the heavy lifting — it rejects grammars *before*
normalization that would produce DGNF conflicts. Normalization itself
is a total function on well-typed CFEs.

## 3. The Fusion Algorithm

This is where the performance gain comes from.

### 3.1 Setup: Two Separate Definitions

The user writes a **lexer** (mapping character sequences to token classes)
and a **parser** (a CFE over token classes), exactly as in a traditional
pipeline:

```
Lexer:   regex_number  = [0-9]+
         regex_string  = '"' [^"]* '"'
         regex_ws      = [ \t\n]+
         ...

Parser:  value  = NUMBER | STRING | array | object
         array  = '[' (value (',' value)*)? ']'
         object = '{' (pair  (',' pair)*)? '}'
         pair   = STRING ':' value
```

### 3.2 Fusion: From Tokens to Characters

The fusion algorithm F[L, G] operates on a canonicalized lexer L and a
normalized DGNF grammar G, producing a fused grammar F over characters.

The lexer L is a set of rules: `{ r => Return t }` (regex r produces
token t) and `{ r => Skip }` (regex r matches whitespace/comments).
The lexer is *canonicalized*: rules are disjoint on the left (no string
matches two regexes) and on the right (each token appears in one Return
rule, exactly one Skip rule).

**The fusion algorithm** (Figure 6 of the paper) produces three parts:

```
F[L, G] = F1 U F2 U F3

F1 = { n -> r n_bar | (r => Return t) in L ^ (n -> t n_bar) in G }   (inline lexer)
F2 = { n -> r n_bar | (r => Skip) in L ^ n in G }                    (whitespace)
F3 = { n ->? -r | (n -> e) in G ^ r = U{r | (n -> r n_bar) in F1 U F2} }
                                                                      (e -> lookahead)
```

**F1 (inline):** For each DGNF production `n -> t n_bar`, find the lexer rule
`r => Return t` and substitute: `n -> r n_bar`. This is the core fusion step
— the token `t` is replaced by the character-level regex `r` that
recognizes it. Lexer rules returning tokens not used by nonterminal n
are implicitly discarded.

**F2 (whitespace):** For each nonterminal n, add a production that matches
the Skip regex (whitespace/comments) and loops back to n. This allows
whitespace to appear before any token, matching the behavior of the
original separate lexer.

**F3 (e -> lookahead):** For nonterminals with an e-production, the
e-production becomes a *lookahead rule* `n ->? -r`, where r is the union
of all regexes appearing in other productions for n. The `?` means
"match but don't consume" — it checks that the next character does NOT
match any of n's active productions, confirming the e-branch is correct.

```
+----------+    +----------+         +--------------------------+
|  Lexer L |    |  DGNF G  |         |  Fused Grammar F         |
| (regexes)|    |          |  fuse   |                          |
| id=>atom |    | n->atom n|  -----> | n -> [a-z]+ n_bar  (F1) |
| sp=>Skip |    | n->lpar n|  -----> | n -> '(' n_bar     (F1) |
| (=>lpar  |    | n->e     |  -----> | n -> sp n           (F2) |
| )=>rpar  |    |          |         | n ->? -(id|sp|'(')  (F3) |
+----------+    +----------+         +--------------------------+
```

**The paper's running example is s-expressions** — directly relevant to
Wile. The fused s-expression grammar from the paper (Figure 3e):

```
Lexer:                          DGNF grammar:
  id    = [a-z]+  => Return atom    sexp  -> lpar sexps rpar | atom
  space = ' '|'\n' => Skip          rpar  -> rpar
  (     => Return lpar              sexps -> lpar sexps rpar sexps
  )     => Return rpar                    | atom sexps | e

Fused grammar (after F[L,G]):
  sexp  -> '(' sexps rpar          (F1: lpar -> '(')
        | [a-z]+ {}                (F1: atom -> id regex)
        | (' '|'\n') sexp          (F2: skip whitespace, retry)
  rpar  -> ')'                     (F1: rpar -> ')')
        | (' '|'\n') rpar          (F2: skip whitespace, retry)
  sexps -> '(' sexps rpar sexps    (F1: lpar -> '(')
        | [a-z]+ sexps             (F1: atom -> id regex)
        | (' '|'\n') sexps         (F2: skip whitespace)
        | ? -([a-z]+|' '|'\n'|'(') (F3: e becomes lookahead)
```

### 3.3 What Gets Eliminated

| Cost in traditional pipeline | After fusion |
|-----|------|
| Allocate Token struct per lexeme | Gone — no Token objects exist |
| Store token type tag | Gone — branch was resolved at fusion time |
| Case-switch on token type in parser | Gone — parser branches on characters directly |
| Two function-call layers (lex + parse) | One pass over characters |
| Cache pollution from Token objects | Gone — only source bytes and AST nodes in cache |

### 3.4 Runtime Execution Model

The fused parser operates as a recursive-descent parser over *characters*:

```
func parseValue(input []byte, pos int) (AST, int) {
    ch := input[pos]
    switch {
    case ch >= '0' && ch <= '9':
        // Fused: this IS the lexer for NUMBER, directly
        // followed by the parser action for NUMBER
        end := pos + 1
        for end < len(input) && input[end] >= '0' && input[end] <= '9' {
            end++
        }
        return NumberNode(input[pos:end]), end

    case ch == '"':
        // Fused: lexer for STRING + parser action, inline
        ...

    case ch == '[':
        // Fused: literal '[' + recursive parseArray
        return parseArray(input, pos+1)
    ...
    }
}
```

No token objects. No intermediate dispatch. The lexer's character-matching
code is *inlined* at each parser decision point. The information about
which token was recognized flows through control flow (which branch was
taken), not through a data structure.

### 3.5 Staging (Multi-Stage Programming)

The flap implementation uses MetaOCaml's staging to generate specialized
code at compile time. The DGNF normalization and fusion happen at
*compile time*; the generated code contains only the fused character-level
parser with no interpretive overhead.

This is analogous to how parser generators (yacc, ANTLR) work — the
grammar analysis happens once, and the output is a specialized parser.
The difference is that flap's output is *also* fused with the lexer.

## 4. Performance Results

Benchmarks from the paper (throughput in MB/s, OCaml, Intel i9-12900K):

| Benchmark | ocamlyacc | asp (staged) | normalized (unfused) | **flap** | flap/yacc |
|-----------|-----------|-------------|---------------------|----------|-----------|
| json      | 236       | 108         | 344                 | **1359** | 5.8x      |
| sexp      | 76        | 92          | 125                 | **213**  | 2.8x      |
| arith     | 30        | 29          | 29                  | **57**   | 1.9x      |
| pgn       | 67        | 81          | 48                  | **286**  | 4.3x      |
| ppm       | 16        | 27          | 14                  | **104**  | 6.5x      |
| csv       | 70        | 89          | 162                 | **323**  | 4.6x      |

Key observations:
- Fusion (flap vs normalized-unfused) alone gives 1.7-3.9x improvement.
- The json benchmark reaches ~1.4 GB/s (~2.3 cycles/byte).
- The speedup varies by grammar: simpler grammars (arith) benefit less
  because token overhead is a smaller fraction of total work.
- The "normalized" column uses flap's DGNF + staging but *without* fusion
  (tokens still materialized via OCaml's Stream type). The difference
  between "normalized" and "flap" isolates the fusion benefit.

## 5. Sketch: Applying Fused Lexing to Wile

### 5.1 Current Wile Architecture

```
source string
    |
    v
+---------------------------------------------+
| Tokenizer  (internal/tokenizer/)            |
|                                             |
|  io.RuneReader -> single rune lookahead     |
|  Next() -> *SimpleToken (heap-allocated)    |
|                                             |
|  SimpleToken ~ 56 bytes:                    |
|    2x SourceIndexes (line, col, offset)     |
|    TokenizerState enum (~70 variants)       |
|    src string (raw text)                    |
|    val string (processed: escapes resolved) |
|    sign, hash, radix metadata               |
+----------------+----------------------------+
                 |  Token interface (Next() call per token)
                 v
+---------------------------------------------+
| Parser  (internal/parser/)                  |
|                                             |
|  Recursive descent, switch on TokenizerState|
|  Single token lookahead (p.cur)             |
|  Builds *Pair / SyntaxValue AST            |
|  Number parsing (int, float, rational,      |
|    complex, bigint, bigfloat)               |
+---------------------------------------------+
```

**Key properties:**
- Streaming: one token at a time via `Next()`
- Heap allocation per token (`*SimpleToken` pointer)
- ~70 token type variants (many for number sub-types)
- Two string fields per token (`src` for raw, `val` for processed)
- Rich position tracking (line/col/offset at start and end)

### 5.2 Where Flap-Style Fusion Would Help

**Token allocation is the obvious target.** Every call to `Next()` creates
a `*SimpleToken` on the heap. For a typical Scheme source file, this means
one allocation per atom — every symbol, number, string, paren, quote mark.
With fusion, these allocations disappear.

**The token type switch is the second target.** The parser's `readSyntax()`
dispatches on `p.cur.Type()`, a ~70-variant enum. With fusion, the parser
would branch on the *first character* of the next lexeme instead, and the
lexer's character-matching code would be inlined at each branch.

**Number parsing is the third target.** Wile currently tokenizes numbers
into ~15 token sub-types (integer, decimal, float, rational, complex, polar,
with radix/sign/hash variants), then the parser re-examines each to build
numeric values. Fusion would let the parser drive number recognition
directly, avoiding the intermediate classification.

### 5.3 What Would NOT Change

**Scheme's grammar is simple enough that DGNF is trivially achievable.**
S-expression syntax is almost already in GNF — every datum starts with a
distinctive character:

```
datum  = '(' list-tail          open paren
       | '#(' vector-tail       hash-paren
       | '"' string-tail        quote
       | '#t' | '#f'            boolean
       | [0-9+-] number-tail    number
       | symbol-char symbol-tail  symbol
       | '\'' datum             quote abbreviation
       | '`' datum              quasiquote
       | ',' datum | ',@' datum unquote
       | '#;' datum             datum comment
       | '#|' block-comment     block comment
```

Each alternative starts with a distinct character (or 2-character prefix).
There are no ambiguous branches. This is essentially DGNF already.

**The expander, compiler, and VM are unaffected.** Fusion only changes
the tokenizer<->parser interface. Everything downstream of `SyntaxValue`
construction is untouched.

### 5.4 What Would Change

#### 5.4.1 Eliminate the Token Interface

Replace `Token` / `*SimpleToken` / `Next()` with direct character-level
reading inside the parser. The parser becomes a scannerless recursive
descent parser over runes.

```
Before:                              After:

Parser.readSyntax()                  Parser.readSyntax()
  tok := p.toks.Next()                ch := p.peekRune()
  switch tok.Type() {                  switch {
  case TokenSymbol:                    case isSymbolStart(ch):
    return makeSymbol(tok)               return p.readSymbol()
  case TokenInteger:                   case isDigit(ch) || ch=='+' || ch=='-':
    return parseInt(tok)                 return p.readNumber()
  case TokenString:                    case ch == '"':
    return makeString(tok)               return p.readString()
  case TokenOpenParen:                 case ch == '(':
    return p.readList()                  return p.readList()
  ...                                  ...
  }                                    }
```

#### 5.4.2 Inline Lexer Logic Into Parser Methods

Each `readXxx()` method in the parser would contain the character-level
scanning that currently lives in the tokenizer. For example, `readSymbol()`
would accumulate runes until a delimiter, `readString()` would handle
escape sequences, `readNumber()` would handle radix prefixes and decimal
points.

This is *not* copy-pasting the tokenizer into the parser. The scanning
logic would be restructured to directly produce `SyntaxValue` nodes
instead of intermediate `Token` objects.

#### 5.4.3 Source Position Tracking

Currently, positions are tracked in `SimpleToken.istart` / `iend`. In a
fused parser, positions would be tracked by the parser's own read cursor.
Before starting each datum, record (line, col, offset); after finishing,
the current cursor is the end position.

```go
type FusedParser struct {
    input    []byte          // or io.RuneReader
    pos      int             // current byte offset
    line     int             // current line
    col      int             // current column
    // ... environment, datumLabels, etc (same as today)
}
```

#### 5.4.4 Number Parsing Simplification

The current tokenizer classifies numbers into ~15 sub-types, then the
parser re-dispatches on them. Fusion collapses this into a single
`readNumber()` that scans characters and directly builds the numeric
value:

```
readNumber():
    scan optional sign
    scan optional radix prefix (#b, #o, #d, #x)
    scan digits in appropriate base
    if '/' -> scan rational denominator -> return Rational
    if '.' -> scan decimal part -> return Float
    if 'e'/'E' -> scan exponent -> return Float
    if '@' or '+i'/'-i' -> scan complex part -> return Complex
    return Integer (or BigInteger on overflow)
```

No intermediate token type enum. The parser builds the value as it scans.

#### 5.4.5 Whitespace and Comment Handling

Currently the tokenizer skips whitespace between tokens. In a fused
parser, each `readSyntax()` call would start by skipping whitespace
and comments:

```go
func (p *FusedParser) skipWhitespaceAndComments() {
    for {
        ch := p.peekRune()
        switch {
        case isWhitespace(ch):
            p.advance()
        case ch == ';':
            p.skipLineComment()
        case ch == '#' && p.peek2() == '|':
            p.skipBlockComment()
        default:
            return
        }
    }
}
```

### 5.5 Architectural Implications

```
Current:
+-----------+  Token   +--------+  SyntaxValue  +----------+
| Tokenizer | -------> | Parser | ------------>  | Expander |
+-----------+ *Simple  +--------+                +----------+
               Token

Fused:
+---------------------+  SyntaxValue  +----------+
| FusedParser          | -----------> | Expander |
| (char-level scanner |               +----------+
|  + recursive descent)|
+---------------------+
```

**The `internal/tokenizer` package would become internal to the parser**
or disappear entirely. The `Token` interface and `*SimpleToken` type
would no longer exist as public API.

### 5.6 What Wile Does NOT Need from Flap

Flap's full machinery includes:

| Flap feature | Needed for Wile? | Why / why not |
|------|------|------|
| Typed CFEs with compile-time checking | No | S-expression grammar is trivially unambiguous |
| DGNF normalization algorithm | No | Grammar is already hand-written in GNF shape |
| Multi-stage programming (MetaOCaml) | No | Wile's parser is hand-written Go, not generated |
| Fixed-point unrolling | No | S-expression grammar has no mutual recursion at the lexical level |
| Formal apartness checking | No | Scheme tokens start with distinct characters by design |

**What Wile DOES need from flap:** The *architectural pattern* — fusing
the lexer's character-level scanning into the parser's recursive descent
structure, eliminating the intermediate token representation.

### 5.7 Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Increased parser complexity | Medium | S-expression grammar is simple; fused parser may actually be *simpler* than tokenizer + parser separately |
| Loss of token stream for tooling | Low | Wile has no external consumers of the token stream today |
| Source position accuracy | Low | Fused parser tracks positions at the character level — same or better precision |
| Error message quality | Medium | Token-level errors ("expected number, got symbol") become character-level ("unexpected character 'x' in number"); needs care |
| Testing surface | Low | Existing parser tests specify input->output, not intermediate tokens; they'd still work |
| `#!fold-case` directive | Low | Currently a parser-level directive that sets tokenizer mode; in fused model, it sets a parser flag directly (simpler) |

### 5.8 Incremental Adoption Path

A full rewrite is not required. The fusion can be done incrementally:

1. **Phase 1: Measure.** Profile the current pipeline to confirm token
   allocation is a meaningful cost. If Wile's bottleneck is the expander
   or VM, fusion won't help much.

2. **Phase 2: Internalize.** Move the tokenizer's scanning methods into
   the parser as private methods. Keep the Token interface temporarily
   as an internal detail. This is a refactoring with no behavioral change.

3. **Phase 3: Eliminate tokens for simple cases.** For single-character
   tokens (`(`, `)`, `'`, etc.), have the parser read the character
   directly instead of going through `Next()`. This eliminates allocations
   for the highest-frequency tokens.

4. **Phase 4: Fuse number parsing.** Merge the tokenizer's number
   classification with the parser's number value construction. This is
   the biggest win — currently ~15 token types collapse into one
   `readNumber()` method.

5. **Phase 5: Fuse remaining token types.** Strings, symbols, booleans,
   characters. At this point, the Token type can be deleted.

6. **Phase 6: Optimize input model.** With fusion complete, the parser
   can operate on `[]byte` directly (instead of `io.RuneReader`) for
   in-memory sources, enabling zero-copy string extraction and SIMD
   whitespace skipping.

### 5.9 Estimated Scope

| Component | Lines today | After fusion | Change |
|-----------|------------|--------------|--------|
| `internal/tokenizer/tokenizer.go` | ~1200 | 0 (deleted) | Absorbed into parser |
| `internal/tokenizer/*_test.go` | ~2000 | ~500 (adapted) | Tests move to parser |
| `internal/parser/parser.go` | ~800 | ~1400 | Gains scanning logic |
| Token interface + SimpleToken | ~150 | 0 (deleted) | No longer needed |
| Source position types | ~80 | ~80 (kept) | Reused in parser |

Net effect: roughly the same total line count, but one fewer abstraction
boundary and zero per-token heap allocations.

## 6. References

- Yallop, Xie, Krishnaswami. *flap: A Deterministic Parser with Fused
  Lexing.* PLDI 2023. https://dl.acm.org/doi/10.1145/3591269
  ([PDF](https://www.cl.cam.ac.uk/~nk480/flap.pdf))

- Krishnaswami, Yallop. *A Typed, Algebraic Approach to Parsing.* PLDI
  2019. https://dl.acm.org/doi/10.1145/3314221.3314625
  (Distinguished Paper + Distinguished Artifact)

- Van Wyk, Schwerdfeger. *Context-Aware Scanning for Parsing Extensible
  Languages.* GPCE 2007. https://dl.acm.org/doi/10.1145/1289971.1289983

- Tratt. *Why Split Lexing and Parsing Into Two Separate Phases?* 2023.
  https://tratt.net/laurie/blog/2023/why_split_lexing_and_parsing_into_two_separate_phases.html

- Paguroidea (Rust reimplementation of flap).
  https://users.rust-lang.org/t/introduce-paguroidea-parser-generator/94690
