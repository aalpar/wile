# Fused Lexing and Parsing: The Flap Approach

Based on: *flap: A Deterministic Parser with Fused Lexing* (Yallop, Xie,
Krishnaswami — PLDI 2023), building on *A Typed, Algebraic Approach to Parsing*
(Krishnaswami & Yallop — PLDI 2019, Distinguished Paper).

## 1. The Core Problem

Traditional pipelines separate lexing and parsing into two phases:

```
source bytes → Lexer → Token stream → Parser → AST
                         ↑
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
g  ::=  ⊥           empty language (matches nothing)
     |  ε           empty string (matches "")
     |  t           terminal (matches single token t)
     |  α           variable (for recursion)
     |  g₁ · g₂    sequencing (g₁ followed by g₂)
     |  g₁ ∨ g₂    alternation (g₁ or g₂)
     |  μα:τ. g    least fixed point (recursion with type annotation)
```

These satisfy the equations of an idempotent semiring:

```
(∨, ⊥)  forms a commutative monoid    g ∨ ⊥ = g,  g ∨ g = g
(·, ε)  forms a monoid                g · ε = ε · g = g
·  distributes over ∨
⊥  annihilates ·                      g · ⊥ = ⊥ · g = ⊥
```

**Denotational semantics** — ⟦g⟧γ maps a CFE to the set of strings it
accepts, given an environment γ for free variables:

```
⟦ε⟧γ         = {ε}                                     singleton empty string
⟦t⟧γ         = {t}                                     singleton one-token string
⟦⊥⟧γ         = ∅                                       empty language
⟦α⟧γ         = γ(α)                                    variable lookup
⟦g₁ · g₂⟧γ  = { w · w' | w ∈ ⟦g₁⟧γ ∧ w' ∈ ⟦g₂⟧γ }  concatenation
⟦g₁ ∨ g₂⟧γ  = ⟦g₁⟧γ ∪ ⟦g₂⟧γ                          union
⟦μα. g⟧γ     = fix(λL. ⟦g⟧(γ, L/α))                   least fixed point

where fix(f) = ⋃ᵢ∈ℕ Lᵢ,  L₀ = ∅,  Lᵢ₊₁ = f(Lᵢ)
```

### 2.2 The Type System — Ensuring Determinism

The key contribution of the 2019 paper: a type system that *statically
rejects* ambiguous grammars. A well-typed CFE is guaranteed parseable in
linear time with single-token lookahead and no backtracking.

**A type τ is a triple:**

```
τ  ∈  { Null : 2;  First : P(Σ);  FLast : P(Σ) }

Null   : 2 (= {true, false})  — can this expression match the empty string?
First  : P(Σ) (power set)     — which tokens can start a string in this language?
FLast  : P(Σ)                 — which tokens can follow the last token of a
                                 string in this language (before the boundary)
```

A type is an *overapproximation*: a language L satisfies type τ if
(1) ε ∈ L implies Null = true, (2) the tokens that start strings in L ⊆ First,
(3) the tokens that can follow the last token of a string in L ⊆ FLast.

**Type combinators** — types compose structurally to track properties
through CFE constructors. The notation `b ? S` means `if b then S else ∅`:

```
τ₁ · τ₂  =  { Null  = τ₁.Null ∧ τ₂.Null
               First = τ₁.First ∪ (τ₁.Null ? τ₂.First)
               FLast = τ₂.FLast ∪ (τ₂.Null ? (τ₂.First ∪ τ₁.FLast)) }

τ₁ ∨ τ₂  =  { Null  = τ₁.Null ∨ τ₂.Null
               First = τ₁.First ∪ τ₂.First
               FLast = τ₁.FLast ∪ τ₂.FLast }
```

**Separability (τ₁ ⊛ τ₂)** — the side condition for *sequencing*. Ensures
the boundary between g₁ and g₂ in `g₁ · g₂` is unambiguous:

```
τ₁ ⊛ τ₂  def=  FLast(τ₁) ∩ First(τ₂) = ∅       no follow/first conflict
             ∧  ¬Null(τ₁)                         g₁ must consume input
```

The `¬Null(τ₁)` condition is critical: it prevents left recursion by
guaranteeing that the first sub-expression in a sequence always consumes
at least one token before the second sub-expression can be reached.

**Apartness (τ₁ # τ₂)** — the side condition for *alternation*. Ensures
the branches of `g₁ ∨ g₂` are distinguishable by a single lookahead token:

```
τ₁ # τ₂  def=  First(τ₁) ∩ First(τ₂) = ∅       disjoint starts
             ∧  ¬(Null(τ₁) ∧ Null(τ₂))           at most one nullable
```

Note: apartness has only two conditions. The FLast/First disjointness
check belongs to *separability* (⊛), not apartness (#). A common
confusion.

**Key typing rules:**

```
Γ; Δ ⊢ g₁ : τ₁     Γ,Δ; • ⊢ g₂ : τ₂     τ₁ ⊛ τ₂
────────────────────────────────────────────────────── TSeq
           Γ; Δ ⊢ g₁ · g₂ : τ₁ · τ₂

Γ; Δ ⊢ g₁ : τ₁     Γ; Δ ⊢ g₂ : τ₂     τ₁ # τ₂
────────────────────────────────────────────────────── TVee
           Γ; Δ ⊢ g₁ ∨ g₂ : τ₁ ∨ τ₂

                 Γ; Δ, α:τ ⊢ g : τ
              ──────────────────────── TFix
              Γ; Δ ⊢ μα:τ. g : τ
```

The two contexts Γ (free) and Δ (guarded) prevent left recursion. In TSeq,
Δ is appended to Γ when typing g₂ — since ⊛ guarantees g₁ is non-nullable,
variables in Δ become available (they're "guarded" by at least one consumed
token). In TFix, α goes into Δ — it can only be used after consuming input.

**Theorem (Unambiguous Parse Derivations):** If ⊢ g : τ, then for every
string w ∈ ⟦g⟧, there is a *unique* parse derivation. Proof by
lexicographic induction on (|w|, rank(g)). (From the 2019 paper.)

### 2.3 Greibach Normal Form (GNF)

Standard GNF: every production in a grammar starts with a terminal:

```
Standard GNF:   n → t n₁ n₂ ... nₖ     (terminal first, then nonterminals)
                n → ε                     (nullable production)
```

This is a classical result — every CFG can be converted to an equivalent
GNF grammar. The significance for parsing: GNF grammars naturally consume
one terminal per step, making them the ideal form for fusion.

### 2.4 Deterministic GNF (DGNF) — The Novel Contribution

DGNF adds a determinism constraint on top of standard GNF.

**Definition 2 (DGNF)** (from the paper): A grammar G is in DGNF if all
productions are either of the form `n → t n̄` or `n → ε`, and moreover:

1. **Determinism:** For any nonterminal n and terminal t, if there are
   two distinct productions `(n → t₁ n̄₁) ∈ G` and `(n → t₂ n̄₂) ∈ G`,
   then `t₁ ≠ t₂`.

2. **Guarded ε-productions:** If `G ⊢ n { t n₁ n₂ n̄` and
   `(n₁ → ε) ∈ G`, then for any terminal t, either
   `(n₁ → t n̄₁) ∉ G` or `(n₂ → t n̄₂) ∉ G`.

The guarded ε-production condition uses the *expansion relation*
(`G ⊢ n { ...`), which captures what a nonterminal can expand to
step by step. The condition says: when parsing reaches a point where
n₁ is followed by n₂ and n₁ has an ε-production, then n₁ and n₂
cannot *both* start with the same terminal. This prevents ambiguity
about whether to take the ε-production or consume the terminal.

```
Example (NOT in DGNF):        Why it fails:

n  → a n₁ n₂                  After consuming 'a', n₁ is followed by n₂.
n₁ → c | ε                    n₁ has ε-production, AND both n₁ and n₂
n₂ → c                        can start with 'c'. Ambiguous: should 'c'
                               be consumed by n₁ or n₂?
```

**Theorem 3.1 (Deterministic Parsing):** If G is a DGNF grammar, then
for any expansion `G ⊢ n { w`, there is a *unique* derivation.

**Why DGNF matters:** Given nonterminal n and next input token t:
- If there's a production n → t n̄, take it (unique by condition 1).
- If there's n → ε and no n-production starts with t, take ε.
- Otherwise, parse error.

No backtracking. No ambiguity. One branch per (nonterminal, terminal) pair.

### 2.5 Normalization: CFE → DGNF

The normalization algorithm N⟦g⟧ transforms any well-typed CFE into an
equivalent DGNF grammar. It is *compositional* — one rule per CFE
constructor, each defined in terms of the normalization of sub-expressions.

**The normalization function** N⟦g⟧ returns `n ⇒ G` — a distinguished
start nonterminal n and a grammar G. Each rule allocates a fresh
nonterminal:

```
(epsilon)  N⟦ε⟧       = n ⇒ { n → ε }
(token)    N⟦t⟧       = n ⇒ { n → t }
(bot)      N⟦⊥⟧       = n ⇒ ∅

(seq)      N⟦g₁ · g₂⟧ = n ⇒ { n → N₁ n₂ | (n₁ → N₁) ∈ G₁ }
                              ∪ G₁ ∪ G₂
           where N⟦g₁⟧ = n₁ ⇒ G₁,  N⟦g₂⟧ = n₂ ⇒ G₂

(alt)      N⟦g₁ ∨ g₂⟧ = n ⇒ { n → N₁ | (n₁ → N₁) ∈ G₁ }
                              ∪ { n → N₂ | (n₂ → N₂) ∈ G₂ }
                              ∪ G₁ ∪ G₂
           where N⟦g₁⟧ = n₁ ⇒ G₁,  N⟦g₂⟧ = n₂ ⇒ G₂

(var)      N⟦α⟧       = n ⇒ { n → α }

(fix)      N⟦μα. g⟧   = α ⇒ { α → N | (n → N) ∈ G }           ①
                              ∪ { n' → N n̄' | (n' → α n̄') ∈ G
                                             ∧ (n → N) ∈ G }    ②
                              ∪ G \ {n' → α n̄'}                  ③
           where N⟦g⟧ = n ⇒ G
```

**How each rule works:**

- **(seq)** copies each production of n₁ and appends n₂. This puts
  productions into GNF shape (terminal first, then nonterminals).
  The separability condition ⊛ guarantees `¬Null(τ₁)`, so n₁ never
  has an ε-production — the appended form `N₁ n₂` is always valid.

- **(alt)** merges productions from both branches under a new start
  symbol. Apartness (#) guarantees disjoint First sets, so no two
  productions share the same leading terminal.

- **(fix)** is the most subtle. It operates in three stages:
  ① Copy productions of n (the body's start symbol) into α.
  ② For any production starting with the placeholder α (i.e. `n' → α n̄'`),
     substitute α with its actual productions (expand `α` away).
  ③ Keep all remaining productions that don't start with α.

  The var rule creates `n → α` placeholders during normalization of g.
  The fix rule resolves them once α's productions are known. The typing
  rules guarantee this terminates: α goes into the guarded context Δ,
  so it cannot appear at the head of its own start symbol's productions
  (which would create `α → α`, an infinite loop).

**Key metatheory:**

- **Theorem 3.3 (Well-definedness):** If Γ; Δ ⊢ g : τ, then N⟦g⟧
  succeeds for some G and n.

- **Corollary 3.5:** Normalizing any *closed* well-typed expression
  produces only `n → ε` and `n → t n̄` forms (no leftover `α n̄` forms).

- **Theorem 3.7 (N⟦g⟧ produces DGNF):** If •; • ⊢ g : τ, then
  N⟦g⟧ returns `n ⇒ D` where D is a DGNF grammar.

- **Theorem 3.8 (Soundness):** Given •; • ⊢ g : τ and N⟦g⟧ returns
  `n ⇒ G`, we have `w ∈ ⟦g⟧•` iff `G ⊢ n { w` for any string w.

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

The fusion algorithm F⟦L, G⟧ operates on a canonicalized lexer L and a
normalized DGNF grammar G, producing a fused grammar F over characters.

The lexer L is a set of rules: `{ r ⇒ Return t }` (regex r produces
token t) and `{ r ⇒ Skip }` (regex r matches whitespace/comments).
The lexer is *canonicalized*: rules are disjoint on the left (no string
matches two regexes) and on the right (each token appears in one Return
rule, exactly one Skip rule).

**The fusion algorithm** (Figure 6 of the paper) produces three parts:

```
F⟦L, G⟧ = F₁ ∪ F₂ ∪ F₃

F₁ = { n → r n̄ | (r ⇒ Return t) ∈ L ∧ (n → t n̄) ∈ G }   (inline lexer)
F₂ = { n → r n̄ | (r ⇒ Skip) ∈ L ∧ n ∈ G }                (whitespace)
F₃ = { n →? ¬r | (n → ε) ∈ G ∧ r = ⋃{r | (n → r n̄) ∈ F₁∪F₂} }
                                                             (ε → lookahead)
```

**F₁ (inline):** For each DGNF production `n → t n̄`, find the lexer rule
`r ⇒ Return t` and substitute: `n → r n̄`. This is the core fusion step
— the token `t` is replaced by the character-level regex `r` that
recognizes it. Lexer rules returning tokens not used by nonterminal n
are implicitly discarded.

**F₂ (whitespace):** For each nonterminal n, add a production that matches
the Skip regex (whitespace/comments) and loops back to n. This allows
whitespace to appear before any token, matching the behavior of the
original separate lexer.

**F₃ (ε → lookahead):** For nonterminals with an ε-production, the
ε-production becomes a *lookahead rule* `n →? ¬r`, where r is the union
of all regexes appearing in other productions for n. The `?` means
"match but don't consume" — it checks that the next character does NOT
match any of n's active productions, confirming the ε-branch is correct.

```
┌──────────┐    ┌──────────┐         ┌──────────────────────────┐
│  Lexer L │    │  DGNF G  │         │  Fused Grammar F         │
│ (regexes)│    │          │  fuse   │                          │
│ id⇒atom  │    │ n→atom n̄│ ──────▶ │ n → [a-z]+ n̄      (F₁) │
│ sp⇒Skip  │    │ n→lpar n̄│ ──────▶ │ n → '(' n̄         (F₁) │
│ (⇒lpar   │    │ n→ε      │ ──────▶ │ n → sp n           (F₂) │
│ )⇒rpar   │    │          │         │ n →? ¬(id|sp|'(')  (F₃) │
└──────────┘    └──────────┘         └──────────────────────────┘
```

**The paper's running example is s-expressions** — directly relevant to
Wile. The fused s-expression grammar from the paper (Figure 3e):

```
Lexer:                          DGNF grammar:
  id    = [a-z]+  ⇒ Return atom    sexp  → lpar sexps rpar | atom
  space = ' '|'\n' ⇒ Skip          rpar  → rpar
  (     ⇒ Return lpar              sexps → lpar sexps rpar sexps
  )     ⇒ Return rpar                    | atom sexps | ε

Fused grammar (after F⟦L,G⟧):
  sexp  → '(' sexps rpar          (F₁: lpar → '(')
        | [a-z]+ ∅                 (F₁: atom → id regex)
        | (' '|'\n') sexp          (F₂: skip whitespace, retry)
  rpar  → ')'                      (F₁: rpar → ')')
        | (' '|'\n') rpar          (F₂: skip whitespace, retry)
  sexps → '(' sexps rpar sexps    (F₁: lpar → '(')
        | [a-z]+ sexps             (F₁: atom → id regex)
        | (' '|'\n') sexps         (F₂: skip whitespace)
        | ? ¬([a-z]+|' '|'\n'|'(') (F₃: ε becomes lookahead)
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
| json      | 236       | 108         | 344                 | **1359** | 5.8×      |
| sexp      | 76        | 92          | 125                 | **213**  | 2.8×      |
| arith     | 30        | 29          | 29                  | **57**   | 1.9×      |
| pgn       | 67        | 81          | 48                  | **286**  | 4.3×      |
| ppm       | 16        | 27          | 14                  | **104**  | 6.5×      |
| csv       | 70        | 89          | 162                 | **323**  | 4.6×      |

Key observations:
- Fusion (flap vs normalized-unfused) alone gives 1.7-3.9× improvement.
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
    │
    ▼
┌─────────────────────────────────────────────┐
│ Tokenizer  (internal/tokenizer/)            │
│                                             │
│  io.RuneReader → single rune lookahead      │
│  Next() → *SimpleToken (heap-allocated)     │
│                                             │
│  SimpleToken ≈ 56 bytes:                    │
│    2× SourceIndexes (line, col, offset)     │
│    TokenizerState enum (~70 variants)       │
│    src string (raw text)                    │
│    val string (processed: escapes resolved) │
│    sign, hash, radix metadata               │
└────────────────┬────────────────────────────┘
                 │  Token interface (Next() call per token)
                 ▼
┌─────────────────────────────────────────────┐
│ Parser  (internal/parser/)                  │
│                                             │
│  Recursive descent, switch on TokenizerState│
│  Single token lookahead (p.cur)             │
│  Builds *Pair / SyntaxValue AST             │
│  Number parsing (int, float, rational,      │
│    complex, bigint, bigfloat)               │
└─────────────────────────────────────────────┘
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
       | [0-9±] number-tail     number
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
the tokenizer↔parser interface. Everything downstream of `SyntaxValue`
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
    if '/' → scan rational denominator → return Rational
    if '.' → scan decimal part → return Float
    if 'e'/'E' → scan exponent → return Float
    if '@' or '+i'/'-i' → scan complex part → return Complex
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
┌───────────┐  Token   ┌────────┐  SyntaxValue  ┌──────────┐
│ Tokenizer │ ───────▶ │ Parser │ ────────────▶  │ Expander │
└───────────┘ *Simple  └────────┘                └──────────┘
               Token

Fused:
┌─────────────────────┐  SyntaxValue  ┌──────────┐
│ FusedParser          │ ────────────▶ │ Expander │
│ (char-level scanner │               └──────────┘
│  + recursive descent)│
└─────────────────────┘
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
| Testing surface | Low | Existing parser tests specify input→output, not intermediate tokens; they'd still work |
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
