# Subsystem Simplification Plan — COMPLETE

**Status**: All phases (1-9) complete as of commit a310a22.

This plan organized 9 simplification opportunities across core packages. All work is done. The sections below are preserved as reference for architectural decisions that remain relevant.

## Evaluation: Go 1.24 `unique.Handle` and `maphash.Comparable` for Symbols

Go 1.24 introduces two features that appear relevant to Wile's symbol interning and hashing. This section evaluates both against Wile's actual architecture.

### Current Architecture

```
Parser ──→ env.InternSymbol(NewSymbol(name)) ──→ TopLevelEnvironment
string->symbol ──→ env.InternSymbol(sym) ──────→ TopLevelEnvironment
                                                       │
                                            map[Symbol]*Symbol + sync.RWMutex
                                            (per-VM, with parent delegation)
```

Two comparison regimes coexist:

| Context | Comparison | Why |
|---------|-----------|-----|
| Internal (compiler, expander, match) | `.Key ==` (string) | Anticipates future string comparison optimization; works without interning |
| User-facing (`eq?`, environment lookup) | `*Symbol` pointer identity | R7RS §6.5 requires `(eq? 'foo 'foo) → #t` for interned symbols |

Per-VM interning is intentional: two `Engine` instances in the same Go process must have separate symbol identity spaces. This is a sandboxing property — symbols from one VM cannot be `eq?`-equal to symbols from another.

### `unique.Handle[string]` — Does Not Fit

`unique.Handle` (Go 1.24, `unique` package) provides process-global canonical interning with pointer-equality semantics and automatic GC cleanup via internal weak pointers.

**The fatal conflict is scope.** `unique.Make("foo")` returns the same `Handle` regardless of which goroutine, which VM, or which universe called it. This is process-global by design — the entire point is that equal strings always produce the same pointer.

```
unique.Handle:    process-global identity
                  unique.Make("foo") == unique.Make("foo")  ← always, everywhere

Wile Symbols:     per-VM identity
                  vm1.Intern("foo") != vm2.Intern("foo")   ← intentional isolation
```

If Wile adopted `unique.Handle[string]` as the interning substrate, symbol identity would leak across VM instances. A symbol created in a sandboxed VM would be `eq?`-equal to one in the host VM. This violates the isolation invariant.

**Could we use Handle internally but compare by VM-tagged wrapper?** Possible but backwards — we'd add complexity to recover an invariant we currently get for free. The current `map[Symbol]*Symbol` per `TopLevelEnvironment` is simple, correct, and has the right scope.

**What unique.Handle does well that we don't need:**

| Feature | unique.Handle | Wile's needs |
|---------|--------------|-------------|
| Automatic GC of unused entries | Yes (weak pointers) | Not needed — symbols are cheap, and a Scheme session's symbol table is small enough to hold forever |
| Lock-free concurrent reads | Yes (hash-trie) | RWMutex is adequate — interning happens at parse time and `string->symbol`, not in hot loops |
| String cloning (substring leak prevention) | Yes | Not relevant — symbol keys are standalone strings, not substrings of larger buffers |
| Process-global deduplication | Yes | **Unwanted** — violates per-VM isolation |

**Verdict: Do not adopt.** The per-VM isolation invariant is non-negotiable. `unique.Handle` is the wrong scope.

### `maphash.Comparable` — Partial Fit, Not Worth It

`maphash.Comparable[T]` (Go 1.24) exposes Go's runtime hash function (AES on amd64/arm64, Wyhash elsewhere) for any `comparable` type. It replaces the need for hand-written hash functions like FNV-1a.

**Where it could replace custom hashing:**

Wile's `Hashable` interface requires `HashCode() uint64` for Scheme hash table keys. Currently implemented with FNV-1a via `hashString`/`hashUint64` in `values/hash.go`. Types implementing `Hashable`: Integer, BigInteger, Float, Rational, Boolean, Character, Symbol, Byte, String.

`maphash.Comparable` could replace FNV-1a for these types — but with trade-offs:

**1. Process-local seeds break determinism.**

`maphash.Comparable` requires a `Seed` that produces process-unique hashes. Different process runs produce different hashes for the same value. This is fine for Go's built-in maps (which also randomize), and fine for Wile's `Hashtable` (which uses hashes only for bucket indexing at runtime). But it means:

- Hash values cannot appear in error messages or debugging output reproducibly
- If Wile ever serializes hash tables (e.g., for image-based persistence), stored hashes would be invalid on reload
- Test assertions on specific hash values would break across runs

The current FNV-1a produces deterministic hashes. This isn't load-bearing today, but it's a property we'd lose.

**2. Heap escape for non-string values.**

`maphash.Comparable` forces non-string arguments to escape to the heap (documented Go limitation). For `Integer{Value: 42}`, this means a heap allocation per hash call. The current `hashUint64` operates on a bare `uint64` with zero allocation. For hot paths (hash table lookups with integer keys), this adds GC pressure.

Strings are explicitly exempted from this escape — `abi.EscapeNonString` skips them — so symbol hashing (string-based) would be fine.

**3. The hash functions are 27 lines total.**

`values/hash.go` is 27 lines. Two functions. FNV-1a is well-understood, deterministic, and has no external dependency. Replacing it with `maphash.Comparable` would:

- Add a Go 1.24 minimum version requirement (currently Go 1.23)
- Require a `maphash.Seed` to be created and stored somewhere (per-Hashtable? global?)
- Not improve hash distribution in any way that matters for Wile's workloads (symbol tables and small hash tables, not millions-of-keys scenarios where FNV-1a's weaknesses matter)

**4. Doesn't help with `equal?`-based hash tables.**

Scheme's `equal?` operates on recursive structures (lists, vectors). Go's `comparable` constraint excludes these. Wile would still need custom hashing for any `equal?`-based hash table. `maphash.Comparable` only helps with `eq?`/`eqv?`-based tables — which are already the fast path.

**Verdict: Not worth adopting.** The benefit (delete 27 lines of hash code) doesn't justify the cost (Go 1.24 minimum, process-local seeds, heap escapes for non-strings, still need custom hashing for `equal?`). Revisit if/when Go 1.24 becomes the minimum version for other reasons.

### Future-Facing Notes

**If the `.Key ==` comparisons become a bottleneck**, the right optimization is not `unique.Handle` but rather trusting the existing interning invariant and switching internal comparisons from `.Key ==` to pointer `==`.

Wile already interns at parse boundaries via the two-gate design (see `plans/TOP_LEVEL_ENVIRONMENT.md` §Symbol Interning Gates):

```
Source text ──→ Parser.wrapSyntaxSymbol() ──→ env.InternSymbol() ──→ interned *Symbol
                     (gate 1)                                              │
                                                                    compiler, expander,
Runtime ──→ string->symbol primitive ──→ env.InternSymbol()         match — all receive
                  (gate 2)                                          already-interned symbols
```

Every symbol reaching the compiler, expander, and pattern matcher has already passed through one of these two gates. The internal `.Key ==` comparisons are a conservative choice — they work correctly regardless of whether the symbol is interned. The optimization path is:

1. **Close known gate violations** (see below) so the invariant holds universally
2. **Switch** internal comparisons from `.Key ==` to pointer `==` at sites shown in profiling
3. **Document** the stronger invariant: "all symbols in the system are interned; pointer comparison is sound"

This is a discipline change (trust the invariant), not an infrastructure change (new interning mechanism). It preserves per-VM isolation while getting O(1) comparison at every site.

**Known violations of the two-gate invariant** (symbols created via `values.NewSymbol` without interning):

| Location | What | Impact |
|----------|------|--------|
| ~~`values/thread.go` `StateSymbol()`~~ | ~~Returns fresh symbols each call~~ | **CLOSED** — now returns `SymbolThread*` singletons |
| ~~`values/mutex.go` `StateValue()`~~ | ~~Returns fresh symbols each call~~ | **CLOSED** — now returns `SymbolMutex*` singletons |
| ~~`internal/extensions/threads/prim_threads.go`~~ | ~~`NewSymbol("primordial")` each call~~ | **CLOSED** — now returns `values.SymbolPrimordial` |
| `ffi.go:858` | Struct field names as symbols in reflection | Used as dict keys; works via `EqualTo()` but lacks `eq?` identity |
| `value.go:89` public `NewSymbol()` | Embedding API returns uninterned symbols | May be intentional — Go callers may not have an env to intern against |
| `internal/syntax/syntax_symbol.go:49` | `NewSyntaxSymbol` creates uninterned underlying symbol | Acceptable if syntax values use `.Key ==` internally |

The thread/mutex state symbols are the most actionable — they should be package-level singletons (one `NewSymbol` at init, reused on every call). The public API (`value.go`) is a design question: should embedding callers be required to intern? The ffi and syntax cases are lower priority.

**If Go's minimum version advances to 1.24 for unrelated reasons**, reconsider `maphash.Comparable` specifically for `Symbol.HashCode()` (strings don't heap-escape). The other `Hashable` types (Integer, Byte, Boolean, Character) would still be better served by `hashUint64` to avoid heap allocation.

---
