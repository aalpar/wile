# Bibliography

Academic papers, specifications, and references used in the Wile Scheme implementation.

## Pedagogical Foundations

Widely known techniques — explanations for new contributors.

### Stack-Based Virtual Machines

Wile's VM uses a stack-based architecture descended from Landin's SECD machine. The evaluation stack holds intermediate results, a value register carries the most recent result, and continuation frames preserve state across calls.

- **Origin**: Peter J. Landin, "The Mechanical Evaluation of Expressions", The Computer Journal, Vol. 6, No. 4, 1964
- **DOI**: https://doi.org/10.1093/comjnl/6.4.308
- **Location**: `machine/machine_context.go`, `machine/stack.go`

### Tail Call Optimization

Tail calls reuse the caller's continuation frame instead of allocating a new one, enabling recursive procedures in tail position to run in constant stack space. Required by R7RS §3.5.

- **Origin**: Guy L. Steele Jr., "Debunking the 'Expensive Procedure Call' Myth", ACM Conference on AI and Programming Languages, 1977
- **Location**: `machine/compile_time_call_context.go`

### Peephole Optimization

Examines a small window of generated instructions and replaces inefficient patterns with shorter or faster equivalents. In Wile, the Push+BranchOnFalse+Pop sequence is replaced with a single BranchOnFalseValue that reads the value register directly.

- **Reference**: Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman, *Compilers: Principles, Techniques, and Tools*, 2nd edition, §8.9
- **ISBN**: 978-0-321-48681-3
- **Location**: `machine/operation_branch_on_false_value_offset_immediate.go`

### Constant Folding

Evaluates known expressions at compile time rather than runtime. When the test of an `if`-form is a compile-time literal, the entire form reduces to one branch.

- **Reference**: Aho et al., *Compilers*, §8.5
- **Location**: `machine/compile_validated.go`

### Object Pooling

Recycles short-lived allocations that follow an acquire/release lifecycle via Go's `sync.Pool`. Each non-tail call creates a continuation frame and eval stack; pooling avoids per-call heap allocations.

- **Location**: `machine/pool.go`

### Copy-on-Write

Shares the keys map between original and copy until a mutation forces a clone. Most copies are never mutated, so the clone cost is avoided entirely. The standard CoW technique from OS virtual memory (fork) applied to environment frames.

- **Location**: `environment/local_environment_frame.go`

### Flyweight Pattern / Value Caching

Pre-allocates a pool of frequently-used immutable objects and returns shared references instead of new allocations. Python caches small integers [-5, 256], Java caches [-128, 127]; Wile caches ASCII characters [0, 127] and integers [-32768, 32767].

- **Reference**: Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software*, Addison-Wesley, 1994
- **ISBN**: 978-0-201-63361-0
- **Location**: `values/character.go`, `values/integer.go`

### FNV-1a Hash Function

A non-cryptographic hash function chosen for simplicity and good distribution. The type seed byte ensures that values with identical content but different types (e.g., symbol "x" vs string "x") produce different hashes.

- **Origin**: Glenn Fowler, Landon Curt Noll, Kiem-Phong Vo
- **URL**: http://www.isthe.com/chongo/tech/comp/fnv/
- **Location**: `values/hash.go`

### Separate Chaining Hash Table

Collisions are resolved by storing all entries with the same hash in a linked list (here, a Go slice). O(1) amortized with a good hash function.

- **Reference**: Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein, *Introduction to Algorithms*, Ch. 11
- **ISBN**: 978-0-262-03384-8
- **Location**: `values/hashtable.go`

### String Interning

Ensures that structurally equal short strings share a single allocation, reducing memory use and enabling fast pointer comparison. The 64-byte threshold balances memory savings against the cost of the sync.Map lookup.

- **Location**: `values/string.go`

### Amortized Batch Checking

Amortizes the cost of a syscall-like check (context cancellation) over many cheap operations. The VM checks `ctx.Done()` every 1024 operations; the mask ensures the branch compiles to a single AND instruction.

- **Location**: `machine/machine_context.go`

### Structural Sharing

When a tree transformation leaves children unchanged, the original node is returned instead of allocating a new one. This is the core idea behind persistent data structures (Okasaki, 1998). Used in scope propagation and macro template expansion.

- **Reference**: Chris Okasaki, *Purely Functional Data Structures*, Cambridge University Press, 1998
- **ISBN**: 978-0-521-66350-2
- **Location**: `internal/syntax/scope_utils.go`, `machine/operation_syntax_rules_transform.go`

### Floyd's Cycle Detection (Tortoise-and-Hare)

Used in `values/pair.go` for `IsList()` to detect circular lists per R7RS §6.4. The algorithm uses two pointers advancing at different speeds through the list; if they meet, the structure is circular.

- **Origin**: Robert W. Floyd, "Nondeterministic Algorithms", Journal of the ACM, Vol. 14, No. 4, 1967
- **DOI**: https://doi.org/10.1145/321420.321422

### Hacker's Delight Overflow Detection (Warren 2012)

Integer overflow detection techniques used in `values/integer.go`. The overflow-detecting helpers (`addInt64`, `subInt64`, `mulInt64`, `negateInt64`) use XOR sign-bit tests for addition/subtraction overflow (§2-12, §2-13) and division-based verification for multiplication overflow (§2-12).

- **Book**: Henry S. Warren Jr., *Hacker's Delight*, 2nd edition, Addison-Wesley, 2012
- **ISBN**: 978-0-321-84268-8

## Research and Novel Techniques

Specific papers and less commonly known techniques.

### Binding as Sets of Scopes (Flatt 2016)

The foundation for Wile's hygienic macro system.

- **Paper**: Matthew Flatt, "Binding as Sets of Scopes", POPL 2016
- **URL**: https://www.cs.utah.edu/plt/scope-sets/
- **DOI**: https://doi.org/10.1145/2837614.2837620

This paper introduces the "sets of scopes" model for macro hygiene, which Wile uses for `syntax-rules` macro expansion. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes.

### Composable and Compilable Macros (Flatt 2002)

Phase-dependent binding: the same symbol can bind to different values at different phases (runtime, expand, compile). This is the theoretical basis for Wile's `PhaseRegistry` and phased import system.

- **Paper**: Matthew Flatt, "Composable and Compilable Macros: You Want It When?", ICFP 2002
- **DOI**: https://doi.org/10.1145/581478.581486
- **Location**: `environment/phase_registry.go`

### Adding Delimited and Composable Control to a Production Programming Environment (Flatt et al. 2007)

The basis for Wile's delimited continuation implementation: prompt tags, `call-with-continuation-prompt`, `abort-current-continuation`, and `call-with-composable-continuation`.

- **Paper**: Matthew Flatt, Gang Yu, Robert Bruce Findler, Matthias Felleisen, "Adding Delimited and Composable Control to a Production Programming Environment", ICFP 2007
- **DOI**: https://doi.org/10.1145/1291151.1291178

### The Theory and Practice of First-Class Prompts (Felleisen 1988)

Original formalization of continuation prompts and aborts.

- **Paper**: Matthias Felleisen, "The Theory and Practice of First-Class Prompts", POPL 1988
- **DOI**: https://doi.org/10.1145/73560.73576

### Abstracting Control (Danvy & Filinski 1990)

Introduces shift/reset as composable delimited control operators, the theoretical foundation for composable continuations.

- **Paper**: Olivier Danvy, Andrzej Filinski, "Abstracting Control", LFP 1990
- **DOI**: https://doi.org/10.1145/91556.91622

### Optimistic Bisimilarity for Structural Equality

Used in `values/utils.go` for `EqualTo()` on compound types (Pair, Vector, ArrayList). When a pointer pair is re-encountered during recursive comparison, it returns true (optimistic assumption). This is the same technique used by Chez Scheme and Racket for `equal?` on circular structures per R7RS §6.1.

### Split Value Register

A custom optimization that separates single-value and multi-value return paths. Nearly all Scheme operations produce one value; the `singleValue` field avoids allocating a `[]values.Value` slice for the common case.

- **Location**: `machine/vm_state.go`

### Two-Pass Datum Label Output

Implements R7RS §2.4 datum label notation for shared/circular structures. Pass 1 (`findShared`) traverses the value graph to identify multiply-referenced objects; pass 2 (`write`) emits `#n=` definitions on first encounter and `#n#` references thereafter.

- **Location**: `values/scheme_writer.go`

### Reflection-Based FFI Bridging

Pre-computes argument and return converters at registration time using Go's `reflect` package. Each call uses the cached converters to translate between Scheme values and Go types, avoiding per-call reflection overhead.

- **Location**: `ffi.go`

## Language Specifications

### R7RS-small (Revised⁷ Report on the Algorithmic Language Scheme)

The primary language specification that Wile implements.

- **PDF**: https://small.r7rs.org/attachment/r7rs.pdf
- **HTML (Corrected)**: https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html
- **Homepage**: https://small.r7rs.org/
- **R7RS-large Wiki**: https://codeberg.org/scheme/r7rs/wiki

### R5RS (Revised⁵ Report on the Algorithmic Language Scheme)

Earlier Scheme standard, referenced for `scheme-report-environment` and `null-environment`.

- **URL**: https://www.schemers.org/Documents/Standards/R5RS/

### IEEE 754: Floating-Point Arithmetic

Standard for floating-point representation used by `Float` type.

- **Standard**: IEEE 754-2019 (ISO/IEC/IEEE 60559:2020)

## SRFIs (Scheme Requests for Implementation)

### SRFI-1: List Library

Canonical definitions for list processing procedures including `fold`. Wile's implementation in `lib/srfi/1/` is from Chibi-Scheme.

- **URL**: https://srfi.schemers.org/srfi-1/srfi-1.html

### SRFI-9: Defining Record Types

Record type definitions, integrated into R7RS as `define-record-type`.

- **URL**: https://srfi.schemers.org/srfi-9/srfi-9.html

### SRFI-18: Multithreading Support

Threading primitives implemented in Wile: threads, mutexes, condition variables, and time objects.

- **URL**: https://srfi.schemers.org/srfi-18/srfi-18.html

### SRFI-64: A Scheme API for Test Suites

Test framework specification. Wile's `(chibi test)` library is a portable subset of SRFI-64, providing `test-begin`, `test-end`, and `test`.

- **URL**: https://srfi.schemers.org/srfi-64/srfi-64.html

### SRFI-141: Integer Division

Specification for integer division operations (`quotient`, `remainder`, `modulo`, `floor/`, `truncate/`).

- **URL**: https://srfi.schemers.org/srfi-141/srfi-141.html
- **Wiki**: https://small.r7rs.org/wiki/DivisionRiastradh/

### SRFI-170: POSIX API (Planned)

Comprehensive POSIX API for file system operations.

- **URL**: https://srfi.schemers.org/srfi-170/srfi-170.html

### SRFI-198: Foreign Object Error Handling (Planned)

Error handling for foreign function interfaces.

- **URL**: https://srfi.schemers.org/srfi-198/srfi-198.html

## Unicode Standards

### Unicode Case Folding

Referenced for `char-foldcase` and `string-foldcase` implementations.

- **CaseFolding.txt**: https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
- **SpecialCasing.txt**: https://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt

### UAX #29: Unicode Text Segmentation

Unicode standard annex for text boundary analysis.

- **URL**: https://unicode.org/reports/tr29/

## Tutorials and Learning Resources

### An Introduction to Scheme and its Implementation

Comprehensive Scheme tutorial covering implementation concepts.

- **URL**: https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v14/schintro_toc.html

## Related Systems

### Chez Scheme

Implementation behavior reference for zero-dominance in multiplication (`values/float.go`, `values/integer.go`, `values/big_integer.go`), optimistic bisimilarity for `equal?` (`values/utils.go`), and pointer-based equality for syntax objects (`internal/syntax/`).

- **URL**: https://cisco.github.io/ChezScheme/
- **Source**: https://github.com/cisco/ChezScheme

### Racket

Implementation model for delimited continuations (prompt tags, composable continuations), phase numbering conventions, and phased imports. Also referenced for `@`-expression reader syntax (planned feature) and syntax object equality semantics.

- **Homepage**: https://racket-lang.org/
- **Scribble Reader**: https://docs.racket-lang.org/scribble/reader.html
- **At-expressions**: https://docs.racket-lang.org/at-exp/index.html

### Chibi-Scheme (Alex Shinn)

Source of portable Scheme library code used in Wile. The `lib/chibi/` directory contains Chibi-Scheme's test framework, diff library, optional argument macros, and ANSI terminal library. The `lib/srfi/1/` directory contains Chibi-Scheme's SRFI-1 list library implementation split into functional modules.

- **Homepage**: https://synthcode.com/wiki/chibi-scheme
- **Source**: https://github.com/ashinn/chibi-scheme
- **License**: BSD

### Schelog (Dorai Sitaram)

Prolog-in-Scheme embedding. Wile runs the unmodified upstream `schelog.scm` as an integration test for `call/cc`, `syntax-rules`, and mutable state working together on third-party code. Located in `examples/logic/schelog/`.

- **Documentation**: https://ds26gte.github.io/schelog/
- **Source**: https://github.com/ds26gte/schelog
- **Book**: Dorai Sitaram, *Teach Yourself Scheme in Fixnum Days*, 1998-2024

### Sterling & Shapiro, "The Art of Prolog"

Source of logic programming examples used in the schelog test suite: map coloring (p. 212), puzzle solver and games (p. 214), and the Zebra puzzle (Exercise 14.1, p. 217-8).

- **Book**: Leon Sterling, Ehud Shapiro, *The Art of Prolog*, 2nd edition, MIT Press, 1994
- **ISBN**: 978-0-262-19338-2

### Go x/text Package

Used for Unicode case mapping operations.

- **Documentation**: https://pkg.go.dev/golang.org/x/text/cases

## Citation Format

When citing R7RS sections in code comments, use the format `R7RS §X.Y.Z`:

```go
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
```

Key R7RS sections:

| Section | Topic |
|---------|-------|
| §4.1-4.3 | Expressions, syntax |
| §5.1-5.5 | Program structure, definitions |
| §6.1 | Equivalence predicates |
| §6.2 | Numbers (tower, exactness, operations) |
| §6.3 | Booleans, pairs, lists, symbols, characters, strings, vectors |
| §6.4 | Bytevectors |
| §6.5 | Control features |
| §6.6 | Exceptions |
| §6.7-6.13 | Environments, I/O, system interface |
