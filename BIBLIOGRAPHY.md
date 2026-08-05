# Bibliography

Academic papers, specifications, and references used in the Wile Scheme implementation.

## Foundational Theory

The formal foundations that Scheme inherits and Wile implements.

### Lambda Calculus (Church 1936)

The formal system underlying Scheme. Lambda abstraction, beta reduction (function application), and variable binding are the core mechanisms. Every Scheme procedure is a lambda expression; every function call is beta reduction made operational.

- **Paper**: Alonzo Church, "An Unsolvable Problem of Elementary Number Theory", American Journal of Mathematics, Vol. 58, No. 2, 1936
- **DOI**: https://doi.org/10.2307/2371045
- **Location**: `pkg/machine/machine_closure.go` (closure = lambda abstraction), `pkg/machine/machine_context.go` (Apply = beta reduction)

### McCarthy, "Recursive Functions of Symbolic Expressions" (1960)

Introduces Lisp, the `eval` function, and the read-eval-print loop. McCarthy's key insight — that a language's interpreter can be written in the language itself — established the foundation for all Lisp-family languages. Wile's REPL descends directly from this structure, though it compiles to bytecode rather than tree-walking.

- **Paper**: John McCarthy, "Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I", Communications of the ACM, Vol. 3, No. 4, 1960
- **DOI**: https://doi.org/10.1145/367177.367199
- **Location**: `pkg/repl/repl.go`, `pkg/wile/engine.go` (Eval method)

### Strachey, "Fundamental Concepts in Programming Languages" (1967)

Introduces the classification of values as first-class or second-class. A first-class value can be passed as argument, returned from a function, assigned to a variable, and tested for equality. Scheme's defining choice is that procedures are first-class. Wile's `Callable` interface extends `Value`, making closures indistinguishable from other values in the type system.

- **Paper**: Christopher Strachey, "Fundamental Concepts in Programming Languages", Higher-Order and Symbolic Computation, Vol. 13, No. 1/2, 2000 (originally lecture notes from the 1967 NATO Summer School in Copenhagen)
- **DOI**: https://doi.org/10.1023/A:1010000313106
- **Location**: `pkg/machine/closure.go`, `pkg/values/values.go` (Callable interface)

### Reynolds, "Definitional Interpreters for Higher-Order Programming Languages" (1972)

Introduces continuation-passing style (CPS) as a technique for defining language semantics. Continuations represent "the rest of the computation" — the key insight that Scheme later made first-class via `call/cc`. Wile's explicit `MachineContinuation` linked list is a defunctionalized continuation in the sense of Reynolds.

- **Paper**: John C. Reynolds, "Definitional Interpreters for Higher-Order Programming Languages", ACM Annual Conference 1972, reprinted in Higher-Order and Symbolic Computation, Vol. 11, No. 4, 1998
- **DOI**: https://doi.org/10.1023/A:1010027404223 (1998 reprint)
- **Location**: `pkg/machine/machine_continuation.go`

### Sussman & Steele, "Scheme: An Interpreter for Extended Lambda Calculus" (1975)

The paper that introduced Scheme. Established that actors and closures are the same concept, that lexical scoping is the correct default, and that a practical language can be built directly on the lambda calculus. The name "Scheme" comes from this paper.

- **Paper**: Gerald Jay Sussman, Guy Lewis Steele Jr., "Scheme: An Interpreter for Extended Lambda Calculus", AI Memo 349, MIT, 1975
- **URL**: https://dspace.mit.edu/handle/1721.1/5794

### The Lambda Papers (Steele & Sussman, 1975–1980)

A series of MIT AI Memos establishing the theoretical and practical foundations of Scheme. Key results: tail calls are GOTOs; `let` is lambda application; imperative and declarative programming are both lambda. Wile's bootstrap macros embody "Lambda: The Ultimate Imperative" directly: `cond`, `case`, `when`, `unless`, `and`, `or`, and `do` expand to `if`/`lambda` in `pkg/registry/core/bootstrap_macros.scm`.

The identity `let` = lambda application is retained as *semantics*, not as *compilation strategy*: `let` is a core validated form compiled to direct slot stores, not a macro. The equivalence is how R7RS §7.3 specifies the form; expanding it at compile time would allocate a closure and an application frame per binding group. See `docs/compiler/core-let.md`.

- AIM-349: "Scheme: An Interpreter for Extended Lambda Calculus" (1975)
- AIM-353: "Lambda: The Ultimate Imperative" (1976)
- AIM-379: "Lambda: The Ultimate Declarative" (1976)
- AIM-443: "Lambda: The Ultimate GOTO" (1977) — already cited for TCO
- AIM-514: "The Art of the Interpreter" (1978)
- **URL**: https://dspace.mit.edu/handle/1721.1/6091 (collection)
- **Location**: `pkg/registry/core/bootstrap_macros.scm` (derived forms as lambda/if), `pkg/machine/compilation/` (core forms compiled directly)

## Pedagogical Foundations

Widely known techniques — explanations for new contributors.

### Stack-Based Virtual Machines

Wile's VM uses a stack-based architecture descended from Landin's SECD machine. The evaluation stack holds intermediate results, a value register carries the most recent result, and continuation frames preserve state across calls.

- **Origin**: Peter J. Landin, "The Mechanical Evaluation of Expressions", The Computer Journal, Vol. 6, No. 4, 1964
- **DOI**: https://doi.org/10.1093/comjnl/6.4.308
- **Location**: `pkg/machine/machine_context.go`, `pkg/machine/stack.go`

### CESK Abstract Machine (Felleisen & Friedman 1987)

The abstract machine model underlying Wile's VM. The `vmState` holds Control (template + pc), Environment (env), Store (evals stack), and Kontinuation (cont chain). Unlike Landin's SECD (which uses an implicit dump for continuations), the CESK machine uses explicit, first-class continuation values. This directly enables `call/cc` and delimited continuations without special dump-manipulation primitives. The key evidence: `Apply()` replaces the current state in-place (CEK transition style), `SaveContinuation()` reifies state as a linked-list node, not a dump push.

- **Paper**: Matthias Felleisen, Daniel P. Friedman, "Control Operators, the SECD-machine, and the λ-calculus", in M. Wirsing (ed.), *Formal Description of Programming Concepts III*, Elsevier, 1987
- **Also**: Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, *Semantics Engineering with PLT Redex*, MIT Press, 2009, Chapter 4
- **ISBN** (Redex): 978-0-262-06274-6
- **Location**: `pkg/machine/vm_state.go`, `pkg/machine/machine_context.go` (Run, Apply, SaveContinuation, Restore)

### Tail Call Optimization

Tail calls reuse the caller's continuation frame instead of allocating a new one, enabling recursive procedures in tail position to run in constant stack space. Required by R7RS §3.5.

- **Origin**: Guy L. Steele Jr., "Debunking the 'Expensive Procedure Call' Myth", ACM Conference on AI and Programming Languages, 1977
- **Location**: `pkg/machine/compilation/compile_time_call_context.go`

### Proper Tail Recursion (Clinger 1998)

Formalizes what "proper tail recursion" means: an implementation satisfies it if and only if it evaluates tail-recursive programs in bounded space. Wile satisfies this by omitting `SaveContinuation` for tail calls — the continuation chain does not grow. This provides the formal definition against which Steele's 1977 optimization claim can be verified.

- **Paper**: William D. Clinger, "Proper Tail Recursion and Space Efficiency", PLDI 1998
- **DOI**: https://doi.org/10.1145/277650.277719
- **Location**: `pkg/machine/compilation/compile_time_call_context.go`, `pkg/machine/compilation/compile_validated.go`

### De Bruijn Indices / Lexical Addressing (de Bruijn 1972)

Local variable addressing by `(slot, depth)` pair. Eliminates runtime name lookup by resolving variable references to numeric coordinates at compile time. The depth counts enclosing scopes (parent chain hops); the slot indexes within a scope's binding array. This is a two-dimensional generalization of de Bruijn's nameless representation.

- **Paper**: N.G. de Bruijn, "Lambda calculus notation with nameless dummies, a tool for automatic formula manipulation, with application to the Church-Rosser theorem", *Indagationes Mathematicae*, 34:381-392, 1972
- **DOI**: https://doi.org/10.1016/1385-7258(72)90034-0
- **Also**: Harold Abelson, Gerald Jay Sussman, *Structure and Interpretation of Computer Programs*, 2nd edition, MIT Press, 1996, §5.5.6 "Lexical Addressing"
- **Also**: R. Kent Dybvig, *Three Implementation Models for Scheme*, PhD Dissertation, University of North Carolina, 1987
- **Location**: `pkg/machine/instruction.go` (EncodeLocalIndex, DecodeLocalIndex), `pkg/environment/environment_frame.go` (resolveLocal, GetLocalBindingBySlotDepth)

### Linked Closure Representation (Cardelli 1983)

Closures capture a pointer to the enclosing environment frame rather than copying free variables into a flat vector. Free variable access traverses parent pointers (O(depth)). This representation naturally supports mutable free variables (R7RS `set!`) without requiring explicit boxing. The trade-off: O(1) closure creation, O(depth) variable access — the natural complement to de Bruijn lexical addressing.

- **Paper**: Luca Cardelli, "The Functional Abstract Machine", *Polymorphism*, Vol. 1, No. 1, 1983
- **Also**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992, Chapter 10
- **ISBN** (Appel): 978-0-521-41695-5
- **Location**: `pkg/machine/operations_closure.go`, `pkg/machine/machine_closure.go`

### Direct-Style Compilation (Dybvig 1987)

Wile compiles Scheme directly to stack-machine bytecode without intermediate CPS or A-normal form conversion. The eval stack manages intermediate results that CPS would name with continuation lambdas or ANF with `let`-bindings. Tail position is tracked explicitly via `CompileTimeCallContext.inTail` during compilation.

- **Reference**: R. Kent Dybvig, *Three Implementation Models for Scheme*, PhD Dissertation, University of North Carolina, 1987, Chapter 3
- **Contrast**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992 (CPS approach)
- **Contrast**: Cormac Flanagan, Amr Sabry, Bruce F. Duba, Matthias Felleisen, "The Essence of Compiling with Continuations", PLDI 1993 (A-normal form)
- **Location**: `pkg/machine/compilation/compile_validated.go` (compileValidatedCall), `pkg/machine/compilation/compile_time_call_context.go`

### Environment Escape Analysis (Appel 1992)

Static bytecode scan to determine whether a closure's environment can escape its call. When it cannot (no continuation capture, no nested closure creation), the environment frame is reused in place rather than copied, eliminating allocation. **Removed in PR #561** — the optimization was unsafe under concurrent SRFI-18 thread invocation. Apply now always copies the env frame.

- **Reference**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992, §10.3
- **Location (historical)**: `pkg/machine/native_template.go`, `pkg/machine/machine_context_apply.go`

### Lexical Scoping (Landin 1966, Strachey 1967)

Closures capture their lexical (definition-site) environment, not the dynamic (call-site) environment. This is the foundation of Scheme's scoping semantics (R7RS §1.1) and enables reasoning about variable binding from program text alone.

- **Paper**: Peter J. Landin, "The Next 700 Programming Languages", Communications of the ACM, Vol. 9, No. 3, 1966
- **Also**: Christopher Strachey, "Fundamental Concepts in Programming Languages", 1967 (reprinted 2000)
- **DOI** (Strachey reprint): https://doi.org/10.1023/A:1010000313106
- **Location**: `pkg/environment/environment_frame.go` (NewApplyFrame), `pkg/machine/machine_closure.go`

### Environment Model (Landin 1964)

Wile uses Landin's environment model for variable resolution: closures pair a code body with a captured environment frame, and variable lookup traverses the frame chain at runtime. This is opposed to the substitution model (Church 1941), where beta-reduction physically replaces formal parameters with arguments in the body. The environment model enables O(1) closure creation (just capture the current frame pointer) at the cost of O(depth) variable lookup.

- **Paper**: Peter J. Landin, "The Mechanical Evaluation of Expressions", 1964
- **Location**: `pkg/environment/environment_frame.go` (frame chain), `pkg/machine/machine_closure.go` (closure = template + env)

### Hash Consing / Symbol Interning (Goto 1974)

Symbol interning ensures that structurally equal symbols share a single pointer, enabling O(1) `eq?` comparison. Wile previously interned symbols per-Namespace but removed symbol interning in favor of string-key comparison via `values.EqIdentity` (symbols compare by `.Key` field). The historical references are retained for context.

- **Origin**: Eiichi Goto, "Monocopy and Associative Algorithms in an Extended Lisp", Technical Report 74-03, University of Tokyo, 1974
- **Earlier**: Andrei P. Ershov, "On Programming of Arithmetic Operations", Communications of the ACM, Vol. 1, No. 8, 1958
- **DOI** (Ershov): https://doi.org/10.1145/368892.368907
- **Location (historical)**: `pkg/environment/namespace.go` (InternSymbol, removed — symbols now compared by string key via `values.EqIdentity`)

### Superinstruction Formation (Ertl & Gregg 2003)

Fusing adjacent bytecode instructions into single superinstructions to reduce dispatch overhead. Each fusion eliminates one opcode decode and switch dispatch, which is the dominant cost in switch-dispatch interpreters. Wile fuses Load+Push → PushX, Pull+Apply → PullApply, and multi-instruction call sequences → CallForeignCached.

- **Paper**: M. Anton Ertl, David Gregg, "The Structure and Performance of Efficient Interpreters", *Journal of Instruction-Level Parallelism*, Vol. 5, pp. 1-25, 2003
- **URL**: http://www.jilp.org/vol5/v5paper12.pdf
- **Also**: M. Anton Ertl, David Gregg, "Optimizing Indirect Branch Prediction Accuracy in Virtual Machine Interpreters", PLDI 2003
- **DOI**: https://doi.org/10.1145/781131.781162
- **Location**: `pkg/machine/peephole.go` (fuseLoadPush, fusePullApply, fuseCallForeignCached)

### Inline Caching (Deutsch & Schiffman 1984)

`CallForeignCached` is an inline cache in the classical shape: cache, guard, deoptimize. The peephole optimizer resolves a global primitive reference to its `*environment.Binding` cell at compile time and stores the pointer in `NativeTemplate.cachedBindings`; the call site then reads the cell with no name lookup and no hash. The guard is the type assertion to `*ForeignClosure`; a `set!` that rebinds the primitive fails the guard and falls to `callForeignCachedReassigned`. The cache holds the *cell*, not the *value*, so mutation stays visible — which is why the deopt path is a slow path, not an invalidation protocol. Monomorphic only: Wile has no polymorphic call-site dispatch to cache.

- **Paper**: L. Peter Deutsch, Allan M. Schiffman, "Efficient Implementation of the Smalltalk-80 System", POPL 1984
- **DOI**: https://doi.org/10.1145/800017.800542
- **Contrast**: Urs Hölzle, Craig Chambers, David Ungar, "Optimizing Dynamically-Typed Object-Oriented Languages With Polymorphic Inline Caches", ECOOP 1991 (PICs — not needed here; call sites are monomorphic by construction)
- **Location**: `pkg/machine/call_foreign_cached.go`, `pkg/machine/native_template.go` (cachedBindings), `pkg/machine/peephole.go` (fuseCallForeignCached)

### Peephole Optimization

Examines a small window of generated instructions and replaces inefficient patterns with shorter or faster equivalents. In Wile, the Push+BranchOnFalse+Pop sequence is replaced with a single BranchOnFalseValue that reads the value register directly.

- **Reference**: Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman, *Compilers: Principles, Techniques, and Tools*, 2nd edition, §8.9
- **ISBN**: 978-0-321-48681-3
- **Location**: `pkg/machine/operations_control.go`

### Constant Folding

Evaluates known expressions at compile time rather than runtime. When the test of an `if`-form is a compile-time literal, the entire form reduces to one branch.

- **Reference**: Aho et al., *Compilers*, §8.5
- **Location**: `pkg/machine/compilation/compile_validated.go`

### Object Pooling

Recycles short-lived allocations that follow an acquire/release lifecycle via Go's `sync.Pool`. Each non-tail call creates a continuation frame and eval stack; pooling avoids per-call heap allocations.

- **Location**: `pkg/machine/pool.go`

### Copy-on-Write

Shares the keys map between original and copy until a mutation forces a clone. Most copies are never mutated, so the clone cost is avoided entirely. The standard CoW technique from OS virtual memory (fork) applied to environment frames.

- **Location**: `pkg/environment/local_environment_frame.go`

### Flyweight Pattern / Value Caching

Pre-allocates a pool of frequently-used immutable objects and returns shared references instead of new allocations. Python caches small integers [-5, 256], Java caches [-128, 127]; Wile caches ASCII characters [0, 127] and integers [-32768, 32767].

- **Reference**: Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software*, Addison-Wesley, 1994
- **ISBN**: 978-0-201-63361-0
- **Location**: `pkg/values/character.go`, `pkg/values/integer.go`

### FNV-1a Hash Function

A non-cryptographic hash function chosen for simplicity and good distribution. The type seed byte ensures that values with identical content but different types (e.g., symbol "x" vs string "x") produce different hashes.

- **Origin**: Glenn Fowler, Landon Curt Noll, Kiem-Phong Vo
- **URL**: http://www.isthe.com/chongo/tech/comp/fnv/
- **Location**: `pkg/values/hash.go`

### Separate Chaining Hash Table

Collisions are resolved by storing all entries with the same hash in a linked list (here, a Go slice). O(1) amortized with a good hash function.

- **Reference**: Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein, *Introduction to Algorithms*, Ch. 11
- **ISBN**: 978-0-262-03384-8
- **Location**: `pkg/values/hashtable.go`

### String Interning (removed)

Structurally equal short strings once shared a single allocation, giving pointer-equality comparison for free. **Removed in PR #529**: R7RS strings are mutable, and `eq?` on separately-allocated strings is unspecified, so the sharing bought a pointer comparison that no conforming program may rely on while adding a `sync.Map` lookup to every string construction. `NewString` now always allocates.

- **Location (historical)**: `pkg/values/string.go` (interning removed; `NewString` allocates)

### Amortized Batch Checking

Amortizes the cost of a syscall-like check (context cancellation) over many cheap operations. The VM checks `ctx.Done()` every 1024 operations; the mask ensures the branch compiles to a single AND instruction.

- **Location**: `pkg/machine/machine_context.go`

### Structural Sharing

When a tree transformation leaves children unchanged, the original node is returned instead of allocating a new one. This is the core idea behind persistent data structures (Okasaki, 1998). Used in scope propagation and macro template expansion.

- **Reference**: Chris Okasaki, *Purely Functional Data Structures*, Cambridge University Press, 1998
- **ISBN**: 978-0-521-66350-2
- **Location**: `pkg/syntax/scope_utils.go`, `pkg/machine/compilation/operation_syntax_rules_transform.go`

### Floyd's Cycle Detection (Tortoise-and-Hare)

Used in `pkg/values/pair.go` for `IsList()` to detect circular lists per R7RS §6.4. The algorithm uses two pointers advancing at different speeds through the list; if they meet, the structure is circular.

- **Origin**: Robert W. Floyd, "Nondeterministic Algorithms", Journal of the ACM, Vol. 14, No. 4, 1967
- **DOI**: https://doi.org/10.1145/321420.321422

### Hacker's Delight Overflow Detection (Warren 2012)

Integer overflow detection techniques used in `pkg/values/integer.go`. The overflow-detecting helpers (`addInt64`, `subInt64`, `mulInt64`, `negateInt64`) use XOR sign-bit tests for addition/subtraction overflow (§2-12, §2-13) and division-based verification for multiplication overflow (§2-12).

- **Book**: Henry S. Warren Jr., *Hacker's Delight*, 2nd edition, Addison-Wesley, 2012
- **ISBN**: 978-0-321-84268-8

### Shortest Round-Trip Float Printing (Steele & White 1990; Burger & Dybvig 1996)

R7RS §6.2.6 requires `number->string` to produce a representation that reads back as the same inexact number, with no more digits than necessary. Wile does not implement the digit-generation algorithm: `Float.String()` calls `strconv.FormatFloat(f, 'f', -1, 64)`, and Go's `strconv` supplies the shortest-round-trip guarantee (Ryū, which subsumes Dragon4/Grisu). The citation records what the guarantee *is* and where it comes from, not code Wile owns.

- **Paper**: Guy L. Steele Jr., Jon L. White, "How to Print Floating-Point Numbers Accurately", PLDI 1990 (Dragon4)
- **DOI**: https://doi.org/10.1145/93542.93559
- **Paper**: Robert G. Burger, R. Kent Dybvig, "Printing Floating-Point Numbers Quickly and Accurately", PLDI 1996
- **DOI**: https://doi.org/10.1145/231379.231397
- **Paper**: Ulf Adams, "Ryū: Fast Float-to-String Conversion", PLDI 2018 (what Go's strconv implements)
- **DOI**: https://doi.org/10.1145/3192366.3192369
- **Location**: `pkg/values/float.go` (String, delegating to strconv)

### Arbitrary-Precision Arithmetic (Knuth, TAOCP Vol. 2)

The exact half of the numeric tower above `int64` — `BigInteger`, `Rational`, `BigFloat`, `BigComplex` — delegates to Go's `math/big`, which implements the classical algorithms (schoolbook and Karatsuba multiplication, Knuth Algorithm D division, binary GCD). Wile owns the *promotion* policy (when to leave `int64`), not the arithmetic.

- **Reference**: Donald E. Knuth, *The Art of Computer Programming, Vol. 2: Seminumerical Algorithms*, 3rd edition, Addison-Wesley, 1997, §4.3 (ISBN 978-0-201-89684-8)
- **Location**: `pkg/values/big_integer.go`, `pkg/values/big_float.go`, `pkg/values/big_complex.go`, `pkg/values/rational.go`

## Research and Novel Techniques

Specific papers and less commonly known techniques.

### Binding as Sets of Scopes (Flatt 2016)

The foundation for Wile's hygienic macro system.

- **Paper**: Matthew Flatt, "Binding as Sets of Scopes", POPL 2016
- **URL**: https://www.cs.utah.edu/plt/scope-sets/
- **DOI**: https://doi.org/10.1145/2837614.2837620

This paper introduces the "sets of scopes" model for macro hygiene, which Wile uses for `syntax-rules` macro expansion. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes. Among multiple compatible bindings, the one with the largest scope set is selected (lattice maximality, §3.2). The scope sets form a bounded join-semilattice under the subset partial order, with the empty set as bottom element. `FlipScopeInSet` is symmetric difference (XOR in the Boolean ring Z/2Z^S), used by `syntax-local-introduce`.

### Composable and Compilable Macros (Flatt 2002)

Phase-dependent binding: the same symbol can bind to different values at different phases (runtime, expand, compile). This is the theoretical basis for Wile's `PhaseRegistry` and phased import system.

- **Paper**: Matthew Flatt, "Composable and Compilable Macros: You Want It When?", ICFP 2002
- **DOI**: https://doi.org/10.1145/581478.581486
- **Location**: `pkg/environment/phase_registry.go`

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

Used in `pkg/values/utils.go` for `EqualTo()` on compound types (Pair, Vector). When a pointer pair is re-encountered during recursive comparison, it returns true (optimistic assumption). This is the same technique used by Chez Scheme and Racket for `equal?` on circular structures per R7RS §6.1. The formal basis is bisimulation equivalence — see "Bisimulation Equivalence for equal?" entry below.

### Split Value Register

A custom optimization that separates single-value and multi-value return paths. Nearly all Scheme operations produce one value; the `singleValue` field avoids allocating a `[]values.Value` slice for the common case.

- **Location**: `pkg/machine/vm_state.go`

### Two-Pass Datum Label Output

Implements R7RS §2.4 datum label notation for shared/circular structures. Pass 1 (`findShared`) traverses the value graph to identify multiply-referenced objects; pass 2 (`write`) emits `#n=` definitions on first encounter and `#n#` references thereafter.

- **Location**: `pkg/values/scheme_writer.go`

### Reflection-Based FFI Bridging

Pre-computes argument and return converters at registration time using Go's `reflect` package. Each call uses the cached converters to translate between Scheme values and Go types, avoiding per-call reflection overhead. This is a form of partial evaluation (Futamura 1971): the bridging logic is specialized at registration time against the known function type, producing cached converters that avoid per-call reflection.

- **Paper**: Yoshihiko Futamura, "Partial Evaluation of Computation Process — An Approach to a Compiler-Compiler", Systems, Computers, Controls, Vol. 2, No. 5, 1971. Reprinted in Higher-Order and Symbolic Computation, Vol. 12, No. 4, 1999
- **DOI**: https://doi.org/10.1023/A:1010095604496 (reprint)
- **Location**: `pkg/wile/ffi.go`

### Hygienic Macro Expansion (Kohlbecker et al. 1986)

The original formulation of hygienic macro expansion: macro-introduced bindings must not inadvertently capture identifiers from the macro use site. Wile's intro scope mechanism (a fresh scope added to all expansion output) implements this property. Flatt's scope sets generalize this to arbitrary nesting.

- **Paper**: Eugene Kohlbecker, Daniel P. Friedman, Matthias Felleisen, Bruce Duba, "Hygienic Macro Expansion", LFP 1986
- **DOI**: https://doi.org/10.1145/319838.319859
- **Location**: `pkg/machine/compilation/operation_syntax_rules_transform.go` (introScope)

### Referential Transparency for Macros (Clinger & Rees 1991)

Extends hygiene with the dual property: free identifiers in a macro template must resolve to their definition-site bindings, not the use-site bindings. Wile implements this via `ResolvedBinding` on `SyntaxSymbol`, which captures the definition-time `GlobalIndex` for cross-library macro hygiene. Combined with the intro scope mechanism, this completes the two-sided hygiene contract.

- **Paper**: William Clinger, Jonathan Rees, "Macros That Work", POPL 1991
- **DOI**: https://doi.org/10.1145/99583.99607
- **Location**: `pkg/syntax/syntax_symbol.go` (ResolvedBinding), `pkg/machine/compilation/compile_syntax_rules.go` (freeIds)

### Explicit Renaming Macros (Clinger 1991)

The procedural escape hatch beside `syntax-rules`. An `er-macro-transformer` receives the form plus a `rename` procedure (which maps an identifier into the macro's definition environment) and a `compare` procedure (which tests two identifiers for binding equivalence); hygiene becomes something the macro author invokes explicitly rather than something the pattern language guarantees. Wile implements this as a transformer kind alongside `syntax-rules`, so the same expander drives both.

- **Paper**: William Clinger, "Hygienic Macros Through Explicit Renaming", ACM SIGPLAN Lisp Pointers, Vol. IV, No. 4, 1991
- **DOI**: https://doi.org/10.1145/1317265.1317269
- **Location**: `pkg/machine/compilation/compile_er_macro.go` (TransformerERMacro), `pkg/machine/compilation/compile_transformer.go`

### Syntax Objects (Dybvig, Hieb & Bruggeman 1993)

Syntax objects are AST nodes decorated with binding information. Each parsed form is wrapped in a `SyntaxValue` carrying source location, scope sets, and macro expansion origin. The representation originates with Dybvig, Hieb, and Bruggeman, who introduced the concept for Chez Scheme's macro system. Wile's syntax objects are immutable: scope operations return new objects (persistent data structure discipline).

- **Paper**: R. Kent Dybvig, Robert Hieb, Carl Bruggeman, "Syntactic Abstraction in Scheme", Lisp and Symbolic Computation, Vol. 5, No. 4, 1993
- **DOI**: https://doi.org/10.1007/BF01806308
- **Location**: `pkg/syntax/syntax_value.go`, `pkg/syntax/syntax_symbol.go`, `pkg/syntax/source_context.go`

### Alpha-Equivalence and the Variable Convention (Barendregt 1984)

Wile's scope sets address the same problem as Barendregt's variable convention (choosing bound variable names to avoid capture). Where alpha-conversion renames variables, scope sets tag identifiers with their binding context. Two identifiers with the same name but different scope sets are effectively alpha-inequivalent. The bidirectional subset check in `scopesCompatibleForSubstitution` is the scope-set analog of alpha-equivalence.

- **Book**: Henk P. Barendregt, *The Lambda Calculus: Its Syntax and Semantics*, revised edition, Studies in Logic, Vol. 103, North-Holland, 1984
- **ISBN**: 978-0-444-87508-2
- **Location**: `pkg/syntax/syntax_value.go` (Scope type), `pkg/internal/match/syntax_expand.go` (scopesCompatibleForSubstitution)

### Reflective Tower (Smith 1984)

Wile's phase hierarchy (runtime/expand/compile) is a finite, statically-determined form of Smith's reflective tower. Phase 0 is the object level (program execution), phase 1 is the meta level (macros operating on program text), and phase 2 is the meta-meta level (syntax compilers operating on macro definitions). Unlike Smith's infinite tower in 3-Lisp, Wile's tower is finite and does not support arbitrary reification.

- **Paper**: Brian Cantwell Smith, "Reflection and Semantics in Lisp", POPL 1984
- **DOI**: https://doi.org/10.1145/800017.800513
- **Location**: `pkg/environment/phase_registry.go`

### Pattern Matching Compilation (Augustsson 1985)

Wile's pattern matching engine compiles R7RS `syntax-rules` patterns into bytecode at macro definition time, then executes the bytecode at each invocation. The compilation strategy produces a deterministic tree automaton that traverses the input in a fixed order. This is one-directional term matching (the input is ground, the pattern has holes), not bidirectional unification. Ellipsis patterns generate loop structures (SkipIfEmpty/Jump), making the automaton a form of regular tree pattern matcher with Kleene star.

- **Paper**: Lennart Augustsson, "Compiling Pattern Matching", in *Functional Programming Languages and Computer Architecture*, Springer LNCS 201, 1985
- **DOI**: https://doi.org/10.1007/3-540-15975-4_48
- **Location**: `pkg/internal/match/syntax_compiler.go`, `pkg/internal/match/match.go`

### Continuation-Wind Interaction (Clinger et al. 1999)

Formal treatment of how `dynamic-wind` extents interact with first-class continuations. The winding stack is separate from the continuation chain; invocation computes the common prefix of source and target stacks, then runs exit thunks (innermost-first) and entry thunks (outermost-first). The winding stack is captured by value at `call/cc` time but NOT stored per continuation frame — it is part of the dynamic extent, not the lexical continuation.

- **Paper**: William D. Clinger, Anne H. Hartheimer, Eric M. Ost, "Implementation Strategies for Continuations", Higher-Order and Symbolic Computation, Vol. 12, No. 1, pp. 7-45, 1999
- **DOI**: https://doi.org/10.1023/A:1010016816429
- **Location**: `pkg/machine/dynamic_wind.go` (FindCommonWindingPrefix), `pkg/machine/machine_context.go` (RestoreWithWindingFrom)

### Dynamic-Wind (Friedman & Haynes 1985)

Introduces `dynamic-wind` as a mechanism to constrain the effects of first-class continuations. When control enters or exits a dynamic extent, before/after thunks are called, enabling resource cleanup and proper interaction between continuations and side effects. Wile compiles `dynamic-wind` to inline bytecode (PushWind/PopWind operations).

- **Paper**: Daniel P. Friedman, Christopher T. Haynes, "Constraining Control", POPL 1985
- **DOI**: https://doi.org/10.1145/318593.318654
- **Location**: `pkg/machine/dynamic_wind.go`, `pkg/machine/compilation/compile_validated.go` (CompileValidatedDynamicWind)

### Continuation Marks (Clements, Flatt & Felleisen 2001)

Key/value annotations attached to continuation frames, with the defining property that a tail call *overwrites* the mark on the frame it reuses rather than pushing a new one — so marks describe the logical (space-safe) stack, not the Go stack. Wile exposes `with-continuation-mark` and continuation-mark sets, and implements `parameterize` on top of them: a parameterization is a mark, not a `dynamic-wind` thunk pair. That is what makes `parameterize` correct under `call/cc` re-entry without paying for wind/unwind thunks on every crossing.

- **Paper**: John Clements, Matthew Flatt, Matthias Felleisen, "Modeling an Algebraic Stepper", ESOP 2001
- **DOI**: https://doi.org/10.1007/3-540-45309-1_21
- **Also**: John Clements, *Portable and High-level Access to the Stack with Continuation Marks*, PhD Dissertation, Northeastern University, 2006
- **Location**: `pkg/machine/continuation_mark_set.go`, `pkg/machine/operation_cont_mark.go`, `pkg/registry/core/cont_marks.go`, `pkg/registry/core/bootstrap_macros.scm` (parameterize), `docs/continuations/marks.md`

### Trampolined Style (Ganz, Friedman & Wand 1999)

Invoking a captured continuation does not recursively call back into `Run()`. `applyCapturedContinuation` returns a control signal (`ErrResumeContinuation`) that unwinds to the VM's driver loop, which then re-enters with the restored state. The bounce is what keeps repeated resumption of the same continuation (generators, backtracking, `call/cc`-based loops) from growing the Go stack — the depth of Go frames stays constant no matter how many times a continuation is re-entered. `ErrPromptAbort` bounces the same way.

- **Paper**: Steven E. Ganz, Daniel P. Friedman, Mitchell Wand, "Trampolined Style", ICFP 1999
- **DOI**: https://doi.org/10.1145/317636.317779
- **Location**: `pkg/machine/machine_context.go` (Run driver loop), `pkg/machine/captured_continuation.go`, `docs/continuations/resume-trampoline.md`

### call/cc as Peirce's Law (Griffin 1990)

Shows that `call/cc` corresponds to Peirce's law `((A → B) → A) → A` under the Curry-Howard isomorphism, extending the correspondence from intuitionistic logic to classical logic. Adding `call/cc` to a language is equivalent to adding the law of excluded middle to the logic. The continuation received by the callback has type `A → B` (since invoking it never returns), making the overall type `((A → B) → A) → A`.

- **Paper**: Timothy G. Griffin, "A Formulae-as-Types Notion of Control", POPL 1990
- **DOI**: https://doi.org/10.1145/96709.96714
- **Location**: `pkg/registry/core/prim_control.go` (PrimCallCC)

### Numeric Promotion Lattice (Davey & Priestley 2002)

The numeric type promotion table is a finite join-semilattice (upper semilattice) over the product of two total orders: precision (Integer < BigInteger < Rational < Float < BigFloat) and complexity (Real < Complex). Each table entry is the join (least upper bound) of two numeric kinds. Commutativity is enforced by symmetric writes; the "no lossy promotions" invariant ensures monotonicity with respect to the exactness ordering.

- **Reference**: B. A. Davey, H. A. Priestley, *Introduction to Lattices and Order*, 2nd edition, Cambridge University Press, 2002
- **ISBN**: 978-0-521-78451-1
- **Location**: `pkg/values/promotion.go` (promotionTable, initPromotionTable), `pkg/values/numeric_lattice_test.go`

### Exactness as Abstract Interpretation (Cousot & Cousot 1977)

R7RS exactness tracking (exact/inexact contagion) is an instance of abstract interpretation. The abstract domain is the two-point lattice {exact < inexact}; the transfer function for arithmetic operations computes the abstract result from the abstract operands, joining them (inexact wins). The exact-zero rules are **strong updates**: they return a strictly more precise result than the naive join, because the mathematical result is known exactly. There are two, and they are the same rule — `(* 0 x)` and `(/ 0 x)` both yield an exact `0` for *any* `x`, overriding IEEE even against `+inf.0` and `+nan.0`, because an exact `0` is a mathematical zero and not an IEEE value.

The update is licensed by the **exactness of the zero alone**; the other operand's finiteness is irrelevant. It applies only when the exact zero is an *operand*: applied to a complex's component it would place an exact zero inside an inexact number, which contagion forbids.

- **Paper**: Patrick Cousot, Radhia Cousot, "Abstract interpretation: a unified lattice model for static analysis of programs by construction or approximation of fixpoints", POPL 1977
- **DOI**: https://doi.org/10.1145/512950.512973
- **Location**: `pkg/values/exact_zero.go` (the rule: `isExactZero`, `exactZeroTable`, `contagionOverParts`); `pkg/values/numeric_tower.go` (`Exactness`, `ExactnessOf`, `Simplify`)

### Lists as Initial Algebras (Bird & de Moor 1997; Meijer et al. 1991)

Proper Scheme lists (chains of Pair cells terminated by EmptyList) form the initial algebra of the polynomial functor F(X) = 1 + Value × X. The empty list is the nil constructor; cons is the pair constructor. `ForEach` is the catamorphism (unique fold). The type-level separation of `EmptyList` from `*Pair` encodes the two constructors as distinct injections in the coproduct.

- **Reference**: Richard Bird, Oege de Moor, *Algebra of Programming*, Prentice Hall, 1997
- **ISBN**: 978-0-13-507245-5
- **Paper**: Erik Meijer, Maarten Fokkinga, Ross Paterson, "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire", FPCA 1991
- **DOI**: https://doi.org/10.1007/3540543961_7
- **Location**: `pkg/values/pair.go` (Pair, NewCons, ForEach), `pkg/values/empty_list.go` (emptyListType)

### Bisimulation Equivalence for equal? (Milner 1989)

The `equal?` predicate on cyclic structures (pairs, vectors) is bisimulation equivalence — the greatest fixpoint of the structural matching relation. The visited set implements optimistic coinduction: re-encountered pointer pairs are assumed equal, which correctly computes the greatest fixed point. This is the same technique used by Chez Scheme and Racket.

- **Reference**: Robin Milner, *Communication and Concurrency*, Prentice Hall, 1989, Ch. 5
- **ISBN**: 978-0-13-114984-7
- **Also**: Davide Sangiorgi, David Walker, *The Pi-Calculus: A Theory of Mobile Processes*, Cambridge University Press, 2001, Ch. 2
- **ISBN** (Sangiorgi): 978-0-521-78177-0
- **Location**: `pkg/values/utils.go` (EqualTo, equalToDeep)

### Multiple Dispatch via Dispatch Tables (Chambers & Chen 1999)

Numeric arithmetic uses pre-built dispatch tables indexed by `NumericKind`, implementing symmetric binary multimethods with coercion. Both operands are promoted to their least upper bound type before the operation. The dispatch matrix is materialized at init() time for O(1) runtime dispatch.

- **Paper**: Craig Chambers, Weimin Chen, "Efficient Multiple and Predicate Dispatching", OOPSLA 1999
- **DOI**: https://doi.org/10.1145/320384.320386
- **Location**: `pkg/values/promotion.go` (makeArithmeticDispatch)

### Units: Module System Foundations (Flatt & Felleisen 1998)

Wile's library isolation model (shared syntax interning, isolated binding stores) follows the "units" approach to modular linking. Each library has its own `GlobalEnvironmentFrame` for bindings but shares a `Namespace` for syntax interning. R7RS §6.5 symbol identity is ensured by string-key comparison via `values.EqIdentity`.

- **Paper**: Matthew Flatt, Matthias Felleisen, "Units: Cool Modules for HOT Languages", PLDI 1998
- **DOI**: https://doi.org/10.1145/277650.277730
- **Location**: `pkg/environment/namespace.go` (NewChildNamespace, NewChildRuntime)

### Languages as Libraries (Tobin-Hochstadt et al. 2011)

The dialect system takes the Racket position that the language a program is written in is itself a value the host chooses, not a constant baked into the implementation. `WithDialect(d)` installs a per-engine form registry and bootstrap fragment, so `NoMutation` removes `set!`, `set-car!`, and the mutable-vector/string operations from the *language surface*, not merely from a lint pass. Two engines in one process can speak different Schemes.

Wile's version is deliberately weaker than `#lang`: a dialect selects and attenuates a fixed set of core forms and a bootstrap fragment. It cannot introduce a new reader or a wholly new expander. The honest claim is per-engine language attenuation, not language extension.

- **Paper**: Sam Tobin-Hochstadt, Vincent St-Amour, Ryan Culpepper, Matthew Flatt, Matthias Felleisen, "Languages as Libraries", PLDI 2011
- **DOI**: https://doi.org/10.1145/1993498.1993514
- **Also**: Matthias Felleisen et al., "A Programmable Programming Language", *Communications of the ACM*, Vol. 61, No. 3, 2018
- **DOI**: https://doi.org/10.1145/3127323
- **Location**: `pkg/wile/dialect.go` (Dialect, WithDialect), `pkg/wile/dialect_nomutation.go` (NoMutation), `pkg/internal/forms/` (per-engine FormRegistry)

### Exception Handling (Goodenough 1975)

The foundational paper on structured exception handling design. Identifies the key design dimensions: termination vs resumption semantics, dynamic vs lexical handler scope, and handler selection. Wile's exception model supports both termination (`raise`) and resumption (`raise-continuable`) with dynamic handler scope (`with-exception-handler` installs handlers with dynamic extent).

- **Paper**: John B. Goodenough, "Exception Handling: Issues and a Proposed Notation", Communications of the ACM, Vol. 18, No. 12, 1975
- **DOI**: https://doi.org/10.1145/361227.361230
- **Location**: `pkg/registry/core/prim_exceptions.go`

## Security & Sandboxing

Capability-based security foundations underlying Wile's sandboxing model.

### Dennis & Van Horn, "Programming Semantics for Multiprogrammed Computations" (1966)

The original formalization of capabilities: authority as an unforgeable token carried by the computation, not ambient access determined by identity. Wile's extension model embodies this — primitives are capabilities granted at engine construction, not ambient names that exist by default.

- **Paper**: Jack B. Dennis, Earl C. Van Horn, "Programming Semantics for Multiprogrammed Computations", Communications of the ACM, Vol. 9, No. 3, 1966
- **DOI**: https://doi.org/10.1145/365230.365252

### Hardy, "The Confused Deputy" (1988)

Motivates why capability-based models are more robust than ACL-based ones: ambient authority allows a trusted program (the "deputy") to be tricked into misusing its privileges. Wile avoids this by eliminating ambient authority entirely — if an extension isn't loaded, its primitives don't exist.

- **Paper**: Norm Hardy, "The Confused Deputy (or why capabilities might have been invented)", ACM SIGOPS Operating Systems Review, Vol. 22, No. 4, 1988
- **DOI**: https://doi.org/10.1145/54289.871709

### Rees, "A Security Kernel Based on the Lambda Calculus" (1996)

Argues that lexical scoping + first-class procedures = a capability-safe language kernel. This is the core theoretical justification for Wile's Layer 1 sandboxing: if you don't introduce a binding, code can't use it. The extension registry is the set of capabilities granted to the engine.

- **Paper**: Jonathan Rees, "A Security Kernel Based on the Lambda Calculus", AI Memo 1564, MIT, 1996
- **URL**: https://dspace.mit.edu/handle/1721.1/5944
- **Location**: `docs/security/sandboxing.md` (extension-level sandboxing), `pkg/wile/engine.go` (registry construction)

### Saltzer & Schroeder, "The Protection of Information in Computer Systems" (1975)

The original formal statement of the Principle of Least Authority (POLA), along with seven other design principles for protection mechanisms. Wile's profiles apply POLA at the language level: `WithProfile(Tiny)` grants core computation and nothing else, and each larger profile (`Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) names the additional authority it confers. `WithoutCore()` takes this to the extreme — an engine with zero primitives.

- **Paper**: Jerome H. Saltzer, Michael D. Schroeder, "The Protection of Information in Computer Systems", Proceedings of the IEEE, Vol. 63, No. 9, 1975
- **DOI**: https://doi.org/10.1109/PROC.1975.9939
- **Location**: `pkg/wile/profile.go` (Profile, WithProfile), `pkg/wile/options.go` (WithoutCore, WithExtension)

### Anderson, "Computer Security Technology Planning Study" (1972)

The original definition of the reference monitor concept: a mediation point that is always invoked, tamperproof, and complete. Wile's `security.Check()` implements this pattern — every privileged operation path calls `Check`, the authorizer is injected via `context.Context` (immutable after construction), and all gate sites are enumerated.

- **Report**: James P. Anderson, "Computer Security Technology Planning Study", ESD-TR-73-51, Air Force Electronic Systems Division, 1972
- **Location**: `pkg/security/context.go` (Check), `pkg/wile/engine.go` (withAuth)

### Lampson, "A Note on the Confinement Problem" (1973)

Defines the confinement property of capability systems: a program cannot extend its own authority beyond what was granted at creation. Wile's `LibraryEnvFactory` closes over the engine's registry, ensuring transitive confinement — libraries cannot acquire capabilities not present in the engine. This is the "no amplification" property.

- **Paper**: Butler W. Lampson, "A Note on the Confinement Problem", Communications of the ACM, Vol. 16, No. 10, 1973
- **DOI**: https://doi.org/10.1145/362375.362389
- **Location**: `pkg/wile/engine.go` (LibraryEnvFactory)

### Miller, "Robust Composition" (2006)

Formalizes the object-capability model and the principle of least authority (POLA). Capability attenuation (`Registry.Without()`, `Registry.WithoutCategory()`, `Registry.WithoutBindings()`) implements Miller's monotonicity property (§2.1): derived registries never have more authority than the source. Profiles are the named attenuation points; `WithSandbox()` is an orthogonal attenuator that further restricts the authorizer.

- **Paper**: Mark S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control", PhD Dissertation, Johns Hopkins University, 2006
- **URL**: http://www.erights.org/talks/thesis/
- **Location**: `pkg/registry/primitive_registry.go` (Without, WithoutCategory, WithoutBindings)

### Miller et al., "Caja: Safe active content in sanitized JavaScript" (2008)

Closest analog in another language: restricting JavaScript to a capability-safe subset. Demonstrates why compile-time capability elimination is harder in JavaScript (prototype chains, ambient globals like `window`) than in Scheme (lexical scoping provides natural isolation).

- **Paper**: Mark S. Miller, Mike Samuel, Ben Laurie, Ihab Awad, Mike Stay, "Caja: Safe active content in sanitized JavaScript", Google Technical Report, 2008
- **URL**: https://google-code-archive-downloads.storage.googleapis.com/v2/code.google.com/google-caja/caja-spec-2008-06-06.pdf

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

Canonical definitions for list processing procedures including `fold`. Wile's implementation in `pkg/stdlib/lib/srfi/1/` is from Chibi-Scheme.

- **URL**: https://srfi.schemers.org/srfi-1/srfi-1.html

### SRFI-9: Defining Record Types

Record type definitions, integrated into R7RS as `define-record-type`.

- **URL**: https://srfi.schemers.org/srfi-9/srfi-9.html

### SRFI-13: String Libraries

String processing procedures (predicates, search, filter, fold, tokenize, etc.). Implemented in Wile as the `(srfi 13)` library.

- **Author**: Olin Shivers
- **URL**: https://srfi.schemers.org/srfi-13/srfi-13.html
- **Status**: Final (2000-07-25)
- **Wile reference**: `(srfi 13)` library — `pkg/stdlib/lib/srfi/13.sld`, backed by the string primitives in `pkg/registry/core/`

### SRFI-14: Character-Set Library

Character-set algebra (union, intersection, complement, difference, fold, filter, named sets) with Unicode coverage via Go's `unicode` tables. Implemented in Wile as the `(srfi 14)` library.

- **Author**: Olin Shivers
- **URL**: https://srfi.schemers.org/srfi-14/srfi-14.html
- **Status**: Final (2000-07-25)
- **Wile reference**: `(srfi 14)` library — `pkg/stdlib/lib/srfi/14.sld`, backed by the `charsets` extension (`extensions/charsets/`)

### SRFI-18: Multithreading Support

Threading primitives implemented in Wile: threads, mutexes, condition variables, and time objects. Shared memory with mutual exclusion — the monitor pattern; see the Concurrency section.

- **URL**: https://srfi.schemers.org/srfi-18/srfi-18.html

### SRFI-39: Parameter Objects

Dynamic parameters (`make-parameter`, `parameterize`) provide controlled dynamic binding in a lexically-scoped language. Parameters have lexical identity but dynamic extent. Wile's `parameterize` expands to nested `with-continuation-mark` forms, one per parameter — not to `dynamic-wind` save/restore thunks (**changed in PR #542/#543**). The mark *is* the binding, so a continuation carries its parameterization with it and re-entry needs no unwind protocol. See "Continuation Marks" above.

- **URL**: https://srfi.schemers.org/srfi-39/srfi-39.html
- **Location**: `pkg/machine/parameter.go`, `pkg/registry/core/bootstrap_macros.scm` (parameterize macro)

### SRFI-45: Primitives for Iterative Lazy Algorithms

The source of R7RS `delay-force` (§4.2.5). Naive `delay`/`force` leaks space on iterative lazy algorithms: forcing a promise whose value is another promise builds an unbounded chain. SRFI-45's `delay-force` (van Tonder's `lazy`) collapses that chain, making iterative lazy loops run in bounded space. Wile implements `delay`, `delay-force`, `make-promise`, and `force` per R7RS.

- **Author**: André van Tonder
- **URL**: https://srfi.schemers.org/srfi-45/srfi-45.html
- **Location**: `pkg/stdlib/lib/scheme/lazy.sld`, `pkg/registry/core/bootstrap_macros.scm` (delay, delay-force)

### SRFI-64: A Scheme API for Test Suites

Test framework specification. Wile's `(chibi test)` library is a portable subset of SRFI-64, providing `test-begin`, `test-end`, and `test`.

- **URL**: https://srfi.schemers.org/srfi-64/srfi-64.html

### SRFI-132: Sort Libraries

Sorting, merging, selection, median, and deduplication over lists and vectors, with stability and in-place variants distinguished in the names. Implemented in Wile as the `(srfi 132)` library, split into functional modules.

- **Author**: John Cowan (after Olin Shivers' sort package)
- **URL**: https://srfi.schemers.org/srfi-132/srfi-132.html
- **Location**: `pkg/stdlib/lib/srfi/132/` (list-sort, vector-sort, list-merge, vector-merge, select, median, dedup, predicates)

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

## Concurrency

### Hoare, "Communicating Sequential Processes" (1978)

Go channels are an implementation of Hoare's CSP. Wile's `gointerop` extension exposes Go channels as Scheme primitives, providing CSP-style synchronization through message passing — distinct from SRFI-18's shared-memory coordination via mutexes and condition variables.

- **Paper**: C. A. R. Hoare, "Communicating Sequential Processes", Communications of the ACM, Vol. 21, No. 8, 1978
- **DOI**: https://doi.org/10.1145/359576.359585
- **Location**: `extensions/gointerop/prim_gointerop.go` (channel primitives)

### Hoare, "Monitors: An Operating System Structuring Concept" (1974)

The other half of Wile's concurrency story, and the older one. SRFI-18's mutex + condition-variable pairing is the monitor: mutual exclusion for the invariant, condition variables for waiting on it. Wile ships both models side by side — monitors (SRFI-18, shared memory) and CSP (`gointerop`, message passing) — because Go supplies both and Scheme programs written against either should run unmodified.

- **Paper**: C. A. R. Hoare, "Monitors: An Operating System Structuring Concept", Communications of the ACM, Vol. 17, No. 10, 1974
- **DOI**: https://doi.org/10.1145/355620.361161
- **Location**: `extensions/threads/` (SRFI-18 mutexes, condition variables)

## Benchmarks

The benchmark suites Wile is measured against, and where they come from.

### Gabriel Benchmarks (Gabriel 1985)

The canonical Lisp benchmark suite. Wile runs the Larceny-normalized R7RS ports: `tak`, `takl`, `ntakl`, `ctak`, `cpstak`, `browse`, `destruc`, `deriv`, `diviter`, `divrec`, `fft`, `puzzle`, `triangl`, and the Boyer prover as `nboyer`/`sboyer`. Gabriel's contribution was methodological as much as empirical: publish the programs, measure what implementations actually do, make the numbers reproducible. The suite is the standard cross-implementation baseline for Scheme, which is why Wile reports it.

- **Book**: Richard P. Gabriel, *Performance and Evaluation of Lisp Systems*, MIT Press, 1985
- **ISBN**: 978-0-262-07093-5
- **Location**: `benchmarks/larceny/src/`, `make bench-gabriel`

### Larceny Benchmark Suite

Wile runs the R7RS-ported Gabriel programs plus the larger Larceny additions (`compiler`, `earley`, `conform`, `gcbench`, `graphs`, `nboyer`, `sboyer`). Provenance matters here: these are third-party programs Wile did not author and cannot tune against, which is the property that makes them useful.

- **Project**: Larceny Scheme (Clinger et al.), Northeastern University
- **URL**: https://larcenists.org/benchmarksAboutR7.html
- **Location**: `benchmarks/larceny/`, `make bench-extended`

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

Implementation behavior reference for zero-dominance in multiplication (`pkg/values/float.go`, `pkg/values/integer.go`, `pkg/values/big_integer.go`), optimistic bisimilarity for `equal?` (`pkg/values/utils.go`), and pointer-based equality for syntax objects (`pkg/syntax/`).

- **URL**: https://cisco.github.io/ChezScheme/
- **Source**: https://github.com/cisco/ChezScheme

### Racket

Implementation model for delimited continuations (prompt tags, composable continuations), phase numbering conventions, and phased imports. Also referenced for `@`-expression reader syntax (planned feature) and syntax object equality semantics.

- **Homepage**: https://racket-lang.org/
- **Scribble Reader**: https://docs.racket-lang.org/scribble/reader.html
- **At-expressions**: https://docs.racket-lang.org/at-exp/index.html

### Chibi-Scheme (Alex Shinn)

Source of portable Scheme library code used in Wile. The `pkg/stdlib/lib/chibi/` directory contains Chibi-Scheme's test framework, diff library, optional argument macros, and ANSI terminal library. The `pkg/stdlib/lib/srfi/1/` directory contains Chibi-Scheme's SRFI-1 list library implementation split into functional modules.

- **Homepage**: https://synthcode.com/wiki/chibi-scheme
- **Source**: https://github.com/ashinn/chibi-scheme
- **License**: BSD

### microKanren / miniKanren (Hemann & Friedman 2013; Byrd 2009)

Wile ships relational programming as a library, not a language feature. `(wile microkanren)` is Hemann and Friedman's ~40-line functional core (goals as functions from substitution/counter to a stream, `unify`, `mplus`/`bind` interleaving); `(wile kanren)` is the miniKanren macro layer (`fresh`, `conde`, `run`, `run*`) built on it with `syntax-rules`. The pair is a load-bearing test of the macro system and the VM together: `conde` interleaving stresses stream construction, and the relational arithmetic benchmarks stress deep recursion and variable chains.

- **Paper**: Jason Hemann, Daniel P. Friedman, "µKanren: A Minimal Functional Core for Relational Programming", Scheme and Functional Programming Workshop, 2013
- **URL**: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
- **Dissertation**: William E. Byrd, *Relational Programming in miniKanren: Techniques, Applications, and Implementations*, Indiana University, 2009
- **Book**: Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, *The Reasoned Schemer*, MIT Press, 2005 (2nd ed. 2018; ISBN 978-0-262-53551-0)
- **Location**: `pkg/stdlib/lib/wile/microkanren.scm`, `pkg/stdlib/lib/wile/kanren.scm`, `examples/benchmarks/kanren-benchmark.scm`

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

## Algebra & Combinatorics

Canonical references for the `(wile algebra ...)` libraries — graph and
combinatorial abstract algebra. Many of these are also cited inline in the
relevant library docstrings; this section is the consolidated index.

### Lattice Theory & Birkhoff Representation (Birkhoff 1937)

Finite distributive lattices are, up to isomorphism, the downset lattices of
their poset of join-irreducibles (Birkhoff's representation theorem). Wile uses
this both directly (`lattice` Birkhoff representation/reconstruction roundtrip)
and structurally — the Conway lattice of stable matchings is built by treating
rotations as join-irreducibles. Heyting algebras add relative pseudo-complement
(`⇒`) over a bounded distributive lattice.

- **Paper**: Garrett Birkhoff, "Rings of sets", *Duke Math. J.* 3(3), 1937
- **Reference**: Garrett Birkhoff, *Lattice Theory*, 3rd edition, AMS Colloquium Publications, 1967
- **Reference**: B. A. Davey, H. A. Priestley, *Introduction to Lattices and Order*, 2nd edition, Cambridge University Press, 2002 (ISBN 978-0-521-78451-1)
- **Reference**: George Grätzer, *Lattice Theory: Foundation*, Birkhäuser, 2011 (ISBN 978-3-0348-0017-4)
- **Location**: `pkg/stdlib/lib/wile/algebra/lattice.scm`, `heyting.scm`, `order.scm`

### Formal Concept Analysis (Ganter & Wille 1999)

A formal context (objects × attributes × incidence) induces a complete lattice
of concepts via the Galois connection between extents and intents. This is the
mathematical foundation of the `fca` library and connects to the lattice and
Galois-connection libraries.

- **Reference**: Bernhard Ganter, Rudolf Wille, *Formal Concept Analysis: Mathematical Foundations*, Springer, 1999 (ISBN 978-3-540-62771-5)
- **Location**: `pkg/stdlib/lib/wile/algebra/fca.scm`, `galois.scm`

### Incidence Algebras & Möbius Functions (Rota 1964)

The incidence algebra of a locally-finite poset, with its Möbius function as the
multiplicative inverse of the zeta function, unifies inclusion-exclusion,
number-theoretic Möbius inversion, and combinatorial counting. Foundation of the
`incidence` library.

- **Paper**: Gian-Carlo Rota, "On the foundations of combinatorial theory I: Theory of Möbius functions", *Z. Wahrscheinlichkeitstheorie* 2, 1964
- **Reference**: Richard P. Stanley, *Enumerative Combinatorics, Vol. 1*, 2nd edition, Cambridge University Press, 2011 (Ch. 3; ISBN 978-1-107-60262-5)
- **Location**: `pkg/stdlib/lib/wile/algebra/incidence.scm`

### Group Actions & Pólya–Burnside Enumeration (Pólya 1937)

The number of orbits of a finite group action equals the average number of
fixed points over the group (the Cauchy–Frobenius / "Burnside" lemma); Pólya's
enumeration theorem refines this for colorings (e.g. counting necklaces — `k`
colorings of an `n`-cycle up to rotation). Foundation of the `group` library's
orbit/stabilizer/`burnside-count` operations; the necklace closed form
`(1/n)·Σ_{d|n} φ(d)·k^(n/d)` is used as the independent oracle in the Sage
validation harness.

- **Reference**: William Burnside, *Theory of Groups of Finite Order*, 2nd edition, Cambridge University Press, 1911
- **Paper**: George Pólya, "Kombinatorische Anzahlbestimmungen für Gruppen, Graphen und chemische Verbindungen", *Acta Math.* 68, 1937
- **Paper**: J. Howard Redfield, "The theory of group-reduced distributions", *Amer. J. Math.* 49(3), 1927
- **Reference**: David S. Dummit, Richard M. Foote, *Abstract Algebra*, 3rd edition, Wiley, 2004 (ISBN 978-0-471-43334-7)
- **Location**: `pkg/stdlib/lib/wile/algebra/group.scm`; `tools/sage/verify_algebra.sage` (`validate_group`)

### Stable Matching & the Assignment Problem (Gale & Shapley 1962)

Gale–Shapley deferred acceptance produces a stable matching; the set of stable
matchings forms a distributive lattice (Conway), traversed by rotations
(Gusfield–Irving). Egalitarian stable matching (minimize total rank-sum) is
**polynomial** (Irving–Leather–Gusfield 1987); the sex-equal variant (minimize
|rank-sum difference|) is **NP-hard** (Kato 1993). The minimum-cost assignment
(`tropical-assignment`) uses the Hungarian algorithm. Foundation of the
`matching` library.

- **Paper**: David Gale, Lloyd S. Shapley, "College admissions and the stability of marriage", *Amer. Math. Monthly* 69(1), 1962
- **Reference**: Dan Gusfield, Robert W. Irving, *The Stable Marriage Problem: Structure and Algorithms*, MIT Press, 1989 (ISBN 978-0-262-07118-5)
- **Reference**: Alvin E. Roth, Marilda Sotomayor, *Two-Sided Matching: A Study in Game-Theoretic Modeling and Analysis*, Cambridge University Press, 1990 (ISBN 978-0-521-43788-2)
- **Reference**: Donald E. Knuth, *Stable Marriage and Its Relation to Other Combinatorial Problems*, AMS, 1997 (orig. *Mariages Stables*, 1976)
- **Paper**: Robert W. Irving, Paul Leather, Dan Gusfield, "An efficient algorithm for the 'optimal' stable marriage", *JACM* 34(3), 1987 (egalitarian, polynomial)
- **Paper**: Akiko Kato, "Complexity of the sex-equal stable marriage problem", *Japan J. Indust. Appl. Math.* 10(1), 1993 (sex-equal, NP-hard)
- **Paper**: Harold W. Kuhn, "The Hungarian method for the assignment problem", *Naval Res. Logist. Quart.* 2, 1955
- **Paper**: Roy Jonker, Anton Volgenant, "A shortest augmenting path algorithm for dense and sparse linear assignment problems", *Computing* 38, 1987
- **Location**: `pkg/stdlib/lib/wile/algebra/matching.scm`

### Semirings, Dioids & Path Algebra (Gondran & Minoux 2008)

Path problems on graphs (reachability, shortest paths, path counting,
transitive closure) are instances of matrix algebra over a semiring: the
Kleene star `M* = I + M + M² + …` generalizes all of them. Foundation of the
`semiring` and `matrix` libraries and the semiring-parameterized `graph` path
analytics.

- **Reference**: Michel Gondran, Michel Minoux, *Graphs, Dioids and Semirings: New Models and Algorithms*, Springer, 2008 (ISBN 978-0-387-75449-9)
- **Paper**: Daniel J. Lehmann, "Algebraic structures for transitive closure", *Theoret. Comput. Sci.* 4(1), 1977
- **Paper**: Roland C. Backhouse, Bernard A. Carré, "Regular algebra applied to path-finding problems", *J. Inst. Math. Appl.* 15(2), 1975
- **Reference**: Jonathan S. Golan, *Semirings and their Applications*, Kluwer, 1999 (ISBN 978-0-7923-5786-5)
- **Location**: `pkg/stdlib/lib/wile/algebra/semiring.scm`, `matrix.scm`, `graph.scm`

### Tropical / Max-Plus Algebra (Maclagan & Sturmfels 2015)

The tropical (min,+) and max-plus semirings linearize shortest-path and
scheduling problems; the tropical permanent is the minimum-cost perfect
assignment. Used by the `tropical-semiring` carrier and `matrix` permanent.

- **Reference**: Diane Maclagan, Bernd Sturmfels, *Introduction to Tropical Geometry*, AMS, 2015 (ISBN 978-0-8218-5198-2)
- **Reference**: Peter Butkovič, *Max-linear Systems: Theory and Algorithms*, Springer, 2010 (ISBN 978-1-84996-298-5)
- **Location**: `pkg/stdlib/lib/wile/algebra/semiring.scm`, `matrix.scm`

### Algebraic & Combinatorial Graph Theory (Godsil & Royle 2001)

Graph invariants computed by the `combinatorial-graph` library rest on classical
results: spanning-tree count via Kirchhoff's Matrix-Tree theorem, the chromatic
polynomial via Whitney/Tutte deletion-contraction, the Tutte polynomial,
canonical labeling for isomorphism (McKay–Piperno), bipartite matching
(Hopcroft–Karp), and strongly-connected components (Tarjan). The `graph-partition`
balanced-cut primitive uses Kernighan–Lin pair-swaps (Fiduccia–Mattheyses is the
linear-time single-move variant; not used because it deadlocks under a tight
balance from a balanced seed).

- **Reference**: Chris Godsil, Gordon Royle, *Algebraic Graph Theory*, Springer, 2001 (ISBN 978-0-387-95220-8)
- **Reference**: Reinhard Diestel, *Graph Theory*, 5th edition, Springer, 2017 (ISBN 978-3-662-53621-6)
- **Paper**: Gustav Kirchhoff, "Über die Auflösung der Gleichungen, auf welche man bei der Untersuchung der linearen Vertheilung galvanischer Ströme geführt wird", *Ann. Phys. Chem.* 72, 1847 (Matrix-Tree theorem)
- **Paper**: William T. Tutte, "A contribution to the theory of chromatic polynomials", *Canad. J. Math.* 6, 1954
- **Paper**: Hassler Whitney, "The coloring of graphs", *Ann. of Math.* 33(4), 1932
- **Paper**: Brendan D. McKay, Adolfo Piperno, "Practical graph isomorphism, II", *J. Symbolic Comput.* 60, 2014
- **Paper**: John E. Hopcroft, Richard M. Karp, "An n^{5/2} algorithm for maximum matchings in bipartite graphs", *SIAM J. Comput.* 2(4), 1973
- **Paper**: Robert E. Tarjan, "Depth-first search and linear graph algorithms", *SIAM J. Comput.* 1(2), 1972
- **Paper**: Brian W. Kernighan, Shen Lin, "An efficient heuristic procedure for partitioning graphs", *Bell System Technical Journal* 49(2), 1970 (`graph-partition` balanced cut)
- **Paper**: C. M. Fiduccia, R. M. Mattheyses, "A linear-time heuristic for improving network partitions", *19th Design Automation Conf.*, 1982 (single-move variant; rejected, see design doc §5)
- **Location**: `pkg/stdlib/lib/wile/algebra/combinatorial-graph.scm`, `graph.scm`, `incidence.scm`

### Polynomial Rings & Computer Algebra (von zur Gathen & Gerhard 2013)

Ring-parameterized univariate polynomial arithmetic — addition, multiplication,
Horner evaluation, Euclidean division, and GCD — follows the standard
computer-algebra treatment. Foundation of the `polynomial` library over the
`ring` abstraction.

- **Reference**: Joachim von zur Gathen, Jürgen Gerhard, *Modern Computer Algebra*, 3rd edition, Cambridge University Press, 2013 (ISBN 978-1-107-03903-2)
- **Reference**: Donald E. Knuth, *The Art of Computer Programming, Vol. 2: Seminumerical Algorithms*, 3rd edition, Addison-Wesley, 1997 (ISBN 978-0-201-89684-8)
- **Reference**: Serge Lang, *Algebra*, 3rd edition, Springer, 2002 (ISBN 978-0-387-95385-4)
- **Location**: `pkg/stdlib/lib/wile/algebra/polynomial.scm`, `ring.scm`

### Term Rewriting & Unification (Baader & Nipkow 1998)

Equational reasoning, normalization by rewriting, and associative-commutative
unification underpin the `rewrite`, `unification`, and `symbolic` libraries.

- **Reference**: Franz Baader, Tobias Nipkow, *Term Rewriting and All That*, Cambridge University Press, 1998 (ISBN 978-0-521-77920-3)
- **Paper**: Donald E. Knuth, Peter B. Bendix, "Simple word problems in universal algebras", in J. Leech (ed.), *Computational Problems in Abstract Algebra*, Pergamon, 1970
- **Paper**: Mark E. Stickel, "A unification algorithm for associative-commutative functions", *JACM* 28(3), 1981
- **Location**: `pkg/stdlib/lib/wile/algebra/rewrite.scm`, `unification.scm`, `symbolic.scm`

### Satisfiability — DPLL & CDCL (Marques-Silva & Sakallah 1999)

The `sat` library implements conflict-driven clause learning, the modern
descendant of the DPLL procedure, and closes the axiomatic-equivalence gap via
`boolean-decide-equivalent?`.

- **Paper**: Martin Davis, George Logemann, Donald Loveland, "A machine program for theorem-proving", *CACM* 5(7), 1962 (DPLL)
- **Paper**: João P. Marques-Silva, Karem A. Sakallah, "GRASP: A search algorithm for propositional satisfiability", *IEEE Trans. Computers* 48(5), 1999 (CDCL)
- **Reference**: Armin Biere, Marijn Heule, Hans van Maaren, Toby Walsh (eds.), *Handbook of Satisfiability*, 2nd edition, IOS Press, 2021 (ISBN 978-1-64368-160-3)
- **Location**: `pkg/stdlib/lib/wile/algebra/sat.scm`

### Universal Algebra (Burris & Sankappanavar 1981)

The common foundation under every `(wile algebra ...)` structure: signatures,
homomorphisms, subalgebras, and the validate-X law-checking discipline are
universal-algebra constructions. Category theory grounds the `category` library.

- **Reference**: Stanley Burris, H. P. Sankappanavar, *A Course in Universal Algebra*, Millennium edition, 2012 (orig. Springer GTM 78, 1981; freely available)
- **Reference**: Saunders Mac Lane, *Categories for the Working Mathematician*, 2nd edition, Springer, 1998 (ISBN 978-0-387-98403-2)
- **Location**: `pkg/stdlib/lib/wile/algebra/` (all structures), `category.scm`, `setoid.scm`

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
