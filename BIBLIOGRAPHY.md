# Bibliography

Academic papers, specifications, and references used in the Wile Scheme implementation.

## Foundational Theory

The formal foundations that Scheme inherits and Wile implements.

### Lambda Calculus (Church 1936)

The formal system underlying Scheme. Lambda abstraction, beta reduction (function application), and variable binding are the core mechanisms. Every Scheme procedure is a lambda expression; every function call is beta reduction made operational.

- **Paper**: Alonzo Church, "An Unsolvable Problem of Elementary Number Theory", American Journal of Mathematics, Vol. 58, No. 2, 1936
- **DOI**: https://doi.org/10.2307/2371045
- **Location**: `machine/machine_closure.go` (closure = lambda abstraction), `machine/machine_context.go` (Apply = beta reduction)

### McCarthy, "Recursive Functions of Symbolic Expressions" (1960)

Introduces Lisp, the `eval` function, and the read-eval-print loop. McCarthy's key insight — that a language's interpreter can be written in the language itself — established the foundation for all Lisp-family languages. Wile's REPL descends directly from this structure, though it compiles to bytecode rather than tree-walking.

- **Paper**: John McCarthy, "Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I", Communications of the ACM, Vol. 3, No. 4, 1960
- **DOI**: https://doi.org/10.1145/367177.367199
- **Location**: `internal/repl/repl.go`, `engine.go` (Eval method)

### Strachey, "Fundamental Concepts in Programming Languages" (1967)

Introduces the classification of values as first-class or second-class. A first-class value can be passed as argument, returned from a function, assigned to a variable, and tested for equality. Scheme's defining choice is that procedures are first-class. Wile's `Callable` interface extends `Value`, making closures indistinguishable from other values in the type system.

- **Paper**: Christopher Strachey, "Fundamental Concepts in Programming Languages", Higher-Order and Symbolic Computation, Vol. 13, No. 1/2, 2000 (originally lecture notes from the 1967 NATO Summer School in Copenhagen)
- **DOI**: https://doi.org/10.1023/A:1010000313106
- **Location**: `machine/closure.go`, `values/values.go` (Callable interface)

### Reynolds, "Definitional Interpreters for Higher-Order Programming Languages" (1972)

Introduces continuation-passing style (CPS) as a technique for defining language semantics. Continuations represent "the rest of the computation" — the key insight that Scheme later made first-class via `call/cc`. Wile's explicit `MachineContinuation` linked list is a defunctionalized continuation in the sense of Reynolds.

- **Paper**: John C. Reynolds, "Definitional Interpreters for Higher-Order Programming Languages", ACM Annual Conference 1972, reprinted in Higher-Order and Symbolic Computation, Vol. 11, No. 4, 1998
- **DOI**: https://doi.org/10.1023/A:1010027404223 (1998 reprint)
- **Location**: `machine/machine_continuation.go`

### Sussman & Steele, "Scheme: An Interpreter for Extended Lambda Calculus" (1975)

The paper that introduced Scheme. Established that actors and closures are the same concept, that lexical scoping is the correct default, and that a practical language can be built directly on the lambda calculus. The name "Scheme" comes from this paper.

- **Paper**: Gerald Jay Sussman, Guy Lewis Steele Jr., "Scheme: An Interpreter for Extended Lambda Calculus", AI Memo 349, MIT, 1975
- **URL**: https://dspace.mit.edu/handle/1721.1/5794

### The Lambda Papers (Steele & Sussman, 1975–1980)

A series of MIT AI Memos establishing the theoretical and practical foundations of Scheme. Key results: tail calls are GOTOs; `let` is lambda application; imperative and declarative programming are both lambda. Wile's bootstrap macros embody "Lambda: The Ultimate Imperative" directly: `let` expands to `((lambda (name ...) body ...) val ...)` in `registry/core/bootstrap.scm`.

- AIM-349: "Scheme: An Interpreter for Extended Lambda Calculus" (1975)
- AIM-353: "Lambda: The Ultimate Imperative" (1976)
- AIM-379: "Lambda: The Ultimate Declarative" (1976)
- AIM-443: "Lambda: The Ultimate GOTO" (1977) — already cited for TCO
- AIM-514: "The Art of the Interpreter" (1978)
- **URL**: https://dspace.mit.edu/handle/1721.1/6091 (collection)
- **Location**: `registry/core/bootstrap.scm` (derived forms as lambda)

## Pedagogical Foundations

Widely known techniques — explanations for new contributors.

### Stack-Based Virtual Machines

Wile's VM uses a stack-based architecture descended from Landin's SECD machine. The evaluation stack holds intermediate results, a value register carries the most recent result, and continuation frames preserve state across calls.

- **Origin**: Peter J. Landin, "The Mechanical Evaluation of Expressions", The Computer Journal, Vol. 6, No. 4, 1964
- **DOI**: https://doi.org/10.1093/comjnl/6.4.308
- **Location**: `machine/machine_context.go`, `machine/stack.go`

### CESK Abstract Machine (Felleisen & Friedman 1987)

The abstract machine model underlying Wile's VM. The `vmState` holds Control (template + pc), Environment (env), Store (evals stack), and Kontinuation (cont chain). Unlike Landin's SECD (which uses an implicit dump for continuations), the CESK machine uses explicit, first-class continuation values. This directly enables `call/cc` and delimited continuations without special dump-manipulation primitives. The key evidence: `Apply()` replaces the current state in-place (CEK transition style), `SaveContinuation()` reifies state as a linked-list node, not a dump push.

- **Paper**: Matthias Felleisen, Daniel P. Friedman, "Control Operators, the SECD-machine, and the λ-calculus", in M. Wirsing (ed.), *Formal Description of Programming Concepts III*, Elsevier, 1987
- **Also**: Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, *Semantics Engineering with PLT Redex*, MIT Press, 2009, Chapter 4
- **ISBN** (Redex): 978-0-262-06274-6
- **Location**: `machine/vm_state.go`, `machine/machine_context.go` (Run, Apply, SaveContinuation, Restore)

### Tail Call Optimization

Tail calls reuse the caller's continuation frame instead of allocating a new one, enabling recursive procedures in tail position to run in constant stack space. Required by R7RS §3.5.

- **Origin**: Guy L. Steele Jr., "Debunking the 'Expensive Procedure Call' Myth", ACM Conference on AI and Programming Languages, 1977
- **Location**: `machine/compile_time_call_context.go`

### Proper Tail Recursion (Clinger 1998)

Formalizes what "proper tail recursion" means: an implementation satisfies it if and only if it evaluates tail-recursive programs in bounded space. Wile satisfies this by omitting `SaveContinuation` for tail calls — the continuation chain does not grow. This provides the formal definition against which Steele's 1977 optimization claim can be verified.

- **Paper**: William D. Clinger, "Proper Tail Recursion and Space Efficiency", PLDI 1998
- **DOI**: https://doi.org/10.1145/277650.277719
- **Location**: `machine/compile_time_call_context.go`, `machine/compile_validated.go`

### De Bruijn Indices / Lexical Addressing (de Bruijn 1972)

Local variable addressing by `(slot, depth)` pair. Eliminates runtime name lookup by resolving variable references to numeric coordinates at compile time. The depth counts enclosing scopes (parent chain hops); the slot indexes within a scope's binding array. This is a two-dimensional generalization of de Bruijn's nameless representation.

- **Paper**: N.G. de Bruijn, "Lambda calculus notation with nameless dummies, a tool for automatic formula manipulation, with application to the Church-Rosser theorem", *Indagationes Mathematicae*, 34:381-392, 1972
- **DOI**: https://doi.org/10.1016/1385-7258(72)90034-0
- **Also**: Harold Abelson, Gerald Jay Sussman, *Structure and Interpretation of Computer Programs*, 2nd edition, MIT Press, 1996, §5.5.6 "Lexical Addressing"
- **Also**: R. Kent Dybvig, *Three Implementation Models for Scheme*, PhD Dissertation, University of North Carolina, 1987
- **Location**: `machine/instruction.go` (EncodeLocalIndex, DecodeLocalIndex), `environment/environment_frame.go` (resolveLocal, GetLocalBindingBySlotDepth)

### Linked Closure Representation (Cardelli 1983)

Closures capture a pointer to the enclosing environment frame rather than copying free variables into a flat vector. Free variable access traverses parent pointers (O(depth)). This representation naturally supports mutable free variables (R7RS `set!`) without requiring explicit boxing. The trade-off: O(1) closure creation, O(depth) variable access — the natural complement to de Bruijn lexical addressing.

- **Paper**: Luca Cardelli, "The Functional Abstract Machine", *Polymorphism*, Vol. 1, No. 1, 1983
- **Also**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992, Chapter 10
- **ISBN** (Appel): 978-0-521-41695-5
- **Location**: `machine/operations_closure.go`, `machine/machine_closure.go`

### Direct-Style Compilation (Dybvig 1987)

Wile compiles Scheme directly to stack-machine bytecode without intermediate CPS or A-normal form conversion. The eval stack manages intermediate results that CPS would name with continuation lambdas or ANF with `let`-bindings. Tail position is tracked explicitly via `CompileTimeCallContext.inTail` during compilation.

- **Reference**: R. Kent Dybvig, *Three Implementation Models for Scheme*, PhD Dissertation, University of North Carolina, 1987, Chapter 3
- **Contrast**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992 (CPS approach)
- **Contrast**: Cormac Flanagan, Amr Sabry, Bruce F. Duba, Matthias Felleisen, "The Essence of Compiling with Continuations", PLDI 1993 (A-normal form)
- **Location**: `machine/compile_validated.go` (compileValidatedCall), `machine/compile_time_call_context.go`

### Environment Escape Analysis (Appel 1992)

Static bytecode scan to determine whether a closure's environment can escape its call. When it cannot (no continuation capture, no nested closure creation), the environment frame is reused in place rather than copied, eliminating allocation. **Removed in PR #561** — the optimization was unsafe under concurrent SRFI-18 thread invocation. Apply now always copies the env frame.

- **Reference**: Andrew W. Appel, *Compiling with Continuations*, Cambridge University Press, 1992, §10.3
- **Location (historical)**: `machine/native_template.go`, `machine/machine_context_apply.go`

### Lexical Scoping (Landin 1966, Strachey 1967)

Closures capture their lexical (definition-site) environment, not the dynamic (call-site) environment. This is the foundation of Scheme's scoping semantics (R7RS §1.1) and enables reasoning about variable binding from program text alone.

- **Paper**: Peter J. Landin, "The Next 700 Programming Languages", Communications of the ACM, Vol. 9, No. 3, 1966
- **Also**: Christopher Strachey, "Fundamental Concepts in Programming Languages", 1967 (reprinted 2000)
- **DOI** (Strachey reprint): https://doi.org/10.1023/A:1010000313106
- **Location**: `environment/environment_frame.go` (NewApplyFrame), `machine/machine_closure.go`

### Environment Model (Landin 1964)

Wile uses Landin's environment model for variable resolution: closures pair a code body with a captured environment frame, and variable lookup traverses the frame chain at runtime. This is opposed to the substitution model (Church 1941), where beta-reduction physically replaces formal parameters with arguments in the body. The environment model enables O(1) closure creation (just capture the current frame pointer) at the cost of O(depth) variable lookup.

- **Paper**: Peter J. Landin, "The Mechanical Evaluation of Expressions", 1964
- **Location**: `environment/environment_frame.go` (frame chain), `machine/machine_closure.go` (closure = template + env)

### Hash Consing / Symbol Interning (Goto 1974)

Symbol interning ensures that structurally equal symbols share a single pointer, enabling O(1) `eq?` comparison. Wile previously interned symbols per-Namespace but removed symbol interning in favor of string-key comparison via `helpers.EqIdentity` (symbols compare by `.Key` field). The historical references are retained for context.

- **Origin**: Eiichi Goto, "Monocopy and Associative Algorithms in an Extended Lisp", Technical Report 74-03, University of Tokyo, 1974
- **Earlier**: Andrei P. Ershov, "On Programming of Arithmetic Operations", Communications of the ACM, Vol. 1, No. 8, 1958
- **DOI** (Ershov): https://doi.org/10.1145/368892.368907
- **Location (historical)**: `environment/namespace.go` (InternSymbol, removed — symbols now compared by string key via `helpers.EqIdentity`)

### Superinstruction Formation (Ertl & Gregg 2003)

Fusing adjacent bytecode instructions into single superinstructions to reduce dispatch overhead. Each fusion eliminates one opcode decode and switch dispatch, which is the dominant cost in switch-dispatch interpreters. Wile fuses Load+Push → PushX, Pull+Apply → PullApply, and multi-instruction call sequences → CallForeignCached.

- **Paper**: M. Anton Ertl, David Gregg, "The Structure and Performance of Efficient Interpreters", *Journal of Instruction-Level Parallelism*, Vol. 5, pp. 1-25, 2003
- **URL**: http://www.jilp.org/vol5/v5paper12.pdf
- **Also**: M. Anton Ertl, David Gregg, "Optimizing Indirect Branch Prediction Accuracy in Virtual Machine Interpreters", PLDI 2003
- **DOI**: https://doi.org/10.1145/781131.781162
- **Location**: `machine/peephole.go` (fuseLoadPush, fusePullApply, fuseCallForeignCached)

### Peephole Optimization

Examines a small window of generated instructions and replaces inefficient patterns with shorter or faster equivalents. In Wile, the Push+BranchOnFalse+Pop sequence is replaced with a single BranchOnFalseValue that reads the value register directly.

- **Reference**: Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman, *Compilers: Principles, Techniques, and Tools*, 2nd edition, §8.9
- **ISBN**: 978-0-321-48681-3
- **Location**: `machine/operations_control.go`

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

This paper introduces the "sets of scopes" model for macro hygiene, which Wile uses for `syntax-rules` macro expansion. Each identifier carries a set of scopes, and variable resolution checks that the binding's scopes are a subset of the use site's scopes. Among multiple compatible bindings, the one with the largest scope set is selected (lattice maximality, §3.2). The scope sets form a bounded join-semilattice under the subset partial order, with the empty set as bottom element. `FlipScopeInSet` is symmetric difference (XOR in the Boolean ring Z/2Z^S), used by `syntax-local-introduce`.

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

Used in `values/utils.go` for `EqualTo()` on compound types (Pair, Vector). When a pointer pair is re-encountered during recursive comparison, it returns true (optimistic assumption). This is the same technique used by Chez Scheme and Racket for `equal?` on circular structures per R7RS §6.1. The formal basis is bisimulation equivalence — see "Bisimulation Equivalence for equal?" entry below.

### Split Value Register

A custom optimization that separates single-value and multi-value return paths. Nearly all Scheme operations produce one value; the `singleValue` field avoids allocating a `[]values.Value` slice for the common case.

- **Location**: `machine/vm_state.go`

### Two-Pass Datum Label Output

Implements R7RS §2.4 datum label notation for shared/circular structures. Pass 1 (`findShared`) traverses the value graph to identify multiply-referenced objects; pass 2 (`write`) emits `#n=` definitions on first encounter and `#n#` references thereafter.

- **Location**: `values/scheme_writer.go`

### Reflection-Based FFI Bridging

Pre-computes argument and return converters at registration time using Go's `reflect` package. Each call uses the cached converters to translate between Scheme values and Go types, avoiding per-call reflection overhead. This is a form of partial evaluation (Futamura 1971): the bridging logic is specialized at registration time against the known function type, producing cached converters that avoid per-call reflection.

- **Paper**: Yoshihiko Futamura, "Partial Evaluation of Computation Process — An Approach to a Compiler-Compiler", Systems, Computers, Controls, Vol. 2, No. 5, 1971. Reprinted in Higher-Order and Symbolic Computation, Vol. 12, No. 4, 1999
- **DOI**: https://doi.org/10.1023/A:1010095604496 (reprint)
- **Location**: `ffi.go`

### Hygienic Macro Expansion (Kohlbecker et al. 1986)

The original formulation of hygienic macro expansion: macro-introduced bindings must not inadvertently capture identifiers from the macro use site. Wile's intro scope mechanism (a fresh scope added to all expansion output) implements this property. Flatt's scope sets generalize this to arbitrary nesting.

- **Paper**: Eugene Kohlbecker, Daniel P. Friedman, Matthias Felleisen, Bruce Duba, "Hygienic Macro Expansion", LFP 1986
- **DOI**: https://doi.org/10.1145/319838.319859
- **Location**: `machine/operation_syntax_rules_transform.go` (introScope)

### Referential Transparency for Macros (Clinger & Rees 1991)

Extends hygiene with the dual property: free identifiers in a macro template must resolve to their definition-site bindings, not the use-site bindings. Wile implements this via `ResolvedBinding` on `SyntaxSymbol`, which captures the definition-time `GlobalIndex` for cross-library macro hygiene. Combined with the intro scope mechanism, this completes the two-sided hygiene contract.

- **Paper**: William Clinger, Jonathan Rees, "Macros That Work", POPL 1991
- **DOI**: https://doi.org/10.1145/99583.99607
- **Location**: `internal/syntax/syntax_symbol.go` (ResolvedBinding), `machine/compile_syntax_rules.go` (freeIds)

### Syntax Objects (Dybvig, Hieb & Bruggeman 1993)

Syntax objects are AST nodes decorated with binding information. Each parsed form is wrapped in a `SyntaxValue` carrying source location, scope sets, and macro expansion origin. The representation originates with Dybvig, Hieb, and Bruggeman, who introduced the concept for Chez Scheme's macro system. Wile's syntax objects are immutable: scope operations return new objects (persistent data structure discipline).

- **Paper**: R. Kent Dybvig, Robert Hieb, Carl Bruggeman, "Syntactic Abstraction in Scheme", Lisp and Symbolic Computation, Vol. 5, No. 4, 1993
- **DOI**: https://doi.org/10.1007/BF01806308
- **Location**: `internal/syntax/syntax_value.go`, `internal/syntax/syntax_symbol.go`, `internal/syntax/source_context.go`

### Alpha-Equivalence and the Variable Convention (Barendregt 1984)

Wile's scope sets address the same problem as Barendregt's variable convention (choosing bound variable names to avoid capture). Where alpha-conversion renames variables, scope sets tag identifiers with their binding context. Two identifiers with the same name but different scope sets are effectively alpha-inequivalent. The bidirectional subset check in `scopesCompatibleForSubstitution` is the scope-set analog of alpha-equivalence.

- **Book**: Henk P. Barendregt, *The Lambda Calculus: Its Syntax and Semantics*, revised edition, Studies in Logic, Vol. 103, North-Holland, 1984
- **ISBN**: 978-0-444-87508-2
- **Location**: `internal/syntax/syntax_value.go` (Scope type), `internal/match/syntax_expand.go` (scopesCompatibleForSubstitution)

### Reflective Tower (Smith 1984)

Wile's phase hierarchy (runtime/expand/compile) is a finite, statically-determined form of Smith's reflective tower. Phase 0 is the object level (program execution), phase 1 is the meta level (macros operating on program text), and phase 2 is the meta-meta level (syntax compilers operating on macro definitions). Unlike Smith's infinite tower in 3-Lisp, Wile's tower is finite and does not support arbitrary reification.

- **Paper**: Brian Cantwell Smith, "Reflection and Semantics in Lisp", POPL 1984
- **DOI**: https://doi.org/10.1145/800017.800513
- **Location**: `environment/phase_registry.go`

### Pattern Matching Compilation (Augustsson 1985)

Wile's pattern matching engine compiles R7RS `syntax-rules` patterns into bytecode at macro definition time, then executes the bytecode at each invocation. The compilation strategy produces a deterministic tree automaton that traverses the input in a fixed order. This is one-directional term matching (the input is ground, the pattern has holes), not bidirectional unification. Ellipsis patterns generate loop structures (SkipIfEmpty/Jump), making the automaton a form of regular tree pattern matcher with Kleene star.

- **Paper**: Lennart Augustsson, "Compiling Pattern Matching", in *Functional Programming Languages and Computer Architecture*, Springer LNCS 201, 1985
- **DOI**: https://doi.org/10.1007/3-540-15975-4_48
- **Location**: `internal/match/syntax_compiler.go`, `internal/match/match.go`

### Continuation-Wind Interaction (Clinger et al. 1999)

Formal treatment of how `dynamic-wind` extents interact with first-class continuations. The winding stack is separate from the continuation chain; invocation computes the common prefix of source and target stacks, then runs exit thunks (innermost-first) and entry thunks (outermost-first). The winding stack is captured by value at `call/cc` time but NOT stored per continuation frame — it is part of the dynamic extent, not the lexical continuation.

- **Paper**: William D. Clinger, Anne H. Hartheimer, Eric M. Ost, "Implementation Strategies for Continuations", Higher-Order and Symbolic Computation, Vol. 12, No. 1, pp. 7-45, 1999
- **DOI**: https://doi.org/10.1023/A:1010016816429
- **Location**: `machine/dynamic_wind.go` (FindCommonWindingPrefix), `machine/machine_context.go` (RestoreWithWindingFrom)

### Dynamic-Wind (Friedman & Haynes 1985)

Introduces `dynamic-wind` as a mechanism to constrain the effects of first-class continuations. When control enters or exits a dynamic extent, before/after thunks are called, enabling resource cleanup and proper interaction between continuations and side effects. Wile compiles `dynamic-wind` to inline bytecode (PushWind/PopWind operations).

- **Paper**: Daniel P. Friedman, Christopher T. Haynes, "Constraining Control", POPL 1985
- **DOI**: https://doi.org/10.1145/318593.318654
- **Location**: `machine/dynamic_wind.go`, `machine/compile_validated.go` (CompileValidatedDynamicWind)

### call/cc as Peirce's Law (Griffin 1990)

Shows that `call/cc` corresponds to Peirce's law `((A → B) → A) → A` under the Curry-Howard isomorphism, extending the correspondence from intuitionistic logic to classical logic. Adding `call/cc` to a language is equivalent to adding the law of excluded middle to the logic. The continuation received by the callback has type `A → B` (since invoking it never returns), making the overall type `((A → B) → A) → A`.

- **Paper**: Timothy G. Griffin, "A Formulae-as-Types Notion of Control", POPL 1990
- **DOI**: https://doi.org/10.1145/96709.96714
- **Location**: `registry/core/prim_control.go` (PrimCallCC)

### Numeric Promotion Lattice (Davey & Priestley 2002)

The numeric type promotion table is a finite join-semilattice (upper semilattice) over the product of two total orders: precision (Integer < BigInteger < Rational < Float < BigFloat) and complexity (Real < Complex). Each table entry is the join (least upper bound) of two numeric kinds. Commutativity is enforced by symmetric writes; the "no lossy promotions" invariant ensures monotonicity with respect to the exactness ordering.

- **Reference**: B. A. Davey, H. A. Priestley, *Introduction to Lattices and Order*, 2nd edition, Cambridge University Press, 2002
- **ISBN**: 978-0-521-78451-1
- **Location**: `values/promotion.go` (promotionTable, initPromotionTable), `values/numeric_lattice_test.go`

### Exactness as Abstract Interpretation (Cousot & Cousot 1977)

R7RS exactness tracking (exact/inexact contagion) is an instance of abstract interpretation. The abstract domain is the two-point lattice {exact < inexact}; the transfer function for arithmetic operations computes the abstract result from the abstract operands. The exact-zero-absorbs rule for multiplication (R7RS §6.2.2) is a strong update that returns a more precise result than the naive join.

- **Paper**: Patrick Cousot, Radhia Cousot, "Abstract interpretation: a unified lattice model for static analysis of programs by construction or approximation of fixpoints", POPL 1977
- **DOI**: https://doi.org/10.1145/512950.512973
- **Location**: `values/numeric_tower.go` (Exactness, ExactnessOf)

### Lists as Initial Algebras (Bird & de Moor 1997; Meijer et al. 1991)

Proper Scheme lists (chains of Pair cells terminated by EmptyList) form the initial algebra of the polynomial functor F(X) = 1 + Value × X. The empty list is the nil constructor; cons is the pair constructor. `ForEach` is the catamorphism (unique fold). The type-level separation of `EmptyList` from `*Pair` encodes the two constructors as distinct injections in the coproduct.

- **Reference**: Richard Bird, Oege de Moor, *Algebra of Programming*, Prentice Hall, 1997
- **ISBN**: 978-0-13-507245-5
- **Paper**: Erik Meijer, Maarten Fokkinga, Ross Paterson, "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire", FPCA 1991
- **DOI**: https://doi.org/10.1007/3540543961_7
- **Location**: `values/pair.go` (Pair, NewCons, ForEach), `values/empty_list.go` (emptyListType)

### Bisimulation Equivalence for equal? (Milner 1989)

The `equal?` predicate on cyclic structures (pairs, vectors) is bisimulation equivalence — the greatest fixpoint of the structural matching relation. The visited set implements optimistic coinduction: re-encountered pointer pairs are assumed equal, which correctly computes the greatest fixed point. This is the same technique used by Chez Scheme and Racket.

- **Reference**: Robin Milner, *Communication and Concurrency*, Prentice Hall, 1989, Ch. 5
- **ISBN**: 978-0-13-114984-7
- **Also**: Davide Sangiorgi, David Walker, *The Pi-Calculus: A Theory of Mobile Processes*, Cambridge University Press, 2001, Ch. 2
- **ISBN** (Sangiorgi): 978-0-521-78177-0
- **Location**: `values/utils.go` (EqualTo, equalToDeep)

### Multiple Dispatch via Dispatch Tables (Chambers & Chen 1999)

Numeric arithmetic uses pre-built dispatch tables indexed by `NumericKind`, implementing symmetric binary multimethods with coercion. Both operands are promoted to their least upper bound type before the operation. The dispatch matrix is materialized at init() time for O(1) runtime dispatch.

- **Paper**: Craig Chambers, Weimin Chen, "Efficient Multiple and Predicate Dispatching", OOPSLA 1999
- **DOI**: https://doi.org/10.1145/320384.320386
- **Location**: `values/promotion.go` (makeArithmeticDispatch)

### Units: Module System Foundations (Flatt & Felleisen 1998)

Wile's library isolation model (shared syntax interning, isolated binding stores) follows the "units" approach to modular linking. Each library has its own `GlobalEnvironmentFrame` for bindings but shares a `Namespace` for syntax interning. R7RS §6.5 symbol identity is ensured by string-key comparison via `helpers.EqIdentity`.

- **Paper**: Matthew Flatt, Matthias Felleisen, "Units: Cool Modules for HOT Languages", PLDI 1998
- **DOI**: https://doi.org/10.1145/277650.277730
- **Location**: `environment/namespace.go` (NewChildNamespace, NewChildRuntime)

### Exception Handling (Goodenough 1975)

The foundational paper on structured exception handling design. Identifies the key design dimensions: termination vs resumption semantics, dynamic vs lexical handler scope, and handler selection. Wile's exception model supports both termination (`raise`) and resumption (`raise-continuable`) with dynamic handler scope (`with-exception-handler` installs handlers with dynamic extent).

- **Paper**: John B. Goodenough, "Exception Handling: Issues and a Proposed Notation", Communications of the ACM, Vol. 18, No. 12, 1975
- **DOI**: https://doi.org/10.1145/361227.361230
- **Location**: `registry/core/prim_exceptions.go`

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
- **Location**: `docs/SANDBOXING.md` (extension-level sandboxing), `engine.go` (registry construction)

### Saltzer & Schroeder, "The Protection of Information in Computer Systems" (1975)

The original formal statement of the Principle of Least Authority (POLA), along with seven other design principles for protection mechanisms. Wile's `SafeExtensions()` applies POLA at the language level: grant only the capabilities needed for the task, nothing more. `WithoutCore()` takes this to the extreme — an engine with zero primitives.

- **Paper**: Jerome H. Saltzer, Michael D. Schroeder, "The Protection of Information in Computer Systems", Proceedings of the IEEE, Vol. 63, No. 9, 1975
- **DOI**: https://doi.org/10.1109/PROC.1975.9939
- **Location**: `options.go` (SafeExtensions, WithoutCore)

### Anderson, "Computer Security Technology Planning Study" (1972)

The original definition of the reference monitor concept: a mediation point that is always invoked, tamperproof, and complete. Wile's `security.Check()` implements this pattern — every privileged operation path calls `Check`, the authorizer is injected via `context.Context` (immutable after construction), and all gate sites are enumerated.

- **Report**: James P. Anderson, "Computer Security Technology Planning Study", ESD-TR-73-51, Air Force Electronic Systems Division, 1972
- **Location**: `security/context.go` (Check), `engine.go` (withAuth)

### Lampson, "A Note on the Confinement Problem" (1973)

Defines the confinement property of capability systems: a program cannot extend its own authority beyond what was granted at creation. Wile's `LibraryEnvFactory` closes over the engine's registry, ensuring transitive confinement — libraries cannot acquire capabilities not present in the engine. This is the "no amplification" property.

- **Paper**: Butler W. Lampson, "A Note on the Confinement Problem", Communications of the ACM, Vol. 16, No. 10, 1973
- **DOI**: https://doi.org/10.1145/362375.362389
- **Location**: `engine.go` (LibraryEnvFactory)

### Miller, "Robust Composition" (2006)

Formalizes the object-capability model and the principle of least authority (POLA). Wile's `WithSafeExtensions()` is POLA applied at the language level — grant only the capabilities needed, nothing more. Capability attenuation (`Registry.Without()`, `Registry.WithoutCategory()`) implements Miller's monotonicity property (§2.1): derived registries never have more authority than the source.

- **Paper**: Mark S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control", PhD Dissertation, Johns Hopkins University, 2006
- **URL**: http://www.erights.org/talks/thesis/

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

Canonical definitions for list processing procedures including `fold`. Wile's implementation in `stdlib/lib/srfi/1/` is from Chibi-Scheme.

- **URL**: https://srfi.schemers.org/srfi-1/srfi-1.html

### SRFI-9: Defining Record Types

Record type definitions, integrated into R7RS as `define-record-type`.

- **URL**: https://srfi.schemers.org/srfi-9/srfi-9.html

### SRFI-18: Multithreading Support

Threading primitives implemented in Wile: threads, mutexes, condition variables, and time objects. The model is shared-memory with mutual exclusion, following the monitor pattern (Hoare 1974): mutexes for mutual exclusion, condition variables for coordination.

- **URL**: https://srfi.schemers.org/srfi-18/srfi-18.html
- **See also**: Hoare, "Monitors: An Operating System Structuring Concept", CACM 1974

### SRFI-39: Parameter Objects

Dynamic parameters (`make-parameter`, `parameterize`) provide controlled dynamic binding in a lexically-scoped language. Parameters have lexical identity but dynamic extent. Wile's `parameterize` macro implements this via `dynamic-wind`, ensuring proper save/restore even in the presence of continuations.

- **URL**: https://srfi.schemers.org/srfi-39/srfi-39.html
- **Location**: `machine/parameter.go`, `registry/core/bootstrap.scm` (parameterize macro)

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

## Concurrency

### Hoare, "Communicating Sequential Processes" (1978)

Go channels are an implementation of Hoare's CSP. Wile's `gointerop` extension exposes Go channels as Scheme primitives, providing CSP-style synchronization through message passing — distinct from SRFI-18's shared-memory coordination via mutexes and condition variables.

- **Paper**: C. A. R. Hoare, "Communicating Sequential Processes", Communications of the ACM, Vol. 21, No. 8, 1978
- **DOI**: https://doi.org/10.1145/359576.359585
- **Location**: `extensions/gointerop/prim_gointerop.go` (channel primitives)

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

Source of portable Scheme library code used in Wile. The `stdlib/lib/chibi/` directory contains Chibi-Scheme's test framework, diff library, optional argument macros, and ANSI terminal library. The `stdlib/lib/srfi/1/` directory contains Chibi-Scheme's SRFI-1 list library implementation split into functional modules.

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
