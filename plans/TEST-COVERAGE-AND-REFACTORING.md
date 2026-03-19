# Test Coverage & Refactoring Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Complete all high and medium TODO items: machine/ test coverage (52 files), engine.go tests, REPL tests, type switch exhaustiveness linter, special form dual-dispatch unification.

**Architecture:** 12 phases grouped by testability. machine/ files are batched by concern (types, operations, compilation, expansion, VM runtime, library, macros, infrastructure). Each phase produces test files following existing patterns (`package machine`, `qt` assertions, table-driven). Refactoring phases (11-12) come last to avoid changing code under test.

**Tech Stack:** Go 1.24, quicktest (`qt`), `valuestest.SchemeEquals`, `testhelpers.RunSchemeCode`

---

## Conventions

All machine/ tests use:

```go
package machine

import (
    "testing"
    qt "github.com/frankban/quicktest"
)
```

Additional imports as needed: `context`, `values`, `valuestest`, `werr`, `errors`, `environment`, `internal/syntax`.

**Table-driven pattern** per `registry/CLAUDE.md` — mandatory for all tests. **No single-line function definitions** per `CLAUDE.md`.

After each phase: `make lint && make test ./machine/... && make covercheck`

---

## Phase 1: Small Types & Utilities (13 files)

Direct Go unit tests for self-contained types.

### Task 1.1: prompt_tag.go, prompt_abort.go, barrier_token.go

**Files:**
- Test: `machine/prompt_tag_test.go`
- Test: `machine/prompt_abort_test.go`
- Test: `machine/barrier_token_test.go`
- Read: `machine/prompt_tag.go`, `machine/prompt_abort.go`, `machine/barrier_token.go`

**What to test:**
- `PromptTag`: `NewPromptTag()` returns unique tags (pointer inequality), `SchemeString()` format, `IsVoid() == false`, `EqualTo` (self vs other), `DefaultPromptTag` exists
- `ErrPromptAbort`: construction, `Error()` message, `Tag()` accessor, `Handler()` accessor, `Values()` accessor
- `BarrierToken`: `NewBarrierToken()` uniqueness, zero value

### Task 1.2: arity.go, closure.go

**Files:**
- Test: `machine/arity_test.go`
- Test: `machine/closure_test.go`
- Read: `machine/arity.go`, `machine/closure.go`

**What to test:**
- Arity: read the exported functions and test each. Focus on edge cases: 0 params, variadic with 0 required, exact match, mismatch errors
- Closure: interface compliance (if it defines an interface, test implementations satisfy it)

### Task 1.3: dynamic_wind.go

**Files:**
- Test: `machine/dynamic_wind_test.go`
- Read: `machine/dynamic_wind.go`

**What to test:**
- `DynamicWindFrame`: construction, field accessors
- `WindingStack`: `Copy()` independence, `Push()`/`Pop()` LIFO behavior, `Len()`, empty stack behavior
- `FindCommonWindingPrefix()`: same stack, disjoint stacks, shared prefix, nil inputs

### Task 1.4: vm_state.go, compile_time_call_context.go

**Files:**
- Test: `machine/vm_state_test.go`
- Test: `machine/compile_time_call_context_test.go`
- Read: `machine/vm_state.go`, `machine/compile_time_call_context.go`

**What to test:**
- `vmState`: field accessors if exported, zero value behavior
- `CompileTimeCallContext`: `InTail()`, `IsTopLevel()`, `IsExpression()`, `WithTail()`, `WithExpression()`, etc. — test that state transitions produce correct flags

### Task 1.5: named_handler_base.go, captured_continuation.go

**Files:**
- Test: `machine/named_handler_base_test.go`
- Test: `machine/captured_continuation_test.go`
- Read: `machine/named_handler_base.go`, `machine/captured_continuation.go`

**What to test:**
- `NamedHandlerBase`: name accessor, SchemeString
- `CapturedContinuation`: construction, `SchemeString()`, `IsVoid()`, `EqualTo()` (identity), `AcceptsArity()`, field accessors

### Task 1.6: operation_helpers.go, operations.go, phase_registry.go

**Files:**
- Test: `machine/operation_helpers_test.go`
- Test: `machine/operations_test.go` (extend existing if present, else create)
- Test: `machine/phase_registry_test.go`
- Read: `machine/operation_helpers.go`, `machine/operations.go`, `machine/phase_registry.go`

**What to test:**
- `sameType()`: same types, different types, nil inputs
- `fieldMatches()`: matching and non-matching fields
- `Operations` slice type: `EqualTo`, `SchemeString`, `Len`
- `RegisterPhaseBindings` / `LookupPhaseBinding`: register + lookup round-trip, missing key returns nil

### Task 1.7: Verify Phase 1

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestPromptTag|TestPromptAbort|TestBarrier|TestArity|TestClosure|TestDynamicWind|TestVmState|TestCompileTimeCallContext|TestNamedHandler|TestCapturedContinuation|TestOperationHelper|TestOperations|TestPhaseRegistry'`

Verify: all pass, no lint errors.

---

## Phase 2: Operation Types (6 files)

Test operation construction, `EqualTo()`, and `SchemeString()`. Follow the pattern in existing `machine/operation_test.go`.

### Task 2.1: operations_stack.go, operations_control.go

**Files:**
- Test: `machine/operations_stack_test.go`
- Test: `machine/operations_control_test.go`
- Read: `machine/operations_stack.go`, `machine/operations_control.go`
- Reference: `machine/operation_test.go` for existing patterns

**What to test per operation type:**
- Constructor returns non-nil
- `SchemeString()` contains operation name
- `IsVoid() == false`
- `EqualTo(self) == true`
- `EqualTo(different) == false`
- `EqualTo(nil) == false`

Stack ops: `OperationPush`, `OperationPop`, `OperationPull`, `OperationDrop`, `OperationPeekK`
Control ops: `OperationBranch`, `OperationBranchOnFalse`, `OperationSaveContinuation`, `OperationRestoreContinuation`

### Task 2.2: operations_load_store.go, operations_call.go

**Files:**
- Test: `machine/operations_load_store_test.go`
- Test: `machine/operations_call_test.go`
- Read: `machine/operations_load_store.go`, `machine/operations_call.go`

Load/store ops: `OperationLoadLiteral`, `OperationLoadLocal`, `OperationLoadGlobal`, `OperationStoreLocal`, `OperationStoreGlobal`, `OperationLoadVoid`
Call ops: `OperationApply`, `OperationForeignFunctionCall`, `OperationUnpackListToStack`

### Task 2.3: operations_closure.go, operations_winding.go

**Files:**
- Test: `machine/operations_closure_test.go`
- Test: `machine/operations_winding_test.go`
- Read: `machine/operations_closure.go`, `machine/operations_winding.go`

Closure ops: `OperationMakeClosure`, `OperationMakeCaseLambdaClosure`
Winding ops: `OperationPushWind`, `OperationPopWind`, `OperationPopEnv`

### Task 2.4: Verify Phase 2

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestOperationsStack|TestOperationsControl|TestOperationsLoadStore|TestOperationsCall|TestOperationsClosure|TestOperationsWinding'`

---

## Phase 3: VM Runtime (6 files)

Mix of direct Go tests and Scheme-level behavioral tests.

### Task 3.1: call_promoted.go, call_promoted_arithmetic.go

**Files:**
- Test: `machine/call_promoted_test.go`
- Test: `machine/call_promoted_arithmetic_test.go`
- Read: `machine/call_promoted.go`, `machine/call_promoted_arithmetic.go`

**What to test:**
- Promoted predicate ops via Scheme: `(null? '())`, `(null? 1)`, `(pair? '(1))`, `(pair? 1)`, `(eq? 'a 'a)`, `(eq? 'a 'b)`
- Promoted accessor ops via Scheme: `(car '(1 2))`, `(cdr '(1 2))`, `(vector-ref #(a b c) 1)`
- Promoted arithmetic via Scheme: `(+ 1 2)`, `(- 5 3)`, `(* 2 3)`, `(/ 6 2)`, `(< 1 2)`, `(> 2 1)`, `(= 1 1)`, `(<= 1 1)`, `(>= 2 1)`
- Promoted `cons` via Scheme: `(cons 1 2)`, `(cons 1 '())`
- Error paths: `(car 5)` type error, `(/ 1 0)` division by zero
- Fallback to non-promoted path: variadic `(+ 1 2 3)` (3 args, promoted only handles 2)

Use `testhelpers.RunSchemeCode` with table-driven `SchemeCodeTestCase` and `SchemeCodeErrorTestCase`.

### Task 3.2: machine_context_apply.go

**Files:**
- Test: `machine/machine_context_apply_test.go`
- Read: `machine/machine_context_apply.go`

**What to test via Scheme:**
- Apply MachineClosure: `((lambda (x) x) 42)`
- Apply CaseLambdaClosure: `((case-lambda ((x) x) ((x y) (+ x y))) 1 2)`
- Apply Parameter: `(let ((p (make-parameter 10))) (p))`
- Apply non-callable error: `(1 2 3)` → error
- Arity mismatch: `((lambda (x) x) 1 2)` → error

### Task 3.3: machine_context_continuation.go, machine_context_winding.go

**Files:**
- Test: `machine/machine_context_continuation_test.go`
- Test: `machine/machine_context_winding_test.go`
- Read: `machine/machine_context_continuation.go`, `machine/machine_context_winding.go`

**What to test via Scheme:**
- Continuation save/restore: `(call/cc (lambda (k) (k 42)))` → 42
- Re-invocation: `(let ((k #f)) (+ 1 (call/cc (lambda (c) (set! k c) 0))) )` then `(k 10)` → 11
- Delimited continuations: `(call-with-continuation-prompt (lambda () (abort-current-continuation (default-continuation-prompt-tag) 42)) (default-continuation-prompt-tag) (lambda (v) v))` → 42
- Dynamic-wind ordering: verify before/after thunk execution order with `(let ((log '())) (dynamic-wind (lambda () (set! log (cons 'before log))) (lambda () (set! log (cons 'during log))) (lambda () (set! log (cons 'after log)))) (reverse log))` → `(before during after)`
- Wind + call/cc interaction: capture inside dynamic-wind, invoke outside, verify before/after run

### Task 3.4: machine_context_subcontext.go

**Files:**
- Test: `machine/machine_context_subcontext_test.go`
- Read: `machine/machine_context_subcontext.go`

**What to test via Scheme:**
- Sub-context creation is exercised by `apply`: `(apply + '(1 2 3))` → 6
- `call-with-values` exercises sub-context: `(call-with-values (lambda () (values 1 2)) +)` → 3
- Winding stack inheritance: `(let ((log '())) (dynamic-wind (lambda () (set! log (cons 'b log))) (lambda () (apply (lambda () (set! log (cons 'd log))) '())) (lambda () (set! log (cons 'a log)))) (reverse log))` → includes 'b and 'a

### Task 3.5: Verify Phase 3

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestCallPromoted|TestMachineContextApply|TestMachineContextContinuation|TestMachineContextWinding|TestMachineContextSubcontext'`

---

## Phase 4: Compilation (11 files)

Scheme-level tests exercising compilation through the full pipeline.

### Task 4.1: compile_closure.go, compile_helpers.go

**Files:**
- Test: `machine/compile_closure_test.go`
- Test: `machine/compile_helpers_test.go`
- Read: `machine/compile_closure.go`, `machine/compile_helpers.go`

**What to test:**
- Closure compilation: `((lambda (x) (lambda () x)) 42)` → closure that returns 42 when called
- Nested closures: `(let ((f (lambda (x) (lambda (y) (+ x y))))) ((f 10) 20))` → 30
- Compile helpers: test through expressions that exercise the helper functions (literal compilation, symbol resolution)

### Task 4.2: compile_cond_expand.go, compile_import.go

**Files:**
- Test: `machine/compile_cond_expand_test.go`
- Test: `machine/compile_import_test.go`
- Read: `machine/compile_cond_expand.go`, `machine/compile_import.go`

**What to test:**
- `cond-expand`: `(cond-expand (r7rs 'yes) (else 'no))` → yes
- `cond-expand` with `and`/`or`/`not`: `(cond-expand ((and r7rs wile) 'both) (else 'no))` → both
- `cond-expand` fallthrough to else: `(cond-expand (nonexistent 'no) (else 'yes))` → yes
- Import: tested via library system tests in Phase 6

### Task 4.3: compile_define_syntax.go, compile_er_macro.go

**Files:**
- Test: `machine/compile_define_syntax_test.go`
- Test: `machine/compile_er_macro_test.go`
- Read: `machine/compile_define_syntax.go`, `machine/compile_er_macro.go`

**What to test:**
- `define-syntax` with `syntax-rules`: basic macro definition and expansion
- ER macros: `(define-syntax my-if (er-macro-transformer ...))` then use

### Task 4.4: compile_time_continuation_quasiquote.go, compile_time_continuation_include.go, compile_time_continuation_library.go

**Files:**
- Test: `machine/compile_time_continuation_quasiquote_test.go`
- Test: `machine/compile_time_continuation_include_test.go`
- Test: `machine/compile_time_continuation_library_test.go`
- Read: the three source files

**What to test:**
- Quasiquote: `` `(a ,(+ 1 2) b) `` → `(a 3 b)`, `` `(a ,@(list 1 2) b) `` → `(a 1 2 b)`, nested quasiquote
- Include: requires file on disk — test via library system or skip if existing library tests cover it
- Library body compilation: tested via Phase 6 library tests

### Task 4.5: compile_library_forms.go

**Files:**
- Test: `machine/compile_library_forms_test.go`
- Read: `machine/compile_library_forms.go`

**What to test:**
- `define-library` compilation: basic library definition with `export`, `begin`, `import`
- Error: malformed library declaration

### Task 4.6: Verify Phase 4

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestCompileClosure|TestCompileHelpers|TestCompileCondExpand|TestCompileImport|TestCompileDefineSyntax|TestCompileErMacro|TestCompileQuasiquote|TestCompileInclude|TestCompileLibraryBody|TestCompileLibraryForms'`

---

## Phase 5: Expansion (5 files)

Scheme-level tests verifying macro expansion behavior.

### Task 5.1: expander_primitive_forms.go, expander_body.go

**Files:**
- Test: `machine/expander_primitive_forms_test.go`
- Test: `machine/expander_body_test.go`
- Read: `machine/expander_primitive_forms.go`, `machine/expander_body.go`

**What to test:**
- Primitive form expansion: `if`, `begin`, `set!`, `define`, `quote`, `import` each expand their subforms correctly
- Test via behavior: `(if #t 'yes 'no)` → yes, `(begin 1 2 3)` → 3, `(let ((x 1)) (set! x 2) x)` → 2
- Body with internal defines: `(let () (define x 1) (define y 2) (+ x y))` → 3
- Body with internal `define-syntax`: `(let () (define-syntax my-const (syntax-rules () ((_ v) v))) (my-const 42))` → 42

### Task 5.2: expander_lambda.go, expander_let_syntax.go

**Files:**
- Test: `machine/expander_lambda_test.go`
- Test: `machine/expander_let_syntax_test.go`
- Read: `machine/expander_lambda.go`, `machine/expander_let_syntax.go`

**What to test:**
- Lambda expansion: `(lambda (x y) (+ x y))` callable, `(lambda x x)` rest params, `(lambda (x . rest) rest)` dotted
- `let-syntax`: `(let-syntax ((double (syntax-rules () ((_ x) (+ x x))))) (double 5))` → 10
- `letrec-syntax`: mutually recursive macros
- `let-syntax` shadowing: local macro shadows outer

### Task 5.3: quasi_expand.go

**Files:**
- Test: `machine/quasi_expand_test.go`
- Read: `machine/quasi_expand.go`

**What to test:**
- Quasiquote expansion: `` `(a b c) `` → `(a b c)`
- Unquote: `` `(a ,(+ 1 2) c) `` → `(a 3 c)`
- Unquote-splicing: `` `(a ,@(list 1 2) c) `` → `(a 1 2 c)`
- Nested: `` `(a `(b ,(+ 1 2))) `` → `` `(a (b ,(+ 1 2))) `` (inner quasiquote not evaluated)
- Vector quasiquote: `` `#(1 ,(+ 1 1) 3) `` → `#(1 2 3)`

### Task 5.4: Verify Phase 5

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestExpanderPrimitiveForms|TestExpanderBody|TestExpanderLambda|TestExpanderLetSyntax|TestQuasiExpand'`

---

## Phase 6: Library System (3 files)

### Task 6.1: library_registry.go, library_bindings.go, library_loader.go

**Files:**
- Test: `machine/library_registry_test.go`
- Test: `machine/library_bindings_test.go`
- Test: `machine/library_loader_test.go`
- Read: the three source files

**What to test:**
- Library name parsing and comparison
- Library registration and lookup
- Export/import binding resolution
- Behavioral tests via Scheme: define a library, import from it, verify bindings visible
- Error: import nonexistent library, duplicate export, missing export

### Task 6.2: Verify Phase 6

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestLibrary'`

---

## Phase 7: Macro Runtime (3 files)

### Task 7.1: operation_syntax_rules_transform.go, er_macro_rename.go, er_macro_compare.go

**Files:**
- Test: `machine/operation_syntax_rules_transform_test.go`
- Test: `machine/er_macro_rename_test.go`
- Test: `machine/er_macro_compare_test.go`
- Read: the three source files

**What to test:**
- Syntax-rules transform: basic pattern match + template expansion, ellipsis patterns, literal matching
- ER macro rename: renamed identifiers preserve hygiene
- ER macro compare: identifier comparison semantics
- All tested via Scheme macros exercising these paths

### Task 7.2: Verify Phase 7

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestSyntaxRulesTransform|TestErMacroRename|TestErMacroCompare'`

---

## Phase 8: Infrastructure (5 files)

### Task 8.1: register.go, primitive_expanders_registry.go, syntax_compilers_registry.go, letrec_semantics.go, primitive_expander.go

**Files:**
- Test: `machine/register_test.go`
- Test: `machine/primitive_expanders_registry_test.go`
- Test: `machine/syntax_compilers_registry_test.go`
- Test: `machine/letrec_semantics_test.go`
- Test: `machine/primitive_expander_test.go`
- Read: the five source files

**What to test:**
- Registration: after init(), all expected forms are registered (spot-check 5-10 form names)
- Primitive expanders: registry populated, lookup returns non-nil for `if`, `begin`, `lambda`, `quote`
- Syntax compilers: registry populated, lookup returns non-nil for `define-syntax`, `syntax-case`, `import`
- Letrec semantics: `(letrec ((x 1) (y (+ x 1))) y)` — test letrec* sequential evaluation
- PrimitiveExpander: type satisfies Value interface, SchemeString format

### Task 8.2: Verify Phase 8

Run: `make lint && go test -v -count=1 ./machine/... -run 'TestRegister|TestPrimitiveExpandersRegistry|TestSyntaxCompilersRegistry|TestLetrecSemantics|TestPrimitiveExpander'`

---

## Phase 9: engine.go Unit Tests

### Task 9.1: Engine core API

**Files:**
- Create: `engine_unit_test.go`
- Read: `engine.go`
- Reference: `wile_test.go`, `engine_error_chain_test.go` for existing patterns

**What to test (table-driven):**
- `Eval`: `(+ 1 2)` → 3, `"hello"` → "hello", `#t` → #t
- `EvalMultiple`: `(define x 1) (define y 2) (+ x y)` → 3
- `Compile` + `Run`: compile `(+ 1 2)`, run it, get 3; run again, still 3
- `Define` + `Get`: define `x` = 42, get `x` → 42; get `y` → not found
- `Call`: define a procedure, call it from Go
- `Close`: close engine, verify no panic
- Error wrapping: eval `(/ 1 0)` → RuntimeError, eval `(if)` → CompilationError
- Options: `WithMaxCallDepth(5)` then deep recursion → error

### Task 9.2: Verify Phase 9

Run: `make lint && go test -v -count=1 -run 'TestEngine' .`

---

## Phase 10: REPL Test Coverage

### Task 10.1: DebugContext tests

**Files:**
- Create: `internal/repl/debug_test.go`
- Read: `internal/repl/debug.go`

**What to test:**
- `NewDebugContext()` returns non-nil
- `HandleDebugCommand(",break file:10", out)` → breakpoint added, verify via `Debugger().Breakpoints()`
- `HandleDebugCommand(",list", out)` → output lists breakpoints
- `HandleDebugCommand(",delete 1", out)` → breakpoint removed
- `HandleDebugCommand(",enable 1", out)` / `",disable 1"` → breakpoint state changes
- `HandleDebugCommand(",step", out)` → step mode set
- `HandleDebugCommand(",continue", out)` → step mode cleared
- Unknown command → returns false
- `DebugCommands()` returns non-empty list with expected names

### Task 10.2: MetaCommandHandler extended tests

**Files:**
- Modify: `internal/repl/meta_test.go` (extend existing)
- Read: `internal/repl/meta.go`

**What to test (additions to existing tests):**
- `Commands()` returns all expected command names
- `Handle` with debug commands delegates to DebugContext
- `Handle` with unknown command returns false
- `,help` with specific command name → targeted help output

### Task 10.3: Verify Phase 10

Run: `make lint && go test -v -count=1 ./internal/repl/...`

---

## Phase 11: Type Switch Exhaustiveness Linter

### Task 11.1: Design the linter

**Files:**
- Create: `cmd/typeswitchlint/main.go`

**Approach:** Use `go/ast` + `go/types` to:
1. Find all `switch v := x.(type)` statements in `*.go` files
2. For each switch, collect the set of `case` types
3. Collect all concrete types implementing `values.Value` and `values.Number`
4. Report switches that are missing types AND lack a `default:` that returns/panics with an error

**What to implement:**
- Parse Go files with `go/parser`
- Walk AST for `TypeSwitchStmt` nodes
- Extract case types from each switch
- Load type info with `go/types` to find all `values.Value` implementors
- Compare and report gaps
- Integrate with `go generate` directive in `values/values.go`

### Task 11.2: Implement and test

**Files:**
- Create: `cmd/typeswitchlint/main.go`
- Create: `cmd/typeswitchlint/main_test.go`
- Modify: `values/values.go` — add `//go:generate go run ../cmd/typeswitchlint` directive

**Test approach:** Create test fixtures with known gaps, verify the tool reports them.

### Task 11.3: Verify Phase 11

Run: `go run ./cmd/typeswitchlint ./...` and verify output is reasonable (may report known gaps in extension code).

---

## Phase 12: Special Form Dual-Dispatch Unification

### Task 12.1: Understand current state

**Read:**
- `internal/forms/form_spec.go` — already has `FormSpec` with both `Validate` and `Compile` fields
- `internal/validate/register.go` — calls `forms.RegisterValidator` in `init()`
- `machine/register.go` — calls `forms.RegisterCompiler` in `init()`

**Key insight:** `internal/forms/form_spec.go` already defines `FormSpec` with both fields. The two `init()` functions populate the same registry from different sides. The unification is about making this explicit: register both validator and compiler in a single call at a single site.

### Task 12.2: Unify registration

**Files:**
- Modify: `internal/forms/form_spec.go` — add `RegisterForm(name, validator, compiler)` that sets both at once
- Modify: `internal/validate/register.go` — remove `init()`, export registration data as a slice
- Modify: `machine/register.go` — consume the exported slice, register both validator and compiler together
- Create: `internal/forms/form_spec_test.go` (or extend existing)

**Constraints:**
- Must not break import cycles: `validate` cannot import `machine`, `machine` cannot import `validate`
- The `forms` package is the bridge — both sides register into it
- Solution: keep the two `init()` functions but add a verification test that checks every form with a validator also has a compiler and vice versa (for non-passthrough forms)

**Revised approach — verification over unification:**
Given the import cycle constraint, full unification requires moving all registration to a third package, which adds complexity without clear benefit. Instead:
1. Add `forms.Verify() error` that checks validator/compiler pairing
2. Call it in a test: `TestFormRegistrationConsistency`
3. This catches the "forgot to add the compiler" class of bug at test time

### Task 12.3: Implement verification

**Files:**
- Modify: `internal/forms/form_spec.go` — add `Verify() error`
- Create: `internal/forms/form_spec_test.go`
- Read: `internal/validate/register.go` for the list of passthrough forms (these legitimately lack typed compilers)

**What `Verify()` checks:**
- Every form with a validator has a compiler
- Every form with a compiler has a validator
- Returns error listing mismatched forms

**Test:** `TestFormRegistrationConsistency` calls `Verify()` and asserts no error.

### Task 12.4: Verify Phase 12

Run: `make lint && go test -v -count=1 ./internal/forms/... && go test -v -count=1 ./machine/... && make covercheck`

---

## Final Verification

After all phases:

```bash
make lint && make test && make covercheck
```

Update `TODO.md`: mark all 5 items as done.
