# Plan: Quasisyntax Test Coverage

## Current State

**Coverage**: 37.5% overall, but critical functions at 0%:
- ❌ `compileQuasisyntaxTemplate` (0%) - Core compilation logic
- ❌ `quasisyntaxNeedsRuntime` (0%) - Compile-time vs runtime decision
- ❌ `expandQuasisyntax` (0%) - Recursive expansion with depth tracking
- ❌ `expandQuasisyntaxList` (0%) - List expansion and splicing

**Existing tests**: Only 3 error case tests (no args, unsyntax outside context, unsyntax-splicing outside context)

**Why critical**: SRFI-10 quasisyntax is a complete R7RS feature with complex semantics (depth tracking, splicing, hygiene) and zero real test coverage.

---

## Quasisyntax Semantics

From the code and SRFI-10:

### Core Concept
`quasisyntax` is like `quasiquote` but for syntax objects with hygiene:
```scheme
(quasiquote (+ 1 ,(* 2 3)))   ; => (+ 1 6)      - produces data
(quasisyntax (+ 1 #,(* 2 3))) ; => #'(+ 1 6)   - produces syntax
```

### Depth Tracking
- `unsyntax` (`#,expr`) evaluates `expr` at depth 1
- Nested `quasisyntax` increases depth
- `unsyntax` at depth > 1 becomes literal

Example:
```scheme
(quasisyntax (a #,(quasisyntax (b #,c))))
; depth=1: outer unsyntax NOT evaluated (depth 1 but inside another quasisyntax)
; depth=2: inner context, c not evaluated
; Result: syntax object representing (a (quasisyntax (b #,c)))
```

### Splicing
- `unsyntax-splicing` (`#,@expr`) splices a list into surrounding list
- Only works at depth 1
- Transforms list construction to use `append`

Example:
```scheme
(quasisyntax (a #,@(list 'b 'c) d))  ; => #'(a b c d)
```

---

## Test Strategy

### Phase 1: Unit Tests (Direct Function Testing)

Test the internal functions directly by constructing `CompileTimeContinuation` with test data.

#### Test File: `compile_quasisyntax_test.go`

Add to existing file (currently only has error tests).

### Phase 2: Integration Tests (End-to-End)

Test through the full compile-eval pipeline using `wile.Engine`.

#### Test File: `go/integration/quasisyntax_test.go` (new)

Use the pattern from other integration tests.

---

## Phase 1: Unit Test Cases

### 1. `quasisyntaxNeedsRuntime` Tests

**Function purpose**: Determines if template contains `unsyntax` at current depth (needs runtime evaluation) or is pure literal (compile-time only).

#### Test: `TestQuasisyntaxNeedsRuntime`

| Case | Input | Depth | Expected | Rationale |
|------|-------|-------|----------|-----------|
| Pure literal | `#'foo` | 1 | false | Symbol, no unsyntax |
| Pure list | `#'(a b c)` | 1 | false | List with no special forms |
| Unsyntax at depth 1 | `#'(a #,b)` | 1 | true | Has unsyntax at matching depth |
| Unsyntax at depth 2 | `#'(a #,b)` | 2 | false | unsyntax not at this depth |
| Nested quasisyntax | `#'(quasisyntax #,b)` | 1 | true | Body checked at depth+1 |
| Unsyntax-splicing | `#'(a #,@b)` | 1 | true | Splicing at depth 1 |
| Empty list | `#'()` | 1 | false | No elements to check |
| Nested unsyntax | `#'(quasisyntax (a #,b))` | 1 | true | Inner unsyntax at effective depth 1 |

**Implementation approach**:
- Create `SyntaxValue` test data using syntax constructors
- Call `ccnt.quasisyntaxNeedsRuntime(stx, depth)` directly
- Assert boolean result

### 2. `expandQuasisyntax` Tests

**Function purpose**: Transforms quasisyntax template into equivalent Scheme code using `list`, `append`, `syntax`, etc.

#### Test: `TestExpandQuasisyntax`

| Case | Input | Depth | Expected Expansion | Rationale |
|------|-------|-------|-------------------|-----------|
| Simple symbol | `#'foo` | 1 | `(syntax foo)` | Atoms wrap in syntax |
| Simple list | `#'(a b)` | 1 | `(list (syntax a) (syntax b))` | No unsyntax → list of literals |
| Unsyntax | `#'(a #,expr)` | 1 | `(list (syntax a) expr)` | Unsyntax evaluates expr at depth 1 |
| Nested list | `#'(a (b c))` | 1 | `(list (syntax a) (list (syntax b) (syntax c)))` | Recursive expansion |
| Unsyntax-splicing | `#'(a #,@xs b)` | 1 | `(append (list (syntax a)) xs (list (syntax b)))` | Splicing uses append |
| Nested quasisyntax | `#'(quasisyntax #,x)` | 1 | `(list (syntax quasisyntax) x)` | Body at depth+1 |
| Unsyntax at depth 2 | `#'(quasisyntax #,x)` | 2 | `(list (syntax quasisyntax) (syntax x))` | x literal at depth 2 |
| Multiple splice | `#'(#,@a #,@b)` | 1 | `(append a b)` | Multiple splices appended |
| Mixed splice | `#'(x #,@ys z)` | 1 | `(append (list (syntax x)) ys (list (syntax z)))` | Splice mixed with literals |

**Implementation approach**:
- Create syntax input
- Call `ccnt.expandQuasisyntax(stx, depth)`
- Compare resulting syntax structure (may need helper to compare syntax trees)

### 3. `expandQuasisyntaxList` Tests

**Function purpose**: Handles list-specific expansion, detecting splicing and choosing between `list`, `list*`, or `append`.

#### Test: `TestExpandQuasisyntaxList`

| Case | Input | Expected | Rationale |
|------|-------|----------|-----------|
| Simple list | `(a b c)` | `(list ...)` | No splicing → use list |
| Improper list | `(a . b)` | `(list* ...)` | Dotted pair → use list* |
| With splice | `(a #,@xs b)` | `(append ...)` | Splicing detected → use append |
| Only splice | `(#,@xs)` | `(append xs)` | Single splice element |
| Multiple splices | `(#,@a #,@b #,@c)` | `(append a b c)` | Multiple splices combined |

**Implementation approach**:
- Create `SyntaxPair` structures
- Call `ccnt.expandQuasisyntaxList(pair, depth)`
- Verify resulting syntax form

### 4. `compileQuasisyntaxTemplate` Tests

**Function purpose**: Top-level compilation entry. Decides between literal loading (fast path) or runtime expansion (complex path).

#### Test: `TestCompileQuasisyntaxTemplate`

| Case | Template | Expected Behavior | Rationale |
|------|----------|------------------|-----------|
| Pure literal | `#'foo` | Emits `LoadLiteral` op only | No unsyntax → compile-time |
| With unsyntax | `#'(a #,b)` | Emits compiled expansion code | Has unsyntax → runtime |
| Nested pure | `#'(a (b c))` | Emits `LoadLiteral` op only | Nested but no unsyntax |
| Nested with unsyntax | `#'(quasisyntax #,x)` | Emits compiled expansion | Needs runtime for inner quasisyntax body |

**Implementation approach**:
- Create `CompileTimeContinuation` with fresh template
- Call `compileQuasisyntaxTemplate(ctctx, stx, depth)`
- Inspect emitted operations (either single `LoadLiteral` or complex expansion)

---

## Phase 2: Integration Test Cases

### Test File: `go/integration/quasisyntax_test.go`

Use `wile.Engine` for end-to-end testing through parse → expand → compile → execute.

#### Test: `TestQuasisyntaxIntegration`

| Test Name | Scheme Code | Expected Result | Rationale |
|-----------|-------------|----------------|-----------|
| **Basic Forms** ||||
| Simple literal | `(quasisyntax foo)` | Syntax object `foo` | Simplest case |
| Simple list | `(quasisyntax (a b c))` | Syntax object `(a b c)` | List literal |
| Number literal | `(quasisyntax 42)` | Syntax object `42` | Self-evaluating |
| **Unsyntax** ||||
| Basic unsyntax | `(quasisyntax (+ 1 #,(* 2 3)))` | Syntax `(+ 1 6)` | Unsyntax evaluates |
| Multiple unsyntax | `(quasisyntax (+ #,(* 2 3) #,(/ 8 2)))` | Syntax `(+ 6 4)` | Multiple evaluations |
| Unsyntax in nested | `(quasisyntax (list #,(+ 1 2)))` | Syntax `(list 3)` | Nested position |
| **Unsyntax-splicing** ||||
| Basic splice | `(quasisyntax (a #,@(list 'b 'c) d))` | Syntax `(a b c d)` | Splices list |
| Empty splice | `(quasisyntax (a #,@'() b))` | Syntax `(a b)` | Empty list spliced |
| Multiple splices | `(quasisyntax (#,@'(a) #,@'(b c)))` | Syntax `(a b c)` | Multiple lists |
| Beginning splice | `(quasisyntax (#,@'(a b) c))` | Syntax `(a b c)` | At list start |
| End splice | `(quasisyntax (a #,@'(b c)))` | Syntax `(a b c)` | At list end |
| **Nested quasisyntax** ||||
| Depth 2 inner unsyntax | `(let ((x 5)) (quasisyntax (quasisyntax #,x)))` | Syntax with literal `#,x` | Inner unsyntax not evaluated |
| Depth 2 outer unsyntax | `(let ((x 5)) (quasisyntax (a #,(quasisyntax b))))` | Syntax `(a (quasisyntax b))` | Outer evaluates, inner literal |
| Double nested | `(quasisyntax (q (quasisyntax (r #,x))))` | Complex nesting | Depth tracking |
| **Edge Cases** ||||
| Empty list | `(quasisyntax ())` | Syntax `()` | Empty list preserved |
| Improper list | `(quasisyntax (a . b))` | Syntax `(a . b)` | Dotted pair |
| Unsyntax improper | `(quasisyntax (a . #,b))` | Syntax with evaluated cdr | Unsyntax in cdr position |
| Vector (if supported) | `(quasisyntax #(a #,b c))` | Syntax vector | Vector template |
| **With macros** ||||
| In macro definition | `(define-syntax m (lambda (stx) (quasisyntax ...)))` | Macro works | Real macro usage |
| Hygiene preservation | Test identifier scopes preserved | Scopes intact | Hygiene critical |
| **Error Cases** ||||
| No arguments | `(quasisyntax)` | Error | Validation |
| Too many args | `(quasisyntax a b)` | Error | Validation |
| Naked unsyntax | `(unsyntax x)` | Error | Outside quasisyntax |
| Naked splicing | `(unsyntax-splicing x)` | Error | Outside quasisyntax |

#### Implementation Pattern

```go
func TestQuasisyntaxIntegration(t *testing.T) {
    tests := []struct {
        name     string
        code     string
        expected string // SchemeString of result
    }{
        {
            name:     "basic literal",
            code:     `(quasisyntax foo)`,
            expected: `foo`, // syntax object prints as underlying
        },
        {
            name:     "basic unsyntax",
            code:     `(quasisyntax (+ 1 #,(* 2 3)))`,
            expected: `(+ 1 6)`,
        },
        // ... more cases
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            c := qt.New(t)
            engine, err := wile.NewEngine()
            c.Assert(err, qt.IsNil)

            result, err := engine.Eval(context.Background(), tt.code)
            c.Assert(err, qt.IsNil)
            c.Assert(result.SchemeString(), qt.Equals, tt.expected)
        })
    }
}
```

---

## Implementation Order

### Week 1: Foundation (8-12 hours)

1. **`quasisyntaxNeedsRuntime` tests** (2 hours)
   - 8 test cases covering depth tracking
   - Pure unit tests, easiest to write
   - File: `machine/compile_quasisyntax_test.go`

2. **`expandQuasisyntax` tests** (4 hours)
   - 10 test cases covering expansion rules
   - Need syntax tree comparison helper
   - Same file

3. **`expandQuasisyntaxList` tests** (2 hours)
   - 5 test cases for list-specific logic
   - Tests splicing detection
   - Same file

4. **`compileQuasisyntaxTemplate` tests** (4 hours)
   - 4 test cases checking operation emission
   - Verifies fast path vs expansion path
   - Same file

**Estimated coverage gain**: Should bring quasisyntax from 37.5% to **80-85%**

### Week 2: Integration (6-8 hours)

5. **Integration tests** (6-8 hours)
   - 25+ end-to-end test cases
   - Real Scheme code through full pipeline
   - File: `integration/quasisyntax_test.go` (new)
   - Tests hygiene, macro interaction, error handling

**Estimated coverage gain**: Exercises code paths missed by unit tests, should reach **90%+**

---

## Test Helpers Needed

### 1. Syntax Tree Equality

```go
// compareSyntax checks if two syntax objects are structurally equal
func compareSyntax(a, b syntax.SyntaxValue) bool {
    // Handle nil cases
    // Compare types
    // For pairs: recursively compare car/cdr
    // For symbols: compare names
    // For atoms: compare values
}
```

### 2. Template Operation Inspector

```go
// getEmittedOperations returns all operations in the template
func getEmittedOperations(ccnt *CompileTimeContinuation) []Operation {
    return ccnt.template.operations
}

// hasOnlyLoadLiteral checks if template only has LoadLiteral operation
func hasOnlyLoadLiteral(ccnt *CompileTimeContinuation) bool {
    ops := getEmittedOperations(ccnt)
    return len(ops) == 1 && isLoadLiteralOp(ops[0])
}
```

### 3. Syntax Builders

```go
// buildTestQuasisyntax creates test quasisyntax forms
func buildTestQuasisyntax(template string) syntax.SyntaxValue {
    // Parse and convert to syntax
}

// buildTestUnsyntax creates (unsyntax expr) form
func buildTestUnsyntax(expr syntax.SyntaxValue) syntax.SyntaxValue {
    // Create pair: (unsyntax expr)
}
```

---

## Coverage Goals

| Component | Current | Target | Strategy |
|-----------|---------|--------|----------|
| `compile_quasisyntax.go` | 37.5% | 90%+ | Unit + integration tests |
| `quasisyntaxNeedsRuntime` | 0% | 100% | 8 unit tests |
| `expandQuasisyntax` | 0% | 95% | 10 unit + integration |
| `expandQuasisyntaxList` | 0% | 95% | 5 unit + integration |
| `compileQuasisyntaxTemplate` | 0% | 90% | 4 unit + integration |

---

## Risk Areas

### 1. **Hygiene Interaction**
   - Quasisyntax must preserve scope sets
   - `datum->syntax` wrapping in compilation
   - Test: Identifiers from different contexts stay distinct

### 2. **Depth Tracking Edge Cases**
   - Triple-nested quasisyntax
   - Unsyntax at each depth level
   - Test: `(quasisyntax (quasisyntax (quasisyntax #,#,#,x)))`

### 3. **Splicing with Improper Lists**
   - What happens with `#,@` in dotted pair position?
   - Test: `(quasisyntax (a #,@'(b c) . d))`

### 4. **Empty Cases**
   - Empty list: `(quasisyntax ())`
   - Empty splice: `#,@'()`
   - Test both compile and run correctly

---

## Verification

After implementation, verify:

```bash
cd go && go test -cover ./machine/ -run Quasisyntax
# Should show ~90% coverage for compile_quasisyntax.go

cd go && go test -v ./integration/ -run Quasisyntax
# All integration tests pass

cd go && make cover
# Overall machine package stays at or above 80%
```

---

## Success Criteria

- ✅ All 4 core functions have direct unit tests
- ✅ `quasisyntaxNeedsRuntime`: 100% coverage
- ✅ `expandQuasisyntax`: 95%+ coverage
- ✅ `expandQuasisyntaxList`: 95%+ coverage
- ✅ `compileQuasisyntaxTemplate`: 90%+ coverage
- ✅ 25+ integration tests covering real-world usage
- ✅ Hygiene preservation verified
- ✅ Nested depth tracking verified
- ✅ Splicing edge cases verified
- ✅ Overall `compile_quasisyntax.go`: 90%+ coverage

---

## Notes

- **Why this matters**: Quasisyntax is used by advanced macro writers for template construction with hygiene. Bugs here break macro reliability.
- **SRFI-10 reference**: https://srfi.schemers.org/srfi-10/srfi-10.html
- **Similar to quasiquote**: Can reference `compile_quasiquote.go` and its tests as a model (if they exist)
- **Racket compatibility**: Wile's quasisyntax follows Racket's model (Flatt's scope sets)
