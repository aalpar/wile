# Expander Time Continuation Decomposition

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Split `machine/expander_time_continuation.go` (1,327 lines) into 4 focused files without changing any behavior.

**Architecture:** Pure file reorganization. All functions stay in package `machine`. No renames, no signature changes, no logic changes. The only code change is updating the TODO.md checkbox.

**Tech Stack:** Go (same package split — no import changes needed between new files)

**Cross-reference map** (functions used across file boundaries):

| Function | Defined in | Called from |
|----------|-----------|-------------|
| `isSyntaxFormWithKeyword` | `expander_let_syntax.go` | `expander_let_syntax.go`, `expander_body.go` |
| `collectBodyExpressions` | `expander_lambda.go` | `expander_lambda.go`, `expander_primitive_forms.go` (expandBeginForm) |
| `extractIdentifierList` | `expander_lambda.go` | `expander_primitive_forms.go` (expandWithBindingScope) |

All cross-references are within package `machine` — no visibility issues.

---

### Task 1: Create branch

**Step 1: Create feature branch from master**

```bash
git checkout master
git pull --rebase
git checkout -b refactor/expander-decomposition
```

**Step 2: Verify clean state**

```bash
git status
```

Expected: clean working tree on `refactor/expander-decomposition`

---

### Task 2: Create `expander_let_syntax.go`

Extract let-syntax/letrec-syntax expansion and its helper.

**Files:**
- Create: `machine/expander_let_syntax.go`
- Source lines from `machine/expander_time_continuation.go`: 195–444

**Contents (functions to move):**

| Function | Current lines | Size |
|----------|--------------|------|
| `expandLetSyntax` | 195–197 | 3 |
| `expandLetrecSyntax` | 199–205 | 7 |
| `expandLetSyntaxImpl` | 207–430 | 224 |
| `isSyntaxFormWithKeyword` | 432–444 | 13 |

**Step 1: Create the new file**

Copy lines 195–444 from `expander_time_continuation.go` into `machine/expander_let_syntax.go` with this header:

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package machine

// expander_let_syntax.go implements let-syntax and letrec-syntax expansion.
//
// Both forms are fully resolved during the expansion phase: macro bindings
// are compiled, the body is expanded in a child environment, and the
// let-syntax/letrec-syntax wrapper disappears from the output.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)
```

Then the 4 functions (with their doc comments), preserving exact code.

**Step 2: Delete the moved lines from `expander_time_continuation.go`**

Remove lines 195–444 (from `expandLetSyntax` through end of `isSyntaxFormWithKeyword`).

**Step 3: Verify build**

```bash
cd machine && go build ./...
```

Expected: success (same package, no visibility changes)

**Step 4: Run tests**

```bash
go test -count=1 ./machine/...
```

Expected: all pass, no changes in behavior

**Step 5: Run lint**

```bash
make lint
```

Expected: clean

**Step 6: Commit**

```bash
git add machine/expander_let_syntax.go machine/expander_time_continuation.go
git commit -m "refactor(machine): extract expander_let_syntax.go

Move let-syntax/letrec-syntax expansion (expandLetSyntax, expandLetrecSyntax,
expandLetSyntaxImpl) and isSyntaxFormWithKeyword helper to dedicated file.
No behavior changes."
```

---

### Task 3: Create `expander_primitive_forms.go`

Extract the simple primitive form expanders.

**Files:**
- Create: `machine/expander_primitive_forms.go`
- Source lines from `machine/expander_time_continuation.go` (post-Task 2 line numbers will differ — use function names to locate)

**Contents (functions to move):**

| Function | Approx original lines | Size |
|----------|-----------------------|------|
| `expandUnchanged` | 186–188 | 3 |
| `expandWithBindingScope` | 471–530 | 60 |
| `expandSyntaxError` | 543–585 | 43 |
| `formatIrritants` | 588–590 | 3 |
| `expandBeginForm` | 595–619 | 25 |
| `expandIfForm` | 622–663 | 42 |
| `expandSetForm` | 665–689 | 25 |
| `expandDefineForm` | 691–732 | 42 |
| `expandImportForm` | 734–771 | 38 |

**Step 1: Create the new file**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package machine

// expander_primitive_forms.go implements expand-time handlers for primitive
// special forms: quote-family (via expandUnchanged), if, begin, set!, define,
// import, with-binding-scope, and syntax-error.
//
// Each handler is registered in primitive_expanders_registry.go and invoked
// by ExpandSyntaxExpression when the expander encounters a primitive keyword.
//
// Extracted from expander_time_continuation.go.

import (
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)
```

Then the 9 functions listed above with their doc comments, preserving exact code.

**Step 2: Delete the moved functions from `expander_time_continuation.go`**

Remove the 9 functions by name. After this task, `expander_time_continuation.go` should no longer import `"strings"`.

**Step 3: Verify build**

```bash
cd machine && go build ./...
```

**Step 4: Run tests**

```bash
go test -count=1 ./machine/...
```

**Step 5: Run lint**

```bash
make lint
```

**Step 6: Commit**

```bash
git add machine/expander_primitive_forms.go machine/expander_time_continuation.go
git commit -m "refactor(machine): extract expander_primitive_forms.go

Move primitive form expanders (expandUnchanged, expandWithBindingScope,
expandSyntaxError, expandBeginForm, expandIfForm, expandSetForm,
expandDefineForm, expandImportForm, formatIrritants) to dedicated file.
No behavior changes."
```

---

### Task 4: Create `expander_lambda.go`

Extract lambda/case-lambda expansion and their shared helpers.

**Files:**
- Create: `machine/expander_lambda.go`
- Source lines from `machine/expander_time_continuation.go` (use function names)

**Contents (functions/types to move):**

| Symbol | Kind | Approx original lines | Size |
|--------|------|-----------------------|------|
| `expandLambdaForm` | method | 773–845 | 73 |
| `collectBodyExpressions` | function | 848–864 | 17 |
| `unwrapBeginBodyWithFlag` | function | 869–896 | 28 |
| `formalSymbol` | struct | 899–903 | 5 |
| `extractFormalSymbols` | function | 907–939 | 33 |
| `extractIdentifierList` | function | 944–975 | 32 |
| `expandCaseLambdaForm` | method | 978–1029 | 52 |

**Step 1: Create the new file**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package machine

// expander_lambda.go implements expand-time handling of lambda and
// case-lambda forms, plus shared helpers for formal parameter extraction
// and body expression collection.
//
// Extracted from expander_time_continuation.go.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)
```

Then the 5 functions, 1 struct, and 1 method listed above, preserving exact code.

**Step 2: Delete the moved symbols from `expander_time_continuation.go`**

Remove all 7 symbols by name.

**Step 3: Verify build**

```bash
cd machine && go build ./...
```

**Step 4: Run tests**

```bash
go test -count=1 ./machine/...
```

**Step 5: Run lint**

```bash
make lint
```

**Step 6: Commit**

```bash
git add machine/expander_lambda.go machine/expander_time_continuation.go
git commit -m "refactor(machine): extract expander_lambda.go

Move lambda/case-lambda expansion and helpers (expandLambdaForm,
expandCaseLambdaForm, collectBodyExpressions, unwrapBeginBodyWithFlag,
formalSymbol, extractFormalSymbols, extractIdentifierList) to dedicated file.
No behavior changes."
```

---

### Task 5: Verify final state of `expander_time_continuation.go`

After Tasks 2–4, the remaining file should contain only the core dispatch:

| Function | Role |
|----------|------|
| `NewExpanderTimeContinuation` | Constructor |
| `Context` | Accessor |
| `hasLocalVariableBinding` | Delegation helper |
| `ExpandExpression` | Main entry — dispatches by syntax type |
| `ExpandSymbol` | Symbol pass-through |
| `ExpandSyntaxOrProcedureCall` | Pair dispatch: macro vs procedure call |
| `ExpandSelfEvaluating` | Self-evaluating pass-through |
| `ExpandPrimitiveForm` | Primitive form lookup + dispatch |
| `ExpandSyntaxExpression` | Macro detection + invocation |
| `invokeTransformerClosure` | Runs transformer closure in sub-context |
| `expandMacroInvocation` | Builds input form, invokes transformer, recurse-expands |
| `ExpandOnce` | Single-step expansion (no recursion) |
| `ExpandSyntaxArgumentList` | Expand each argument in a list |
| `ExpandQuasiquote` | Stub |
| `ExpandQuote` | Stub |

**Step 1: Verify the file contains exactly these symbols**

```bash
grep -n '^func ' machine/expander_time_continuation.go
```

Expected: 15 function/method definitions. No primitive form handlers, no lambda helpers, no let-syntax code.

**Step 2: Verify imports are minimal**

The remaining file should import:
- `"context"` — struct field, constructor, invokeTransformerClosure
- `"github.com/aalpar/wile/environment"` — struct field, ExpandSyntaxExpression, expandMacroInvocation
- `"github.com/aalpar/wile/internal/syntax"` — everywhere
- `"github.com/aalpar/wile/values"` — type assertions, IsVoid checks
- `"github.com/aalpar/wile/werr"` — error wrapping

NOT `"strings"` (moved with formatIrritants).

**Step 3: Run full verification**

```bash
make lint && go test -count=1 ./machine/...
```

Expected: all clean, all pass.

**Step 4: Verify file sizes**

```bash
wc -l machine/expander_time_continuation.go machine/expander_let_syntax.go machine/expander_primitive_forms.go machine/expander_lambda.go machine/expander_body.go
```

Expected approximate sizes:
- `expander_time_continuation.go`: ~470 lines
- `expander_let_syntax.go`: ~280 lines
- `expander_primitive_forms.go`: ~320 lines
- `expander_lambda.go`: ~270 lines
- `expander_body.go`: unchanged (~170 lines)

---

### Task 6: Update TODO.md and CLAUDE.local.md

**Step 1: Update TODO.md**

Mark the item done:
```
- [x] **Expander time continuation decomposition** [Medium, M]: ...
```

Also mark the forms test item done (verified earlier — test file exists from #452):
```
- [x] **internal/forms tests** [Medium, S]: ...
```

**Step 2: Update machine/CLAUDE.local.md expansion file table**

In the "Expansion" table, replace the single `expander_time_continuation.go` entry with the 4-file split:

| File | Purpose |
|------|---------|
| `expander_time_continuation.go` | Core expander: struct, main dispatch, macro invocation, argument list expansion |
| `expander_let_syntax.go` | let-syntax/letrec-syntax expansion, isSyntaxFormWithKeyword helper |
| `expander_primitive_forms.go` | Primitive form expanders: if, begin, set!, define, import, with-binding-scope, syntax-error, expandUnchanged |
| `expander_lambda.go` | Lambda/case-lambda expansion, formal extraction, body collection helpers |
| `expander_body.go` | Body processing with define-syntax support (R7RS 5.3) |
| `expander_context.go` | ExpanderContext: bridges expander and VM for syntax-local-* |
| `primitive_expander.go` | PrimitiveExpander type (wraps expand-time handlers as values.Value) |
| `primitive_expanders_registry.go` | Registers all primitive expanders |

**Step 3: Commit**

```bash
git add TODO.md machine/CLAUDE.local.md
git commit -m "docs: update TODO.md and CLAUDE.local.md for expander decomposition"
```

---

### Task 7: Final verification and PR

**Step 1: Run full test suite**

```bash
make lint && make test
```

Expected: all clean, all pass.

**Step 2: Verify no behavior change with coverage check**

```bash
make covercheck
```

Expected: coverage unchanged or improved (no code changes, just file splits).

**Step 3: Create PR**

Branch: `refactor/expander-decomposition` → `master`

PR title: `refactor(machine): decompose expander_time_continuation.go into 4 files`

PR body should note:
- Pure file reorganization, no behavior changes
- 1,327-line file → 4 files (~270–470 lines each)
- All tests pass, lint clean
- Cross-package references verified (isSyntaxFormWithKeyword used by expander_body.go)
