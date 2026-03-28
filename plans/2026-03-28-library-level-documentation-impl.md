# Library-Level Documentation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `(description <string>)` clause to `define-library` so libraries are self-documenting, queryable via `,doc` and `(library-description)`.

**Architecture:** New `description` case in `processLibraryDeclaration` stores string on `CompiledLibrary.Description`. REPL's `,doc` detects parenthesized args and does library lookup. New `library-description` primitive reads from `LibraryRegistry`.

**Tech Stack:** Go, Scheme (.sld files), existing machine/registry/repl packages.

**Design doc:** `plans/2026-03-28-library-level-documentation-design.md`

---

### Task 1: Add `Description` field to `CompiledLibrary`

**Files:**
- Modify: `machine/library_registry.go:72-79` (struct definition)

**Step 1: Add the field**

In the `CompiledLibrary` struct at `machine/library_registry.go:73`, add `Description string` after `Name`:

```go
type CompiledLibrary struct {
	Name        LibraryName                   // Library name
	Description string                        // from (description ...) clause; "" if absent
	Env         *environment.EnvironmentFrame // Library's private environment
	Exports     map[string]string             // external-name -> internal-name
	SourceFile  string                        // Path to .sld file (for error messages)
	Template    *NativeTemplate               // Compiled bytecode (for execution)
}
```

**Step 2: Run existing tests to verify no breakage**

Run: `go test ./machine/... -count=1 2>&1 | tail -5`
Expected: PASS (struct field addition is backward-compatible)

**Step 3: Commit**

```
feat(machine): add Description field to CompiledLibrary
```

---

### Task 2: Parse `(description ...)` clause in `processLibraryDeclaration`

**Files:**
- Modify: `machine/compile_library_forms.go:130-175` (add case)
- Test: `machine/library_test.go` (or new `machine/library_description_test.go`)

**Step 1: Write failing tests**

Add to library tests (use existing test patterns from `machine/library_test.go`). Four cases:

1. `(define-library (test desc) (description "A test library.") (export))` — description parsed and stored
2. `(define-library (test nodesc) (export))` — description is `""`
3. `(define-library (test bad) (description 42))` — compile error (non-string)
4. `(define-library (test multi) (description "first") (description "second") (export))` — last wins

Each test compiles a library via the existing test infrastructure, then checks `CompiledLibrary.Description`.

**Step 2: Run tests to verify they fail**

Run: `go test ./machine/... -run TestLibraryDescription -v`
Expected: FAIL — "unknown library declaration: description"

**Step 3: Add the `description` case**

In `processLibraryDeclaration` at `machine/compile_library_forms.go`, add before the `default` case:

```go
case "description":
	return p.processLibraryDescription(lib, argsExpr)
```

Add the handler method:

```go
// processLibraryDescription handles (description <string>) within a library.
func (p *CompileTimeContinuation) processLibraryDescription(lib *CompiledLibrary, args syntax.SyntaxValue) error {
	if syntax.IsSyntaxEmptyList(args) {
		return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "description: expected a string argument")
	}
	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "description: expected a string argument")
	}
	strExpr := argsPair.SyntaxCar()
	str, ok := strExpr.UnwrapAll().(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "description: argument must be a string")
	}
	lib.Description = str.Value
	return nil
}
```

**Step 4: Run tests to verify they pass**

Run: `go test ./machine/... -run TestLibraryDescription -v`
Expected: PASS

**Step 5: Commit**

```
feat(machine): parse (description ...) clause in define-library
```

---

### Task 3: Add `library-description` primitive

**Files:**
- Modify: `registry/core/reflection.go` (registration)
- Modify: `registry/core/prim_reflection.go` (implementation)
- Test: `registry/core/prim_reflection_test.go`

**Step 1: Write failing tests**

Add table-driven tests in `registry/core/prim_reflection_test.go`. The tests need a library in the environment, so use `(import (scheme time))` first to ensure a library is loaded:

```go
// TestLibraryDescription tests the (library-description) primitive.
func TestLibraryDescription(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "known library", Code: `(begin (import (scheme time)) (library-description '(scheme time)))`, Expected: values.FalseValue},
		{Name: "unknown library", Code: `(library-description '(nonexistent lib))`, Expected: values.FalseValue},
	}
	// ... standard table-driven loop
}
```

Note: Initially the "known library" test expects `#f` since no `.sld` files have descriptions yet. Once Task 5 adds descriptions, update this test.

**Step 2: Run test to verify it fails**

Run: `go test ./registry/core/... -run TestLibraryDescription -v`
Expected: FAIL — `library-description` undefined

**Step 3: Register the primitive**

In `registry/core/reflection.go`, add to the `addReflection` specs slice:

```go
{Name: "library-description", ParamCount: 1, Impl: PrimLibraryDescription,
    Doc: "Returns the description string of a loaded library, or #f if none or not loaded.",
    ParamNames: []string{"library-name"}, Category: "reflection",
    ParamTypes: []values.ValueType{values.TypeList}, ReturnType: values.TypeAny},
```

**Step 4: Implement the primitive**

In `registry/core/prim_reflection.go`, add:

```go
// PrimLibraryDescription implements (library-description '(lib name)).
// Returns the description string or #f.
func PrimLibraryDescription(mc *machine.MachineContext) error {
	nameList := mc.Arg(0)
	libName, err := machine.ParseLibraryNameFromList(nameList)
	if err != nil {
		return werr.WrapForeignErrorf(err, "library-description: invalid library name")
	}

	regAny := mc.EnvironmentFrame().LibraryRegistry()
	if regAny == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	reg, ok := regAny.(*machine.LibraryRegistry)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}

	lib := reg.Lookup(libName)
	if lib == nil || lib.Description == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(lib.Description))
	return nil
}
```

**Step 5: Check if `ParseLibraryNameFromList` exists**

`ParseLibraryNameFromDatum` in `machine/import_set_datum.go` takes a `values.Value` (unwrapped datum). The primitive receives a quoted list like `'(scheme time)` which is already a `values.Value` list. Verify the function signature matches — if it expects syntax values, write a thin wrapper `ParseLibraryNameFromList` that walks a `Tuple` and extracts symbol keys.

**Step 6: Run tests to verify they pass**

Run: `go test ./registry/core/... -run TestLibraryDescription -v`
Expected: PASS

**Step 7: Commit**

```
feat(reflection): add library-description primitive
```

---

### Task 4: Wire `,doc` for library names in the REPL

**Files:**
- Modify: `internal/repl/meta.go:191-248` (cmdDoc)
- Modify: `internal/repl/meta.go:108-130` (metaCommands help text)
- Test: `internal/repl/meta_test.go`

**Step 1: Write failing test**

Add a test in `internal/repl/meta_test.go` that calls `,doc (scheme time)` and checks the output. This requires an environment with the library registry and a loaded library. Check existing `,doc` test patterns in that file to match setup.

**Step 2: Run test to verify it fails**

Expected: FAIL — `,doc` treats `(scheme` as an unrecognized binding name

**Step 3: Update `cmdDoc` to detect library names**

At the top of `cmdDoc` in `meta.go:191`, after the `len(args) == 0` check, add library name detection. The REPL splits on whitespace, so `,doc (scheme time)` arrives as `args = ["(scheme", "time)"]`. Reconstruct if the first arg starts with `(`:

```go
// Check if arguments form a library name like (scheme base)
if strings.HasPrefix(args[0], "(") {
    joined := strings.Join(args, " ")
    if strings.HasSuffix(joined, ")") {
        p.cmdDocLibrary(joined, out)
        return
    }
}
```

**Step 4: Implement `cmdDocLibrary`**

```go
func (p *MetaCommandHandler) cmdDocLibrary(nameStr string, out io.Writer) {
    // Strip parens: "(scheme base)" -> "scheme base"
    inner := strings.TrimPrefix(strings.TrimSuffix(nameStr, ")"), "(")
    parts := strings.Fields(inner)
    if len(parts) == 0 {
        fmt.Fprintln(out, "Usage: ,doc (library-name)")
        return
    }

    libName := machine.NewLibraryName(parts...)

    // Get registry from environment
    regAny := p.env.LibraryRegistry()
    if regAny == nil {
        fmt.Fprintf(out, "Library %s: not loaded (no library registry)\n", libName.SchemeString())
        return
    }
    reg, ok := regAny.(*machine.LibraryRegistry)
    if !ok {
        fmt.Fprintf(out, "Library %s: not loaded\n", libName.SchemeString())
        return
    }

    lib := reg.Lookup(libName)
    if lib == nil {
        fmt.Fprintf(out, "Library %s: not loaded\n", libName.SchemeString())
        return
    }

    var content strings.Builder
    formatLibraryDoc(&content, lib)
    writeWithPager(out, content.String(), os.Getenv("PAGER"))
}

func formatLibraryDoc(w *strings.Builder, lib *machine.CompiledLibrary) {
    fmt.Fprintf(w, "Library: %s\n", lib.Name.SchemeString())
    if lib.Description != "" {
        fmt.Fprintf(w, "\n  %s\n", lib.Description)
    }
    if lib.SourceFile != "" {
        fmt.Fprintf(w, "\nSource: %s\n", lib.SourceFile)
    }

    exports := make([]string, 0, len(lib.Exports))
    for name := range lib.Exports {
        exports = append(exports, name)
    }
    sort.Strings(exports)

    fmt.Fprintf(w, "\nExports (%d):\n", len(exports))
    for _, name := range exports {
        fmt.Fprintf(w, "  %s\n", name)
    }
}
```

**Step 5: Update help text**

In `metaCommands` at `meta.go:115`, update the `doc` detail string to mention library support:

```
"Usage: ,doc <name> or ,doc (<library-name>)\n\n..."
```

**Step 6: Run tests to verify they pass**

Run: `go test ./internal/repl/... -run TestCmdDocLibrary -v`
Expected: PASS

**Step 7: Commit**

```
feat(repl): wire ,doc for library names
```

---

### Task 5: Add descriptions to all stdlib `.sld` files

**Files:**
- Modify: all 34 files in `stdlib/lib/**/*.sld`

**Step 1: Add `(description ...)` clause to each `.sld` file**

Add a `(description "...")` line after `(define-library ...)` and before `(export ...)`. One sentence per library. Examples:

| Library | Description |
|---------|-------------|
| `(scheme base)` | "R7RS base library: pairs, lists, numbers, strings, vectors, control, I/O, exceptions." |
| `(scheme write)` | "Output procedures: write, display, write-shared, write-simple." |
| `(scheme read)` | "Input procedures: read." |
| `(scheme time)` | "Time-related procedures: current-second, current-jiffy, jiffies-per-second." |
| `(scheme char)` | "Unicode character procedures: char-alphabetic?, char-upcase, string-upcase, etc." |
| `(scheme case-lambda)` | "Case-lambda dispatch form for arity-based procedure selection." |
| `(scheme complex)` | "Complex number operations: make-rectangular, make-polar, real-part, imag-part, magnitude, angle." |
| `(scheme cxr)` | "Compositions of car and cdr up to four deep: caar, cadr, ..., cddddr." |
| `(scheme eval)` | "Evaluation and environment procedures: eval, environment." |
| `(scheme file)` | "File I/O: open-input-file, open-output-file, file-exists?, delete-file." |
| `(scheme inexact)` | "Inexact number operations: exp, log, sin, cos, tan, sqrt, etc." |
| `(scheme lazy)` | "Lazy evaluation: delay, force, delay-force, make-promise, promise?." |
| `(scheme load)` | "Source file loading: load." |
| `(scheme process-context)` | "Process context: command-line, exit, emergency-exit, get-environment-variable." |
| `(scheme r5rs)` | "R5RS compatibility library: re-exports core R5RS bindings." |
| `(scheme repl)` | "REPL support: interaction-environment." |
| `(chibi test)` | "Lightweight test framework: test, test-group, test-begin, test-end." |
| `(chibi optional)` | "Optional/Maybe type with pattern matching." |
| `(chibi diff)` | "Sequence diff algorithm: LCS-based edit scripts." |
| `(chibi term ansi)` | "ANSI terminal escape codes: colors, styles, cursor control." |
| `(srfi 1)` | "SRFI 1: List library — comprehensive list operations beyond R7RS." |
| `(wile algebra)` | "Algebraic structures: orders, lattices, monoids, semirings, groups, rings, fields." |
| `(wile algebra order)` | "Partial and total orders with comparison operations." |
| `(wile algebra lattice)` | "Lattice types: flat, powerset, product, map lattices with join/meet." |
| `(wile algebra monoid)` | "Monoids with identity and associative binary operation." |
| `(wile algebra semiring)` | "Semirings: boolean, tropical, counting, and lifted variants." |
| `(wile algebra group)` | "Groups: integer addition, modular arithmetic." |
| `(wile algebra ring)` | "Rings: integer ring, modular rings." |
| `(wile algebra galois)` | "Galois connections between lattices." |
| `(wile algebra rewrite)` | "Term rewriting with algebraic simplification rules." |
| `(wile control)` | "Extended control flow: reset, shift, delimited continuations." |
| `(wile kanren)` | "miniKanren relational programming: run, fresh, conde, ==." |
| `(wile microkanren)` | "microKanren core: minimal relational programming kernel." |
| `(wile er-macro-test)` | "Explicit-renaming macro test utilities." |

**Step 2: Run full test suite to verify nothing breaks**

Run: `make test`
Expected: PASS

**Step 3: Update the `library-description` test from Task 3**

Change the "known library" test case expected value from `#f` to the actual description string for `(scheme time)`.

**Step 4: Run the updated test**

Run: `go test ./registry/core/... -run TestLibraryDescription -v`
Expected: PASS

**Step 5: Commit**

```
docs(stdlib): add descriptions to all .sld library files
```

---

### Task 6: Update TODO.md and run lint

**Files:**
- Modify: `TODO.md`

**Step 1: Mark library-level documentation as done**

Update the `[ ]` checkbox for "Library-level documentation" to `[x]` and add a summary.

**Step 2: Run lint and covercheck**

Run: `make lint && make covercheck`
Expected: PASS

**Step 3: Commit**

```
docs: mark library-level documentation complete in TODO
```

---

## Implementation Notes

- **Task ordering matters**: Task 1 (struct field) → Task 2 (parser) → Task 3 (primitive) → Task 4 (REPL) → Task 5 (content) → Task 6 (cleanup). Each task builds on the previous.
- **`ParseLibraryNameFromDatum` vs `ParseLibraryNameFromList`**: Check the exact signature in `machine/import_set_datum.go`. The primitive receives a Scheme list of symbols (e.g., `(scheme time)` as a `*values.Pair`), not syntax objects. If the existing function expects syntax objects, add a thin wrapper.
- **REPL arg splitting**: `,doc (scheme base)` arrives as `args = ["(scheme", "base)"]` because the REPL splits on whitespace. The library detection code must rejoin the args.
- **Test environment for REPL tests**: Check how existing `,doc` tests set up the environment in `internal/repl/meta_test.go` and follow the same pattern.
