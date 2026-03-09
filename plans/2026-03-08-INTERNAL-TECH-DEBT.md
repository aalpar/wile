# Internal Package Technical Debt Reduction

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate confirmed structural debt across `internal/` packages — duplicated logic, hardcoded tables, missing abstractions, sync hazards.

**Architecture:** Bottom-up by dependency order. Quick wins first (isolated changes, no cross-package impact), then parser/match refactoring (higher risk, needs careful test verification), then structural splits (file reorganization, no behavior changes).

**Tech Stack:** Go 1.23, project error conventions (`werr`), table-driven tests.

**Validated findings only.** One assessment finding (unguarded type assertion in `validate_case_lambda.go`) was verified as false — the code already has a proper guard at line 53. All remaining items below are confirmed against actual code.

---

## Phase 1: Quick Wins (S effort, high value) ✅

### Task 1.1: Unify REPL debug command metadata

The same command names, aliases, and summaries are defined twice:
- `internal/repl/meta.go:97-129` — `metaCommands` array (help text source)
- `internal/repl/debug.go:67-94` — switch statement (dispatch source)

The code itself admits this at `meta.go:94-96`:
```
// Debug command metadata is duplicated from DebugContext to provide unified
// help output. If debug commands are added or changed, update both here
// and in DebugContext.HandleDebugCommand.
```

**Files:**
- Modify: `internal/repl/debug.go`
- Modify: `internal/repl/meta.go`
- Test: `internal/repl/repl_test.go` (if exists), otherwise add a sync test

**Step 1: Write a test that verifies metadata and dispatch stay in sync**

Create a test that extracts command names from both sources and asserts they match. This test should fail if either source is updated without the other.

```go
// internal/repl/meta_test.go
package repl

import (
	"testing"
)

func TestDebugCommandMetadataInSync(t *testing.T) {
	// Collect command names from metaCommands (debug category only)
	metaNames := make(map[string]bool)
	for _, cmd := range metaCommands {
		if cmd.category == "debug" {
			metaNames[cmd.name] = true
			for _, alias := range cmd.aliases {
				metaNames[alias] = true
			}
		}
	}

	// Collect command names that DebugContext dispatches
	dc := NewDebugContext()
	dispatchNames := make(map[string]bool)
	for _, name := range []string{
		"break", "b", "delete", "d", "list", "l",
		"enable", "disable",
		"step", "s", "next", "n", "finish", "f",
		"continue", "c", "backtrace", "bt", "where",
		"help", "h", "?",
	} {
		if dc.HandleDebugCommand(","+name, io.Discard) {
			dispatchNames[name] = true
		}
	}

	// Every name in meta must dispatch
	for name := range metaNames {
		if !dispatchNames[name] {
			t.Errorf("metaCommands has debug command %q but HandleDebugCommand does not dispatch it", name)
		}
	}
}
```

**Step 2: Run the test to verify it passes (baseline)**

Run: `go test -v -run TestDebugCommandMetadataInSync ./internal/repl/`
Expected: PASS (both sources are currently in sync)

**Step 3: Make DebugContext self-describing**

Add a `Commands()` method to `DebugContext` that returns the canonical command list. Then make `metaCommands` derive debug entries from it.

In `debug.go`, add a `debugCommandInfo` struct and a `DebugCommands()` function:

```go
// DebugCommandInfo describes a debug command for help and dispatch.
type DebugCommandInfo struct {
	Name    string
	Aliases []string
	Summary string
	Detail  string
	Handler func(p *DebugContext, args []string, out io.Writer)
}

// DebugCommands returns the canonical list of debug commands.
func DebugCommands() []DebugCommandInfo {
	return []DebugCommandInfo{
		{"break", []string{"b"}, "Set breakpoint at FILE:LINE[:COLUMN]",
			"Usage: ,break FILE:LINE[:COLUMN]", (*DebugContext).cmdBreak},
		{"delete", []string{"d"}, "Delete a breakpoint",
			"Usage: ,delete ID", (*DebugContext).cmdDelete},
		// ... all commands
	}
}
```

Then rewrite `HandleDebugCommand` to dispatch from this table instead of a switch:

```go
func (p *DebugContext) HandleDebugCommand(line string, out io.Writer) bool {
	// ... parse cmd and args as before ...
	for _, c := range DebugCommands() {
		if cmd == c.Name || slices.Contains(c.Aliases, cmd) {
			c.Handler(p, args, out)
			return true
		}
	}
	// handle "help" and unknown separately
	return true
}
```

Then in `meta.go`, replace the hardcoded debug entries in `metaCommands` with a loop over `DebugCommands()`:

```go
func init() {
	for _, dc := range DebugCommands() {
		metaCommands = append(metaCommands, commandInfo{
			name:     dc.Name,
			aliases:  dc.Aliases,
			summary:  dc.Summary,
			detail:   dc.Detail,
			category: "debug",
		})
	}
}
```

Note: `DebugContext.cmdHelp` currently takes only `out io.Writer` (no `args`). All handlers must share the same signature `(args []string, out io.Writer)` for the function pointer table to work. Adjust `cmdHelp` to accept `args` (ignore them). Similarly, commands like `cmdList`, `cmdStep`, `cmdNext`, `cmdFinish`, `cmdContinue`, `cmdWhere` take only `out` — add `_ []string` parameter.

**Step 4: Run tests**

Run: `go test -v ./internal/repl/`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Delete the sync test from Step 1**

The sync test is no longer needed — there's only one source of truth now. Remove `TestDebugCommandMetadataInSync`.

**Step 7: Commit**

```bash
git add internal/repl/debug.go internal/repl/meta.go internal/repl/meta_test.go
git commit -m "refactor(repl): unify debug command metadata into single source of truth

DebugCommands() is now the canonical list. HandleDebugCommand dispatches
from it; metaCommands derives debug entries from it. Eliminates manual
sync point documented at meta.go:94-96."
```

---

### Task 1.2: Table-drive tokenizer character mnemonics

`internal/tokenizer/tokenizer.go:1000-1023` has a hand-unrolled switch with 12 `strings.EqualFold` calls for character mnemonics (`#\newline`, `#\tab`, etc.). This should be a map lookup.

**Files:**
- Modify: `internal/tokenizer/tokenizer.go`
- Test: existing tests cover this (character mnemonic tests)

**Step 1: Write the table**

Add a package-level map near the top of `tokenizer.go` (or in a new `tokenizer_tables.go` if preferred):

```go
// charMnemonics maps R7RS character mnemonic names to their rune values.
// R7RS §6.6: Character names are case-insensitive.
var charMnemonics = map[string]rune{
	"alarm":        '\a',
	"backspace":    '\b',
	"back-space":   '\b',
	"delete":       '\x7F',
	"escape":       '\x1B',
	"newline":      '\n',
	"null":         '\x00',
	"return":       '\r',
	"space":        ' ',
	"tab":          '\t',
	"vertical-tab": '\v',
	"form-feed":    '\f',
}
```

**Step 2: Replace the switch with a map lookup**

Replace lines 1000-1025 with:

```go
		r, ok := charMnemonics[strings.ToLower(mnemonic)]
		if ok {
			return r
		}
		p.err = NewTokenizerError(MessageInvalidCharacterMnemonic)
		return utf8.RuneError
```

**Step 3: Run all tokenizer tests**

Run: `go test -v ./internal/tokenizer/...`
Expected: PASS (all character mnemonic tests should pass identically)

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/tokenizer/tokenizer.go
git commit -m "refactor(tokenizer): table-drive character mnemonic lookup

Replace 12-case strings.EqualFold switch with map lookup.
O(1) instead of O(N), easier to extend."
```

---

## Phase 2: Parser Refactoring (M effort, high value) ✅

### Task 2.1: ~~Extract integer overflow promotion helper~~ Eliminate parseDecimalInteger

`parseDecimalInteger` (parser_number.go:36-63) and `parseIntegerWithBase` (parser_number.go:70-98) are nearly identical. Both:
1. Call `strconv.ParseInt` with a base
2. On `ErrRange`, retry with `big.Int`
3. If hash digits present, convert to float
4. Otherwise return integer or big integer

The only difference is the base parameter.

**Files:**
- Modify: `internal/parser/parser_number.go`
- Test: existing tests cover this

**Step 1: Write a test for the new helper**

Add a test that exercises the overflow path directly:

```go
// internal/parser/parser_number_test.go (or existing test file)
func TestParseIntegerOverflowPromotion(t *testing.T) {
	tests := []struct {
		input string
		base  int
		want  string // type name
	}{
		{"123", 10, "*values.Integer"},
		{"9999999999999999999999", 10, "*values.BigInteger"},
		{"FFFFFFFFFFFFFFFFFFFF", 16, "*values.BigInteger"},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			p := newTestParser(tt.input)
			// ... test parseIntegerInBase
		})
	}
}
```

Note: The exact test shape depends on how the parser creates test instances. Read existing test patterns in `parser_test.go` first and follow them.

**Step 2: Extract the helper**

Create `parseIntegerInBase(base int)` that contains the shared logic:

```go
// parseIntegerInBase parses the current token as an integer in the given base.
// Handles overflow promotion to BigInteger and hash digit inexactness.
func (p *Parser) parseIntegerInBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())
	a, err := strconv.ParseInt(s, base, 64)
	if err != nil {
		var numErr *strconv.NumError
		if errors.As(err, &numErr) && errors.Is(numErr.Err, strconv.ErrRange) {
			bigInt := new(big.Int)
			_, ok := bigInt.SetString(s, base)
			if ok {
				if p.cur.HasHashDigit() {
					f, _ := bigInt.Float64()
					q := p.wrapSyntax(values.NewFloat(f), p.cur)
					return q, p.cur, nil
				}
				q := p.wrapSyntax(values.NewBigInteger(bigInt), p.cur)
				return q, p.cur, nil
			}
		}
		return nil, p.cur, err
	}
	if p.cur.HasHashDigit() {
		q := p.wrapSyntax(values.NewFloat(float64(a)), p.cur)
		return q, p.cur, nil
	}
	q := p.wrapSyntax(values.NewInteger(a), p.cur)
	return q, p.cur, nil
}
```

**Step 3: Rewrite callers**

```go
func (p *Parser) parseDecimalInteger() (syntax.SyntaxValue, tokenizer.Token, error) {
	return p.parseIntegerInBase(10)
}

func (p *Parser) parseIntegerWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	return p.parseIntegerInBase(base)
}
```

Question for implementor: `parseDecimalInteger` and `parseIntegerWithBase` may have callers that depend on these exact names. Use `go_symbol_references` to check before renaming or removing either.

**Step 4: Run all parser tests**

Run: `go test -v ./internal/parser/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```bash
git add internal/parser/parser_number.go
git commit -m "refactor(parser): extract parseIntegerInBase to deduplicate overflow promotion

parseDecimalInteger and parseIntegerWithBase were identical except for
the base parameter. Now both delegate to parseIntegerInBase."
```

---

### Task 2.2: Extract delimiter validation helper ✅

The error message `"mismatched delimiters: opened with %s but closed with %s"` appears at 4 sites in `parser.go`:
- `readLabeledList` line 235
- `readLabeledList` line 272
- `readList` line 519
- `readList` line 532

**Files:**
- Modify: `internal/parser/parser.go`
- Test: existing bracket tests cover this

**Step 1: Extract the helper**

```go
// checkDelimiterMatch returns an error if the current token is a mismatched
// closing delimiter for the given opener. Returns nil if no mismatch.
func (p *Parser) checkDelimiterMatch(opener tokenizer.TokenizerState) error {
	expectedClose := p.matchingClose(opener)
	if p.cur.Type() == expectedClose {
		return nil
	}
	if p.isListCloser(p.cur.Type()) {
		return NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
			p.delimiterString(opener), p.delimiterString(p.cur.Type()))
	}
	return nil
}
```

**Step 2: Replace all 4 sites**

In each location, replace the inline mismatch check with `checkDelimiterMatch`. Example for `readList` line 530-533:

```go
	case p.isListCloser(p.cur.Type()):
		if err := p.checkDelimiterMatch(opener); err != nil {
			return nil, p.cur, err
		}
```

Be careful: some sites check `p.cur.Type() != expectedClose` and then separately check `p.isListCloser`. The helper combines both checks. Read each site carefully to ensure semantic equivalence.

**Step 3: Run all parser tests, especially bracket tests**

Run: `go test -v -run TestBracket ./internal/parser/...`
Run: `go test -v ./internal/parser/...`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/parser/parser.go
git commit -m "refactor(parser): extract checkDelimiterMatch to deduplicate bracket validation

Replaces 4 inline mismatch checks in readList and readLabeledList."
```

---

## Phase 3: Match Refactoring (M effort, medium value)

### Task 3.1: Extract shared symbol expansion helper

`expandSyntaxValue` (syntax_adapter.go:271-302) and `expandEscapedSyntaxTemplate` (syntax_adapter.go:641-663) have identical `SyntaxSymbol` handling: check pattern variable binding, check scope compatibility, apply hygiene. The only difference is the surrounding pair handling (escaped form doesn't check for ellipsis).

**Files:**
- Modify: `internal/match/syntax_adapter.go`
- Test: existing expansion tests cover this

**Step 1: Extract the helper**

```go
// expandSymbol handles symbol expansion for both normal and escaped template contexts.
// It checks pattern variable bindings, scope compatibility, and applies hygiene.
func (p *SyntaxMatcher) expandSymbol(
	t *syntax.SyntaxSymbol,
	ctx *captureContext,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	symVal := t.Unwrap().(*values.Symbol)

	capturedVal, ok := ctx.bindings[symVal.Key]
	if ok {
		if opts.PatternVarSyntax != nil {
			patternSym, hasPattern := opts.PatternVarSyntax[symVal.Key]
			if hasPattern {
				templateScopes := t.Scopes()
				patternScopes := patternSym.Scopes()
				if !scopesCompatibleForSubstitution(templateScopes, patternScopes) {
					return p.applyHygieneToSymbol(t, opts), nil
				}
			}
		}
		return p.capturedValueToSyntax(capturedVal, opts)
	}

	return p.applyHygieneToSymbol(t, opts), nil
}
```

**Step 2: Replace both call sites**

In `expandSyntaxValue`, replace the `case *syntax.SyntaxSymbol:` block (~lines 272-302) with:

```go
	case *syntax.SyntaxSymbol:
		return p.expandSymbol(t, ctx, opts)
```

In `expandEscapedSyntaxTemplate`, replace the `case *syntax.SyntaxSymbol:` block (~lines 642-663) with:

```go
	case *syntax.SyntaxSymbol:
		return p.expandSymbol(t, ctx, opts)
```

**Step 3: Run all match tests**

Run: `go test -v ./internal/match/...`
Expected: PASS

**Step 4: Run the full macro test suite**

Macro hygiene is subtle — run the integration tests too:

Run: `go test -v -run TestMacro ./...`
Run: `go test -v -run TestHygiene ./...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```bash
git add internal/match/syntax_adapter.go
git commit -m "refactor(match): extract expandSymbol to deduplicate symbol expansion

expandSyntaxValue and expandEscapedSyntaxTemplate had identical
SyntaxSymbol handling. Now both delegate to expandSymbol."
```

---

## Phase 4: Tokenizer Structural (L effort, medium value)

### Task 4.1: Extract shared escape sequence table

String escapes (`tokenizer.go:626-651`) and character mnemonics (`tokenizer.go:1000-1023`, already table-driven after Task 1.2) express overlapping R7RS escape mappings. The string escape handler uses single-character matching; the mnemonic table uses full names. They share semantic intent but not implementation.

**Files:**
- Modify: `internal/tokenizer/tokenizer.go`
- Test: existing string escape and character tests

**Step 1: Add a string escape table**

```go
// stringEscapes maps single escape characters to their replacement strings.
// R7RS §6.7: String escape sequences.
var stringEscapes = map[rune]string{
	'a':  "\a",
	'b':  "\b",
	't':  "\t",
	'n':  "\n",
	'r':  "\r",
	'\\': "\\",
	'"':  "\"",
	'|':  "|",
}
```

**Step 2: Replace the switch in readEscapeSequence**

Replace lines 626-651:

```go
func (p *Tokenizer) readEscapeSequence() {
	if p.curr() == 'x' {
		p.readHexEscapeToken()
		return
	}
	if isIntralineWhitespace(p.curr()) || isLineEnding(p.curr()) {
		p.skipLineContinuation()
		return
	}
	replacement, ok := stringEscapes[p.curr()]
	if ok {
		p.value += replacement
		p.next()
		return
	}
	p.err = NewTokenizerError(MessageExpectingEscape)
}
```

**Step 3: Run all tokenizer tests**

Run: `go test -v ./internal/tokenizer/...`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/tokenizer/tokenizer.go
git commit -m "refactor(tokenizer): table-drive string escape sequences

Replace 8-case switch in readEscapeSequence with map lookup.
Complements the character mnemonic table from previous commit."
```

---

### Task 4.2: Split tokenizer.go into logical files

`tokenizer.go` is 2130 lines. Split by concern. **No behavior changes — pure file reorganization.**

**Files:**
- Modify: `internal/tokenizer/tokenizer.go` (reduce to ~400 lines: struct, constructors, Next, dispatch)
- Create: `internal/tokenizer/tokenizer_numbers.go` (number scanning: ~500 lines)
- Create: `internal/tokenizer/tokenizer_literals.go` (strings, characters, symbols: ~400 lines)
- Create: `internal/tokenizer/tokenizer_comments.go` (comments, block comments: ~100 lines)
- Create: `internal/tokenizer/tokenizer_hash.go` (#-prefix dispatch: booleans, vectors, exactness, radix: ~200 lines)
- Create: `internal/tokenizer/tokenizer_predicates.go` (character predicates: ~200 lines)
- Test: all existing tests must pass unchanged

**Step 1: Plan the split**

Read the full file and identify function groups. Map each function to its target file. The key constraint: all functions are methods on `*Tokenizer`, so they can live in any file in the package.

Suggested grouping:
- `tokenizer.go`: Tokenizer struct, NewTokenizer*, Next, Close, mark/term, skipWhitespace, read (main dispatch), scan/scanWith/scanCaseInsensitive
- `tokenizer_numbers.go`: readIntegerAndFraction, readSignedComplexSuffix, mayReadSignedImaginaryPart, mayReadPolarPart, readUnsignedFractionalRealNumber*, readDigits*, readSpecialNumber, integerStateForRadix, number-related state transitions
- `tokenizer_literals.go`: readDelimited, readEscapeSequence, readHexEscapeToken, readCharacterMnemonic*, readString, readSymbol, readExtendedSymbol, charMnemonics table, stringEscapes table
- `tokenizer_comments.go`: continueBlockComment, readDatumComment (if separate from hash dispatch)
- `tokenizer_hash.go`: readVectorOrExactnessOrRadix... (the #-dispatch function), readTypedArrayOrExactnessOrRadixOrBooleanMarker, related helpers
- `tokenizer_predicates.go`: all isX predicates (isInitial, isSubsequent, isDelimiter, etc.)

**Step 2: Execute the split**

Move functions to their target files. Each target file must have `package tokenizer` header and any necessary imports. Do NOT change any function signatures or logic.

**Step 3: Run all tokenizer tests**

Run: `go test -v ./internal/tokenizer/...`
Expected: PASS (pure reorganization — no behavior change)

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/tokenizer/
git commit -m "refactor(tokenizer): split tokenizer.go into logical files

No behavior changes. Functions grouped by concern:
- tokenizer.go: core struct, dispatch, scan
- tokenizer_numbers.go: numeric scanning
- tokenizer_literals.go: strings, characters, symbols
- tokenizer_comments.go: comment handling
- tokenizer_hash.go: #-prefix dispatch
- tokenizer_predicates.go: character classification"
```

---

## Phase 5: Extensions Cleanup (M effort, medium value)

### Task 5.1: Split prim_read_write.go

806 lines mixing text I/O, binary I/O, and port helpers. Split by concern.

**Files:**
- Modify: `internal/extensions/io/prim_read_write.go` (reduce to text read)
- Create: `internal/extensions/io/prim_write.go` (text write)
- Create: `internal/extensions/io/prim_binary.go` (byte-level I/O)
- Modify: `internal/extensions/io/register.go` (no changes needed if registration functions are stable)
- Test: existing tests must pass unchanged

**Step 1: Plan the split**

Read `prim_read_write.go` and group functions:
- **Text read** (~250 lines): PrimRead, PrimReadToken, PrimReadSyntax, PrimReadChar, PrimPeekChar, PrimCharReadyQ, PrimReadLine, PrimReadString
- **Text write** (~200 lines): PrimWrite, PrimDisplay, PrimWriteSimple, PrimWriteShared, PrimWriteChar, PrimWriteString, PrimNewline, PrimFlushOutputPort
- **Binary I/O** (~200 lines): PrimReadU8, PrimPeekU8, PrimU8ReadyQ, PrimWriteU8, PrimReadBytevector, PrimReadBytevectorBang, PrimWriteBytevector
- **Shared helpers** (keep in original or new helpers file): extractPort, getOptionalInputPort, getOptionalOutputPort, getOptionalTextualOutputPort, getRequiredBinaryInputPort, getRequiredBinaryOutputPort, fmtPrefix

**Step 2: Execute the split (pure file move)**

**Step 3: Run all tests**

Run: `go test -v ./internal/extensions/io/...`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/extensions/io/
git commit -m "refactor(extensions/io): split prim_read_write.go by concern

No behavior changes. Text read, text write, and binary I/O
now in separate files for better navigation."
```

---

## Phase 6: Bootstrap Parameterization (S-M effort, medium value)

### Task 6.1: Parameterize extension loading in bootstrap

`internal/bootstrap/environment_tiny.go:55-66` hardcodes all extensions. The public API (`wile.Engine`) already supports `WithExtension()` for selective loading, but the bootstrap package doesn't expose this to internal callers.

**Files:**
- Modify: `internal/bootstrap/environment_tiny.go`
- Test: `internal/bootstrap/environment_tiny_test.go`

**Step 1: Read the Engine's extension loading code**

Before changing bootstrap, understand how `wile.Engine` already handles extension selection. Check `engine.go` for `WithExtension` and how it calls bootstrap. The goal is to align bootstrap with the existing public API pattern.

Run: `grep -n "WithExtension\|allExtensions\|NewTopLevel" engine.go`

**Step 2: Add an extensions parameter to the internal initializer**

```go
// initializeEnvironmentWithExtensions is the shared initialization sequence.
// If exts is nil, all extensions are loaded (backward compatible).
func initializeEnvironmentWithExtensions(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	exts []registry.Extension,
) (*registry.Registry, error) {
	if exts == nil {
		exts = allExtensions
	}
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error adding core to registry")
	}
	for _, ext := range exts {
		err := ext.AddToRegistry(reg)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err, "error adding extension %s to registry", ext.Name())
		}
	}
	// ... rest unchanged
}
```

**Step 3: Update existing callers to pass nil (backward compatible)**

`initializeEnvironmentWithRegistry` and `initializeEnvironment` should pass `nil` to get the default behavior. This is a pure refactoring — no caller behavior changes.

**Step 4: Run all bootstrap tests**

Run: `go test -v ./internal/bootstrap/...`
Expected: PASS

**Step 5: Run full test suite (bootstrap affects everything)**

Run: `make test`
Expected: PASS

**Step 6: Run lint**

Run: `make lint && make covercheck`
Expected: PASS

**Step 7: Commit**

```bash
git add internal/bootstrap/environment_tiny.go
git commit -m "refactor(bootstrap): parameterize extension loading

initializeEnvironmentWithExtensions accepts an explicit extension list.
nil means 'all extensions' (backward compatible). Enables future
selective extension loading for sandboxed environments."
```

---

## Phases NOT Included (Deferred)

These were identified in the assessment but are deferred for good reasons:

| Finding | Why Deferred |
|---------|-------------|
| Parser: unify readList + readLabeledList | High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol. |
| Match: extract opcode handlers from VM interpreter | 264-line switch is large but stable. Extraction adds indirection without clear benefit until new opcodes are needed. |
| Syntax: nine types with boilerplate | Go doesn't support sealed type hierarchies or default method implementations. The boilerplate is inherent to the language. Documenting the invariant is cheaper than refactoring. |
| Match: consolidate bytecode type files | Pure cosmetic. Low value. |
| Tokenizer: test file consolidation | Already tracked in TODO.md as "Tokenizer test file consolidation [Low, M]". |
| Extensions: standardize registration patterns | Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring. |
| Schemeutil: grab-bag reorganization | Moving functions risks import cycle issues. Needs careful dependency analysis. |

---

## Verification Checklist

After all phases:

```bash
make lint && make covercheck   # Must both pass
go test ./internal/...         # All internal tests pass
go test ./...                  # Full suite passes
```

No behavioral changes in any phase. Every commit should be independently revertible.
