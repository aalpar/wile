# Medium-Priority Refactoring Batch

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Complete three medium-priority refactoring items from TODO.md: parser readSyntax extraction, environment walk unification, and syntax_adapter responsibility split.

**Architecture:** Three independent work items touching three packages (`internal/parser`, `environment`, `internal/match`). No dependencies between items — can be parallelized or done sequentially.

**Tech Stack:** Go 1.23, standard library only.

---

## Corrections to TODO.md Descriptions

Before designing phases, exploration revealed inaccuracies in the TODO.md descriptions:

| TODO.md Says | Actual Code |
|---|---|
| "500+ line switch on 30+ token types" | 238-line switch (lines 667-904) on 46 cases. Most already delegate to extracted methods. |
| "4 similar parent-chain walks: resolveLocal(), resolveGlobal(), GetLocalIndexWithScopes(), HasLocalVariableBinding()" | `HasLocalVariableBinding` and `GetLocalIndex` already delegate to `resolveLocal`. `resolveGlobal` is fundamentally different (RLock, global bindings, value-keyed). Only `GetLocalIndexWithScopes` genuinely duplicates `resolveLocal`'s walk. |
| "Split into syntax_matcher.go, syntax_expander.go, hygiene_checker.go" | Hygiene validation is interleaved with matching and expansion — a third file would be artificial. Natural split is 2 files, not 3. |

Update TODO.md descriptions after each work item completes.

---

## Work Item A: Parser readSyntax — Extract Remaining Inline Cases

### Current State

`readSyntax()` (lines 621-906 in `internal/parser/parser.go`) has a 238-line switch with 46 cases. Already well-factored:

- Compound forms → `readList()`, `readVector()`, `readByteVector()`, etc.
- Quote forms → `readQuoteForm()` (except `TokenizerStateQuote`)
- Numeric parsing → `parser_number.go` methods
- Simple literals → 2-3 line wraps

Remaining inline code (~100 lines across ~15 cases): character parsing, special floats, exactness markers, BigFloat, decimal fractions, and `TokenizerStateQuote`.

### Phase A1: Extract `readCharacter()`

**Files:**
- Modify: `internal/parser/parser.go`

Three contiguous character cases (`CharGraphic` lines 876-880, `CharMnemonic` lines 881-888, `CharHexEscape` lines 889-897) share a pattern: `TrimPrefixFolded("#\\", ...)` then type-specific parsing.

**Step 1: Extract method**

Add a new method to `parser.go`:

```go
func (p *Parser) readCharacter(tok tokenizer.Token) (syntax.SyntaxValue, tokenizer.Token, error) {
	raw := TrimPrefixFolded(tok.Value, "#\\")
	switch tok.State {
	case tokenizer.TokenizerStateCharGraphic:
		runes := []rune(raw)
		return p.wrapSyntax(values.NewCharacter(runes[0]), tok), tok, nil
	case tokenizer.TokenizerStateCharMnemonic:
		r, ok := mnemonicRunes[strings.ToLower(raw)]
		if !ok {
			return nil, tok, p.syntaxError(tok, "unrecognized character name: %s", raw)
		}
		return p.wrapSyntax(values.NewCharacter(r), tok), tok, nil
	case tokenizer.TokenizerStateCharHexEscape:
		hex := TrimPrefixFolded(raw, "x")
		n, err := strconv.ParseInt(hex, 16, 32)
		if err != nil {
			return nil, tok, p.syntaxError(tok, "invalid character escape: %s", raw)
		}
		return p.wrapSyntax(values.NewCharacter(rune(n)), tok), tok, nil
	default:
		return nil, tok, p.syntaxError(tok, "unexpected character token state")
	}
}
```

Replace the three switch cases with:

```go
case tokenizer.TokenizerStateCharGraphic,
	tokenizer.TokenizerStateCharMnemonic,
	tokenizer.TokenizerStateCharHexEscape:
	return p.readCharacter(tok)
```

**Note:** Verify exact parameter types and return patterns against the actual code. The inline cases may use `p.cur` vs `tok` differently — match the existing convention.

**Step 2: Run tests**

Run: `go test ./internal/parser/ -v -count=1`
Expected: All pass

**Step 3: Run lint**

Run: `make lint`
Expected: Pass

**Step 4: Commit**

```
refactor(parser): extract readCharacter from readSyntax switch

Three character parsing cases (graphic, mnemonic, hex escape) shared
TrimPrefixFolded + type-specific parsing. Consolidate into one method.
```

---

### Phase A2: Fix readQuoteForm source location, unify Quote case

**Files:**
- Modify: `internal/parser/parser.go`

`TokenizerStateQuote` (lines 694-707) is the only quote form not using `readQuoteForm()`. The difference: Quote saves the pre-advance token (`p.curr()`) for the quote symbol's source location, while `readQuoteForm` uses the post-advance token (`p.cur`) — giving quote-like forms the datum's position rather than the quote mark's position.

The Quote case is arguably more correct: the `quote` symbol should point to where `'` appeared. The other forms should too.

**Step 1: Fix readQuoteForm to use pre-advance token**

In `readQuoteForm` (line 373), save the current token before advancing:

```go
func (p *Parser) readQuoteForm(keyword string) (syntax.SyntaxValue, tokenizer.Token, error) {
	t := p.curr() // Save quote mark position before advancing
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}
	q, _, err := p.readSyntax()
	if err != nil {
		return nil, p.cur, err
	}
	sym := p.wrapSyntaxSymbol(keyword, t)
	result := p.listSyntax(t, sym, q)
	return result, p.cur, nil
}
```

**Step 2: Replace Quote case with readQuoteForm**

```go
case tokenizer.TokenizerStateQuote:
	return p.readQuoteForm(ConstQuote)
```

**Step 3: Run tests**

Run: `go test ./internal/parser/ -v -count=1`
Expected: All pass

This changes source location metadata for 7 quote-like forms (quasiquote, unquote, unquote-splicing, syntax, unsyntax, quasisyntax, unsyntax-splicing). The positions shift from the datum token to the quote mark token. Verify that no tests depend on the old positions.

**Step 4: Run full test suite**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass. If source-location-sensitive tests fail, investigate whether the old behavior was intentional.

**Step 5: Commit**

```
refactor(parser): fix readQuoteForm source location, unify Quote case

readQuoteForm used the post-advance token (datum position) for the
quote symbol's source location. Quote used the pre-advance token
(quote mark position). The quote mark position is more correct for
all forms — it's where the user wrote the syntactic sugar.

Fix readQuoteForm to save the pre-advance token. Quote case now
delegates to readQuoteForm like all other quote-like forms.
```

---

### Phase A3: Extract exactness markers

**Files:**
- Modify: `internal/parser/parser.go`

`TokenizerStateMarkerNumberExact` (lines 820-833) and `TokenizerStateMarkerNumberInexact` (lines 834-847) both do: advance token → recursive `readSyntax()` → apply conversion function. The only difference is `makeExact` vs `makeInexact`.

**Step 1: Extract method**

```go
func (p *Parser) readExactnessMarker(
	tok tokenizer.Token,
	convert func(syntax.SyntaxValue) (syntax.SyntaxValue, error),
) (syntax.SyntaxValue, tokenizer.Token, error) {
	p.advance()
	inner, innerTok, err := p.readSyntax()
	if err != nil {
		return nil, innerTok, err
	}
	if inner == nil {
		return nil, innerTok, p.syntaxError(tok, "expected datum after exactness prefix")
	}
	result, err := convert(inner)
	if err != nil {
		return nil, innerTok, err
	}
	return result, innerTok, nil
}
```

Replace cases:

```go
case tokenizer.TokenizerStateMarkerNumberExact:
	return p.readExactnessMarker(tok, p.makeExact)
case tokenizer.TokenizerStateMarkerNumberInexact:
	return p.readExactnessMarker(tok, p.makeInexact)
```

**Note:** Verify exact advance/error handling pattern against the inline code. The method above is a template — match token handling, error wrapping, and return values to the originals.

**Step 2: Run tests**

Run: `go test ./internal/parser/ -v -count=1`
Expected: All pass

**Step 3: Commit**

```
refactor(parser): extract readExactnessMarker from readSyntax switch

Exact and inexact prefix handlers (#e, #i) shared identical structure:
advance, read datum, apply conversion. Parameterize the conversion.
```

---

### Phase A4: Move remaining numeric inline cases to parser_number.go

**Files:**
- Modify: `internal/parser/parser.go`
- Modify: `internal/parser/parser_number.go`

Move remaining numeric inline cases to `parser_number.go` methods:

| Case | Lines | Inline Logic | New Method |
|------|-------|-------------|------------|
| Decimal fractions | 721-728 | `strconv.ParseFloat` + hash digit handling | `parseDecimalFraction()` |
| BigFloat | 858-866 | `TrimPrefixFolded` + `NewBigFloatFromString` + validation | `parseBigFloat()` |
| Signed inf | 764-773 | Sign check + `math.Inf` | `parseSignedInf()` |
| Signed nan | 774-776 | `math.NaN` wrap | `parseSignedNan()` |
| Imaginary inf | 788-797 | Sign check + `math.Inf` + `NewComplexFromParts` | `parseImaginaryInf()` |
| Imaginary nan | 798-800 | `math.NaN` + `NewComplexFromParts` | `parseImaginaryNan()` |

Each becomes a 2-line delegation in the switch.

**Step 1: Extract methods**

Move each inline block into a method on `*Parser` in `parser_number.go`, keeping the exact same logic. Do not change behavior — pure move.

**Step 2: Run tests**

Run: `go test ./internal/parser/ -v -count=1`
Expected: All pass

**Step 3: Run lint**

Run: `make lint`
Expected: Pass

**Step 4: Commit**

```
refactor(parser): move remaining numeric inline cases to parser_number.go

Decimal fractions, BigFloat, and special float cases (inf, nan,
imaginary inf/nan) moved from readSyntax switch to parser_number.go.
Consistent with existing numeric parsing delegation pattern.
```

### Expected Outcome

Switch reduces from ~238 lines to ~120 lines. Remaining inline cases are 2-3 line delegations or simple wraps (symbols, booleans, empty list, string, close delimiters) not worth extracting.

---

## Work Item B: Environment — Unify GetLocalIndexWithScopes with resolveLocal

### Current State

`GetLocalIndexWithScopes()` (lines 603-658 in `environment/environment_frame.go`) duplicates `resolveLocal()`'s parent-chain walk with its own loop. An explicit coupling comment at lines 600-602 acknowledges this. All other search-based walks already delegate to `resolveLocal()`:

| Function | Delegates to resolveLocal? |
|----------|---------------------------|
| `GetLocalIndex()` | Yes (checkScopes=false) |
| `HasLocalVariableBinding()` | Yes (checkScopes=true) |
| `GetLocalIndexWithScopes()` | **No — duplicated walk** |
| `resolveGlobal()` | N/A (different data, different locking) |
| Fixed-depth walks (3 functions) | N/A (deterministic, no search) |

**Relationship to ENVIRONMENT-CLEANUP.md:** That plan's Task 6 only documents the coupling. This work item supersedes it by eliminating the duplication.

### Key Insight

`resolveLocal` with `checkScopes=true` already calls `scopesCompatible()`, which calls `syntax.ScopesMatch()` — the same function `GetLocalIndexWithScopes` uses inline. The visitor pattern supports "collect all, pick best" if the visitor returns nil (continue) for partial matches and non-nil (stop) for perfect matches.

### Phase B1: Characterization test for maximal binding resolution

**Files:**
- Modify: `environment/environment_frame_test.go`

**Step 1: Write test**

```go
func TestGetLocalIndexWithScopes_MaximalBinding(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()

	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	sym := values.NewSymbol("x")

	// Outer: binding with [scopeA] — 1 scope
	outer := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
	outer.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable,
		[]*syntax.Scope{scopeA}, nil)

	// Inner: binding with [scopeA, scopeB] — 2 scopes
	inner := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), outer)
	inner.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable,
		[]*syntax.Scope{scopeA, scopeB}, nil)

	// Reference [scopeA, scopeB, scopeC]: both bindings match,
	// inner wins (more scopes = more specific)
	idx := inner.GetLocalIndexWithScopes(sym,
		[]*syntax.Scope{scopeA, scopeB, scopeC})
	c.Assert(idx, qt.IsNotNil)
	c.Assert(idx[1], qt.Equals, 0) // depth 0 = inner

	// Reference [scopeA, scopeC]: only outer matches
	// (inner requires scopeB which reference doesn't have)
	idx2 := inner.GetLocalIndexWithScopes(sym,
		[]*syntax.Scope{scopeA, scopeC})
	c.Assert(idx2, qt.IsNotNil)
	c.Assert(idx2[1], qt.Equals, 1) // depth 1 = outer

	// No matching scopes
	idx3 := inner.GetLocalIndexWithScopes(sym,
		[]*syntax.Scope{scopeC})
	c.Assert(idx3, qt.IsNil)
}
```

**Step 2: Run test**

Run: `go test ./environment/ -run TestGetLocalIndexWithScopes_MaximalBinding -v`
Expected: PASS (characterization test — validates current behavior before refactoring)

**Step 3: Commit**

```
test(environment): add characterization test for GetLocalIndexWithScopes

Validates maximal binding resolution (most-specific-scope-set wins),
fallback to outer bindings, and no-match nil return before refactoring
the walk to use resolveLocal.
```

---

### Phase B2: Rewrite GetLocalIndexWithScopes using resolveLocal

**Files:**
- Modify: `environment/environment_frame.go`

**Step 1: Rewrite the method**

Replace `GetLocalIndexWithScopes` (lines 595-658) with:

```go
// GetLocalIndexWithScopes returns the LocalIndex for the most specific
// scope-compatible binding of key (Flatt's maximal resolution).
// Returns nil if no compatible binding exists.
func (p *EnvironmentFrame) GetLocalIndexWithScopes(
	key *values.Symbol,
	scopes []*syntax.Scope,
) *LocalIndex {
	if p == nil || !p.hasLocal() {
		return nil
	}

	type candidate struct {
		index      *LocalIndex
		scopeCount int
	}
	var best candidate

	p.resolveLocal(key, scopes, true, func(binding *Binding, slot int, depth int) any {
		scopeCount := len(binding.Scopes())

		// Perfect match — stop walking
		if scopeCount > 0 && scopeCount == len(scopes) {
			best = candidate{NewLocalIndex(slot, depth), scopeCount}
			return true
		}

		// Better candidate than current best?
		if best.index == nil || scopeCount > best.scopeCount {
			best = candidate{NewLocalIndex(slot, depth), scopeCount}
		}
		return nil // continue collecting
	})

	return best.index
}
```

**Why `checkScopes=true` works:** `scopesCompatible()` returns true for (a) zero-scope bindings and (b) bindings where `ScopesMatch(scopes, bindingScopes)` is true. This matches the original's two branches exactly. The visitor only needs to handle candidate collection and perfect-match early exit.

**Step 2: Remove coupling comment**

Delete the comment block at lines 595-602 ("Note: this method accesses local.keys directly..." and "COUPLING: The loop below must mirror..."). No longer needed — we're using `resolveLocal` directly.

**Step 3: Run environment tests**

Run: `go test ./environment/ -v -count=1`
Expected: All pass (including characterization test from Phase B1)

**Step 4: Run full test suite**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 5: Commit**

```
refactor(environment): unify GetLocalIndexWithScopes with resolveLocal

GetLocalIndexWithScopes duplicated resolveLocal's parent-chain walk
with its own loop (documented coupling at line 600). Rewrite using
resolveLocal with checkScopes=true; the visitor handles candidate
collection and perfect-match early exit.

All parent-chain search walks now delegate to resolveLocal.
```

---

### Phase B3: Update documentation

**Files:**
- Modify: `TODO.md` — mark item done, correct description
- Modify: `plans/ENVIRONMENT-CLEANUP.md` — note Task 6 superseded

**Step 1: Update TODO.md**

Mark "Environment resolve: unify parent-chain walks" as done. Add a note correcting the description: only `GetLocalIndexWithScopes` duplicated the walk; the other functions already delegated.

**Step 2: Update ENVIRONMENT-CLEANUP.md Task 6**

Replace Task 6 body with: "Superseded — `GetLocalIndexWithScopes` now delegates to `resolveLocal` directly. See `plans/MEDIUM-REFACTORING-BATCH.md` Work Item B."

**Step 3: Commit**

```
docs: mark environment walk unification complete, update plans
```

---

## Work Item C: syntax_adapter.go — Split by Responsibility

### Current State

`internal/match/syntax_adapter.go` (812 lines) combines:

| Responsibility | Functions | ~Lines |
|----------------|-----------|--------|
| Pattern matching | `NewSyntaxMatcher`, `Match`, `MatchWithBindingChecker`, `CompileSyntaxPattern`, `literalScopesMatchWithChecker`, `filterRebindingScopes`, `GetBindings` | ~200 |
| Template expansion | `Expand`, `expandSyntaxValue`, `expandSymbol`, `applyHygieneToSymbol`, `capturedValueToSyntax`, `expandSyntaxEllipsis`, `findSyntaxPatternVariables`, `findSyntaxVarsRecursive`, `expandEscapedSyntaxTemplate`, `scopesCompatibleForSubstitution` | ~450 |
| Interfaces + config | `localScopesProvider`, `globalBindingProvider`, `hasLocalBindingProvider`, `libraryScopeProvider`, `BindingChecker`, `SyntaxMatcherOpts`, `ExpandOptions`, `CompiledPattern`, `CompilePatternOpts` | ~160 |

### Why Two Files, Not Three

TODO.md proposed splitting into `syntax_matcher.go`, `syntax_expander.go`, and `hygiene_checker.go`. Exploration shows hygiene validation is inseparable from matching and expansion:

- `applyHygieneToSymbol` is called during expansion (free-ID resolution IS expansion)
- `literalScopesMatchWithChecker` is called during matching (literal hygiene IS matching)
- `filterRebindingScopes` is a 9-line helper for `literalScopesMatchWithChecker`

A third file would contain ~50 lines of helpers that belong with their callers. The natural boundary is **matching vs expansion**, not matching vs expansion vs hygiene.

### Proposed Split

**Keep in `syntax_adapter.go` (~300 lines):**

- `SyntaxMatcher` struct definition
- `SyntaxMatcherOpts`, `CompiledPattern`, `CompilePatternOpts` structs
- All 5 interfaces (`localScopesProvider`, `globalBindingProvider`, etc.)
- `NewSyntaxMatcher()`
- `Match()`, `MatchWithBindingChecker()`
- `literalScopesMatchWithChecker()`, `filterRebindingScopes()`
- `CompileSyntaxPattern()`
- `GetBindings()`

**Move to `syntax_expand.go` (~500 lines):**

- `ExpandOptions` struct
- `Expand()`
- `expandSyntaxValue()`
- `expandSymbol()`, `scopesCompatibleForSubstitution()`
- `applyHygieneToSymbol()`
- `capturedValueToSyntax()`
- `expandSyntaxEllipsis()`
- `findSyntaxPatternVariables()`, `findSyntaxVarsRecursive()`
- `expandEscapedSyntaxTemplate()`

### Phase C1: Create syntax_expand.go

**Files:**
- Create: `internal/match/syntax_expand.go`
- Modify: `internal/match/syntax_adapter.go`

**Step 1: Create syntax_expand.go**

Create the file with `package match` header. Move the following from `syntax_adapter.go`:

1. `ExpandOptions` struct and its doc comment (lines 208-240)
2. `Expand()` method (lines 244-256)
3. `expandSyntaxValue()` method (lines 261-353)
4. `scopesCompatibleForSubstitution()` function (lines 362-365)
5. `applyHygieneToSymbol()` method (lines 369-455)
6. `expandSymbol()` method (lines 459-482)
7. `capturedValueToSyntax()` method (lines 490-533)
8. `expandSyntaxEllipsis()` method (lines 536-597)
9. `findSyntaxPatternVariables()` method (lines 600-604)
10. `findSyntaxVarsRecursive()` method (lines 606-624)
11. `expandEscapedSyntaxTemplate()` method (lines 629-668)

Add necessary imports. Methods on `*SyntaxMatcher` work across files in the same package — no type redefinition needed.

**Step 2: Verify imports**

Both files will need subsets of the original imports. `syntax_expand.go` likely needs: `context`, `github.com/aalpar/wile/environment`, `github.com/aalpar/wile/internal/syntax`, `github.com/aalpar/wile/values`, `github.com/aalpar/wile/werr`. Remove unused imports from `syntax_adapter.go` (likely `maps`).

Run: `goimports -w internal/match/syntax_adapter.go internal/match/syntax_expand.go`

**Step 3: Run tests**

Run: `go test ./internal/match/ -v -count=1`
Expected: All pass (pure file split, no code changes)

**Step 4: Run lint**

Run: `make lint`
Expected: Pass

**Step 5: Run full test suite**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 6: Commit**

```
refactor(match): split template expansion from syntax_adapter.go

Move ExpandOptions and 10 expansion methods to syntax_expand.go.
syntax_adapter.go retains: SyntaxMatcher type, matching methods,
interfaces, pattern compilation (~300 lines). syntax_expand.go
contains: expansion entry point, recursive expansion, ellipsis
handling, hygiene application (~500 lines).

Same total LOC, clearer responsibility boundaries. Two files instead
of the three proposed in TODO.md — hygiene validation is inseparable
from the matching/expansion operations that use it.
```

---

### Phase C2: Update documentation

**Files:**
- Modify: `TODO.md` — mark item done, note 2-file split vs proposed 3-file

**Step 1: Update TODO.md**

Mark "syntax_adapter.go responsibility split" as done. Note: split into 2 files (matching + expansion), not 3 as originally proposed. Hygiene validation stays with its callers.

**Step 2: Commit**

```
docs: mark syntax_adapter split complete
```

---

## Execution Order

```
Work Item A (parser)      ─── independent ─── 4 phases
Work Item B (environment) ─── independent ─── 3 phases
Work Item C (match)       ─── independent ─── 2 phases
```

All three touch different packages. Can be parallelized across agents or done sequentially. Within each item, phases must be sequential.

**Recommended order if sequential:** B → C → A
- B is highest impact (eliminates duplication + coupling comment)
- C is mechanical (pure file split)
- A has the most investigation steps (verify inline code before extracting)

Each work item should be its own branch/PR.

## Validation

After all items complete:

```bash
make lint && make covercheck
go test ./... -count=1 -timeout 120s
```

All must pass clean.
