# Go AST File-Level Comment Rebuild

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Standalone comments (between declarations, end-of-file, before first declaration) survive the goast s-expression round-trip. Currently they are mapped but silently dropped during unmapping.

**Architecture:** The mapper interleaves `(comment-group (text . (...)))` entries into the `decls` list at their source positions, using pointer identity against `file.Comments` to distinguish standalone groups from doc/trailing. The unmapper's `unmapDeclList` skips these entries when building `[]ast.Decl`, and `attachComments` processes them in source order alongside declarations, building standalone groups with synthetic positions. No text matching needed — placement is encoded by position in the list.

**Tech Stack:** `go/ast`, `go/token`, `go/parser`, `go/printer`, `go/format` (all stdlib)

**Reference code:**
- Mapper: `extensions/goast/mapper.go` — `mapFile` (line 183), `mapCommentGroups` (line 815)
- Unmapper: `extensions/goast/unmapper_decl.go` — `unmapFile` (line 26), `unmapDeclList` (line 49)
- Comment attachment: `extensions/goast/unmapper_comments.go` — `attachComments` (line 61)
- Primitives: `extensions/goast/prim_goast.go` — `PrimGoFormat` (line 137)
- Tests: `extensions/goast/mapper_test.go` — `roundTripFileWithComments` (line 141)

---

## Design

### Problem

`mapFile` emits a `comments` field containing ALL comment groups from `file.Comments` (line 194). But `attachComments` never reads it — it only collects doc/trailing comments from individual nodes' `doc`/`comment` fields. Groups not attached to any node (standalone comments) are lost.

### Why text matching fails

A naive approach — build all groups from the `comments` field, match doc/trailing by text — can't determine WHERE standalone comments belong relative to doc-less declarations. If `func A()` has no doc, standalone comments before it get absorbed by the next function with doc, ending up with positions after `A` instead of before it.

### Solution: interleave in the decls list

The mapper has the source positions needed to determine placement. Instead of relying on a flat `comments` list, it interleaves standalone comment groups into the `decls` list at their source positions:

```scheme
;; Before (comments mode):
(file
  (name . "main")
  (decls . ((func-decl ...) (func-decl ...)))
  (comments . (("// standalone") ("// Doc for B"))))

;; After (comments mode):
(file
  (name . "main")
  (decls . (
    (comment-group (text . ("// standalone")))
    (func-decl (name . "A") ...)
    (func-decl (doc . ("// Doc for B")) (name . "B") ...)))
  (comments . (("// standalone") ("// Doc for B"))))
```

The `comments` field is unchanged (backward compat). The `decls` list now carries placement information implicitly through ordering.

### Classification

The mapper uses pointer identity to classify groups:

```go
attached := collectAttached(f)  // set of *ast.CommentGroup from Doc/Comment fields
for _, g := range f.Comments {
    if !attached[g] {
        // standalone — emit as (comment-group ...) at this source position
    }
}
```

`collectAttached` walks `file.Doc`, all declaration `Doc` fields, all spec `Doc`/`Comment` fields, and all struct/interface field `Doc`/`Comment` fields. Any group in `file.Comments` not in this set is standalone.

### Unmapper changes

1. `unmapDeclList` skips `comment-group` entries (they're not `ast.Decl` nodes)
2. `attachComments` replaces `walkParallel` with a mixed-list walk that dispatches by tag: `comment-group` entries become standalone groups with synthetic positions; declaration entries are processed as before

---

### Task 1: Write failing tests for standalone comment round-trip

**Files:**
- Modify: `extensions/goast/mapper_test.go`

**Step 1: Add test cases**

Append to `mapper_test.go`, after `TestRoundTripFuncBodyStatements`:

```go
func TestRoundTripStandaloneCommentBetweenDecls(t *testing.T) {
	roundTripFileWithComments(t,
		"package p\n\nvar X int\n\n// standalone between X and Y\n\nvar Y int\n")
}

func TestRoundTripStandaloneCommentEndOfFile(t *testing.T) {
	roundTripFileWithComments(t,
		"package p\n\nvar X int\n\n// end of file comment\n")
}

func TestRoundTripStandaloneCommentBeforeFirstDecl(t *testing.T) {
	roundTripFileWithComments(t,
		"package p\n\n// standalone before first decl\n\nvar X int\n")
}

func TestRoundTripMultipleStandaloneComments(t *testing.T) {
	roundTripFileWithComments(t,
		"package p\n\nvar X int\n\n// first standalone\n\n// second standalone\n\nvar Y int\n")
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goast/... -run TestRoundTripStandalone -timeout 30s`
Expected: FAIL — standalone comments are lost during round-trip.

**Step 3: Commit**

```
test(goast): add failing tests for standalone comment round-trip

Standalone comments between declarations, at end-of-file, and before
the first declaration are lost during map/unmap/format. These tests
document the gap.
```

---

### Task 2: Mapper — interleave standalone comments in decls

Add `collectAttached` to classify comment groups by pointer identity, and `mapDeclsWithStandalone` to interleave standalone groups into the decls list.

**Files:**
- Create: `extensions/goast/mapper_comments.go`
- Modify: `extensions/goast/mapper.go` (only `mapFile`)
- Modify: `extensions/goast/mapper_test.go`

**Step 1: Write the failing test**

Append to `mapper_test.go`:

```go
func TestMapFileEmitsCommentGroupInDecls(t *testing.T) {
	c := qt.New(t)
	fset := token.NewFileSet()
	source := "package p\n\nvar X int\n\n// standalone\n\nvar Y int\n"
	f, err := parser.ParseFile(fset, "test.go", source, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	opts := &mapperOpts{fset: fset, comments: true}
	sexpr := mapNode(f, opts)

	// The decls list should contain a comment-group entry.
	fields := sexpFields(sexpr)
	declsVal, hasDeclsField := GetField(fields, "decls")
	c.Assert(hasDeclsField, qt.IsTrue)

	found := false
	tuple, ok := declsVal.(values.Tuple)
	c.Assert(ok, qt.IsTrue)
	for !values.IsEmptyList(tuple) {
		pair := tuple.(*values.Pair)
		if sexpTag(pair.Car()) == "comment-group" {
			found = true
			break
		}
		tuple = pair.Cdr().(values.Tuple)
	}
	c.Assert(found, qt.IsTrue, qt.Commentf("expected comment-group in decls"))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goast/... -run TestMapFileEmitsCommentGroupInDecls -timeout 30s`
Expected: FAIL — no `comment-group` in decls.

**Step 3: Create `mapper_comments.go`**

```go
package goast

import "go/ast"

// collectAttached returns the set of comment groups that are referenced
// by Doc or Comment fields on AST nodes. Any group in file.Comments
// NOT in this set is a standalone comment.
func collectAttached(f *ast.File) map[*ast.CommentGroup]bool {
	attached := make(map[*ast.CommentGroup]bool)
	if f.Doc != nil {
		attached[f.Doc] = true
	}
	for _, d := range f.Decls {
		switch dd := d.(type) {
		case *ast.FuncDecl:
			if dd.Doc != nil {
				attached[dd.Doc] = true
			}
		case *ast.GenDecl:
			if dd.Doc != nil {
				attached[dd.Doc] = true
			}
			for _, s := range dd.Specs {
				collectSpecAttached(s, attached)
			}
		}
	}
	return attached
}

func collectSpecAttached(s ast.Spec, attached map[*ast.CommentGroup]bool) {
	switch ss := s.(type) {
	case *ast.TypeSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
		collectTypeAttached(ss.Type, attached)
	case *ast.ValueSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
	case *ast.ImportSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
	}
}

func collectTypeAttached(t ast.Expr, attached map[*ast.CommentGroup]bool) {
	switch tt := t.(type) {
	case *ast.StructType:
		if tt.Fields != nil {
			for _, f := range tt.Fields.List {
				if f.Doc != nil {
					attached[f.Doc] = true
				}
				if f.Comment != nil {
					attached[f.Comment] = true
				}
			}
		}
	case *ast.InterfaceType:
		if tt.Methods != nil {
			for _, f := range tt.Methods.List {
				if f.Doc != nil {
					attached[f.Doc] = true
				}
				if f.Comment != nil {
					attached[f.Comment] = true
				}
			}
		}
	}
}

// mapDeclsWithStandalone interleaves declarations with standalone comment
// groups in source order. Standalone groups are emitted as
// (comment-group (text . ("// ..."))) entries.
func mapDeclsWithStandalone(f *ast.File, opts *mapperOpts) []values.Value {
	attached := collectAttached(f)
	var entries []values.Value
	ci := 0
	for _, d := range f.Decls {
		// Emit standalone comments before this declaration.
		for ci < len(f.Comments) && f.Comments[ci].Pos() < d.Pos() {
			if !attached[f.Comments[ci]] {
				entries = append(entries, Node("comment-group",
					Field("text", commentGroupToStrings(f.Comments[ci]))))
			}
			ci++
		}
		entries = append(entries, mapNode(d, opts))
	}
	// Emit remaining standalone comments after all declarations.
	for ci < len(f.Comments) {
		if !attached[f.Comments[ci]] {
			entries = append(entries, Node("comment-group",
				Field("text", commentGroupToStrings(f.Comments[ci]))))
		}
		ci++
	}
	return entries
}
```

**Step 4: Modify `mapFile` in `mapper.go`**

Replace lines 183–197 of `mapper.go`:

```go
func mapFile(f *ast.File, opts *mapperOpts) values.Value {
	var decls []values.Value
	if opts.comments && opts.fset != nil {
		decls = mapDeclsWithStandalone(f, opts)
	} else {
		decls = make([]values.Value, len(f.Decls))
		for i, d := range f.Decls {
			decls[i] = mapNode(d, opts)
		}
	}
	var fs []values.Value
	fs = append(fs,
		Field("name", Str(f.Name.Name)),
		Field("decls", ValueList(decls)),
	)
	if opts.comments {
		fs = append(fs, Field("comments", mapCommentGroups(f.Comments)))
	}
	return Node("file", fs...)
}
```

**Step 5: Run test to verify it passes**

Run: `go test -v ./extensions/goast/... -run TestMapFileEmitsCommentGroupInDecls -timeout 30s`
Expected: PASS.

**Step 6: Run existing tests to verify no regression**

Run: `go test -v ./extensions/goast/... -run 'TestRoundTripFiles$|TestMapComments' -timeout 30s`
Expected: PASS — existing tests don't have standalone comments.

**Step 7: Run lint**

Run: `make lint`
Expected: Clean.

**Step 8: Commit**

```
feat(goast): mapper interleaves standalone comments in decls list

collectAttached walks all Doc/Comment fields to identify attached
groups by pointer identity. mapDeclsWithStandalone emits unattached
groups as (comment-group (text . (...))) entries at their source
positions in the decls list. Only active in comments mode.
```

---

### Task 3: Unmapper — skip comment-group in unmapDeclList

Modify `unmapDeclList` to silently skip `comment-group` entries, since they are not Go declarations.

**Files:**
- Modify: `extensions/goast/unmapper_decl.go`
- Modify: `extensions/goast/mapper_test.go`

**Step 1: Write the failing test**

Append to `mapper_test.go`:

```go
func TestUnmapFileSkipsCommentGroup(t *testing.T) {
	c := qt.New(t)
	fset := token.NewFileSet()
	source := "package p\n\nvar X int\n\n// standalone\n\nvar Y int\n"
	f, err := parser.ParseFile(fset, "test.go", source, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	opts := &mapperOpts{fset: fset, comments: true}
	sexpr := mapNode(f, opts)

	n, unmapErr := unmapNode(sexpr)
	c.Assert(unmapErr, qt.IsNil)

	file := n.(*ast.File)
	// file.Decls should have exactly 2 declarations (the comment-group is skipped).
	c.Assert(len(file.Decls), qt.Equals, 2)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goast/... -run TestUnmapFileSkipsCommentGroup -timeout 30s`
Expected: FAIL — `unmapNode` returns error on `comment-group` tag (unknown node tag).

**Step 3: Modify `unmapDeclList` in `unmapper_decl.go`**

Replace `unmapDeclList` (lines 49–62):

```go
func unmapDeclList(v values.Value) ([]ast.Decl, error) {
	if IsFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of declarations, got %T", v)
	}
	var decls []ast.Decl
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got %T", tuple)
		}
		// Skip comment-group entries (standalone comments interleaved by mapper).
		if sexpTag(pair.Car()) == "comment-group" {
			cdr, ok := pair.Cdr().(values.Tuple)
			if !ok {
				return nil, werr.WrapForeignErrorf(errMalformedGoAST,
					"goast: improper list in declarations")
			}
			tuple = cdr
			continue
		}
		n, err := unmapNode(pair.Car())
		if err != nil {
			return nil, err
		}
		d, ok := n.(ast.Decl)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected declaration, got %T", n)
		}
		decls = append(decls, d)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got improper cdr %T", pair.Cdr())
		}
		tuple = cdr
	}
	return decls, nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goast/... -run TestUnmapFileSkipsCommentGroup -timeout 30s`
Expected: PASS.

**Step 5: Run existing tests**

Run: `go test -v ./extensions/goast/... -timeout 60s`
Expected: All PASS.

**Step 6: Run lint**

Run: `make lint`
Expected: Clean.

**Step 7: Commit**

```
feat(goast): unmapper skips comment-group entries in decls list

unmapDeclList now silently skips (comment-group ...) entries that
the mapper interleaves for standalone comment placement. These
entries are handled by attachComments, not by the structural unmapper.
```

---

### Task 4: attachComments — process mixed decls list

Replace the `walkParallel` call in `attachComments` with a mixed-list walk that handles both `comment-group` entries (building standalone groups) and declaration entries (existing doc/trailing logic).

**Files:**
- Modify: `extensions/goast/unmapper_comments.go`

**Step 1: Run standalone tests to verify they still fail**

Run: `go test -v ./extensions/goast/... -run TestRoundTripStandalone -timeout 30s`
Expected: FAIL — `attachComments` doesn't process comment-group entries yet.

**Step 2: Add `forEachSexpr` helper to `unmapper_comments.go`**

Add before the existing `walkParallel` function:

```go
// forEachSexpr iterates a Scheme proper list, calling fn for each element.
func forEachSexpr(v values.Value, fn func(values.Value) error) error {
	if IsFalse(v) {
		return nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list, got %T", v)
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list, got %T", tuple)
		}
		if err := fn(pair.Car()); err != nil {
			return err
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list")
		}
		tuple = cdr
	}
	return nil
}
```

**Step 3: Rewrite `attachComments`**

Replace `attachComments` (lines 61–81):

```go
func attachComments(file *ast.File, fileSexprFields values.Value, fset *token.FileSet) error {
	alloc := newPosAllocator(fset)
	var cgs []*ast.CommentGroup

	pkgPos := alloc.nextLine()
	file.Package = pkgPos
	file.Name.NamePos = pkgPos

	declsVal, _ := GetField(fileSexprFields, "decls")
	declIdx := 0

	err := forEachSexpr(declsVal, func(elem values.Value) error {
		tag := sexpTag(elem)
		if tag == "comment-group" {
			fields := sexpFields(elem)
			textsVal, _ := GetField(fields, "text")
			if IsFalse(textsVal) {
				return nil
			}
			g, gErr := stringsToCommentGroup(textsVal, func() token.Pos {
				return alloc.nextLine()
			})
			if gErr != nil {
				return gErr
			}
			if g != nil {
				cgs = append(cgs, g)
			}
			return nil
		}
		// Declaration entry — process with existing logic.
		if declIdx >= len(file.Decls) {
			return nil
		}
		dErr := attachDeclComments(file.Decls[declIdx], elem, alloc, &cgs)
		declIdx++
		return dErr
	})
	if err != nil {
		return err
	}

	file.Comments = cgs
	return nil
}
```

**Step 4: Run all tests**

Run: `go test -v ./extensions/goast/... -timeout 120s`
Expected: ALL PASS — including the new standalone tests from Task 1.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(goast): attachComments processes interleaved standalone comments

attachComments now walks the mixed decls list (declarations +
comment-group entries) instead of using walkParallel. Standalone
comment groups get synthetic positions in source order alongside
declarations, so go/printer places them correctly.

Fixes: standalone comments between declarations, at end-of-file,
and before the first declaration now survive the round-trip.
```

---

### Task 5: Edge case tests, cleanup, and plan update

Additional tests for edge cases. Remove `walkParallel` if no longer used. Update plan docs.

**Files:**
- Modify: `extensions/goast/mapper_test.go`
- Modify: `extensions/goast/unmapper_comments.go` (cleanup)
- Modify: `plans/GO-AST.md`
- Modify: `TODO.md`

**Step 1: Add edge case tests**

Append to `mapper_test.go`:

```go
func TestRoundTripStandaloneCommentWithDocComment(t *testing.T) {
	// Standalone comment followed by a doc comment on the next decl.
	roundTripFileWithComments(t,
		"package p\n\nvar X int\n\n// standalone\n\n// Doc for Y.\nvar Y int\n")
}

func TestRoundTripStandaloneBlockComment(t *testing.T) {
	roundTripFileWithComments(t,
		"package p\n\nvar X int\n\n/* block standalone */\n\nvar Y int\n")
}

func TestRoundTripOnlyStandaloneComments(t *testing.T) {
	// File with no doc comments at all — only standalone.
	roundTripFileWithComments(t,
		"package p\n\n// standalone only\n")
}

func TestRoundTripNoCommentGroupInDeclsWithoutCommentsFlag(t *testing.T) {
	// Without comments flag, decls should NOT contain comment-group entries.
	c := qt.New(t)
	fset := token.NewFileSet()
	source := "package p\n\nvar X int\n\n// standalone\n\nvar Y int\n"
	f, err := parser.ParseFile(fset, "test.go", source, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	opts := &mapperOpts{fset: fset, comments: false}
	sexpr := mapNode(f, opts)

	fields := sexpFields(sexpr)
	declsVal, _ := GetField(fields, "decls")
	tuple, ok := declsVal.(values.Tuple)
	c.Assert(ok, qt.IsTrue)
	for !values.IsEmptyList(tuple) {
		pair := tuple.(*values.Pair)
		c.Assert(sexpTag(pair.Car()), qt.Not(qt.Equals), "comment-group",
			qt.Commentf("comment-group should not appear without comments flag"))
		tuple = pair.Cdr().(values.Tuple)
	}
}
```

**Step 2: Run all tests**

Run: `go test -v ./extensions/goast/... -timeout 120s`
Expected: ALL PASS.

**Step 3: Check if `walkParallel` is still used**

Search for `walkParallel` in `unmapper_comments.go`. It is still used by `attachDeclComments` (for specs within a GenDecl) and `attachTypeFieldComments` (for struct/interface fields). Do NOT remove it.

**Step 4: Run full suite**

Run: `make lint && make test`
Expected: Clean and all pass.

**Step 5: Run covercheck**

Run: `make covercheck`
Expected: `extensions/goast` coverage meets threshold.

**Step 6: Update `TODO.md`**

Mark the item done:
```
- [x] **Go AST: file-level comment rebuild** [Standard library, S]: ...
```

**Step 7: Update `plans/GO-AST.md`**

Add a note under the "Round-trip Comment Fidelity" section:

```
Standalone comments (between declarations, end-of-file, before first declaration)
are preserved by interleaving `(comment-group ...)` entries in the `decls` list.
The mapper classifies groups using pointer identity against Doc/Comment fields;
unattached groups are emitted at their source positions.
```

**Step 8: Commit**

```
feat(goast): complete file-level comment rebuild with edge case tests

Adds tests for standalone+doc combos, block comments, standalone-only
files, and comments-flag-off guard. Updates TODO.md and plan docs.
```

---

## Post-implementation checklist

- [ ] All 5 task commits on branch
- [ ] `make lint` clean
- [ ] `make test` passes (full suite)
- [ ] `make covercheck` passes
- [ ] Standalone comments between declarations survive round-trip
- [ ] Standalone comments at end-of-file survive round-trip
- [ ] Standalone comments before first declaration survive round-trip
- [ ] Multiple consecutive standalone comments survive round-trip
- [ ] Standalone + doc comment combos work correctly
- [ ] Block comments (`/* ... */`) work as standalone
- [ ] Without `'comments` flag, no `comment-group` entries in decls
- [ ] Existing round-trip tests still pass (no regression)
- [ ] `TODO.md` updated
- [ ] `plans/GO-AST.md` updated
