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

package wile_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// narrowingModuleRoot resolves the repository root from pkg/wile.
func narrowingModuleRoot(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", ".."))
	qt.Assert(t, err, qt.IsNil)
	_, err = os.Stat(filepath.Join(root, "go.mod"))
	qt.Assert(t, err, qt.IsNil, qt.Commentf("expected go.mod at %s", root))
	return root
}

// TestClosureMarkerImplementorCountIsTwo is half of Wave 4 item 11's ratchet
// (design §8.6.3 G3.3).
//
// machine.Closure is values.Callable + NamedCallable + closureMarker(), and the
// unexported marker is what makes it a CLOSED set: only types in pkg/machine can
// join it. The whole defect was the size of that set — TWO — measured against
// ApplyCallable's SIX dispatch arms. This test pins the two so the arithmetic
// behind item 11 cannot silently change: if a third type acquires the marker,
// whoever added it must come back and re-derive whether the narrowing sites
// (now widened to values.Callable) should have been narrowed after all.
//
// It fails on a change in EITHER direction, which is the point of a count.
//
// This half of G3.3 is green at 003b3353 by construction and cannot be
// must-fail-first: the count was already two, and two is the correct answer.
// It is a PIN on the arithmetic, not a gate on the fix. The gate half is
// TestNoCallableNarrowingOutsideMachine below, which is red at 003b3353.
func TestClosureMarkerImplementorCountIsTwo(t *testing.T) {
	root := narrowingModuleRoot(t)
	dir := filepath.Join(root, "pkg", "machine")

	// One ParseFile per .go file rather than parser.ParseDir, which is
	// deprecated as of Go 1.25 for ignoring build tags. That caveat does not
	// bite here — this ratchet WANTS every file in the directory counted,
	// build-constrained or not, since a closureMarker implementor hidden
	// behind a tag would still be one. The loop reproduces ParseDir's exact
	// reach: non-recursive, every .go file including _test.go.
	fset := token.NewFileSet()
	entries, err := os.ReadDir(dir)
	qt.Assert(t, err, qt.IsNil)

	var implementors []string
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") {
			continue
		}
		file, parseErr := parser.ParseFile(fset, filepath.Join(dir, entry.Name()), nil, 0)
		qt.Assert(t, parseErr, qt.IsNil)
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || fn.Name.Name != "closureMarker" {
				continue
			}
			implementors = append(implementors, receiverTypeName(fn))
		}
	}

	qt.Assert(t, implementors, qt.HasLen, 2,
		qt.Commentf("closureMarker implementors changed: %v", implementors))
	qt.Assert(t, slices.Contains(implementors, "MachineClosure"), qt.IsTrue,
		qt.Commentf("got %v", implementors))
	qt.Assert(t, slices.Contains(implementors, "ForeignClosure"), qt.IsTrue,
		qt.Commentf("got %v", implementors))
}

// closureNarrowingExemptions lists files under pkg/registry, pkg/extensions or
// extensions that are still allowed to reference machine.Closure in code.
//
// EXACTLY ONE, AND IT IS SCHEDULED FOR DELETION. once-do! was the twelfth
// narrowing site and the one the original review led with; it is removed
// wholesale by plans/2026-08-01-remove-rwmutex-once-impl.local.md, landing on
// branch refactor/remove-rwmutex-once. Item 11 deliberately did not touch it, to
// avoid a conflict with that branch. DELETE THIS ENTRY when the removal lands —
// the test fails loudly (below) if the file is already gone, so the exemption
// cannot outlive its subject.
var closureNarrowingExemptions = map[string]string{
	"extensions/gointerop/prim_gointerop.go": "once-do!, deleted by refactor/remove-rwmutex-once",
}

// TestNoCallableNarrowingOutsideMachine is the other half of the ratchet.
//
// The behavioural gate (TestCallableNarrowingSitesAcceptEveryProcedure) pins the
// eleven sites that existed; it cannot see a TWELFTH added later. This one can:
// a primitive that declares `ParamTypes: TypeProcedure` and then asserts
// machine.Closure has promised six types and accepted two, and the only way to
// state that as a rule rather than a list is to forbid the mention outright
// above the pkg/machine layer. helpers.RequireCallable is the replacement.
//
// Observed at 003b3353: ELEVEN code mentions across six files —
// prim_control.go x3, prim_barrier.go, prim_parameters.go, prim_exit.go,
// prim_prompt.go x4, prim_files.go — plus the exempt gointerop site.
//
// Comment mentions are allowed (this file and helpers.RequireCallable's doc
// comment both name the type to explain the rule). The scan therefore skips
// whole-line // comments; a mention inside a block comment or trailing a
// statement would still trip it, which errs toward noisy rather than silent.
func TestNoCallableNarrowingOutsideMachine(t *testing.T) {
	root := narrowingModuleRoot(t)
	scanned := 0
	var offenders []string

	for _, sub := range []string{"pkg/registry", "pkg/extensions", "extensions"} {
		base := filepath.Join(root, sub)
		err := filepath.WalkDir(base, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(path, ".go") {
				return nil
			}
			rel, relErr := filepath.Rel(root, path)
			if relErr != nil {
				return relErr
			}
			rel = filepath.ToSlash(rel)
			_, exempt := closureNarrowingExemptions[rel]
			if exempt {
				return nil
			}
			scanned++
			src, readErr := os.ReadFile(path)
			if readErr != nil {
				return readErr
			}
			for i, line := range strings.Split(string(src), "\n") {
				if strings.HasPrefix(strings.TrimSpace(line), "//") {
					continue
				}
				if strings.Contains(line, "machine.Closure") {
					offenders = append(offenders, rel+":"+strconv.Itoa(i+1))
				}
			}
			return nil
		})
		qt.Assert(t, err, qt.IsNil)
	}

	qt.Assert(t, scanned > 0, qt.IsTrue, qt.Commentf("scanned no files — the walk is broken"))
	qt.Assert(t, offenders, qt.HasLen, 0, qt.Commentf(
		"these sites narrow to machine.Closure (2 implementors) where ApplyCallable "+
			"dispatches 6; use helpers.RequireCallable: %v", offenders))

	// The exemption must not outlive its subject.
	for rel, why := range closureNarrowingExemptions {
		_, statErr := os.Stat(filepath.Join(root, rel))
		qt.Assert(t, statErr, qt.IsNil, qt.Commentf(
			"exempt file %s is gone (%s) — delete its entry from "+
				"closureNarrowingExemptions", rel, why))
	}
}

func receiverTypeName(fn *ast.FuncDecl) string {
	if len(fn.Recv.List) == 0 {
		return "<no receiver>"
	}
	expr := fn.Recv.List[0].Type
	star, ok := expr.(*ast.StarExpr)
	if ok {
		expr = star.X
	}
	ident, ok := expr.(*ast.Ident)
	if !ok {
		return "<unnamed>"
	}
	return ident.Name
}
