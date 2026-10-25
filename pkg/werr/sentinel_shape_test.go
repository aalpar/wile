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

package werr_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// typeSentinelName is the name shape of a type-mismatch sentinel: ErrNotAFoo,
// ErrNotAnInteger. Both letters of the article are matched capitalised so that
// ErrNotable and ErrNotAnalysis are not swept in.
var typeSentinelName = regexp.MustCompile(`^ErrNot(A|An)[A-Z]`)

// typeSentinelMessage is the message shape NewTypeSentinel produces, matched
// without regard to case: "Not a pair" makes the same claim as "not a pair",
// and a sentinel spelled either way is the type-sentinel shape.
var typeSentinelMessage = regexp.MustCompile(`(?i)^not an? `)

// typeSentinelFindings parses one source file and reports every declaration
// that breaks one of three syntactic rules:
//
//	arm 1: a package-level var named like a type sentinel is not built by
//	       NewTypeSentinel (the constructor that sets TypeName). An alias,
//	       a var bound to another identifier that is itself named like a type
//	       sentinel (ErrNotAMatch = werr.ErrNotAMatch), is accepted: the
//	       original is checked where it is declared.
//	arm 2: a NewStaticError call whose literal message reads "not a(n) ..."
//	       (the message shape of a type sentinel) is not built by
//	       NewTypeSentinel. Only a plain string literal as the first argument
//	       is read: a concatenated, constant or computed message, and a call
//	       with no argument, escape the rule. The match is anchored at the
//	       start of the message and ignores case.
//	arm 3: an identifier named like a type sentinel is the target of a plain
//	       assignment (=). The walk reads declarations, so a sentinel replaced
//	       after declaration, by a message arm 2 cannot recognise, would
//	       otherwise be unseen.
//
// Each finding is prefixed with its arm so a failure says which rule fired.
// src is passed to go/parser.ParseFile: nil reads filename.
func typeSentinelFindings(t *testing.T, filename string, src any) []string {
	t.Helper()
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, filename, src, 0)
	if err != nil {
		t.Fatalf("parse %s: %v", filename, err)
	}
	var q []string
	for _, decl := range file.Decls {
		gen, ok := decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.VAR {
			continue
		}
		for _, spec := range gen.Specs {
			vs := spec.(*ast.ValueSpec)
			for i, name := range vs.Names {
				if !typeSentinelName.MatchString(name.Name) {
					continue
				}
				if i < len(vs.Values) && calleeName(vs.Values[i]) == "NewTypeSentinel" {
					continue
				}
				if i < len(vs.Values) && typeSentinelName.MatchString(identName(vs.Values[i])) {
					continue
				}
				q = append(q, "arm 1: "+fset.Position(name.Pos()).String()+": "+name.Name+
					" is named like a type sentinel but is not built by NewTypeSentinel")
			}
		}
	}
	ast.Inspect(file, func(n ast.Node) bool {
		assign, ok := n.(*ast.AssignStmt)
		if !ok || assign.Tok != token.ASSIGN {
			return true
		}
		for _, lhs := range assign.Lhs {
			name := identName(lhs)
			if typeSentinelName.MatchString(name) {
				q = append(q, "arm 3: "+fset.Position(lhs.Pos()).String()+": "+name+
					" is named like a type sentinel and is reassigned")
			}
		}
		return true
	})
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok || calleeName(call) != "NewStaticError" || len(call.Args) == 0 {
			return true
		}
		lit, ok := call.Args[0].(*ast.BasicLit)
		if !ok || lit.Kind != token.STRING {
			return true
		}
		msg, err := strconv.Unquote(lit.Value)
		if err != nil || !typeSentinelMessage.MatchString(msg) {
			return true
		}
		q = append(q, "arm 2: "+fset.Position(call.Pos()).String()+": NewStaticError("+lit.Value+
			") has a type-sentinel message and should be NewTypeSentinel")
		return true
	})
	return q
}

// identName returns the name an expression spells, bare or qualified (ErrNotAFoo
// or werr.ErrNotAFoo), and "" for anything else.
func identName(e ast.Expr) string {
	switch x := e.(type) {
	case *ast.Ident:
		return x.Name
	case *ast.SelectorExpr:
		return x.Sel.Name
	}
	return ""
}

// calleeName returns the function name of a call expression, whether spelled
// bare or qualified (NewTypeSentinel(...) or werr.NewTypeSentinel(...)), and ""
// for anything that is not a call.
func calleeName(e ast.Expr) string {
	call, ok := e.(*ast.CallExpr)
	if !ok {
		return ""
	}
	switch fn := call.Fun.(type) {
	case *ast.Ident:
		return fn.Name
	case *ast.SelectorExpr:
		return fn.Sel.Name
	}
	return ""
}

// TestTypeSentinelDeclarationShape replaces a hand-maintained list of the 51
// type sentinels. The list checked TypeName() != "" on each listed value, which
// does fail for a listed sentinel rebuilt with NewStaticError, but it was blind
// to every name nobody had added: ErrNotAPort and ErrNotAnUncaughtException
// were both already absent from it, so the guard shrank silently as the package
// grew. The assertion itself is not the defect and is not kept: NewTypeSentinel
// panics on an empty noun and always sets the expected type, so for anything
// built with it the check is vacuous, and the property that matters is how a
// sentinel is DECLARED.
//
// So the test reads the declarations. Go has no package-scope reflection, so a
// source walk is the only implementable form. It covers every non-test file in
// the module, because a type sentinel declared outside pkg/werr reaches the same
// registry/helpers rendering as one inside it. Known limits, each the price of
// reading source instead of values:
//
//   - Declarations, literals and assignments only. A sentinel replaced through
//     a pointer, or by reflection, is unseen; arm 3 sees every plain assignment
//     to a type-sentinel name, init() bodies included.
//   - Build constraints are ignored: a file excluded by //go:build is scanned
//     too, which errs toward flagging.
//
// The rules are in typeSentinelFindings. If one fires, fix the name or the
// constructor; do not delete the rule. Arm 2 matches message text, so a future
// non-type sentinel that begins "not a ..." fires it by design: the message
// shape is the claim that registry/helpers argument errors rely on to render
// "expected a Foo".
func TestTypeSentinelDeclarationShape(t *testing.T) {
	sources := moduleSources(t)
	if len(sources) < 100 {
		t.Fatalf("found %d non-test files in the module, want at least 100", len(sources))
	}
	for _, arm := range []string{"arm 1", "arm 2", "arm 3"} {
		t.Run(arm, func(t *testing.T) {
			for _, f := range sources {
				for _, finding := range typeSentinelFindings(t, f, nil) {
					if strings.HasPrefix(finding, arm) {
						t.Error(finding)
					}
				}
			}
		})
	}
}

// moduleSources returns every non-test .go file in the module, skipping the
// directories that hold no source of this module (the skip set of the walk in
// pkg/values TestValueImplementorPackages_ListIsComplete).
func moduleSources(t *testing.T) []string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("module root: %v", err)
	}
	_, err = os.Stat(filepath.Join(root, "go.mod"))
	if err != nil {
		t.Fatalf("expected go.mod at %s: %v", root, err)
	}
	var q []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			switch d.Name() {
			case "testdata", ".git", "dist", ".claude", ".worktrees":
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasSuffix(path, ".go") && !strings.HasSuffix(path, "_test.go") {
			q = append(q, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walking %s: %v", root, err)
	}
	return q
}

// TestTypeSentinelDeclarationShapeCatchesItsTargets pins each branch of the
// walk on synthetic source, so a gate that has gone blind cannot pass for a
// gate that found nothing.
func TestTypeSentinelDeclarationShapeCatchesItsTargets(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want []string
	}{
		{
			"clean type sentinel",
			`package p
var ErrNotAFoo = NewTypeSentinel("foo")`,
			nil,
		},
		{
			"qualified constructor is accepted",
			`package p
var ErrNotAnInteger = werr.NewTypeSentinel("integer")`,
			nil,
		},
		{
			"arm 1: type-sentinel name built by NewStaticError with a different message",
			`package p
var ErrNotAFoo = NewStaticError("foo missing")`,
			[]string{"arm 1: x.go:2:5: ErrNotAFoo is named like a type sentinel but is not built by NewTypeSentinel"},
		},
		{
			"arm 1: type-sentinel name bound to a non-call",
			`package p
var ErrNotAFoo = other`,
			[]string{"arm 1: x.go:2:5: ErrNotAFoo is named like a type sentinel but is not built by NewTypeSentinel"},
		},
		{
			"arm 1 and arm 2 together",
			`package p
var ErrNotACloseParen = NewStaticError("not a close parenthesis")`,
			[]string{
				"arm 1: x.go:2:5: ErrNotACloseParen is named like a type sentinel but is not built by NewTypeSentinel",
				`arm 2: x.go:2:25: NewStaticError("not a close parenthesis") has a type-sentinel message and should be NewTypeSentinel`,
			},
		},
		{
			"arm 2: any name, an article in the message",
			`package p
var ErrMissing = NewStaticError("not an opener")`,
			[]string{`arm 2: x.go:2:18: NewStaticError("not an opener") has a type-sentinel message and should be NewTypeSentinel`},
		},
		{
			"arm 2: a call inside a function body",
			`package p
func f() error { return NewStaticError("not a thing") }`,
			[]string{`arm 2: x.go:2:25: NewStaticError("not a thing") has a type-sentinel message and should be NewTypeSentinel`},
		},
		{
			"message that merely starts with not is not a type sentinel",
			`package p
var ErrX = NewStaticError("not found")
var ErrY = NewStaticError("note: a thing")
var ErrZ = NewStaticError("cannot be a thing")`,
			nil,
		},
		{
			"name that merely begins ErrNot is not a type sentinel",
			`package p
var ErrNotable = NewStaticError("notable")
var ErrNotAnalysis = NewStaticError("analysis failed")
var ErrNotFound = NewStaticError("lookup failed")`,
			nil,
		},
		{
			"message that contains the phrase after other text is not a type sentinel",
			`package p
var ErrX = NewStaticError("arg is not a thing")`,
			nil,
		},
		{
			"concatenated message escapes arm 2",
			`package p
var ErrX = NewStaticError("not a " + "thing")`,
			nil,
		},
		{
			"call with no argument is not inspected",
			`package p
var ErrX = NewStaticError()`,
			nil,
		},
		{
			"arm 2 ignores case: a capitalised phrase is the same claim",
			`package p
var ErrPairExpected = NewStaticError("Not a pair")`,
			[]string{`arm 2: x.go:2:23: NewStaticError("Not a pair") has a type-sentinel message and should be NewTypeSentinel`},
		},
		{
			"alias of a type-sentinel name is accepted",
			`package p
var ErrNotAMatch = werr.ErrNotAMatch
var ErrNotAPair = ErrNotAList`,
			nil,
		},
		{
			"arm 3: a type-sentinel name reassigned in init",
			`package p
var ErrNotAFoo = NewTypeSentinel("foo")
func init() { ErrNotAFoo = NewStaticError("foo missing") }`,
			[]string{"arm 3: x.go:3:15: ErrNotAFoo is named like a type sentinel and is reassigned"},
		},
		{
			"arm 3: a qualified type-sentinel name reassigned",
			`package p
func f() { werr.ErrNotAnInteger = other }`,
			[]string{"arm 3: x.go:2:12: ErrNotAnInteger is named like a type sentinel and is reassigned"},
		},
		{
			"arm 3: a short variable declaration is not a reassignment",
			`package p
func f() {
	ErrNotAFoo := other
	_ = ErrNotAFoo
}`,
			nil,
		},
		{
			"non-literal message (identifier or constant) is not inspected",
			`package p
const msg = "not a thing"
var ErrX = NewStaticError(msg)`,
			nil,
		},
		{
			"local variable is not package level",
			`package p
func f() {
	ErrNotAFoo := NewStaticError("foo missing")
	_ = ErrNotAFoo
}`,
			nil,
		},
		{
			"multi-name declaration checks every name",
			`package p
var ErrNotAFoo, ErrNotABar = NewTypeSentinel("foo"), NewStaticError("bar missing")`,
			[]string{"arm 1: x.go:2:17: ErrNotABar is named like a type sentinel but is not built by NewTypeSentinel"},
		},
		{
			"grouped declaration reports only the offender",
			`package p
var (
	ErrNotAFoo = NewTypeSentinel("foo")
	ErrNotABar = NewStaticError("bar missing")
)`,
			[]string{"arm 1: x.go:4:2: ErrNotABar is named like a type sentinel but is not built by NewTypeSentinel"},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := typeSentinelFindings(t, "x.go", tc.src)
			c.Assert(got, qt.DeepEquals, tc.want)
		})
	}
}

// TestErrNotACloseParenMessageIsUnchanged pins what the declaration repair
// must preserve: the sentinel's text and its type-name phrase. NewStaticError
// produced the message; NewTypeSentinel("close parenthesis") produces the same
// message and additionally sets the phrase.
func TestErrNotACloseParenMessageIsUnchanged(t *testing.T) {
	c := qt.New(t)
	c.Assert(werr.ErrNotACloseParen.Error(), qt.Equals, "not a close parenthesis")
	c.Assert(werr.ErrNotACloseParen.TypeName(), qt.Equals, "a close parenthesis")
}
