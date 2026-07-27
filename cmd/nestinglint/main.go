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

// Command nestinglint enforces the project's shallow-nesting rule
// (CODING_STYLE.md: "Keep Indentation Shallow" / "avoids 'arrow code,' where
// indentation grows with each condition checked"). It flags any function whose
// control-structure nesting exceeds a threshold, so that "arrow code" is caught
// mechanically rather than only in review.
//
// # What counts as a level
//
// A function body is depth 0. Entering the body of any block-introducing
// control structure adds one level:
//
//   - if / else / else-if     (the else-if chain stays at ONE level — see below)
//   - for / range
//   - switch / type switch    (case bodies live one level below the switch)
//   - select                  (comm-clause bodies one level below the select)
//   - an explicit nested block { }
//
// The reported depth is the deepest such level reached in the function.
//
// # else-if is flat
//
// gofmt renders `if a { } else if b { } else { }` as a flat chain at a single
// indentation level, even though the AST nests each `else if` inside the
// previous `IfStmt.Else`. This linter mirrors gofmt: an else-if continuation is
// measured at the SAME level as the `if` it continues, not one deeper. A ten-arm
// else-if ladder is depth 1, not depth 10.
//
// # Function literals are independent scopes
//
// Each closure (a `func(...) { ... }` literal) is measured as its own function,
// with its body starting fresh at depth 0. The depth walker only descends into
// statement bodies, never into expressions, so a literal's interior is never
// counted against its enclosing function; the top-level ast.Inspect visits the
// literal separately. This keeps the metric per-function and avoids charging a
// deeply-placed callback for the nesting of its call site.
//
// Depth is a positional/structural property that the gocritic/ruleguard DSL
// cannot express, so this lives as a standalone go/ast pass, mirroring
// cmd/singlelinefunclint and cmd/typeswitchlint.
//
// It exits non-zero when any violation is found, so it can gate `make lint`.
//
// Usage:
//
//	go run ./cmd/nestinglint [-max N] [dir...]
//
// If no directories are given, it scans the current directory recursively.
// Test files (_test.go) are skipped, consistent with the other standalone
// linters and the ruleguard rules that exempt tests from production-only
// conventions.
package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
)

// defaultMaxDepth is the deepest control nesting a function may reach before it
// is reported. Calibrated against the codebase: the overwhelming majority of
// functions sit at or below this depth, so a violation is a genuine outlier
// worth splitting, not routine code.
//
// NOTE: this default is not the gate. make lint runs the tool with -max 6; 11
// functions currently exceed depth 4.
const defaultMaxDepth = 4

type finding struct {
	file  string
	line  int
	desc  string
	depth int
}

func main() {
	maxDepth := flag.Int("max", defaultMaxDepth, "maximum allowed control-nesting depth per function")
	flag.Parse()
	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var findings []finding
	for _, dir := range dirs {
		fs, err := scanDir(dir, *maxDepth)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error scanning %s: %v\n", dir, err)
			os.Exit(2)
		}
		findings = append(findings, fs...)
	}

	for _, f := range findings {
		fmt.Printf("%s:%d: %s nests %d levels deep (max %d): flatten with early returns / continue (CODING_STYLE.md)\n",
			f.file, f.line, f.desc, f.depth, *maxDepth)
	}
	if len(findings) > 0 {
		fmt.Fprintf(os.Stderr, "\n%d over-nested function(s) found.\n", len(findings))
		os.Exit(1)
	}
}

func scanDir(root string, maxDepth int) ([]finding, error) {
	var result []finding
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			base := filepath.Base(path)
			if base == ".git" || base == "vendor" || base == "testdata" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		fs, scanErr := scanFile(path, maxDepth)
		if scanErr != nil {
			fmt.Fprintf(os.Stderr, "warning: skipping %s: %v\n", path, scanErr)
			return nil
		}
		result = append(result, fs...)
		return nil
	})
	return result, err
}

func scanFile(path string, maxDepth int) ([]finding, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	var out []finding
	ast.Inspect(file, func(n ast.Node) bool {
		// Every function scope is measured independently: named declarations and
		// closures alike. The depth walker never descends into a nested literal,
		// so this outer Inspect is what reaches each literal's own body.
		body, desc := functionScope(n)
		if body == nil {
			return true
		}
		res := maxNesting(body)
		if res.depth <= maxDepth {
			return true
		}
		pos := fset.Position(res.pos)
		out = append(out, finding{file: pos.Filename, line: pos.Line, desc: desc, depth: res.depth})
		return true
	})
	return out, nil
}

// functionScope returns the body and a human label for a function-defining node
// (declaration or literal), or a nil body for anything else.
func functionScope(n ast.Node) (*ast.BlockStmt, string) {
	switch fn := n.(type) {
	case *ast.FuncDecl:
		return fn.Body, "func " + fn.Name.Name
	case *ast.FuncLit:
		return fn.Body, "func literal"
	default:
		return nil, ""
	}
}

// depthResult carries the deepest nesting reached in a function and the source
// position of that deepest block, so the finding can point at the offender.
type depthResult struct {
	depth int
	pos   token.Pos
}

// maxNesting computes the deepest control-structure nesting within a single
// function body, treating else-if chains as flat and stopping at nested function
// literals (which are measured as their own scopes). The body itself is depth 0.
func maxNesting(body *ast.BlockStmt) depthResult {
	best := depthResult{depth: 0, pos: token.NoPos}
	record := func(depth int, pos token.Pos) {
		if depth <= best.depth {
			return
		}
		best.depth = depth
		best.pos = pos
	}

	var visitBlock func(b *ast.BlockStmt, depth int)
	var visitStmt func(s ast.Stmt, depth int)

	visitBlock = func(b *ast.BlockStmt, depth int) {
		if b == nil {
			return
		}
		record(depth, b.Lbrace)
		for _, s := range b.List {
			visitStmt(s, depth)
		}
	}

	visitStmt = func(s ast.Stmt, depth int) {
		switch st := s.(type) {
		case *ast.IfStmt:
			visitBlock(st.Body, depth+1)
			switch el := st.Else.(type) {
			case *ast.IfStmt:
				// else-if continuation: same visual level as this if, not deeper.
				visitStmt(el, depth)
			case *ast.BlockStmt:
				visitBlock(el, depth+1)
			}
		case *ast.ForStmt:
			visitBlock(st.Body, depth+1)
		case *ast.RangeStmt:
			visitBlock(st.Body, depth+1)
		case *ast.SwitchStmt:
			visitCases(st.Body, depth+1, visitStmt, record)
		case *ast.TypeSwitchStmt:
			visitCases(st.Body, depth+1, visitStmt, record)
		case *ast.SelectStmt:
			visitComm(st.Body, depth+1, visitStmt, record)
		case *ast.BlockStmt:
			// An explicit nested block (scoping brace) is a real indent level.
			visitBlock(st, depth+1)
		case *ast.LabeledStmt:
			// A label does not indent its statement.
			visitStmt(st.Stmt, depth)
		}
	}

	visitBlock(body, 0)
	return best
}

// visitCases walks the case clauses of a (type) switch. The switch already
// consumed one level; every case body lives at that same depth.
func visitCases(body *ast.BlockStmt, depth int, visitStmt func(ast.Stmt, int), record func(int, token.Pos)) {
	if body == nil {
		return
	}
	for _, c := range body.List {
		cc, ok := c.(*ast.CaseClause)
		if !ok {
			continue
		}
		record(depth, cc.Colon)
		for _, s := range cc.Body {
			visitStmt(s, depth)
		}
	}
}

// visitComm walks the comm clauses of a select, mirroring visitCases.
func visitComm(body *ast.BlockStmt, depth int, visitStmt func(ast.Stmt, int), record func(int, token.Pos)) {
	if body == nil {
		return
	}
	for _, c := range body.List {
		cc, ok := c.(*ast.CommClause)
		if !ok {
			continue
		}
		record(depth, cc.Colon)
		for _, s := range cc.Body {
			visitStmt(s, depth)
		}
	}
}
