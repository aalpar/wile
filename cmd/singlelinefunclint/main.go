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

// Command singlelinefunclint enforces the project's no-single-line-function
// rule (CODING_STYLE.md: "Never write single-line functions. Always spread
// function bodies across multiple lines, even for simple implementations.").
//
// A function *declaration* violates the rule when its body has at least one
// statement and its opening and closing braces sit on the same source line.
// Empty bodies are exempt: there is nothing to spread.
//
// Function *literals* (anonymous closures passed as arguments — predicate
// tables, comparators, field accessors) are deliberately NOT checked. The
// codebase uses one-line literals idiomatically and pervasively
// (e.g. `func(a, b rune) bool { return a < b }`); the rule, like the style
// guide's "spread function bodies" wording, targets declarations. Widening
// to literals would flag dozens of correct, deliberate call sites.
//
// Same-line detection is a positional property the gocritic/ruleguard DSL
// cannot express (it matches AST shape, not brace lines), so this lives as a
// standalone go/ast pass, mirroring cmd/typeswitchlint.
//
// It exits non-zero when any violation is found, so it can gate `make lint`.
//
// Usage:
//
//	go run ./cmd/singlelinefunclint [dir...]
//
// If no directories are given, it scans the current directory recursively.
// Test files (_test.go) are skipped, consistent with the ruleguard rules in
// ruleguard/rules.go that exempt tests from the production-only conventions.
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

type finding struct {
	file string
	line int
	desc string
}

func main() {
	flag.Parse()
	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var findings []finding
	for _, dir := range dirs {
		fs, err := scanDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error scanning %s: %v\n", dir, err)
			os.Exit(2)
		}
		findings = append(findings, fs...)
	}

	for _, f := range findings {
		fmt.Printf("%s:%d: single-line function (%s): spread the body across multiple lines (CODING_STYLE.md)\n",
			f.file, f.line, f.desc)
	}
	if len(findings) > 0 {
		fmt.Fprintf(os.Stderr, "\n%d single-line function(s) found.\n", len(findings))
		os.Exit(1)
	}
}

func scanDir(root string) ([]finding, error) {
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
		fs, scanErr := scanFile(path)
		if scanErr != nil {
			fmt.Fprintf(os.Stderr, "warning: skipping %s: %v\n", path, scanErr)
			return nil
		}
		result = append(result, fs...)
		return nil
	})
	return result, err
}

func scanFile(path string) ([]finding, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	var out []finding
	ast.Inspect(file, func(n ast.Node) bool {
		// Only declarations are checked; literals are exempt (see the package
		// doc comment for the rationale).
		fn, ok := n.(*ast.FuncDecl)
		if !ok {
			return true
		}
		f, found := checkBody(fset, fn.Body, "func "+fn.Name.Name)
		if found {
			out = append(out, f)
		}
		return true
	})
	return out, nil
}

// checkBody reports a single-line body: non-empty, with both braces on one
// line. A nil body (e.g. an assembly-backed declaration) or an empty body is
// not a violation.
func checkBody(fset *token.FileSet, body *ast.BlockStmt, desc string) (finding, bool) {
	if body == nil || len(body.List) == 0 {
		return finding{}, false
	}
	open := fset.Position(body.Lbrace)
	openLine := open.Line
	closeLine := fset.Position(body.Rbrace).Line
	if openLine != closeLine {
		return finding{}, false
	}
	return finding{file: open.Filename, line: openLine, desc: desc}, true
}
