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

// Command configconstlint enforces that the engine-configurable defaults
// DefaultMaxExpandDepth and DefaultInlineThreshold are not passed as call
// arguments in production code. A call that hands the package constant to
// ExpandAndCompile overwrites the bound the engine configured
// (WithMaxExpandDepth, WithInlineThreshold); the reader of that bound is
// compilation.NamespaceMaxExpandDepth / NamespaceInlineThreshold.
//
// The one allowed site is load_bootstrap.go, which runs before the engine
// forwards its configuration and deliberately uses the defaults. Using a
// constant outside a call argument (the readers' own fallback, a struct
// default) is not flagged.
//
// It exits non-zero when any violation is found, so it can gate `make lint`.
// Test files are skipped, consistent with the other standalone lint passes.
//
// Usage:
//
//	go run ./tools/cmd/configconstlint [dir...]
//
// If no directories are given, it scans the current directory recursively.
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

// configConsts are the defaults whose call-argument use is restricted.
var configConsts = map[string]bool{
	"DefaultMaxExpandDepth":  true,
	"DefaultInlineThreshold": true,
}

// allowedFile is the only file that may pass the constants as call arguments.
const allowedFile = "load_bootstrap.go"

type finding struct {
	file string
	line int
	name string
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
		fmt.Printf("%s:%d: %s passed as a call argument: read the engine's value with compilation.Namespace%s(env)\n",
			f.file, f.line, f.name, strings.TrimPrefix(f.name, "Default"))
	}
	if len(findings) > 0 {
		fmt.Fprintf(os.Stderr, "\n%d config-constant call argument(s) found.\n", len(findings))
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

// scanFile reports each call argument that is a bare or package-qualified
// reference to a configConst, unless the file is allowedFile.
func scanFile(path string) ([]finding, error) {
	if filepath.Base(path) == allowedFile {
		return nil, nil
	}
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	var out []finding
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		for _, arg := range call.Args {
			name := constName(arg)
			if name == "" {
				continue
			}
			pos := fset.Position(arg.Pos())
			out = append(out, finding{file: pos.Filename, line: pos.Line, name: name})
		}
		return true
	})
	return out, nil
}

// constName returns the config constant an expression names, or "".
func constName(e ast.Expr) string {
	switch x := e.(type) {
	case *ast.Ident:
		if configConsts[x.Name] {
			return x.Name
		}
	case *ast.SelectorExpr:
		if configConsts[x.Sel.Name] {
			return x.Sel.Name
		}
	}
	return ""
}
