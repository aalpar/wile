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

// Command typeswitchlint finds type switch statements on values.Value or
// values.Number that may be missing cases for concrete types.
//
// It works by parsing Go source files with go/ast, finding type switch
// statements, extracting case types, and comparing against a known list
// of concrete types. Switches with a default branch are reported as
// informational rather than warnings, since the default may already
// handle unknown types.
//
// Usage:
//
//	go run ./cmd/typeswitchlint [dir...]
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
	"sort"
	"strings"
)

// knownValueTypes lists all concrete types that implement values.Value.
// This list must be updated when a new value type is added.
// See also: values/values.go "ADDING A NEW VALUE TYPE" guide.
var knownValueTypes = []string{
	// Numeric tower
	"*values.Integer",
	"*values.BigInteger",
	"*values.Float",
	"*values.BigFloat",
	"*values.Rational",
	"*values.Complex",
	"*values.BigComplex",
	// Core values
	"*values.Boolean",
	"*values.Character",
	"*values.String",
	"*values.Symbol",
	"*values.Byte",
	"*values.Pair",
	"*values.Vector",
	"*values.ByteVector",
	"*values.Hashtable",
	"*values.Record",
	"*values.RecordType",
	"*values.Box",
	"*values.Promise",
	// Ports
	"*values.CharacterInputPort",
	"*values.CharacterOutputPort",
	"*values.BinaryInputPort",
	"*values.BinaryOutputPort",
	"*values.StringInputPort",
	"*values.StringOutputPort",
	"*values.ByteVectorInputPort",
	"*values.ByteVectorOutputPort",
	"*values.ByteVectorBufferedOutputPort",
	"*values.ByteVectorInputOutputPort",
	// Concurrency
	"*values.Thread",
	"*values.Mutex",
	"*values.ConditionVariable",
	"*values.Channel",
	"*values.WaitGroup",
	"*values.RWMutex",
	"*values.Once",
	"*values.AtomicBox",
	"*values.AtomicInt64",
	"*values.Time",
	// Advanced
	"*values.CompileTimeValue",
	"*values.SchemeEnvironment",
	"*values.NativeError",
	"*values.ForeignError",
	"*values.StaticError",
}

type switchInfo struct {
	file       string
	line       int
	caseTypes  []string
	hasDefault bool
}

func main() {
	verbose := flag.Bool("v", false, "show INFO switches (with default) in addition to WARNINGs")
	flag.Parse()
	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var switches []switchInfo
	skipped := 0
	for _, dir := range dirs {
		s, sk, err := scanDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error scanning %s: %v\n", dir, err)
			os.Exit(1)
		}
		switches = append(switches, s...)
		skipped += sk
	}

	if len(switches) == 0 {
		fmt.Println("No type switches found.")
		return
	}

	reported := 0
	for _, sw := range switches {
		missing := findMissing(sw.caseTypes, knownValueTypes)
		if len(missing) == 0 {
			continue
		}
		if sw.hasDefault && !*verbose {
			continue
		}
		reported++
		level := "WARNING"
		if sw.hasDefault {
			level = "INFO"
		}
		fmt.Printf("%s: %s:%d — %d cases, missing %d types",
			level, sw.file, sw.line, len(sw.caseTypes), len(missing))
		if sw.hasDefault {
			fmt.Print(" (has default)")
		}
		fmt.Println()
		for _, m := range missing {
			fmt.Printf("  - %s\n", m)
		}
	}

	summary := fmt.Sprintf("\n%d type switches scanned, %d with potential gaps",
		len(switches), reported)
	if skipped > 0 {
		summary += fmt.Sprintf(", %d files skipped due to errors", skipped)
	}
	fmt.Println(summary + ".")
}

func scanDir(root string) ([]switchInfo, int, error) {
	var result []switchInfo
	skipped := 0
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
		switches, err := scanFile(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "warning: skipping %s: %v\n", path, err)
			skipped++
			return nil
		}
		result = append(result, switches...)
		return nil
	})
	return result, skipped, err
}

func scanFile(path string) ([]switchInfo, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	var result []switchInfo
	ast.Inspect(file, func(n ast.Node) bool {
		ts, ok := n.(*ast.TypeSwitchStmt)
		if !ok {
			return true
		}
		cases, hasDefault := extractCaseTypes(ts)
		// Heuristic: match any case type containing "values." — relies on
		// the codebase convention of importing values without alias.
		valuesCase := false
		for _, c := range cases {
			if strings.Contains(c, "values.") {
				valuesCase = true
				break
			}
		}
		if !valuesCase {
			return true
		}
		pos := fset.Position(ts.Pos())
		result = append(result, switchInfo{
			file:       pos.Filename,
			line:       pos.Line,
			caseTypes:  cases,
			hasDefault: hasDefault,
		})
		return true
	})
	return result, nil
}

func extractCaseTypes(ts *ast.TypeSwitchStmt) (types []string, hasDefault bool) {
	for _, stmt := range ts.Body.List {
		cc, ok := stmt.(*ast.CaseClause)
		if !ok {
			continue
		}
		if cc.List == nil {
			hasDefault = true
			continue
		}
		for _, expr := range cc.List {
			types = append(types, typeExprString(expr))
		}
	}
	return
}

func typeExprString(expr ast.Expr) string {
	switch e := expr.(type) {
	case *ast.StarExpr:
		return "*" + typeExprString(e.X)
	case *ast.SelectorExpr:
		return typeExprString(e.X) + "." + e.Sel.Name
	case *ast.Ident:
		return e.Name
	default:
		return fmt.Sprintf("<%T>", expr)
	}
}

func findMissing(present []string, known []string) []string {
	set := make(map[string]bool, len(present))
	for _, p := range present {
		set[p] = true
	}
	var missing []string
	for _, k := range known {
		if !set[k] {
			missing = append(missing, k)
		}
	}
	sort.Strings(missing)
	return missing
}
