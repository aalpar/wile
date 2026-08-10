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

// Command typeswitchlint checks that type switches over values.Value are
// exhaustive — but only the ones that opt in with an //exhaustive marker.
//
// Most type switches over values.Value are intentionally partial (eqv? only
// compares numeric pairs; a transcendental op only handles the real tower).
// Flagging every such switch produces pure noise, so this tool checks a
// switch only when it is annotated:
//
//	//exhaustive
//	switch v := x.(type) {
//	case *values.Integer: ...
//	}
//
// or with a trailing marker on the switch line:
//
//	switch v := x.(type) { //exhaustive
//
// For a marked switch, every exported values.Value type that is not cased is
// reported, and the tool exits non-zero — so it can gate `make lint`. A
// default clause does NOT excuse a missing case: marking a switch exhaustive
// is a declaration that the cases enumerate the whole domain.
//
// Scope: completeness is checked against the value types DEFINED IN package
// values (the data types), addressed as values.X from another package. It
// does not know about Value implementers defined elsewhere (closures,
// continuations, parameters), nor about unexported singletons (voidType,
// eofType, emptyListType) that cannot be named in a cross-package case. The
// known-type list is kept honest by TestKnownValueTypesMatchesSource, which
// derives the real set from the values source and fails on drift.
//
// Usage:
//
//	go run ./cmd/typeswitchlint [-v] [dir...]
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
	"slices"
	"strings"
)

// knownValueTypes lists the exported concrete types in package values that
// implement values.Value. It is asserted complete and current against the
// values source by TestKnownValueTypesMatchesSource — do not hand-edit it to
// silence that test; fix the underlying type set or the derivation.
var knownValueTypes = []string{
	// Numeric tower
	"*values.Integer",
	"*values.BigInteger",
	"*values.Float",
	"*values.BigFloat",
	"*values.Rational",
	"*values.Complex",
	"*values.BigComplex",
	// Core data
	"*values.Boolean",
	"*values.Character",
	"*values.CharSet",
	"*values.String",
	"*values.Symbol",
	"*values.Byte",
	"*values.Pair",
	"*values.Vector",
	"*values.ByteVector",
	"*values.SyntaxVector",
	"*values.Hashtable",
	"*values.Record",
	"*values.RecordType",
	"*values.Box",
	"*values.Promise",
	"*values.OpaqueValue",
	// Ports — single concrete type after the port-unification SR pass.
	"*values.PortObject",
	// Concurrency
	"*values.Thread",
	"*values.Mutex",
	"*values.ConditionVariable",
	"*values.UncaughtException",
	"*values.JoinTimeoutException",
	"*values.TerminatedThreadException",
	"*values.AbandonedMutexException",
	"*values.AtomicBox",
	"*values.AtomicInt64",
	"*values.Time",
	"*values.Process",
	// Advanced / internal-facing
	"*values.CompileTimeValue",
	"*values.NativeError",
	"*values.SourceContext",
	"*values.SourceIndexes",
}

// switchInfo is one //exhaustive-marked type switch found during the scan:
// where it lives and the case types it enumerates.
type switchInfo struct {
	file      string
	line      int
	caseTypes []string
	hasValues bool // at least one case names a values.* type
}

// main scans the directories named on the command line (or "." if none),
// reports every marked switch that is missing a value type, and exits non-zero
// if any gap is found so the tool can gate `make lint`. Exit 2 is a scan error
// (unreadable tree); exit 1 is a real exhaustiveness gap.
func main() {
	verbose := flag.Bool("v", false, "list every checked //exhaustive switch, not only those with gaps")
	flag.Parse()
	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var switches []switchInfo
	for _, dir := range dirs {
		s, err := scanDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error scanning %s: %v\n", dir, err)
			os.Exit(2)
		}
		switches = append(switches, s...)
	}

	gaps := 0
	for _, sw := range switches {
		if !sw.hasValues {
			fmt.Printf("WARNING: %s:%d — //exhaustive switch has no values.* cases; "+
				"typeswitchlint only checks switches over values.Value (cased as values.X)\n",
				sw.file, sw.line)
			gaps++
			continue
		}
		missing := findMissing(sw.caseTypes, knownValueTypes)
		if len(missing) == 0 {
			if *verbose {
				fmt.Printf("OK: %s:%d — %d cases, exhaustive\n", sw.file, sw.line, len(sw.caseTypes))
			}
			continue
		}
		gaps++
		fmt.Printf("WARNING: %s:%d — %d cases, missing %d value type(s):\n",
			sw.file, sw.line, len(sw.caseTypes), len(missing))
		for _, m := range missing {
			fmt.Printf("  - %s\n", m)
		}
	}

	fmt.Printf("\n%d //exhaustive switch(es) checked, %d with gaps.\n", len(switches), gaps)
	if gaps > 0 {
		os.Exit(1)
	}
}

// scanDir walks root recursively and returns every marked switch in its .go
// files. It skips .git, vendor, and testdata directories, and _test.go files.
// A parse error in one file is logged and skipped, not fatal; only a walk-level
// error (e.g. an unreadable path) is returned.
func scanDir(root string) ([]switchInfo, error) {
	var result []switchInfo
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
		switches, scanErr := scanFile(path)
		if scanErr != nil {
			fmt.Fprintf(os.Stderr, "warning: skipping %s: %v\n", path, scanErr)
			return nil
		}
		result = append(result, switches...)
		return nil
	})
	return result, err
}

// scanFile parses one .go file and returns its //exhaustive-marked type
// switches. A switch opts in when a marker sits on its own line or the line
// immediately above the switch keyword; the marker positions are collected
// first, then matched against each type switch during the AST walk.
func scanFile(path string) ([]switchInfo, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, parser.ParseComments)
	if err != nil {
		return nil, err
	}

	// Lines carrying an //exhaustive marker. A switch opts in when a marker
	// sits on its own line (trailing) or the line immediately above it.
	markerLines := map[int]bool{}
	for _, group := range file.Comments {
		for _, c := range group.List {
			if isExhaustiveMarker(c.Text) {
				markerLines[fset.Position(c.Slash).Line] = true
			}
		}
	}

	var result []switchInfo
	ast.Inspect(file, func(n ast.Node) bool {
		ts, ok := n.(*ast.TypeSwitchStmt)
		if !ok {
			return true
		}
		pos := fset.Position(ts.Switch)
		if !markerLines[pos.Line] && !markerLines[pos.Line-1] {
			return true
		}
		cases, hasValues := extractCaseTypes(ts)
		result = append(result, switchInfo{
			file:      pos.Filename,
			line:      pos.Line,
			caseTypes: cases,
			hasValues: hasValues,
		})
		return true
	})
	return result, nil
}

// isExhaustiveMarker reports whether a comment is the opt-in marker. It
// accepts "//exhaustive", "// exhaustive", and a trailing reason
// ("//exhaustive: dispatch must cover every value type").
func isExhaustiveMarker(text string) bool {
	t := strings.TrimSpace(strings.TrimPrefix(text, "//"))
	return t == "exhaustive" || strings.HasPrefix(t, "exhaustive:") || strings.HasPrefix(t, "exhaustive ")
}

// extractCaseTypes returns the rendered type names from every case clause of
// ts (default clauses contribute nothing) and reports whether any of them names
// a values.* type, which is what qualifies the switch for the exhaustiveness
// check.
func extractCaseTypes(ts *ast.TypeSwitchStmt) (types []string, hasValues bool) {
	for _, stmt := range ts.Body.List {
		cc, ok := stmt.(*ast.CaseClause)
		if !ok {
			continue
		}
		// A default clause has a nil List and contributes no case types.
		for _, expr := range cc.List {
			s := typeExprString(expr)
			types = append(types, s)
			if strings.Contains(s, "values.") {
				hasValues = true
			}
		}
	}
	return
}

// typeExprString renders a case-clause type expression back to source-like
// text ("*values.Integer"), recursing through pointer and selector nodes so the
// result matches the entries in knownValueTypes. An unhandled node kind yields a
// "<%T>" placeholder rather than a silent empty string, so drift is visible.
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

// findMissing returns the known value types absent from present, sorted
// alphabetically. present is a switch's case types; known is knownValueTypes,
// which is category-grouped rather than sorted, so the sort is what gives the
// stable alphabetical output. Linear scan per known type is intentional: both
// slices are tiny (~40 and a handful), so a lookup set would not pay off.
func findMissing(present []string, known []string) []string {
	var missing []string
	for _, k := range known {
		if !slices.Contains(present, k) {
			missing = append(missing, k)
		}
	}
	slices.Sort(missing)
	return missing
}
