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

// Command cxmeasure reports cyclomatic and cognitive complexity per function,
// file, and package, so that refactoring is aimed at measured density rather
// than at whichever file someone happened to be reading.
//
// It is a REPORTER, not a gate: it always exits 0 on a successful scan and is
// deliberately not wired into `make lint`. Its three siblings under tools/cmd
// (singlelinefunclint, typeswitchlint, nestinglint) do gate the build; this one
// answers "where is the complexity" rather than "is this line legal".
//
// # The two metrics, and why both
//
// Cyclomatic complexity counts decision points. It cannot distinguish a 78-arm
// dispatch table from a 78-branch thicket: both score alike.
//
// Cognitive complexity (Campbell) charges each break in linear control flow 1
// point PLUS the current nesting depth. A flat table stays cheap; a deeply
// nested body gets expensive fast. The gap between the two metrics is the
// signal: cyclomatic >= cognitive means the function is WIDE, and wide is fine.
//
// # The wide-structure test (-arms)
//
// A large function built around one dispatch switch scores high on cognitive
// complexity purely from the nesting penalty its arms pay, even when every arm
// is trivial read on its own. Before filing such a function as a refactoring
// candidate, re-measure its arms in isolation:
//
//	go run ./tools/cmd/cxmeasure -arms pkg/machine/machine_context.go:Run
//
// If the extracted total collapses, the function is wide, not complex, and
// splitting it would relocate the score without making any arm easier to read.
// This is exactly the case for MachineContext.Run: cognitive 305 as written,
// 98 with its 78 arms extracted, 77 of which score below 5.
//
// # Known limits
//
// Nesting is charged from the function body, so a switch at depth 0 costs 1 and
// its arms are charged at depth 1. An else-if continuation does not re-nest,
// mirroring gofmt and tools/cmd/nestinglint. Binary && and || each cost 1 without
// Campbell's sequence-collapsing, so long boolean chains read slightly high.
// Test files are skipped, consistent with the standalone linters.
//
// Usage:
//
//	go run ./tools/cmd/cxmeasure [-by func|file|pkg] [-top N] [dir...]
//	go run ./tools/cmd/cxmeasure -arms <file.go>:<FuncName> [dir...]
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

// minFuncsPerFile and minLOCPerPackage keep a one-function file or a two-file
// package from topping a density ranking on a sample of one. Rows below the
// threshold are counted and reported in a footer rather than dropped silently.
const (
	minFuncsPerFile   = 3
	minLOCPerPackage  = 2000
	notableCognitive  = 25
	seriousCognitive  = 50
	defaultTopEntries = 25
)

// minSwitchShare is the fraction of a function's cognitive score its widest
// switch must account for before the wide-structure verdict means anything. Set
// below it, the switch is incidental and extracting its arms says nothing about
// where the function's complexity actually lives.
const minSwitchShare = 0.5

// complexity is the metric pair. Cyclomatic counts decision points; cognitive
// charges each one by its nesting depth.
type complexity struct {
	cyclomatic int
	cognitive  int
}

// funcStat is one measured function.
type funcStat struct {
	pkg   string
	file  string
	name  string
	line  int
	lines int
	complexity
}

// group is an aggregate over functions, used for both file and package rollups.
type group struct {
	key       string
	cognitive int
	cyclo     int
	funcs     int
	lines     int
	files     map[string]bool
	notable   int
	serious   int
	worst     funcStat
}

func main() {
	by := flag.String("by", "func", "aggregation level: func, file, or pkg")
	top := flag.Int("top", defaultTopEntries, "how many rows to print")
	arms := flag.String("arms", "", "re-measure one function's switch arms in isolation: <file.go>:<FuncName>")
	flag.Parse()

	if *arms != "" {
		reportArms(*arms)
		return
	}

	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var stats []funcStat
	for _, dir := range dirs {
		found, err := scanDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cxmeasure: scanning %s: %v\n", dir, err)
			os.Exit(2)
		}
		stats = append(stats, found...)
	}

	switch *by {
	case "func":
		reportFuncs(stats, *top)
	case "file":
		reportGroups(stats, *top, "file", groupByFile, minFuncsPerFile, 0)
	case "pkg":
		reportGroups(stats, *top, "package", groupByPkg, 0, minLOCPerPackage)
	default:
		fmt.Fprintf(os.Stderr, "cxmeasure: unknown -by value %q (want func, file, or pkg)\n", *by)
		os.Exit(2)
	}
}

// scanDir walks root and measures every function in every non-test .go file.
func scanDir(root string) ([]funcStat, error) {
	var q []funcStat
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if skippedDir(d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		if !measurable(path) {
			return nil
		}
		found, scanErr := scanFile(path, root)
		if scanErr != nil {
			fmt.Fprintf(os.Stderr, "cxmeasure: warning: skipping %s: %v\n", path, scanErr)
			return nil
		}
		q = append(q, found...)
		return nil
	})
	return q, err
}

// skippedDir reports whether a directory is excluded from the walk. testdata is
// excluded so linter fixtures do not pollute the ranking, matching tools/cmd/nestinglint.
func skippedDir(base string) bool {
	switch base {
	case ".git", "vendor", "testdata", "dist", "build", "node_modules":
		return true
	default:
		return false
	}
}

// measurable reports whether path is a non-test Go source file.
func measurable(path string) bool {
	if !strings.HasSuffix(path, ".go") {
		return false
	}
	return !strings.HasSuffix(path, "_test.go")
}

// scanFile measures every function declaration in one file. Paths are reported
// relative to root so the output is stable across checkout locations.
func scanFile(path, root string) ([]funcStat, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	rel, relErr := filepath.Rel(root, path)
	if relErr != nil {
		rel = path
	}

	var q []funcStat
	for _, decl := range file.Decls {
		fn, isFunc := decl.(*ast.FuncDecl)
		if !isFunc || fn.Body == nil {
			continue
		}
		start := fset.Position(fn.Pos()).Line
		end := fset.Position(fn.End()).Line
		q = append(q, funcStat{
			pkg:        filepath.Dir(rel),
			file:       rel,
			name:       qualifiedName(fn),
			line:       start,
			lines:      end - start + 1,
			complexity: measure(fn.Body),
		})
	}
	return q, nil
}

// qualifiedName renders a function as Name, or Type.Name for a method.
func qualifiedName(fn *ast.FuncDecl) string {
	if fn.Recv == nil || len(fn.Recv.List) == 0 {
		return fn.Name.Name
	}
	return receiverName(fn.Recv.List[0].Type) + "." + fn.Name.Name
}

// receiverName renders a receiver type expression, keeping the pointer star so
// value and pointer receivers stay distinguishable in the report.
func receiverName(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.StarExpr:
		return "*" + receiverName(t.X)
	case *ast.Ident:
		return t.Name
	case *ast.IndexExpr:
		return receiverName(t.X)
	case *ast.IndexListExpr:
		return receiverName(t.X)
	default:
		return "?"
	}
}

// measure computes both metrics over one function body.
func measure(body *ast.BlockStmt) complexity {
	var w cognitiveWalker
	w.stmts(body.List, 0)
	return complexity{cyclomatic: cyclomatic(body), cognitive: w.score}
}

// cyclomatic counts decision points, plus one for the function's own entry.
// It is depth-blind by construction, which is what makes it useful only in
// contrast with the cognitive score.
func cyclomatic(body *ast.BlockStmt) int {
	q := 1
	ast.Inspect(body, func(n ast.Node) bool {
		switch t := n.(type) {
		case *ast.IfStmt, *ast.ForStmt, *ast.RangeStmt, *ast.CaseClause, *ast.CommClause:
			q++
		case *ast.BinaryExpr:
			if t.Op == token.LAND || t.Op == token.LOR {
				q++
			}
		}
		return true
	})
	return q
}

// cognitiveWalker accumulates Campbell-style cognitive complexity. Each break
// in linear control flow costs 1 plus the depth at which it occurs, so nesting
// is charged and breadth is not.
type cognitiveWalker struct {
	score int
}

// increment charges one structure at the given nesting depth.
func (p *cognitiveWalker) increment(depth int) {
	p.score += 1 + depth
}

// stmts walks a statement list at a fixed depth.
func (p *cognitiveWalker) stmts(ss []ast.Stmt, depth int) {
	for _, s := range ss {
		p.node(s, depth)
	}
}

// cases walks the arms of a (type) switch, which sit one level below it.
func (p *cognitiveWalker) cases(body *ast.BlockStmt, depth int) {
	if body == nil {
		return
	}
	for _, c := range body.List {
		clause, isCase := c.(*ast.CaseClause)
		if !isCase {
			continue
		}
		p.stmts(clause.Body, depth)
	}
}

// comms walks the arms of a select, mirroring cases.
func (p *cognitiveWalker) comms(body *ast.BlockStmt, depth int) {
	if body == nil {
		return
	}
	for _, c := range body.List {
		clause, isComm := c.(*ast.CommClause)
		if !isComm {
			continue
		}
		p.stmts(clause.Body, depth)
	}
}

// ifStmt handles the one shape with a special rule: an else-if continuation is
// charged at the SAME depth as the if it continues (gofmt renders the chain
// flat), while a bare else costs a flat 1 with no depth surcharge.
func (p *cognitiveWalker) ifStmt(t *ast.IfStmt, depth int) {
	p.increment(depth)
	p.node(t.Cond, depth)
	p.stmts(t.Body.List, depth+1)
	if t.Else == nil {
		return
	}
	elseIf, isElseIf := t.Else.(*ast.IfStmt)
	if isElseIf {
		p.node(elseIf, depth)
		return
	}
	block, isBlock := t.Else.(*ast.BlockStmt)
	if !isBlock {
		return
	}
	p.score++
	p.stmts(block.List, depth+1)
}

// node dispatches one AST node. Expression arms are walked so that boolean
// operators inside conditions and call arguments are counted; the walk
// deliberately does not descend into every expression kind, since only control
// flow and boolean sequencing carry cognitive cost.
func (p *cognitiveWalker) node(n ast.Node, depth int) {
	if n == nil {
		return
	}
	switch t := n.(type) {
	case *ast.IfStmt:
		p.ifStmt(t, depth)
	case *ast.ForStmt:
		p.increment(depth)
		p.stmts(t.Body.List, depth+1)
	case *ast.RangeStmt:
		p.increment(depth)
		p.stmts(t.Body.List, depth+1)
	case *ast.SwitchStmt:
		p.increment(depth)
		p.cases(t.Body, depth+1)
	case *ast.TypeSwitchStmt:
		p.increment(depth)
		p.cases(t.Body, depth+1)
	case *ast.SelectStmt:
		p.increment(depth)
		p.comms(t.Body, depth+1)
	case *ast.FuncLit:
		// A closure's interior is charged to its enclosing function, one level
		// deeper: unlike tools/cmd/nestinglint, which measures literals as their own
		// scopes, cognitive complexity is about what one reader must hold at once.
		p.stmts(t.Body.List, depth+1)
	case *ast.BranchStmt:
		if t.Label != nil {
			p.score++
		}
	case *ast.BinaryExpr:
		if t.Op == token.LAND || t.Op == token.LOR {
			p.score++
		}
		p.node(t.X, depth)
		p.node(t.Y, depth)
	case *ast.BlockStmt:
		p.stmts(t.List, depth)
	case *ast.LabeledStmt:
		p.node(t.Stmt, depth)
	case *ast.DeferStmt:
		p.node(t.Call, depth)
	case *ast.GoStmt:
		p.node(t.Call, depth)
	case *ast.ExprStmt:
		p.node(t.X, depth)
	case *ast.AssignStmt:
		for _, e := range t.Rhs {
			p.node(e, depth)
		}
	case *ast.ReturnStmt:
		for _, e := range t.Results {
			p.node(e, depth)
		}
	case *ast.CallExpr:
		for _, e := range t.Args {
			p.node(e, depth)
		}
	case *ast.ParenExpr:
		p.node(t.X, depth)
	case *ast.UnaryExpr:
		p.node(t.X, depth)
	}
}

// reportFuncs prints the worst individual functions, cognitive-first. The cyc
// column is there to be compared against cog: when it is the larger of the two,
// the function is wide and -arms should be run before filing it.
func reportFuncs(stats []funcStat, top int) {
	sort.Slice(stats, func(i, j int) bool {
		if stats[i].cognitive != stats[j].cognitive {
			return stats[i].cognitive > stats[j].cognitive
		}
		return stats[i].file < stats[j].file
	})

	fmt.Printf("%d functions measured\n\n", len(stats))
	fmt.Printf("%6s %6s %6s  %s\n", "cog", "cyc", "lines", "location")
	for i, s := range stats {
		if i >= top {
			break
		}
		fmt.Printf("%6d %6d %6d  %s:%d %s\n", s.cognitive, s.cyclomatic, s.lines, s.file, s.line, s.name)
	}
	if len(stats) > top {
		fmt.Printf("\n(%d further functions not shown; raise -top to see them)\n", len(stats)-top)
	}
}

// groupByFile and groupByPkg select the rollup key.
func groupByFile(s funcStat) string {
	return s.file
}

func groupByPkg(s funcStat) string {
	return s.pkg
}

// reportGroups rolls functions up by key and ranks by density. Files rank by
// cognitive per function (how hard the average function is); packages rank by
// cognitive per function-LOC (how tangled the package is per line it spends).
// Rows below the relevant floor are suppressed and counted, never dropped
// silently: a one-function file would otherwise top a per-function ranking.
func reportGroups(stats []funcStat, top int, label string, key func(funcStat) string, minFuncs, minLines int) {
	groups := make(map[string]*group)
	for _, s := range stats {
		k := key(s)
		g, seen := groups[k]
		if !seen {
			g = &group{key: k, files: make(map[string]bool)}
			groups[k] = g
		}
		g.cognitive += s.cognitive
		g.cyclo += s.cyclomatic
		g.funcs++
		g.lines += s.lines
		g.files[s.file] = true
		if s.cognitive > notableCognitive {
			g.notable++
		}
		if s.cognitive > seriousCognitive {
			g.serious++
		}
		if s.cognitive > g.worst.cognitive {
			g.worst = s
		}
	}

	var ranked []*group
	suppressed := 0
	for _, g := range groups {
		if g.funcs < minFuncs || g.lines < minLines {
			suppressed++
			continue
		}
		ranked = append(ranked, g)
	}

	sort.Slice(ranked, func(i, j int) bool {
		a, b := density(ranked[i], minLines > 0), density(ranked[j], minLines > 0)
		if a != b {
			return a > b
		}
		return ranked[i].key < ranked[j].key
	})

	fmt.Printf("%d %ss ranked by %s\n\n", len(ranked), label, densityLabel(minLines > 0))
	fmt.Printf("%7s %7s %6s %6s %8s %8s %5s %5s  %s\n",
		"cog", "cyc", "fns", "files", "fnLOC", "density", ">25", ">50", label)
	for i, g := range ranked {
		if i >= top {
			break
		}
		fmt.Printf("%7d %7d %6d %6d %8d %8.3f %5d %5d  %s  (worst: %s=%d)\n",
			g.cognitive, g.cyclo, g.funcs, len(g.files), g.lines,
			density(g, minLines > 0), g.notable, g.serious, g.key, g.worst.name, g.worst.cognitive)
	}
	if len(ranked) > top {
		fmt.Printf("\n(%d further %ss not shown; raise -top to see them)\n", len(ranked)-top, label)
	}
	if suppressed > 0 {
		fmt.Printf("(%d %ss below the ranking floor of %d functions / %d function-LOC, suppressed)\n",
			suppressed, label, minFuncs, minLines)
	}
}

// density is cognitive per function-LOC when perLine is set, else per function.
func density(g *group, perLine bool) float64 {
	if perLine {
		if g.lines == 0 {
			return 0
		}
		return float64(g.cognitive) / float64(g.lines)
	}
	if g.funcs == 0 {
		return 0
	}
	return float64(g.cognitive) / float64(g.funcs)
}

func densityLabel(perLine bool) string {
	if perLine {
		return "cognitive per function-LOC"
	}
	return "cognitive per function"
}

// armStat is one switch arm measured as though it were its own function.
type armStat struct {
	label     string
	cognitive int
	lines     int
}

// reportArms implements the wide-structure test documented above: it finds the
// widest switch in the named function and re-measures each arm at depth 0. A
// large collapse between the function's own score and the extracted total means
// the function is wide rather than complex.
func reportArms(spec string) {
	cut := strings.LastIndex(spec, ":")
	if cut < 0 {
		fmt.Fprintf(os.Stderr, "cxmeasure: -arms wants <file.go>:<FuncName>, got %q\n", spec)
		os.Exit(2)
	}
	path, target := spec[:cut], spec[cut+1:]

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cxmeasure: %v\n", err)
		os.Exit(2)
	}

	found := findFunc(file, target)
	if found == nil {
		fmt.Fprintf(os.Stderr, "cxmeasure: no function %q in %s\n", target, path)
		os.Exit(2)
	}

	stmt, widest := widestSwitch(found.Body)
	if widest == nil {
		fmt.Fprintf(os.Stderr, "cxmeasure: %s in %s contains no switch; the wide-structure test does not apply\n", target, path)
		os.Exit(2)
	}

	own := measure(found.Body)
	arms := measureArms(widest, fset)

	// How much of the function's score does this switch actually account for?
	// Without this check the verdict is unsound: a function whose complexity
	// lives outside its switch (cmd/wile/main.go's flag handling, say) would
	// have its four incidental arms extract to zero and be pronounced WIDE.
	var subtree cognitiveWalker
	subtree.node(stmt, 0)
	share := 0.0
	if own.cognitive > 0 {
		share = float64(subtree.score) / float64(own.cognitive)
	}

	total, trivial, modest, heavy := 0, 0, 0, 0
	for _, a := range arms {
		total += a.cognitive
		switch {
		case a.cognitive == 0:
			trivial++
		case a.cognitive < 5:
			modest++
		default:
			heavy++
		}
	}

	fmt.Printf("%s (%s)\n", target, path)
	fmt.Printf("  as written:        cognitive %d, cyclomatic %d\n", own.cognitive, own.cyclomatic)
	fmt.Printf("  widest switch:     %d arms, accounting for %.0f%% of the function's score\n", len(arms), share*100)
	fmt.Printf("  arms extracted:    cognitive %d\n", total)
	fmt.Printf("  arm distribution:  %d scoring 0, %d scoring 1-4, %d scoring 5+\n", trivial, modest, heavy)
	if share < minSwitchShare {
		fmt.Printf("  verdict: INCONCLUSIVE. This switch is incidental; the function's complexity is elsewhere.\n")
		return
	}
	if heavy == 0 {
		fmt.Printf("  verdict: WIDE. Every arm is trivial in isolation; splitting relocates the score.\n")
		return
	}
	fmt.Printf("  arms scoring 5 or more:\n")
	for _, a := range arms {
		if a.cognitive < 5 {
			continue
		}
		fmt.Printf("    cog %3d  lines %3d  %s\n", a.cognitive, a.lines, a.label)
	}
}

// findFunc locates a function by bare name or by qualified Type.Name.
func findFunc(file *ast.File, target string) *ast.FuncDecl {
	for _, decl := range file.Decls {
		fn, isFunc := decl.(*ast.FuncDecl)
		if !isFunc || fn.Body == nil {
			continue
		}
		if fn.Name.Name == target || qualifiedName(fn) == target {
			return fn
		}
	}
	return nil
}

// widestSwitch returns the switch statement with the most arms in body, along
// with its clause block, or nils if there is no switch. Widest rather than
// first: a function's dispatch table is the thing being tested, and it is not
// necessarily the first switch encountered. The statement is returned as well as
// the clauses so the caller can measure the switch's own subtree and decide
// whether it accounts for the function at all.
func widestSwitch(body *ast.BlockStmt) (ast.Stmt, *ast.BlockStmt) {
	var qStmt ast.Stmt
	var qClauses *ast.BlockStmt
	best := 0
	ast.Inspect(body, func(n ast.Node) bool {
		var clauses *ast.BlockStmt
		stmt, isStmt := n.(ast.Stmt)
		if !isStmt {
			return true
		}
		switch t := n.(type) {
		case *ast.SwitchStmt:
			clauses = t.Body
		case *ast.TypeSwitchStmt:
			clauses = t.Body
		default:
			return true
		}
		if clauses == nil || len(clauses.List) <= best {
			return true
		}
		best = len(clauses.List)
		qStmt = stmt
		qClauses = clauses
		return true
	})
	return qStmt, qClauses
}

// measureArms scores each case clause as though its body were a function body.
func measureArms(clauses *ast.BlockStmt, fset *token.FileSet) []armStat {
	var q []armStat
	for _, c := range clauses.List {
		clause, isCase := c.(*ast.CaseClause)
		if !isCase {
			continue
		}
		var w cognitiveWalker
		w.stmts(clause.Body, 0)
		q = append(q, armStat{
			label:     armLabel(clause),
			cognitive: w.score,
			lines:     fset.Position(clause.End()).Line - fset.Position(clause.Pos()).Line,
		})
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].cognitive > q[j].cognitive
	})
	return q
}

// armLabel renders a case clause's guards, or "default" for the default arm.
func armLabel(clause *ast.CaseClause) string {
	var names []string
	for _, e := range clause.List {
		names = append(names, exprLabel(e))
	}
	if len(names) == 0 {
		return "default"
	}
	return strings.Join(names, ",")
}

// exprLabel renders the identifier-ish forms that appear as case guards.
func exprLabel(e ast.Expr) string {
	switch t := e.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.BasicLit:
		return t.Value
	case *ast.SelectorExpr:
		return t.Sel.Name
	case *ast.StarExpr:
		return "*" + exprLabel(t.X)
	default:
		return "?"
	}
}
