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

// Package cxmeasure measures cyclomatic and cognitive complexity per Go
// function, so refactoring can be aimed at measured density rather than at
// whichever file someone happened to be reading.
//
// It is the measurement half of tools/cmd/cxmeasure, split out so that
// wile-goast can consume the numbers alongside its own call-graph and CFG
// analyses. Everything here returns data; nothing prints.
//
// # The two metrics, and why both
//
// Cyclomatic complexity counts decision points. It cannot distinguish a 78-arm
// dispatch table from a 78-branch thicket: both score alike.
//
// Cognitive complexity (Campbell) charges each break in linear control flow 1
// point PLUS the current nesting depth. A flat table stays cheap; a deeply
// nested body gets expensive fast. The gap between the two is the signal:
// cyclomatic >= cognitive means the function is WIDE, and wide is fine.
//
// # The wide-structure test
//
// A large function built around one dispatch switch scores high purely from the
// nesting penalty its arms pay, even when every arm is trivial on its own.
// Arms re-measures those arms in isolation and returns the verdict. A large
// collapse means the function is wide, not complex, and splitting it would
// relocate the score without making any arm easier to read.
//
// The share check is what keeps that verdict sound: a function whose complexity
// lives OUTSIDE its switch would otherwise have its few incidental arms extract
// to zero and be pronounced wide. Below MinSwitchShare the answer is
// VerdictInconclusive rather than a finding.
//
// # Known limits
//
// Nesting is charged from the function body, so a switch at depth 0 costs 1 and
// its arms are charged at depth 1. An else-if continuation does not re-nest,
// mirroring gofmt and tools/cmd/nestinglint. Binary && and || each cost 1
// without Campbell's sequence-collapsing, so long boolean chains read slightly
// high. Test files are skipped, consistent with the standalone linters.
package cxmeasure

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/aalpar/wile/pkg/werr"
)

// ErrNoSuchFunction is returned by Arms when the named function is absent.
var ErrNoSuchFunction = werr.NewStaticError("cxmeasure: no such function")

// ErrNoSwitch is returned by Arms when the function contains no switch, so the
// wide-structure test does not apply to it.
var ErrNoSwitch = werr.NewStaticError("cxmeasure: function contains no switch")

// SkippedFile is one file ScanDir could not parse.
type SkippedFile struct {
	Path string
	Err  error
}

// Verdict is the wide-structure test's answer.
type Verdict string

const (
	// VerdictWide means every arm is trivial in isolation, so splitting the
	// function relocates its score without making any arm easier to read.
	VerdictWide Verdict = "wide"
	// VerdictComplex means at least one arm is substantial on its own, so the
	// function is a genuine refactoring candidate.
	VerdictComplex Verdict = "complex"
	// VerdictInconclusive means the switch accounts for less than
	// MinSwitchShare of the function's score: it is incidental, and the
	// function's complexity lives elsewhere.
	VerdictInconclusive Verdict = "inconclusive"
)

// ArmsReport is the wide-structure test's result for one function.
type ArmsReport struct {
	Path string
	Func string
	// Own is the function measured as written.
	Own Complexity
	// SwitchCognitive is the widest switch's own cognitive subtree, and Share
	// is its fraction of Own.Cognitive — the soundness check on the verdict.
	SwitchCognitive int
	Share           float64
	// Arms is every case clause re-measured at depth 0, worst first.
	Arms []ArmStat
	// Extracted is the sum of Arms, and the three counts partition them at the
	// 0 / 1-4 / 5+ boundaries the verdict turns on.
	Extracted int
	Trivial   int
	Modest    int
	Heavy     int
	Verdict   Verdict
}

// Arms runs the wide-structure test on one function: it finds the widest switch
// in the named function and re-measures each arm at depth 0.
//
// target is a bare name or a qualified Type.Name. The verdict and every number
// behind it are returned rather than printed, so a caller can cross-reference
// them; tools/cmd/cxmeasure formats the same struct.
func Arms(path, target string) (*ArmsReport, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "cxmeasure: parsing %s", path)
	}
	found := FindFunc(file, target)
	if found == nil {
		return nil, werr.WrapForeignErrorf(ErrNoSuchFunction, "cxmeasure: %s in %s", target, path)
	}
	stmt, widest := WidestSwitch(found.Body)
	if widest == nil {
		return nil, werr.WrapForeignErrorf(ErrNoSwitch, "cxmeasure: %s in %s", target, path)
	}

	q := &ArmsReport{Path: path, Func: target, Own: Measure(found.Body), Arms: MeasureArms(widest, fset)}
	var subtree cognitiveWalker
	subtree.node(stmt, 0)
	q.SwitchCognitive = subtree.score
	if q.Own.Cognitive > 0 {
		q.Share = float64(q.SwitchCognitive) / float64(q.Own.Cognitive)
	}
	for _, a := range q.Arms {
		q.Extracted += a.Cognitive
		switch {
		case a.Cognitive == 0:
			q.Trivial++
		case a.Cognitive < 5:
			q.Modest++
		default:
			q.Heavy++
		}
	}
	switch {
	case q.Share < MinSwitchShare:
		q.Verdict = VerdictInconclusive
	case q.Heavy == 0:
		q.Verdict = VerdictWide
	default:
		q.Verdict = VerdictComplex
	}
	return q, nil
}

// MinSwitchShare is the fraction of a function's cognitive score its widest
// switch must account for before the wide-structure verdict means anything. Set
// below it, the switch is incidental and extracting its arms says nothing about
// where the function's complexity actually lives.
const MinSwitchShare = 0.5

// complexity is the metric pair. Cyclomatic counts decision points; cognitive
// charges each one by its nesting depth.
type Complexity struct {
	Cyclomatic int
	Cognitive  int
}

// funcStat is one measured function.
type FuncStat struct {
	Pkg   string
	File  string
	Name  string
	Line  int
	Lines int
	Complexity
}

// armStat is one switch arm measured as though it were its own function.
type ArmStat struct {
	Label     string
	Cognitive int
	Lines     int
}

// ScanDir walks root and measures every function in every non-test .go file.
//
// A file that fails to parse is reported in the second result rather than
// written to stderr, because a library has no business owning the caller's
// output. Callers that want the old warning print it themselves.
func ScanDir(root string) ([]FuncStat, []SkippedFile, error) {
	var q []FuncStat
	var skipped []SkippedFile
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
		found, scanErr := ScanFile(path, root)
		if scanErr != nil {
			// Carried out through skipped rather than returned: one
			// unparseable file must not abort the walk, and the caller still
			// gets the error. nilerr reads the shape and not the second
			// result, so it sees a swallow where there is a hand-off.
			skipped = append(skipped, SkippedFile{Path: path, Err: scanErr})
			return nil //nolint:nilerr // returned via the skipped slice
		}
		q = append(q, found...)
		return nil
	})
	return q, skipped, err
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

// ScanFile measures every function declaration in one file. Paths are reported
// relative to root so the output is stable across checkout locations.
func ScanFile(path, root string) ([]FuncStat, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return nil, err
	}

	rel, relErr := filepath.Rel(root, path)
	if relErr != nil {
		rel = path
	}

	var q []FuncStat
	for _, decl := range file.Decls {
		fn, isFunc := decl.(*ast.FuncDecl)
		if !isFunc || fn.Body == nil {
			continue
		}
		start := fset.Position(fn.Pos()).Line
		end := fset.Position(fn.End()).Line
		q = append(q, FuncStat{
			Pkg:        filepath.Dir(rel),
			File:       rel,
			Name:       qualifiedName(fn),
			Line:       start,
			Lines:      end - start + 1,
			Complexity: Measure(fn.Body),
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

// Measure computes both metrics over one function body.
func Measure(body *ast.BlockStmt) Complexity {
	var w cognitiveWalker
	w.stmts(body.List, 0)
	return Complexity{Cyclomatic: cyclomatic(body), Cognitive: w.score}
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

// FindFunc locates a function by bare name or by qualified Type.Name.
func FindFunc(file *ast.File, target string) *ast.FuncDecl {
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

// WidestSwitch returns the switch statement with the most arms in body, along
// with its clause block, or nils if there is no switch. Widest rather than
// first: a function's dispatch table is the thing being tested, and it is not
// necessarily the first switch encountered. The statement is returned as well as
// the clauses so the caller can measure the switch's own subtree and decide
// whether it accounts for the function at all.
func WidestSwitch(body *ast.BlockStmt) (ast.Stmt, *ast.BlockStmt) {
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

// MeasureArms scores each case clause as though its body were a function body.
func MeasureArms(clauses *ast.BlockStmt, fset *token.FileSet) []ArmStat {
	var q []ArmStat
	for _, c := range clauses.List {
		clause, isCase := c.(*ast.CaseClause)
		if !isCase {
			continue
		}
		var w cognitiveWalker
		w.stmts(clause.Body, 0)
		q = append(q, ArmStat{
			Label:     armLabel(clause),
			Cognitive: w.score,
			Lines:     fset.Position(clause.End()).Line - fset.Position(clause.Pos()).Line,
		})
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Cognitive > q[j].Cognitive
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
