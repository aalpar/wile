package parser

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestStickyErrorFieldWrittenOnlyInReadSyntax enforces the structural invariant
// that the Parser.err field is assigned only inside ReadSyntax.
//
// p.err is the parser's one genuinely sticky error: ReadSyntax stores a trailing
// io.EOF there so the next call can return it immediately (the cross-call
// lookahead cache). Every other error is transient and must flow to the caller
// through a return value, not this field. The compound readers advance through
// p.advance(), which returns a local error and never touches p.err.
//
// A behavioral test cannot guard this: ReadSyntax's own trailing advance always
// overwrites p.err before the next read, so a re-fused `X, p.err = ...` on a
// transient path is behaviorally inert yet re-introduces the two-role coupling
// this refactor removed. So the guard is structural — it walks the package AST
// and fails, naming the offending function, the moment any function other than
// ReadSyntax assigns p.err.
func TestStickyErrorFieldWrittenOnlyInReadSyntax(t *testing.T) {
	c := qt.New(t)

	const allowedWriter = "ReadSyntax"

	fset := token.NewFileSet()
	for _, file := range []string{"parser.go", "parser_number.go"} {
		f, err := parser.ParseFile(fset, file, nil, 0)
		c.Assert(err, qt.IsNil, qt.Commentf("parsing %s", file))

		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			funcName := fn.Name.Name
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				assign, ok := n.(*ast.AssignStmt)
				if !ok {
					return true
				}
				for _, lhs := range assign.Lhs {
					if !isParserErrSelector(lhs) {
						continue
					}
					c.Assert(funcName, qt.Equals, allowedWriter,
						qt.Commentf("%s writes p.err at %s; only %s may — route transient errors through p.advance()",
							funcName, fset.Position(assign.Pos()), allowedWriter))
				}
				return true
			})
		}
	}
}

// isParserErrSelector reports whether expr is the selector `p.err` — an
// assignment target that writes the Parser's sticky error field.
func isParserErrSelector(expr ast.Expr) bool {
	sel, ok := expr.(*ast.SelectorExpr)
	if !ok {
		return false
	}
	recv, ok := sel.X.(*ast.Ident)
	if !ok {
		return false
	}
	return recv.Name == "p" && sel.Sel.Name == "err"
}
