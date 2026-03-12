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

package goast

import (
	"go/ast"
	"go/token"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// synthLineSize is the number of bytes per synthetic line. go/printer uses
// line numbers derived from token.Pos offsets to determine comment placement.
// The exact value doesn't matter — only the relative ordering.
const synthLineSize = 1000

// posAllocator assigns monotonically increasing synthetic positions with
// line-based allocation. go/printer needs only relative ordering, not exact
// source positions.
type posAllocator struct {
	base    int // file's base offset in the fset
	curLine int // current line, 1-based (0 = no lines allocated yet)
}

func newPosAllocator(fset *token.FileSet) *posAllocator {
	const maxLines = 50000
	f := fset.AddFile("", -1, maxLines*synthLineSize)
	for i := 1; i < maxLines; i++ {
		f.AddLine(i * synthLineSize)
	}
	return &posAllocator{
		base: f.Base(),
	}
}

// nextLine advances to the next line and returns its starting token.Pos.
func (a *posAllocator) nextLine() token.Pos {
	a.curLine++
	return token.Pos(a.base + (a.curLine-1)*synthLineSize)
}

// attachComments is a second-pass operation that walks the AST and the
// original s-expression in parallel, attaching comment groups with synthetic
// positions to enable correct go/printer output.
//
// fileSexprFields is the cdr of the file s-expression (the alist of fields).
// fset receives a synthetic file with pre-registered lines.
func attachComments(file *ast.File, fileSexprFields values.Value, fset *token.FileSet) error {
	alloc := newPosAllocator(fset)
	var cgs []*ast.CommentGroup

	// Assign a position to the package clause so comments before it
	// have a lower offset.
	pkgPos := alloc.nextLine()
	file.Package = pkgPos
	file.Name.NamePos = pkgPos

	declsVal, hasDeclsField := GetField(fileSexprFields, "decls")
	if !hasDeclsField {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"attachComments: file s-expression missing required field 'decls'")
	}
	declIdx := 0

	err := forEachSexpr(declsVal, func(elem values.Value) error {
		tag := sexpTag(elem)
		if tag == "comment-group" {
			fields := sexpFields(elem)
			textsVal, hasTextField := GetField(fields, "text")
			if !hasTextField {
				return werr.WrapForeignErrorf(errMalformedGoAST,
					"attachComments: comment-group missing required field 'text'")
			}
			if IsFalse(textsVal) {
				return nil
			}
			g, gErr := stringsToCommentGroup(textsVal, func() token.Pos {
				return alloc.nextLine()
			})
			if gErr != nil {
				return gErr
			}
			if g != nil {
				cgs = append(cgs, g)
			}
			// Consume an extra line so the next declaration's position
			// is on a different line than this comment group. Without
			// the gap, go/printer treats the comment as a doc comment
			// for the following declaration.
			alloc.nextLine()
			return nil
		}
		if tag == "" {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"attachComments: expected tagged s-expression in decls list")
		}
		// Declaration entry — process with existing logic.
		if declIdx >= len(file.Decls) {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"attachComments: s-expression has more declaration entries than AST (%d decls exhausted)", len(file.Decls))
		}
		dErr := attachDeclComments(file.Decls[declIdx], elem, alloc, &cgs)
		declIdx++
		return dErr
	})
	if err != nil {
		return err
	}

	file.Comments = cgs
	return nil
}

// attachDeclComments attaches doc comments to a single declaration.
func attachDeclComments(decl ast.Decl, sexpr values.Value, alloc *posAllocator, cgs *[]*ast.CommentGroup) error {
	fields := sexpFields(sexpr)

	switch d := decl.(type) {
	case *ast.FuncDecl:
		doc, err := buildDocComment(fields, alloc)
		if err != nil {
			return err
		}
		declPos := alloc.nextLine()
		d.Type.Func = declPos
		if d.Recv != nil {
			d.Recv.Opening = declPos
		}
		if doc != nil {
			d.Doc = doc
			*cgs = append(*cgs, doc)
		}
		// Assign body block positions so go/printer renders multi-line.
		// Without these, partial positions cause the printer to collapse
		// the body onto one line.
		if d.Body != nil {
			d.Body.Lbrace = declPos
			for _, s := range d.Body.List {
				assignStmtLeadingPos(s, alloc.nextLine())
			}
			d.Body.Rbrace = alloc.nextLine()
		}

	case *ast.GenDecl:
		doc, err := buildDocComment(fields, alloc)
		if err != nil {
			return err
		}
		declPos := alloc.nextLine()
		d.TokPos = declPos
		if doc != nil {
			d.Doc = doc
			*cgs = append(*cgs, doc)
		}
		specsVal, _ := GetField(fields, "specs")
		specErr := walkParallel(d.Specs, specsVal, func(spec ast.Spec, specSexpr values.Value) error {
			return attachSpecComments(spec, specSexpr, alloc, cgs)
		})
		if specErr != nil {
			return specErr
		}
	}
	return nil
}

// attachSpecComments attaches doc/comment to a spec node.
func attachSpecComments(spec ast.Spec, sexpr values.Value, alloc *posAllocator, cgs *[]*ast.CommentGroup) error {
	fields := sexpFields(sexpr)

	switch s := spec.(type) {
	case *ast.TypeSpec:
		doc, err := buildDocComment(fields, alloc)
		if err != nil {
			return err
		}
		declPos := alloc.nextLine()
		s.Name.NamePos = declPos
		if doc != nil {
			s.Doc = doc
			*cgs = append(*cgs, doc)
		}
		trailing, err := buildTrailingComment(fields, declPos)
		if err != nil {
			return err
		}
		if trailing != nil {
			s.Comment = trailing
			*cgs = append(*cgs, trailing)
		}
		typeVal, _ := GetField(fields, "type")
		return attachTypeFieldComments(s.Type, typeVal, alloc, cgs)

	case *ast.ValueSpec:
		doc, err := buildDocComment(fields, alloc)
		if err != nil {
			return err
		}
		declPos := alloc.nextLine()
		if len(s.Names) > 0 {
			s.Names[0].NamePos = declPos
		}
		if doc != nil {
			s.Doc = doc
			*cgs = append(*cgs, doc)
		}
		trailing, err := buildTrailingComment(fields, declPos)
		if err != nil {
			return err
		}
		if trailing != nil {
			s.Comment = trailing
			*cgs = append(*cgs, trailing)
		}

	case *ast.ImportSpec:
		doc, err := buildDocComment(fields, alloc)
		if err != nil {
			return err
		}
		declPos := alloc.nextLine()
		s.Path.ValuePos = declPos
		if doc != nil {
			s.Doc = doc
			*cgs = append(*cgs, doc)
		}
		trailing, err := buildTrailingComment(fields, declPos)
		if err != nil {
			return err
		}
		if trailing != nil {
			s.Comment = trailing
			*cgs = append(*cgs, trailing)
		}
	}
	return nil
}

// attachTypeFieldComments walks into struct/interface types to attach
// doc/comment to individual fields.
func attachTypeFieldComments(typ ast.Expr, sexpr values.Value, alloc *posAllocator, cgs *[]*ast.CommentGroup) error {
	if typ == nil || IsFalse(sexpr) {
		return nil
	}
	fields := sexpFields(sexpr)
	tag := sexpTag(sexpr)

	switch tag {
	case "struct-type":
		st, ok := typ.(*ast.StructType)
		if !ok || st.Fields == nil {
			return nil
		}
		fieldsVal, _ := GetField(fields, "fields")
		return walkParallel(st.Fields.List, fieldsVal, func(field *ast.Field, fieldSexpr values.Value) error {
			return attachFieldComments(field, fieldSexpr, alloc, cgs)
		})

	case "interface-type":
		it, ok := typ.(*ast.InterfaceType)
		if !ok || it.Methods == nil {
			return nil
		}
		methodsVal, _ := GetField(fields, "methods")
		return walkParallel(it.Methods.List, methodsVal, func(field *ast.Field, fieldSexpr values.Value) error {
			return attachFieldComments(field, fieldSexpr, alloc, cgs)
		})
	}
	return nil
}

// attachFieldComments attaches doc/comment to a single field.
func attachFieldComments(field *ast.Field, sexpr values.Value, alloc *posAllocator, cgs *[]*ast.CommentGroup) error {
	fields := sexpFields(sexpr)
	_, hasDoc := GetField(fields, "doc")
	if !hasDoc {
		return nil
	}

	doc, err := buildDocComment(fields, alloc)
	if err != nil {
		return err
	}
	declPos := alloc.nextLine()
	if len(field.Names) > 0 {
		field.Names[0].NamePos = declPos
	} else {
		assignExprLeadingPos(field.Type, declPos)
	}
	if doc != nil {
		field.Doc = doc
		*cgs = append(*cgs, doc)
	}
	trailing, err := buildTrailingComment(fields, declPos)
	if err != nil {
		return err
	}
	if trailing != nil {
		field.Comment = trailing
		*cgs = append(*cgs, trailing)
	}
	return nil
}

// --- position assignment for function bodies ---

// assignStmtLeadingPos sets the primary position field of a statement so
// go/printer knows which line to render it on. Only the leading token needs
// a position — the printer outputs the rest of the statement on the same line.
func assignStmtLeadingPos(s ast.Stmt, pos token.Pos) {
	switch v := s.(type) {
	case *ast.ReturnStmt:
		v.Return = pos
	case *ast.ExprStmt:
		assignExprLeadingPos(v.X, pos)
	case *ast.AssignStmt:
		v.TokPos = pos
		if len(v.Lhs) > 0 {
			assignExprLeadingPos(v.Lhs[0], pos)
		}
	case *ast.IfStmt:
		v.If = pos
	case *ast.ForStmt:
		v.For = pos
	case *ast.RangeStmt:
		v.For = pos
	case *ast.BranchStmt:
		v.TokPos = pos
	case *ast.IncDecStmt:
		v.TokPos = pos
		assignExprLeadingPos(v.X, pos)
	case *ast.GoStmt:
		v.Go = pos
	case *ast.DeferStmt:
		v.Defer = pos
	case *ast.SendStmt:
		v.Arrow = pos
		assignExprLeadingPos(v.Chan, pos)
	case *ast.SwitchStmt:
		v.Switch = pos
	case *ast.TypeSwitchStmt:
		v.Switch = pos
	case *ast.SelectStmt:
		v.Select = pos
	case *ast.CaseClause:
		v.Case = pos
	case *ast.CommClause:
		v.Case = pos
	case *ast.LabeledStmt:
		v.Colon = pos
		if v.Label != nil {
			v.Label.NamePos = pos
		}
	case *ast.DeclStmt:
		switch dd := v.Decl.(type) {
		case *ast.GenDecl:
			dd.TokPos = pos
		case *ast.FuncDecl:
			dd.Type.Func = pos
		}
	case *ast.BlockStmt:
		v.Lbrace = pos
	}
}

// assignExprLeadingPos sets the position of the first token in an expression.
func assignExprLeadingPos(e ast.Expr, pos token.Pos) {
	if e == nil {
		return
	}
	switch v := e.(type) {
	case *ast.Ident:
		if !v.NamePos.IsValid() {
			v.NamePos = pos
		}
	case *ast.BasicLit:
		if !v.ValuePos.IsValid() {
			v.ValuePos = pos
		}
	case *ast.CallExpr:
		assignExprLeadingPos(v.Fun, pos)
	case *ast.SelectorExpr:
		assignExprLeadingPos(v.X, pos)
	case *ast.UnaryExpr:
		if !v.OpPos.IsValid() {
			v.OpPos = pos
		}
	case *ast.BinaryExpr:
		assignExprLeadingPos(v.X, pos)
	case *ast.StarExpr:
		if !v.Star.IsValid() {
			v.Star = pos
		}
	case *ast.ParenExpr:
		if !v.Lparen.IsValid() {
			v.Lparen = pos
		}
	}
}

// --- helpers ---

// buildDocComment extracts the "doc" field and converts it to a CommentGroup
// with positions allocated via alloc.nextLine() for each comment line.
func buildDocComment(fields values.Value, alloc *posAllocator) (*ast.CommentGroup, error) {
	docVal, hasDoc := GetField(fields, "doc")
	if !hasDoc || IsFalse(docVal) {
		return nil, nil
	}
	return stringsToCommentGroup(docVal, func() token.Pos {
		return alloc.nextLine()
	})
}

// buildTrailingComment extracts the "comment" field and converts it to a
// CommentGroup with all comments at the given position (same line).
func buildTrailingComment(fields values.Value, pos token.Pos) (*ast.CommentGroup, error) {
	commentVal, hasComment := GetField(fields, "comment")
	if !hasComment || IsFalse(commentVal) {
		return nil, nil
	}
	return stringsToCommentGroup(commentVal, func() token.Pos {
		return pos
	})
}

// stringsToCommentGroup converts a Scheme list of strings to an ast.CommentGroup.
// posFunc is called once per comment to assign its Slash position.
func stringsToCommentGroup(v values.Value, posFunc func() token.Pos) (*ast.CommentGroup, error) {
	var comments []*ast.Comment
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: comment field must be a list, got %T", v)
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list for comment")
		}
		text, ok := pair.Car().(*values.String)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: comment text must be string, got %T", pair.Car())
		}
		pos := posFunc()
		comments = append(comments, &ast.Comment{Slash: pos, Text: text.Value})
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list in comment field")
		}
		tuple = cdr
	}
	if len(comments) == 0 {
		return nil, nil
	}
	return &ast.CommentGroup{List: comments}, nil
}

// forEachSexpr iterates a Scheme proper list, calling fn for each element.
func forEachSexpr(v values.Value, fn func(values.Value) error) error {
	if IsFalse(v) {
		return nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list, got %T", v)
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list, got %T", tuple)
		}
		err := fn(pair.Car())
		if err != nil {
			return err
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list")
		}
		tuple = cdr
	}
	return nil
}

// walkParallel iterates a Go slice and a Scheme list in lockstep.
func walkParallel[T any](goSlice []T, sexprList values.Value, fn func(T, values.Value) error) error {
	if IsFalse(sexprList) {
		return nil
	}
	tuple, ok := sexprList.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected proper list for parallel walk, got %T", sexprList)
	}
	i := 0
	for i < len(goSlice) && !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected pair in parallel walk, got %T", tuple)
		}
		err := fn(goSlice[i], pair.Car())
		if err != nil {
			return err
		}
		i++
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list in parallel walk")
		}
		tuple = cdr
	}
	return nil
}

// sexpFields returns the fields (cdr) of a tagged alist s-expression.
func sexpFields(v values.Value) values.Value {
	pair, ok := v.(*values.Pair)
	if !ok {
		return values.EmptyList
	}
	return pair.Cdr()
}

// sexpTag returns the tag name of a tagged alist s-expression, or "".
func sexpTag(v values.Value) string {
	pair, ok := v.(*values.Pair)
	if !ok {
		return ""
	}
	sym, ok := pair.Car().(*values.Symbol)
	if !ok {
		return ""
	}
	return sym.Key
}
