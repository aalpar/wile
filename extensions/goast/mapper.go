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
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// mapperOpts controls what optional information the mapper emits.
type mapperOpts struct {
	fset      *token.FileSet
	positions bool
	comments  bool
	typeInfo  *types.Info // nil when type-checking was not requested
}

// mapNode dispatches on ast.Node type to the appropriate mapper function.
func mapNode(n ast.Node, opts *mapperOpts) values.Value {
	if n == nil {
		return values.FalseValue
	}
	switch v := n.(type) {
	// Top-level
	case *ast.File:
		return mapFile(v, opts)

	// Declarations
	case *ast.FuncDecl:
		return mapFuncDecl(v, opts)
	case *ast.GenDecl:
		return mapGenDecl(v, opts)

	// Specs
	case *ast.ImportSpec:
		return mapImportSpec(v, opts)
	case *ast.ValueSpec:
		return mapValueSpec(v, opts)
	case *ast.TypeSpec:
		return mapTypeSpec(v, opts)

	// Statements
	case *ast.BlockStmt:
		return mapBlockStmt(v, opts)
	case *ast.ReturnStmt:
		return mapReturnStmt(v, opts)
	case *ast.ExprStmt:
		return mapExprStmt(v, opts)
	case *ast.AssignStmt:
		return mapAssignStmt(v, opts)
	case *ast.IfStmt:
		return mapIfStmt(v, opts)
	case *ast.ForStmt:
		return mapForStmt(v, opts)
	case *ast.RangeStmt:
		return mapRangeStmt(v, opts)
	case *ast.BranchStmt:
		return mapBranchStmt(v, opts)
	case *ast.DeclStmt:
		return mapDeclStmt(v, opts)
	case *ast.IncDecStmt:
		return mapIncDecStmt(v, opts)
	case *ast.GoStmt:
		return mapGoStmt(v, opts)
	case *ast.DeferStmt:
		return mapDeferStmt(v, opts)
	case *ast.SendStmt:
		return mapSendStmt(v, opts)
	case *ast.LabeledStmt:
		return mapLabeledStmt(v, opts)
	case *ast.SwitchStmt:
		return mapSwitchStmt(v, opts)
	case *ast.TypeSwitchStmt:
		return mapTypeSwitchStmt(v, opts)
	case *ast.CaseClause:
		return mapCaseClause(v, opts)
	case *ast.SelectStmt:
		return mapSelectStmt(v, opts)
	case *ast.CommClause:
		return mapCommClause(v, opts)

	// Expressions
	case *ast.Ident:
		return mapIdent(v, opts)
	case *ast.BasicLit:
		return mapBasicLit(v, opts)
	case *ast.BinaryExpr:
		return mapBinaryExpr(v, opts)
	case *ast.UnaryExpr:
		return mapUnaryExpr(v, opts)
	case *ast.CallExpr:
		return mapCallExpr(v, opts)
	case *ast.SelectorExpr:
		return mapSelectorExpr(v, opts)
	case *ast.IndexExpr:
		return mapIndexExpr(v, opts)
	case *ast.StarExpr:
		return mapStarExpr(v, opts)
	case *ast.ParenExpr:
		return mapParenExpr(v, opts)
	case *ast.CompositeLit:
		return mapCompositeLit(v, opts)
	case *ast.KeyValueExpr:
		return mapKeyValueExpr(v, opts)
	case *ast.FuncLit:
		return mapFuncLit(v, opts)
	case *ast.TypeAssertExpr:
		return mapTypeAssertExpr(v, opts)
	case *ast.SliceExpr:
		return mapSliceExpr(v, opts)
	case *ast.Ellipsis:
		return mapEllipsis(v, opts)

	// Types
	case *ast.ArrayType:
		return mapArrayType(v, opts)
	case *ast.MapType:
		return mapMapType(v, opts)
	case *ast.StructType:
		return mapStructType(v, opts)
	case *ast.InterfaceType:
		return mapInterfaceType(v, opts)
	case *ast.FuncType:
		return mapFuncType(v, opts)
	case *ast.ChanType:
		return mapChanType(v, opts)
	case *ast.Field:
		return mapField(v, opts)
	case *ast.FieldList:
		return mapFieldList(v, opts)

	default:
		// Unsupported node types preserve the Go type for diagnostics.
		return Node("unknown",
			Field("go-type", Str(fmt.Sprintf("%T", n))),
		)
	}
}

// mapExpr maps an ast.Expr (which is also an ast.Node).
func mapExpr(e ast.Expr, opts *mapperOpts) values.Value {
	if e == nil {
		return values.FalseValue
	}
	return mapNode(e, opts)
}

// mapStmt maps an ast.Stmt.
func mapStmt(s ast.Stmt, opts *mapperOpts) values.Value {
	if s == nil {
		return values.FalseValue
	}
	return mapNode(s, opts)
}

// --- Top-level ---

func mapFile(f *ast.File, opts *mapperOpts) values.Value {
	decls := make([]values.Value, len(f.Decls))
	for i, d := range f.Decls {
		decls[i] = mapNode(d, opts)
	}
	return Node("file",
		Field("name", Str(f.Name.Name)),
		Field("decls", ValueList(decls)),
	)
}

// --- Declarations ---

func mapFuncDecl(f *ast.FuncDecl, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("name", Str(f.Name.Name)),
		Field("recv", mapFieldListOrFalse(f.Recv, opts)),
		Field("type", mapFuncType(f.Type, opts)),
		Field("body", mapStmt(f.Body, opts)),
	}
	return Node("func-decl", fields...)
}

func mapGenDecl(g *ast.GenDecl, opts *mapperOpts) values.Value {
	specs := make([]values.Value, len(g.Specs))
	for i, s := range g.Specs {
		specs[i] = mapNode(s, opts)
	}
	return Node("gen-decl",
		Field("tok", Sym(g.Tok.String())),
		Field("specs", ValueList(specs)),
	)
}

// --- Specs ---

func mapImportSpec(s *ast.ImportSpec, opts *mapperOpts) values.Value {
	var nameVal values.Value
	if s.Name != nil {
		nameVal = Str(s.Name.Name)
	} else {
		nameVal = values.FalseValue
	}
	return Node("import-spec",
		Field("name", nameVal),
		Field("path", mapBasicLit(s.Path, opts)),
	)
}

func mapValueSpec(s *ast.ValueSpec, opts *mapperOpts) values.Value {
	names := make([]values.Value, len(s.Names))
	for i, n := range s.Names {
		names[i] = Str(n.Name)
	}
	vals := make([]values.Value, len(s.Values))
	for i, v := range s.Values {
		vals[i] = mapExpr(v, opts)
	}
	return Node("value-spec",
		Field("names", ValueList(names)),
		Field("type", mapExpr(s.Type, opts)),
		Field("values", ValueList(vals)),
	)
}

func mapTypeSpec(s *ast.TypeSpec, opts *mapperOpts) values.Value {
	return Node("type-spec",
		Field("name", Str(s.Name.Name)),
		Field("type", mapExpr(s.Type, opts)),
	)
}

// --- Statements ---

func mapBlockStmt(b *ast.BlockStmt, opts *mapperOpts) values.Value {
	if b == nil {
		return values.FalseValue
	}
	stmts := make([]values.Value, len(b.List))
	for i, s := range b.List {
		stmts[i] = mapStmt(s, opts)
	}
	return Node("block",
		Field("list", ValueList(stmts)),
	)
}

func mapReturnStmt(r *ast.ReturnStmt, opts *mapperOpts) values.Value {
	results := make([]values.Value, len(r.Results))
	for i, e := range r.Results {
		results[i] = mapExpr(e, opts)
	}
	return Node("return-stmt",
		Field("results", ValueList(results)),
	)
}

func mapExprStmt(e *ast.ExprStmt, opts *mapperOpts) values.Value {
	return Node("expr-stmt",
		Field("x", mapExpr(e.X, opts)),
	)
}

func mapAssignStmt(a *ast.AssignStmt, opts *mapperOpts) values.Value {
	lhs := make([]values.Value, len(a.Lhs))
	for i, e := range a.Lhs {
		lhs[i] = mapExpr(e, opts)
	}
	rhs := make([]values.Value, len(a.Rhs))
	for i, e := range a.Rhs {
		rhs[i] = mapExpr(e, opts)
	}
	return Node("assign-stmt",
		Field("lhs", ValueList(lhs)),
		Field("tok", Sym(a.Tok.String())),
		Field("rhs", ValueList(rhs)),
	)
}

func mapIfStmt(i *ast.IfStmt, opts *mapperOpts) values.Value {
	return Node("if-stmt",
		Field("init", mapStmt(i.Init, opts)),
		Field("cond", mapExpr(i.Cond, opts)),
		Field("body", mapStmt(i.Body, opts)),
		Field("else", mapStmt(i.Else, opts)),
	)
}

func mapForStmt(f *ast.ForStmt, opts *mapperOpts) values.Value {
	return Node("for-stmt",
		Field("init", mapStmt(f.Init, opts)),
		Field("cond", mapExpr(f.Cond, opts)),
		Field("post", mapStmt(f.Post, opts)),
		Field("body", mapStmt(f.Body, opts)),
	)
}

func mapRangeStmt(r *ast.RangeStmt, opts *mapperOpts) values.Value {
	return Node("range-stmt",
		Field("key", mapExpr(r.Key, opts)),
		Field("value", mapExpr(r.Value, opts)),
		Field("tok", Sym(r.Tok.String())),
		Field("x", mapExpr(r.X, opts)),
		Field("body", mapStmt(r.Body, opts)),
	)
}

func mapBranchStmt(b *ast.BranchStmt, opts *mapperOpts) values.Value { //nolint:unparam // opts unused until Phase 3 (positions/comments)
	var labelVal values.Value
	if b.Label != nil {
		labelVal = Str(b.Label.Name)
	} else {
		labelVal = values.FalseValue
	}
	return Node("branch-stmt",
		Field("tok", Sym(b.Tok.String())),
		Field("label", labelVal),
	)
}

func mapDeclStmt(d *ast.DeclStmt, opts *mapperOpts) values.Value {
	return Node("decl-stmt",
		Field("decl", mapNode(d.Decl, opts)),
	)
}

func mapIncDecStmt(i *ast.IncDecStmt, opts *mapperOpts) values.Value {
	return Node("inc-dec-stmt",
		Field("x", mapExpr(i.X, opts)),
		Field("tok", Sym(i.Tok.String())),
	)
}

func mapGoStmt(g *ast.GoStmt, opts *mapperOpts) values.Value {
	return Node("go-stmt",
		Field("call", mapCallExpr(g.Call, opts)),
	)
}

func mapDeferStmt(d *ast.DeferStmt, opts *mapperOpts) values.Value {
	return Node("defer-stmt",
		Field("call", mapCallExpr(d.Call, opts)),
	)
}

func mapSendStmt(s *ast.SendStmt, opts *mapperOpts) values.Value {
	return Node("send-stmt",
		Field("chan", mapExpr(s.Chan, opts)),
		Field("value", mapExpr(s.Value, opts)),
	)
}

func mapLabeledStmt(l *ast.LabeledStmt, opts *mapperOpts) values.Value {
	return Node("labeled-stmt",
		Field("label", Str(l.Label.Name)),
		Field("stmt", mapStmt(l.Stmt, opts)),
	)
}

func mapSwitchStmt(s *ast.SwitchStmt, opts *mapperOpts) values.Value {
	return Node("switch-stmt",
		Field("init", mapStmt(s.Init, opts)),
		Field("tag", mapExpr(s.Tag, opts)),
		Field("body", mapBlockStmt(s.Body, opts)),
	)
}

func mapTypeSwitchStmt(s *ast.TypeSwitchStmt, opts *mapperOpts) values.Value {
	return Node("type-switch-stmt",
		Field("init", mapStmt(s.Init, opts)),
		Field("assign", mapStmt(s.Assign, opts)),
		Field("body", mapBlockStmt(s.Body, opts)),
	)
}

func mapSelectStmt(s *ast.SelectStmt, opts *mapperOpts) values.Value {
	return Node("select-stmt",
		Field("body", mapBlockStmt(s.Body, opts)),
	)
}

func mapCommClause(c *ast.CommClause, opts *mapperOpts) values.Value {
	var commVal values.Value
	if c.Comm == nil {
		commVal = values.FalseValue
	} else {
		commVal = mapStmt(c.Comm, opts)
	}
	stmts := make([]values.Value, len(c.Body))
	for i, s := range c.Body {
		stmts[i] = mapStmt(s, opts)
	}
	return Node("comm-clause",
		Field("comm", commVal),
		Field("body", ValueList(stmts)),
	)
}

func mapCaseClause(c *ast.CaseClause, opts *mapperOpts) values.Value {
	var listVal values.Value
	if c.List == nil {
		listVal = values.FalseValue
	} else {
		exprs := make([]values.Value, len(c.List))
		for i, e := range c.List {
			exprs[i] = mapExpr(e, opts)
		}
		listVal = ValueList(exprs)
	}
	stmts := make([]values.Value, len(c.Body))
	for i, s := range c.Body {
		stmts[i] = mapStmt(s, opts)
	}
	return Node("case-clause",
		Field("list", listVal),
		Field("body", ValueList(stmts)),
	)
}

// --- Expressions ---

func mapIdent(id *ast.Ident, opts *mapperOpts) values.Value {
	fields := []values.Value{Field("name", Str(id.Name))}
	fields = addTypeAnnotation(id, opts, fields)
	fields = addObjPkgAnnotation(id, opts, fields)
	return Node("ident", fields...)
}

func mapBasicLit(lit *ast.BasicLit, opts *mapperOpts) values.Value {
	if lit == nil {
		return values.FalseValue
	}
	fields := []values.Value{
		Field("kind", Sym(lit.Kind.String())),
		Field("value", Str(lit.Value)),
	}
	fields = addTypeAnnotation(lit, opts, fields)
	return Node("lit", fields...)
}

func mapBinaryExpr(b *ast.BinaryExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("op", Sym(b.Op.String())),
		Field("x", mapExpr(b.X, opts)),
		Field("y", mapExpr(b.Y, opts)),
	}
	fields = addTypeAnnotation(b, opts, fields)
	return Node("binary-expr", fields...)
}

func mapUnaryExpr(u *ast.UnaryExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("op", Sym(u.Op.String())),
		Field("x", mapExpr(u.X, opts)),
	}
	fields = addTypeAnnotation(u, opts, fields)
	return Node("unary-expr", fields...)
}

func mapCallExpr(c *ast.CallExpr, opts *mapperOpts) values.Value {
	args := make([]values.Value, len(c.Args))
	for i, a := range c.Args {
		args[i] = mapExpr(a, opts)
	}
	fields := []values.Value{
		Field("fun", mapExpr(c.Fun, opts)),
		Field("args", ValueList(args)),
	}
	fields = addTypeAnnotation(c, opts, fields)
	return Node("call-expr", fields...)
}

func mapSelectorExpr(s *ast.SelectorExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("x", mapExpr(s.X, opts)),
		Field("sel", Str(s.Sel.Name)),
	}
	fields = addTypeAnnotation(s, opts, fields)
	return Node("selector-expr", fields...)
}

func mapIndexExpr(i *ast.IndexExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("x", mapExpr(i.X, opts)),
		Field("index", mapExpr(i.Index, opts)),
	}
	fields = addTypeAnnotation(i, opts, fields)
	return Node("index-expr", fields...)
}

func mapStarExpr(s *ast.StarExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{Field("x", mapExpr(s.X, opts))}
	fields = addTypeAnnotation(s, opts, fields)
	return Node("star-expr", fields...)
}

func mapParenExpr(p *ast.ParenExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{Field("x", mapExpr(p.X, opts))}
	fields = addTypeAnnotation(p, opts, fields)
	return Node("paren-expr", fields...)
}

func mapCompositeLit(c *ast.CompositeLit, opts *mapperOpts) values.Value {
	elts := make([]values.Value, len(c.Elts))
	for i, e := range c.Elts {
		elts[i] = mapExpr(e, opts)
	}
	fields := []values.Value{
		Field("type", mapExpr(c.Type, opts)),
		Field("elts", ValueList(elts)),
	}
	fields = addTypeAnnotation(c, opts, fields)
	return Node("composite-lit", fields...)
}

func mapKeyValueExpr(kv *ast.KeyValueExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("key", mapExpr(kv.Key, opts)),
		Field("value", mapExpr(kv.Value, opts)),
	}
	fields = addTypeAnnotation(kv, opts, fields)
	return Node("kv-expr", fields...)
}

func mapFuncLit(f *ast.FuncLit, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("type", mapFuncType(f.Type, opts)),
		Field("body", mapStmt(f.Body, opts)),
	}
	fields = addTypeAnnotation(f, opts, fields)
	return Node("func-lit", fields...)
}

func mapTypeAssertExpr(t *ast.TypeAssertExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("x", mapExpr(t.X, opts)),
		Field("type", mapExpr(t.Type, opts)),
	}
	fields = addTypeAnnotation(t, opts, fields)
	return Node("type-assert-expr", fields...)
}

func mapSliceExpr(s *ast.SliceExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		Field("x", mapExpr(s.X, opts)),
		Field("low", mapExpr(s.Low, opts)),
		Field("high", mapExpr(s.High, opts)),
		Field("max", mapExpr(s.Max, opts)),
		Field("slice3", values.BoolToBoolean(s.Slice3)),
	}
	fields = addTypeAnnotation(s, opts, fields)
	return Node("slice-expr", fields...)
}

func mapEllipsis(e *ast.Ellipsis, opts *mapperOpts) values.Value {
	fields := []values.Value{Field("elt", mapExpr(e.Elt, opts))}
	fields = addTypeAnnotation(e, opts, fields)
	return Node("ellipsis", fields...)
}

// --- Type expressions ---

func mapChanType(c *ast.ChanType, opts *mapperOpts) values.Value {
	return Node("chan-type",
		Field("dir", chanDirSymbol(c.Dir)),
		Field("value", mapExpr(c.Value, opts)),
	)
}

// chanDirSymbol converts ast.ChanDir to a Scheme symbol.
func chanDirSymbol(dir ast.ChanDir) values.Value {
	switch dir {
	case ast.SEND:
		return Sym("send")
	case ast.RECV:
		return Sym("recv")
	case ast.SEND | ast.RECV:
		return Sym("both")
	default:
		panic(werr.WrapForeignErrorf(errMalformedGoAST,
			"chanDirSymbol: unknown channel direction %d", dir))
	}
}

func mapArrayType(a *ast.ArrayType, opts *mapperOpts) values.Value {
	return Node("array-type",
		Field("len", mapExpr(a.Len, opts)),
		Field("elt", mapExpr(a.Elt, opts)),
	)
}

func mapMapType(m *ast.MapType, opts *mapperOpts) values.Value {
	return Node("map-type",
		Field("key", mapExpr(m.Key, opts)),
		Field("value", mapExpr(m.Value, opts)),
	)
}

func mapStructType(s *ast.StructType, opts *mapperOpts) values.Value {
	return Node("struct-type",
		Field("fields", mapFieldList(s.Fields, opts)),
	)
}

func mapInterfaceType(i *ast.InterfaceType, opts *mapperOpts) values.Value {
	return Node("interface-type",
		Field("methods", mapFieldList(i.Methods, opts)),
	)
}

func mapFuncType(f *ast.FuncType, opts *mapperOpts) values.Value {
	if f == nil {
		return values.FalseValue
	}
	return Node("func-type",
		Field("params", mapFieldList(f.Params, opts)),
		Field("results", mapFieldListOrFalse(f.Results, opts)),
	)
}

func mapField(f *ast.Field, opts *mapperOpts) values.Value {
	names := make([]values.Value, len(f.Names))
	for i, n := range f.Names {
		names[i] = Str(n.Name)
	}
	fs := []values.Value{
		Field("names", ValueList(names)),
		Field("type", mapExpr(f.Type, opts)),
	}
	if f.Tag != nil {
		fs = append(fs, Field("tag", mapBasicLit(f.Tag, opts)))
	}
	return Node("field", fs...)
}

func mapFieldList(fl *ast.FieldList, opts *mapperOpts) values.Value {
	if fl == nil {
		return values.FalseValue
	}
	fields := make([]values.Value, len(fl.List))
	for i, f := range fl.List {
		fields[i] = mapField(f, opts)
	}
	return ValueList(fields)
}

func mapFieldListOrFalse(fl *ast.FieldList, opts *mapperOpts) values.Value {
	if fl == nil {
		return values.FalseValue
	}
	return mapFieldList(fl, opts)
}

// --- Type annotation helpers ---

// addTypeAnnotation appends an (inferred-type . "TYPE_STRING") field if type
// info is available for e. The key is distinct from the structural "type"
// field used by composite-lit and func-lit to avoid alist ambiguity.
func addTypeAnnotation(e ast.Expr, opts *mapperOpts, fields []values.Value) []values.Value {
	if opts.typeInfo == nil {
		return fields
	}
	tv, ok := opts.typeInfo.Types[e]
	if !ok {
		return fields
	}
	return append(fields, Field("inferred-type", Str(types.TypeString(tv.Type, nil))))
}

// addObjPkgAnnotation appends an (obj-pkg . "PKG_PATH") field to an ident
// when it resolves to an object in a named package. This distinguishes
// e.g. fmt.Errorf (obj-pkg "fmt") from a local variable named fmt.
func addObjPkgAnnotation(id *ast.Ident, opts *mapperOpts, fields []values.Value) []values.Value {
	if opts.typeInfo == nil {
		return fields
	}
	obj, ok := opts.typeInfo.Uses[id]
	if !ok {
		return fields
	}
	pkg := obj.Pkg()
	if pkg == nil {
		return fields
	}
	return append(fields, Field("obj-pkg", Str(pkg.Path())))
}
