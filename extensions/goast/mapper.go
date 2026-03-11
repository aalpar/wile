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
	case *ast.Field:
		return mapField(v, opts)
	case *ast.FieldList:
		return mapFieldList(v, opts)

	default:
		// Unsupported node types preserve the Go type for diagnostics.
		return node("unknown",
			field("go-type", str(fmt.Sprintf("%T", n))),
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
	return node("file",
		field("name", str(f.Name.Name)),
		field("decls", valueList(decls)),
	)
}

// --- Declarations ---

func mapFuncDecl(f *ast.FuncDecl, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("name", str(f.Name.Name)),
		field("recv", mapFieldListOrFalse(f.Recv, opts)),
		field("type", mapFuncType(f.Type, opts)),
		field("body", mapStmt(f.Body, opts)),
	}
	return node("func-decl", fields...)
}

func mapGenDecl(g *ast.GenDecl, opts *mapperOpts) values.Value {
	specs := make([]values.Value, len(g.Specs))
	for i, s := range g.Specs {
		specs[i] = mapNode(s, opts)
	}
	return node("gen-decl",
		field("tok", sym(g.Tok.String())),
		field("specs", valueList(specs)),
	)
}

// --- Specs ---

func mapImportSpec(s *ast.ImportSpec, opts *mapperOpts) values.Value {
	var nameVal values.Value
	if s.Name != nil {
		nameVal = str(s.Name.Name)
	} else {
		nameVal = values.FalseValue
	}
	return node("import-spec",
		field("name", nameVal),
		field("path", mapBasicLit(s.Path, opts)),
	)
}

func mapValueSpec(s *ast.ValueSpec, opts *mapperOpts) values.Value {
	names := make([]values.Value, len(s.Names))
	for i, n := range s.Names {
		names[i] = str(n.Name)
	}
	vals := make([]values.Value, len(s.Values))
	for i, v := range s.Values {
		vals[i] = mapExpr(v, opts)
	}
	return node("value-spec",
		field("names", valueList(names)),
		field("type", mapExpr(s.Type, opts)),
		field("values", valueList(vals)),
	)
}

func mapTypeSpec(s *ast.TypeSpec, opts *mapperOpts) values.Value {
	return node("type-spec",
		field("name", str(s.Name.Name)),
		field("type", mapExpr(s.Type, opts)),
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
	return node("block",
		field("list", valueList(stmts)),
	)
}

func mapReturnStmt(r *ast.ReturnStmt, opts *mapperOpts) values.Value {
	results := make([]values.Value, len(r.Results))
	for i, e := range r.Results {
		results[i] = mapExpr(e, opts)
	}
	return node("return-stmt",
		field("results", valueList(results)),
	)
}

func mapExprStmt(e *ast.ExprStmt, opts *mapperOpts) values.Value {
	return node("expr-stmt",
		field("x", mapExpr(e.X, opts)),
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
	return node("assign-stmt",
		field("lhs", valueList(lhs)),
		field("tok", sym(a.Tok.String())),
		field("rhs", valueList(rhs)),
	)
}

func mapIfStmt(i *ast.IfStmt, opts *mapperOpts) values.Value {
	return node("if-stmt",
		field("init", mapStmt(i.Init, opts)),
		field("cond", mapExpr(i.Cond, opts)),
		field("body", mapStmt(i.Body, opts)),
		field("else", mapStmt(i.Else, opts)),
	)
}

func mapForStmt(f *ast.ForStmt, opts *mapperOpts) values.Value {
	return node("for-stmt",
		field("init", mapStmt(f.Init, opts)),
		field("cond", mapExpr(f.Cond, opts)),
		field("post", mapStmt(f.Post, opts)),
		field("body", mapStmt(f.Body, opts)),
	)
}

func mapRangeStmt(r *ast.RangeStmt, opts *mapperOpts) values.Value {
	return node("range-stmt",
		field("key", mapExpr(r.Key, opts)),
		field("value", mapExpr(r.Value, opts)),
		field("tok", sym(r.Tok.String())),
		field("x", mapExpr(r.X, opts)),
		field("body", mapStmt(r.Body, opts)),
	)
}

func mapBranchStmt(b *ast.BranchStmt, opts *mapperOpts) values.Value { //nolint:unparam // opts unused until Phase 3 (positions/comments)
	var labelVal values.Value
	if b.Label != nil {
		labelVal = str(b.Label.Name)
	} else {
		labelVal = values.FalseValue
	}
	return node("branch-stmt",
		field("tok", sym(b.Tok.String())),
		field("label", labelVal),
	)
}

func mapDeclStmt(d *ast.DeclStmt, opts *mapperOpts) values.Value {
	return node("decl-stmt",
		field("decl", mapNode(d.Decl, opts)),
	)
}

func mapIncDecStmt(i *ast.IncDecStmt, opts *mapperOpts) values.Value {
	return node("inc-dec-stmt",
		field("x", mapExpr(i.X, opts)),
		field("tok", sym(i.Tok.String())),
	)
}

// --- Expressions ---

func mapIdent(id *ast.Ident, opts *mapperOpts) values.Value {
	fields := []values.Value{field("name", str(id.Name))}
	fields = addTypeAnnotation(id, opts, fields)
	fields = addObjPkgAnnotation(id, opts, fields)
	return node("ident", fields...)
}

func mapBasicLit(lit *ast.BasicLit, opts *mapperOpts) values.Value {
	if lit == nil {
		return values.FalseValue
	}
	fields := []values.Value{
		field("kind", sym(lit.Kind.String())),
		field("value", str(lit.Value)),
	}
	fields = addTypeAnnotation(lit, opts, fields)
	return node("lit", fields...)
}

func mapBinaryExpr(b *ast.BinaryExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("op", sym(b.Op.String())),
		field("x", mapExpr(b.X, opts)),
		field("y", mapExpr(b.Y, opts)),
	}
	fields = addTypeAnnotation(b, opts, fields)
	return node("binary-expr", fields...)
}

func mapUnaryExpr(u *ast.UnaryExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("op", sym(u.Op.String())),
		field("x", mapExpr(u.X, opts)),
	}
	fields = addTypeAnnotation(u, opts, fields)
	return node("unary-expr", fields...)
}

func mapCallExpr(c *ast.CallExpr, opts *mapperOpts) values.Value {
	args := make([]values.Value, len(c.Args))
	for i, a := range c.Args {
		args[i] = mapExpr(a, opts)
	}
	fields := []values.Value{
		field("fun", mapExpr(c.Fun, opts)),
		field("args", valueList(args)),
	}
	fields = addTypeAnnotation(c, opts, fields)
	return node("call-expr", fields...)
}

func mapSelectorExpr(s *ast.SelectorExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("x", mapExpr(s.X, opts)),
		field("sel", str(s.Sel.Name)),
	}
	fields = addTypeAnnotation(s, opts, fields)
	return node("selector-expr", fields...)
}

func mapIndexExpr(i *ast.IndexExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("x", mapExpr(i.X, opts)),
		field("index", mapExpr(i.Index, opts)),
	}
	fields = addTypeAnnotation(i, opts, fields)
	return node("index-expr", fields...)
}

func mapStarExpr(s *ast.StarExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{field("x", mapExpr(s.X, opts))}
	fields = addTypeAnnotation(s, opts, fields)
	return node("star-expr", fields...)
}

func mapParenExpr(p *ast.ParenExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{field("x", mapExpr(p.X, opts))}
	fields = addTypeAnnotation(p, opts, fields)
	return node("paren-expr", fields...)
}

func mapCompositeLit(c *ast.CompositeLit, opts *mapperOpts) values.Value {
	elts := make([]values.Value, len(c.Elts))
	for i, e := range c.Elts {
		elts[i] = mapExpr(e, opts)
	}
	fields := []values.Value{
		field("type", mapExpr(c.Type, opts)),
		field("elts", valueList(elts)),
	}
	fields = addTypeAnnotation(c, opts, fields)
	return node("composite-lit", fields...)
}

func mapKeyValueExpr(kv *ast.KeyValueExpr, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("key", mapExpr(kv.Key, opts)),
		field("value", mapExpr(kv.Value, opts)),
	}
	fields = addTypeAnnotation(kv, opts, fields)
	return node("kv-expr", fields...)
}

func mapFuncLit(f *ast.FuncLit, opts *mapperOpts) values.Value {
	fields := []values.Value{
		field("type", mapFuncType(f.Type, opts)),
		field("body", mapStmt(f.Body, opts)),
	}
	fields = addTypeAnnotation(f, opts, fields)
	return node("func-lit", fields...)
}

// --- Type expressions ---

func mapArrayType(a *ast.ArrayType, opts *mapperOpts) values.Value {
	return node("array-type",
		field("len", mapExpr(a.Len, opts)),
		field("elt", mapExpr(a.Elt, opts)),
	)
}

func mapMapType(m *ast.MapType, opts *mapperOpts) values.Value {
	return node("map-type",
		field("key", mapExpr(m.Key, opts)),
		field("value", mapExpr(m.Value, opts)),
	)
}

func mapStructType(s *ast.StructType, opts *mapperOpts) values.Value {
	return node("struct-type",
		field("fields", mapFieldList(s.Fields, opts)),
	)
}

func mapInterfaceType(i *ast.InterfaceType, opts *mapperOpts) values.Value {
	return node("interface-type",
		field("methods", mapFieldList(i.Methods, opts)),
	)
}

func mapFuncType(f *ast.FuncType, opts *mapperOpts) values.Value {
	if f == nil {
		return values.FalseValue
	}
	return node("func-type",
		field("params", mapFieldList(f.Params, opts)),
		field("results", mapFieldListOrFalse(f.Results, opts)),
	)
}

func mapField(f *ast.Field, opts *mapperOpts) values.Value {
	names := make([]values.Value, len(f.Names))
	for i, n := range f.Names {
		names[i] = str(n.Name)
	}
	fs := []values.Value{
		field("names", valueList(names)),
		field("type", mapExpr(f.Type, opts)),
	}
	if f.Tag != nil {
		fs = append(fs, field("tag", mapBasicLit(f.Tag, opts)))
	}
	return node("field", fs...)
}

func mapFieldList(fl *ast.FieldList, opts *mapperOpts) values.Value {
	if fl == nil {
		return values.FalseValue
	}
	fields := make([]values.Value, len(fl.List))
	for i, f := range fl.List {
		fields[i] = mapField(f, opts)
	}
	return valueList(fields)
}

func mapFieldListOrFalse(fl *ast.FieldList, opts *mapperOpts) values.Value {
	if fl == nil {
		return values.FalseValue
	}
	return mapFieldList(fl, opts)
}

// --- Type annotation helpers ---

// addTypeAnnotation appends a (type . "TYPE_STRING") field if type info is
// available for e. Called on all expression-level mapper functions.
func addTypeAnnotation(e ast.Expr, opts *mapperOpts, fields []values.Value) []values.Value {
	if opts.typeInfo == nil {
		return fields
	}
	tv, ok := opts.typeInfo.Types[e]
	if !ok {
		return fields
	}
	return append(fields, field("type", str(types.TypeString(tv.Type, nil))))
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
	return append(fields, field("obj-pkg", str(pkg.Path())))
}
