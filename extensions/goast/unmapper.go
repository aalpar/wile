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

// unmapNode converts a Scheme s-expression (tagged alist) back to an ast.Node.
func unmapNode(v values.Value) (ast.Node, error) {
	pair, ok := v.(*values.Pair)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected tagged alist, got %T", v)
	}
	tagSym, ok := pair.Car().(*values.Symbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected symbol tag, got %T", pair.Car())
	}
	fields := pair.Cdr()

	switch tagSym.Key {
	// Top-level
	case "file":
		return unmapFile(fields)

	// Declarations
	case "func-decl":
		return unmapFuncDecl(fields)
	case "gen-decl":
		return unmapGenDecl(fields)

	// Specs
	case "import-spec":
		return unmapImportSpec(fields)
	case "value-spec":
		return unmapValueSpec(fields)
	case "type-spec":
		return unmapTypeSpec(fields)

	// Statements
	case "block":
		return unmapBlockStmt(fields)
	case "return-stmt":
		return unmapReturnStmt(fields)
	case "expr-stmt":
		return unmapExprStmt(fields)
	case "assign-stmt":
		return unmapAssignStmt(fields)
	case "if-stmt":
		return unmapIfStmt(fields)
	case "for-stmt":
		return unmapForStmt(fields)
	case "range-stmt":
		return unmapRangeStmt(fields)
	case "branch-stmt":
		return unmapBranchStmt(fields)
	case "decl-stmt":
		return unmapDeclStmt(fields)
	case "inc-dec-stmt":
		return unmapIncDecStmt(fields)

	// Expressions
	case "ident":
		return unmapIdent(fields)
	case "lit":
		return unmapBasicLit(fields)
	case "binary-expr":
		return unmapBinaryExpr(fields)
	case "unary-expr":
		return unmapUnaryExpr(fields)
	case "call-expr":
		return unmapCallExpr(fields)
	case "selector-expr":
		return unmapSelectorExpr(fields)
	case "index-expr":
		return unmapIndexExpr(fields)
	case "star-expr":
		return unmapStarExpr(fields)
	case "paren-expr":
		return unmapParenExpr(fields)
	case "composite-lit":
		return unmapCompositeLit(fields)
	case "kv-expr":
		return unmapKeyValueExpr(fields)
	case "func-lit":
		return unmapFuncLit(fields)

	// Types
	case "array-type":
		return unmapArrayType(fields)
	case "map-type":
		return unmapMapType(fields)
	case "struct-type":
		return unmapStructType(fields)
	case "interface-type":
		return unmapInterfaceType(fields)
	case "func-type":
		return unmapFuncType(fields)
	case "field":
		return unmapField(fields)

	case "unknown":
		goType, _ := getField(fields, "go-type")
		s, ok := goType.(*values.String)
		if ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: unsupported Go node type %s (not yet implemented)", s.Value)
		}
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: unsupported Go node type (not yet implemented)")

	default:
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: unknown node tag '%s'", tagSym.Key)
	}
}

// unmapExpr converts a Scheme value to an ast.Expr. Returns nil for #f.
func unmapExpr(v values.Value) (ast.Expr, error) {
	if isFalse(v) {
		return nil, nil
	}
	n, err := unmapNode(v)
	if err != nil {
		return nil, err
	}
	expr, ok := n.(ast.Expr)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected expression, got %T", n)
	}
	return expr, nil
}

// unmapStmt converts a Scheme value to an ast.Stmt. Returns nil for #f.
func unmapStmt(v values.Value) (ast.Stmt, error) {
	if isFalse(v) {
		return nil, nil
	}
	n, err := unmapNode(v)
	if err != nil {
		return nil, err
	}
	stmt, ok := n.(ast.Stmt)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected statement, got %T", n)
	}
	return stmt, nil
}

// unmapExprList converts a Scheme list of expressions to []ast.Expr.
func unmapExprList(v values.Value) ([]ast.Expr, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of expressions, got %T", v)
	}
	var result []ast.Expr
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		expr, err := unmapExpr(pair.Car())
		if err != nil {
			return nil, err
		}
		if expr != nil {
			result = append(result, expr)
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return result, nil
}

// unmapStmtList converts a Scheme list of statements to []ast.Stmt.
func unmapStmtList(v values.Value) ([]ast.Stmt, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of statements, got %T", v)
	}
	var result []ast.Stmt
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		stmt, err := unmapStmt(pair.Car())
		if err != nil {
			return nil, err
		}
		if stmt != nil {
			result = append(result, stmt)
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return result, nil
}

// unmapStringList extracts a list of Go strings from a Scheme list of strings.
func unmapStringList(v values.Value, nodeType, fieldName string) ([]string, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field '%s' expected list, got %T", nodeType, fieldName, v)
	}
	var result []string
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		s, err := requireString(pair.Car(), nodeType, fieldName)
		if err != nil {
			return nil, err
		}
		result = append(result, s)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return result, nil
}

var tokenLookup = func() map[string]token.Token {
	m := make(map[string]token.Token, int(token.TILDE)+1)
	for i := token.ILLEGAL; i <= token.TILDE; i++ {
		m[i.String()] = i
	}
	return m
}()

// tokenFromSymbol converts a Scheme symbol to a token.Token.
func tokenFromSymbol(v values.Value, nodeType, fieldName string) (token.Token, error) {
	name, err := requireSymbol(v, nodeType, fieldName)
	if err != nil {
		return token.ILLEGAL, err
	}
	tok, ok := tokenLookup[name]
	if ok {
		return tok, nil
	}
	return token.ILLEGAL, werr.WrapForeignErrorf(errMalformedGoAST,
		"goast: %s field '%s' unknown token '%s'", nodeType, fieldName, name)
}

// --- Top-level ---

func unmapFile(fields values.Value) (*ast.File, error) {
	nameVal, err := requireField(fields, "file", "name")
	if err != nil {
		return nil, err
	}
	name, err := requireString(nameVal, "file", "name")
	if err != nil {
		return nil, err
	}
	declsVal, err := requireField(fields, "file", "decls")
	if err != nil {
		return nil, err
	}
	decls, err := unmapDeclList(declsVal)
	if err != nil {
		return nil, err
	}
	return &ast.File{
		Name:  ast.NewIdent(name),
		Decls: decls,
	}, nil
}

func unmapDeclList(v values.Value) ([]ast.Decl, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of declarations, got %T", v)
	}
	var result []ast.Decl
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		n, err := unmapNode(pair.Car())
		if err != nil {
			return nil, err
		}
		decl, ok := n.(ast.Decl)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected declaration, got %T", n)
		}
		result = append(result, decl)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return result, nil
}

// --- Declarations ---

func unmapFuncDecl(fields values.Value) (*ast.FuncDecl, error) {
	nameVal, err := requireField(fields, "func-decl", "name")
	if err != nil {
		return nil, err
	}
	name, err := requireString(nameVal, "func-decl", "name")
	if err != nil {
		return nil, err
	}

	recvVal, err := requireField(fields, "func-decl", "recv")
	if err != nil {
		return nil, err
	}
	var recv *ast.FieldList
	if !isFalse(recvVal) {
		recv, err = unmapFieldListValue(recvVal, "func-decl", "recv")
		if err != nil {
			return nil, err
		}
	}

	typeVal, err := requireField(fields, "func-decl", "type")
	if err != nil {
		return nil, err
	}
	typeNode, err := unmapNode(typeVal)
	if err != nil {
		return nil, err
	}
	funcType, ok := typeNode.(*ast.FuncType)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: func-decl 'type' expected func-type, got %T", typeNode)
	}

	bodyVal, err := requireField(fields, "func-decl", "body")
	if err != nil {
		return nil, err
	}
	var body *ast.BlockStmt
	if !isFalse(bodyVal) {
		bodyNode, err := unmapNode(bodyVal)
		if err != nil {
			return nil, err
		}
		body, ok = bodyNode.(*ast.BlockStmt)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: func-decl 'body' expected block, got %T", bodyNode)
		}
	}

	return &ast.FuncDecl{
		Name: ast.NewIdent(name),
		Recv: recv,
		Type: funcType,
		Body: body,
	}, nil
}

func unmapGenDecl(fields values.Value) (*ast.GenDecl, error) {
	tokVal, err := requireField(fields, "gen-decl", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "gen-decl", "tok")
	if err != nil {
		return nil, err
	}
	specsVal, err := requireField(fields, "gen-decl", "specs")
	if err != nil {
		return nil, err
	}
	specs, err := unmapSpecList(specsVal)
	if err != nil {
		return nil, err
	}
	return &ast.GenDecl{
		Tok:   tok,
		Specs: specs,
	}, nil
}

func unmapSpecList(v values.Value) ([]ast.Spec, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of specs, got %T", v)
	}
	var result []ast.Spec
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		n, err := unmapNode(pair.Car())
		if err != nil {
			return nil, err
		}
		spec, ok := n.(ast.Spec)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected spec, got %T", n)
		}
		result = append(result, spec)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return result, nil
}

// --- Specs ---

func unmapImportSpec(fields values.Value) (*ast.ImportSpec, error) {
	nameVal, err := requireField(fields, "import-spec", "name")
	if err != nil {
		return nil, err
	}
	var name *ast.Ident
	if !isFalse(nameVal) {
		s, err := requireString(nameVal, "import-spec", "name")
		if err != nil {
			return nil, err
		}
		name = ast.NewIdent(s)
	}

	pathVal, err := requireField(fields, "import-spec", "path")
	if err != nil {
		return nil, err
	}
	pathNode, err := unmapNode(pathVal)
	if err != nil {
		return nil, err
	}
	pathLit, ok := pathNode.(*ast.BasicLit)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: import-spec 'path' expected lit, got %T", pathNode)
	}

	return &ast.ImportSpec{
		Name: name,
		Path: pathLit,
	}, nil
}

func unmapValueSpec(fields values.Value) (*ast.ValueSpec, error) {
	namesVal, err := requireField(fields, "value-spec", "names")
	if err != nil {
		return nil, err
	}
	nameStrs, err := unmapStringList(namesVal, "value-spec", "names")
	if err != nil {
		return nil, err
	}
	names := make([]*ast.Ident, len(nameStrs))
	for i, s := range nameStrs {
		names[i] = ast.NewIdent(s)
	}

	typeVal, err := requireField(fields, "value-spec", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	valsVal, err := requireField(fields, "value-spec", "values")
	if err != nil {
		return nil, err
	}
	vals, err := unmapExprList(valsVal)
	if err != nil {
		return nil, err
	}

	return &ast.ValueSpec{
		Names:  names,
		Type:   typ,
		Values: vals,
	}, nil
}

func unmapTypeSpec(fields values.Value) (*ast.TypeSpec, error) {
	nameVal, err := requireField(fields, "type-spec", "name")
	if err != nil {
		return nil, err
	}
	name, err := requireString(nameVal, "type-spec", "name")
	if err != nil {
		return nil, err
	}
	typeVal, err := requireField(fields, "type-spec", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}
	return &ast.TypeSpec{
		Name: ast.NewIdent(name),
		Type: typ,
	}, nil
}

// --- Statements ---

func unmapBlockStmt(fields values.Value) (*ast.BlockStmt, error) {
	listVal, err := requireField(fields, "block", "list")
	if err != nil {
		return nil, err
	}
	stmts, err := unmapStmtList(listVal)
	if err != nil {
		return nil, err
	}
	return &ast.BlockStmt{List: stmts}, nil
}

func unmapReturnStmt(fields values.Value) (*ast.ReturnStmt, error) {
	resultsVal, err := requireField(fields, "return-stmt", "results")
	if err != nil {
		return nil, err
	}
	results, err := unmapExprList(resultsVal)
	if err != nil {
		return nil, err
	}
	return &ast.ReturnStmt{Results: results}, nil
}

func unmapExprStmt(fields values.Value) (*ast.ExprStmt, error) {
	xVal, err := requireField(fields, "expr-stmt", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}
	return &ast.ExprStmt{X: x}, nil
}

func unmapAssignStmt(fields values.Value) (*ast.AssignStmt, error) {
	lhsVal, err := requireField(fields, "assign-stmt", "lhs")
	if err != nil {
		return nil, err
	}
	lhs, err := unmapExprList(lhsVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := requireField(fields, "assign-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "assign-stmt", "tok")
	if err != nil {
		return nil, err
	}

	rhsVal, err := requireField(fields, "assign-stmt", "rhs")
	if err != nil {
		return nil, err
	}
	rhs, err := unmapExprList(rhsVal)
	if err != nil {
		return nil, err
	}

	return &ast.AssignStmt{Lhs: lhs, Tok: tok, Rhs: rhs}, nil
}

func unmapIfStmt(fields values.Value) (*ast.IfStmt, error) {
	initVal, err := requireField(fields, "if-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	condVal, err := requireField(fields, "if-stmt", "cond")
	if err != nil {
		return nil, err
	}
	cond, err := unmapExpr(condVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := requireField(fields, "if-stmt", "body")
	if err != nil {
		return nil, err
	}
	bodyNode, err := unmapStmt(bodyVal)
	if err != nil {
		return nil, err
	}
	body, _ := bodyNode.(*ast.BlockStmt)

	elseVal, err := requireField(fields, "if-stmt", "else")
	if err != nil {
		return nil, err
	}
	els, err := unmapStmt(elseVal)
	if err != nil {
		return nil, err
	}

	return &ast.IfStmt{Init: init, Cond: cond, Body: body, Else: els}, nil
}

func unmapForStmt(fields values.Value) (*ast.ForStmt, error) {
	initVal, err := requireField(fields, "for-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	condVal, err := requireField(fields, "for-stmt", "cond")
	if err != nil {
		return nil, err
	}
	cond, err := unmapExpr(condVal)
	if err != nil {
		return nil, err
	}

	postVal, err := requireField(fields, "for-stmt", "post")
	if err != nil {
		return nil, err
	}
	post, err := unmapStmt(postVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := requireField(fields, "for-stmt", "body")
	if err != nil {
		return nil, err
	}
	bodyNode, err := unmapStmt(bodyVal)
	if err != nil {
		return nil, err
	}
	body, _ := bodyNode.(*ast.BlockStmt)

	return &ast.ForStmt{Init: init, Cond: cond, Post: post, Body: body}, nil
}

func unmapRangeStmt(fields values.Value) (*ast.RangeStmt, error) {
	keyVal, err := requireField(fields, "range-stmt", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valueFieldVal, err := requireField(fields, "range-stmt", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valueFieldVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := requireField(fields, "range-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "range-stmt", "tok")
	if err != nil {
		return nil, err
	}

	xVal, err := requireField(fields, "range-stmt", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := requireField(fields, "range-stmt", "body")
	if err != nil {
		return nil, err
	}
	bodyNode, err := unmapStmt(bodyVal)
	if err != nil {
		return nil, err
	}
	body, _ := bodyNode.(*ast.BlockStmt)

	return &ast.RangeStmt{Key: key, Value: val, Tok: tok, X: x, Body: body}, nil
}

func unmapBranchStmt(fields values.Value) (*ast.BranchStmt, error) {
	tokVal, err := requireField(fields, "branch-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "branch-stmt", "tok")
	if err != nil {
		return nil, err
	}

	labelVal, err := requireField(fields, "branch-stmt", "label")
	if err != nil {
		return nil, err
	}
	var label *ast.Ident
	if !isFalse(labelVal) {
		s, err := requireString(labelVal, "branch-stmt", "label")
		if err != nil {
			return nil, err
		}
		label = ast.NewIdent(s)
	}

	return &ast.BranchStmt{Tok: tok, Label: label}, nil
}

func unmapDeclStmt(fields values.Value) (*ast.DeclStmt, error) {
	declVal, err := requireField(fields, "decl-stmt", "decl")
	if err != nil {
		return nil, err
	}
	n, err := unmapNode(declVal)
	if err != nil {
		return nil, err
	}
	decl, ok := n.(ast.Decl)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: decl-stmt 'decl' expected declaration, got %T", n)
	}
	return &ast.DeclStmt{Decl: decl}, nil
}

func unmapIncDecStmt(fields values.Value) (*ast.IncDecStmt, error) {
	xVal, err := requireField(fields, "inc-dec-stmt", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := requireField(fields, "inc-dec-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "inc-dec-stmt", "tok")
	if err != nil {
		return nil, err
	}

	return &ast.IncDecStmt{X: x, Tok: tok}, nil
}

// --- Expressions ---

func unmapIdent(fields values.Value) (*ast.Ident, error) {
	nameVal, err := requireField(fields, "ident", "name")
	if err != nil {
		return nil, err
	}
	name, err := requireString(nameVal, "ident", "name")
	if err != nil {
		return nil, err
	}
	return ast.NewIdent(name), nil
}

func unmapBasicLit(fields values.Value) (*ast.BasicLit, error) {
	kindVal, err := requireField(fields, "lit", "kind")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(kindVal, "lit", "kind")
	if err != nil {
		return nil, err
	}

	valFieldVal, err := requireField(fields, "lit", "value")
	if err != nil {
		return nil, err
	}
	val, err := requireString(valFieldVal, "lit", "value")
	if err != nil {
		return nil, err
	}

	return &ast.BasicLit{Kind: tok, Value: val}, nil
}

func unmapBinaryExpr(fields values.Value) (*ast.BinaryExpr, error) {
	opVal, err := requireField(fields, "binary-expr", "op")
	if err != nil {
		return nil, err
	}
	op, err := tokenFromSymbol(opVal, "binary-expr", "op")
	if err != nil {
		return nil, err
	}

	xVal, err := requireField(fields, "binary-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	yVal, err := requireField(fields, "binary-expr", "y")
	if err != nil {
		return nil, err
	}
	y, err := unmapExpr(yVal)
	if err != nil {
		return nil, err
	}

	return &ast.BinaryExpr{X: x, Op: op, Y: y}, nil
}

func unmapUnaryExpr(fields values.Value) (*ast.UnaryExpr, error) {
	opVal, err := requireField(fields, "unary-expr", "op")
	if err != nil {
		return nil, err
	}
	op, err := tokenFromSymbol(opVal, "unary-expr", "op")
	if err != nil {
		return nil, err
	}

	xVal, err := requireField(fields, "unary-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	return &ast.UnaryExpr{Op: op, X: x}, nil
}

func unmapCallExpr(fields values.Value) (*ast.CallExpr, error) {
	funVal, err := requireField(fields, "call-expr", "fun")
	if err != nil {
		return nil, err
	}
	fun, err := unmapExpr(funVal)
	if err != nil {
		return nil, err
	}

	argsVal, err := requireField(fields, "call-expr", "args")
	if err != nil {
		return nil, err
	}
	args, err := unmapExprList(argsVal)
	if err != nil {
		return nil, err
	}

	return &ast.CallExpr{Fun: fun, Args: args}, nil
}

func unmapSelectorExpr(fields values.Value) (*ast.SelectorExpr, error) {
	xVal, err := requireField(fields, "selector-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	selVal, err := requireField(fields, "selector-expr", "sel")
	if err != nil {
		return nil, err
	}
	sel, err := requireString(selVal, "selector-expr", "sel")
	if err != nil {
		return nil, err
	}

	return &ast.SelectorExpr{X: x, Sel: ast.NewIdent(sel)}, nil
}

func unmapIndexExpr(fields values.Value) (*ast.IndexExpr, error) {
	xVal, err := requireField(fields, "index-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	indexVal, err := requireField(fields, "index-expr", "index")
	if err != nil {
		return nil, err
	}
	index, err := unmapExpr(indexVal)
	if err != nil {
		return nil, err
	}

	return &ast.IndexExpr{X: x, Index: index}, nil
}

func unmapStarExpr(fields values.Value) (*ast.StarExpr, error) {
	xVal, err := requireField(fields, "star-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}
	return &ast.StarExpr{X: x}, nil
}

func unmapParenExpr(fields values.Value) (*ast.ParenExpr, error) {
	xVal, err := requireField(fields, "paren-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}
	return &ast.ParenExpr{X: x}, nil
}

func unmapCompositeLit(fields values.Value) (*ast.CompositeLit, error) {
	typeVal, err := requireField(fields, "composite-lit", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	eltsVal, err := requireField(fields, "composite-lit", "elts")
	if err != nil {
		return nil, err
	}
	elts, err := unmapExprList(eltsVal)
	if err != nil {
		return nil, err
	}

	return &ast.CompositeLit{Type: typ, Elts: elts}, nil
}

func unmapKeyValueExpr(fields values.Value) (*ast.KeyValueExpr, error) {
	keyVal, err := requireField(fields, "kv-expr", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valFieldVal, err := requireField(fields, "kv-expr", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valFieldVal)
	if err != nil {
		return nil, err
	}

	return &ast.KeyValueExpr{Key: key, Value: val}, nil
}

func unmapFuncLit(fields values.Value) (*ast.FuncLit, error) {
	typeVal, err := requireField(fields, "func-lit", "type")
	if err != nil {
		return nil, err
	}
	typeNode, err := unmapNode(typeVal)
	if err != nil {
		return nil, err
	}
	funcType, ok := typeNode.(*ast.FuncType)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: func-lit 'type' expected func-type, got %T", typeNode)
	}

	bodyVal, err := requireField(fields, "func-lit", "body")
	if err != nil {
		return nil, err
	}
	bodyNode, err := unmapStmt(bodyVal)
	if err != nil {
		return nil, err
	}
	body, ok := bodyNode.(*ast.BlockStmt)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: func-lit 'body' expected block, got %T", bodyNode)
	}

	return &ast.FuncLit{Type: funcType, Body: body}, nil
}

// --- Type expressions ---

func unmapArrayType(fields values.Value) (*ast.ArrayType, error) {
	lenVal, err := requireField(fields, "array-type", "len")
	if err != nil {
		return nil, err
	}
	length, err := unmapExpr(lenVal)
	if err != nil {
		return nil, err
	}

	eltVal, err := requireField(fields, "array-type", "elt")
	if err != nil {
		return nil, err
	}
	elt, err := unmapExpr(eltVal)
	if err != nil {
		return nil, err
	}

	return &ast.ArrayType{Len: length, Elt: elt}, nil
}

func unmapMapType(fields values.Value) (*ast.MapType, error) {
	keyVal, err := requireField(fields, "map-type", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valFieldVal, err := requireField(fields, "map-type", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valFieldVal)
	if err != nil {
		return nil, err
	}

	return &ast.MapType{Key: key, Value: val}, nil
}

func unmapStructType(fields values.Value) (*ast.StructType, error) {
	fieldsVal, err := requireField(fields, "struct-type", "fields")
	if err != nil {
		return nil, err
	}
	fl, err := unmapFieldListValue(fieldsVal, "struct-type", "fields")
	if err != nil {
		return nil, err
	}
	return &ast.StructType{Fields: fl}, nil
}

func unmapInterfaceType(fields values.Value) (*ast.InterfaceType, error) {
	methodsVal, err := requireField(fields, "interface-type", "methods")
	if err != nil {
		return nil, err
	}
	fl, err := unmapFieldListValue(methodsVal, "interface-type", "methods")
	if err != nil {
		return nil, err
	}
	return &ast.InterfaceType{Methods: fl}, nil
}

func unmapFuncType(fields values.Value) (*ast.FuncType, error) {
	paramsVal, err := requireField(fields, "func-type", "params")
	if err != nil {
		return nil, err
	}
	params, err := unmapFieldListValue(paramsVal, "func-type", "params")
	if err != nil {
		return nil, err
	}

	resultsVal, err := requireField(fields, "func-type", "results")
	if err != nil {
		return nil, err
	}
	var results *ast.FieldList
	if !isFalse(resultsVal) {
		results, err = unmapFieldListValue(resultsVal, "func-type", "results")
		if err != nil {
			return nil, err
		}
	}

	return &ast.FuncType{Params: params, Results: results}, nil
}

func unmapField(fields values.Value) (*ast.Field, error) {
	namesVal, err := requireField(fields, "field", "names")
	if err != nil {
		return nil, err
	}
	nameStrs, err := unmapStringList(namesVal, "field", "names")
	if err != nil {
		return nil, err
	}
	var names []*ast.Ident
	for _, s := range nameStrs {
		names = append(names, ast.NewIdent(s))
	}

	typeVal, err := requireField(fields, "field", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	f := &ast.Field{
		Names: names,
		Type:  typ,
	}

	tagVal, ok := getField(fields, "tag")
	if ok && !isFalse(tagVal) {
		tagNode, err := unmapNode(tagVal)
		if err != nil {
			return nil, err
		}
		tagLit, ok := tagNode.(*ast.BasicLit)
		if ok {
			f.Tag = tagLit
		}
	}

	return f, nil
}

// unmapFieldListValue converts a Scheme list of field nodes to *ast.FieldList.
func unmapFieldListValue(v values.Value, nodeType, fieldName string) (*ast.FieldList, error) {
	if isFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field '%s' expected list, got %T", nodeType, fieldName, v)
	}
	var fields []*ast.Field
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		n, err := unmapNode(pair.Car())
		if err != nil {
			return nil, err
		}
		f, ok := n.(*ast.Field)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: %s field '%s' expected field node, got %T", nodeType, fieldName, n)
		}
		fields = append(fields, f)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return &ast.FieldList{List: fields}, nil
}
