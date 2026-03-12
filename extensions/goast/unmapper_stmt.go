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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func unmapBlockStmt(fields values.Value) (*ast.BlockStmt, error) {
	listVal, err := RequireField(fields, "block", "list")
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
	resultsVal, err := RequireField(fields, "return-stmt", "results")
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
	xVal, err := RequireField(fields, "expr-stmt", "x")
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
	lhsVal, err := RequireField(fields, "assign-stmt", "lhs")
	if err != nil {
		return nil, err
	}
	lhs, err := unmapExprList(lhsVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := RequireField(fields, "assign-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "assign-stmt", "tok")
	if err != nil {
		return nil, err
	}

	rhsVal, err := RequireField(fields, "assign-stmt", "rhs")
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
	initVal, err := RequireField(fields, "if-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	condVal, err := RequireField(fields, "if-stmt", "cond")
	if err != nil {
		return nil, err
	}
	cond, err := unmapExpr(condVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := RequireField(fields, "if-stmt", "body")
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
			"goast: if-stmt field 'body' expected block, got %T", bodyNode)
	}

	elseVal, err := RequireField(fields, "if-stmt", "else")
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
	initVal, err := RequireField(fields, "for-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	condVal, err := RequireField(fields, "for-stmt", "cond")
	if err != nil {
		return nil, err
	}
	cond, err := unmapExpr(condVal)
	if err != nil {
		return nil, err
	}

	postVal, err := RequireField(fields, "for-stmt", "post")
	if err != nil {
		return nil, err
	}
	post, err := unmapStmt(postVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := RequireField(fields, "for-stmt", "body")
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
			"goast: for-stmt field 'body' expected block, got %T", bodyNode)
	}

	return &ast.ForStmt{Init: init, Cond: cond, Post: post, Body: body}, nil
}

func unmapRangeStmt(fields values.Value) (*ast.RangeStmt, error) {
	keyVal, err := RequireField(fields, "range-stmt", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valueFieldVal, err := RequireField(fields, "range-stmt", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valueFieldVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := RequireField(fields, "range-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "range-stmt", "tok")
	if err != nil {
		return nil, err
	}

	xVal, err := RequireField(fields, "range-stmt", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := RequireField(fields, "range-stmt", "body")
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
			"goast: range-stmt field 'body' expected block, got %T", bodyNode)
	}

	return &ast.RangeStmt{Key: key, Value: val, Tok: tok, X: x, Body: body}, nil
}

func unmapBranchStmt(fields values.Value) (*ast.BranchStmt, error) {
	tokVal, err := RequireField(fields, "branch-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "branch-stmt", "tok")
	if err != nil {
		return nil, err
	}

	labelVal, err := RequireField(fields, "branch-stmt", "label")
	if err != nil {
		return nil, err
	}
	var label *ast.Ident
	if !IsFalse(labelVal) {
		s, err := RequireString(labelVal, "branch-stmt", "label")
		if err != nil {
			return nil, err
		}
		label = ast.NewIdent(s)
	}

	return &ast.BranchStmt{Tok: tok, Label: label}, nil
}

func unmapDeclStmt(fields values.Value) (*ast.DeclStmt, error) {
	declVal, err := RequireField(fields, "decl-stmt", "decl")
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
	xVal, err := RequireField(fields, "inc-dec-stmt", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	tokVal, err := RequireField(fields, "inc-dec-stmt", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "inc-dec-stmt", "tok")
	if err != nil {
		return nil, err
	}

	return &ast.IncDecStmt{X: x, Tok: tok}, nil
}
