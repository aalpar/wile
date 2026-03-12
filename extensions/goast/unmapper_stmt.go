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

// requireBlockBody unmaps a body field and asserts it is a *ast.BlockStmt.
// Returns a clear error if the field is #f or the wrong node type.
func requireBlockBody(bodyVal values.Value, nodeType string) (*ast.BlockStmt, error) {
	bodyNode, err := unmapStmt(bodyVal)
	if err != nil {
		return nil, err
	}
	if bodyNode == nil {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field 'body' must not be #f", nodeType)
	}
	body, ok := bodyNode.(*ast.BlockStmt)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: %s field 'body' expected block, got %T", nodeType, bodyNode)
	}
	return body, nil
}

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
	body, err := requireBlockBody(bodyVal, "if-stmt")
	if err != nil {
		return nil, err
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
	body, err := requireBlockBody(bodyVal, "for-stmt")
	if err != nil {
		return nil, err
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
	body, err := requireBlockBody(bodyVal, "range-stmt")
	if err != nil {
		return nil, err
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

func unmapGoStmt(fields values.Value) (*ast.GoStmt, error) {
	callVal, err := RequireField(fields, "go-stmt", "call")
	if err != nil {
		return nil, err
	}
	callNode, err := unmapNode(callVal)
	if err != nil {
		return nil, err
	}
	call, ok := callNode.(*ast.CallExpr)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: go-stmt 'call' expected call-expr, got %T", callNode)
	}
	return &ast.GoStmt{Call: call}, nil
}

func unmapDeferStmt(fields values.Value) (*ast.DeferStmt, error) {
	callVal, err := RequireField(fields, "defer-stmt", "call")
	if err != nil {
		return nil, err
	}
	callNode, err := unmapNode(callVal)
	if err != nil {
		return nil, err
	}
	call, ok := callNode.(*ast.CallExpr)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: defer-stmt 'call' expected call-expr, got %T", callNode)
	}
	return &ast.DeferStmt{Call: call}, nil
}

func unmapSendStmt(fields values.Value) (*ast.SendStmt, error) {
	chanVal, err := RequireField(fields, "send-stmt", "chan")
	if err != nil {
		return nil, err
	}
	ch, err := unmapExpr(chanVal)
	if err != nil {
		return nil, err
	}

	valueVal, err := RequireField(fields, "send-stmt", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valueVal)
	if err != nil {
		return nil, err
	}

	return &ast.SendStmt{Chan: ch, Value: val}, nil
}

func unmapLabeledStmt(fields values.Value) (*ast.LabeledStmt, error) {
	labelVal, err := RequireField(fields, "labeled-stmt", "label")
	if err != nil {
		return nil, err
	}
	label, err := RequireString(labelVal, "labeled-stmt", "label")
	if err != nil {
		return nil, err
	}

	stmtVal, err := RequireField(fields, "labeled-stmt", "stmt")
	if err != nil {
		return nil, err
	}
	stmt, err := unmapStmt(stmtVal)
	if err != nil {
		return nil, err
	}

	return &ast.LabeledStmt{Label: ast.NewIdent(label), Stmt: stmt}, nil
}

func unmapSwitchStmt(fields values.Value) (*ast.SwitchStmt, error) {
	initVal, err := RequireField(fields, "switch-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	tagVal, err := RequireField(fields, "switch-stmt", "tag")
	if err != nil {
		return nil, err
	}
	tag, err := unmapExpr(tagVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := RequireField(fields, "switch-stmt", "body")
	if err != nil {
		return nil, err
	}
	body, err := requireBlockBody(bodyVal, "switch-stmt")
	if err != nil {
		return nil, err
	}

	return &ast.SwitchStmt{Init: init, Tag: tag, Body: body}, nil
}

func unmapTypeSwitchStmt(fields values.Value) (*ast.TypeSwitchStmt, error) {
	initVal, err := RequireField(fields, "type-switch-stmt", "init")
	if err != nil {
		return nil, err
	}
	init, err := unmapStmt(initVal)
	if err != nil {
		return nil, err
	}

	assignVal, err := RequireField(fields, "type-switch-stmt", "assign")
	if err != nil {
		return nil, err
	}
	assign, err := unmapStmt(assignVal)
	if err != nil {
		return nil, err
	}

	bodyVal, err := RequireField(fields, "type-switch-stmt", "body")
	if err != nil {
		return nil, err
	}
	body, err := requireBlockBody(bodyVal, "type-switch-stmt")
	if err != nil {
		return nil, err
	}

	return &ast.TypeSwitchStmt{Init: init, Assign: assign, Body: body}, nil
}

func unmapSelectStmt(fields values.Value) (*ast.SelectStmt, error) {
	bodyVal, err := RequireField(fields, "select-stmt", "body")
	if err != nil {
		return nil, err
	}
	body, err := requireBlockBody(bodyVal, "select-stmt")
	if err != nil {
		return nil, err
	}
	return &ast.SelectStmt{Body: body}, nil
}

func unmapCommClause(fields values.Value) (*ast.CommClause, error) {
	commVal, err := RequireField(fields, "comm-clause", "comm")
	if err != nil {
		return nil, err
	}
	var comm ast.Stmt
	if !IsFalse(commVal) {
		comm, err = unmapStmt(commVal)
		if err != nil {
			return nil, err
		}
	}

	bodyVal, err := RequireField(fields, "comm-clause", "body")
	if err != nil {
		return nil, err
	}
	body, err := unmapStmtList(bodyVal)
	if err != nil {
		return nil, err
	}

	return &ast.CommClause{Comm: comm, Body: body}, nil
}

func unmapCaseClause(fields values.Value) (*ast.CaseClause, error) {
	listVal, err := RequireField(fields, "case-clause", "list")
	if err != nil {
		return nil, err
	}
	var list []ast.Expr
	if !IsFalse(listVal) {
		list, err = unmapExprList(listVal)
		if err != nil {
			return nil, err
		}
	}

	bodyVal, err := RequireField(fields, "case-clause", "body")
	if err != nil {
		return nil, err
	}
	body, err := unmapStmtList(bodyVal)
	if err != nil {
		return nil, err
	}

	return &ast.CaseClause{List: list, Body: body}, nil
}
