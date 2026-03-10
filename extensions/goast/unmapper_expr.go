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
