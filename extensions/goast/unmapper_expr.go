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
	nameVal, err := RequireField(fields, "ident", "name")
	if err != nil {
		return nil, err
	}
	name, err := RequireString(nameVal, "ident", "name")
	if err != nil {
		return nil, err
	}
	return ast.NewIdent(name), nil
}

func unmapBasicLit(fields values.Value) (*ast.BasicLit, error) {
	kindVal, err := RequireField(fields, "lit", "kind")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(kindVal, "lit", "kind")
	if err != nil {
		return nil, err
	}

	valFieldVal, err := RequireField(fields, "lit", "value")
	if err != nil {
		return nil, err
	}
	val, err := RequireString(valFieldVal, "lit", "value")
	if err != nil {
		return nil, err
	}

	return &ast.BasicLit{Kind: tok, Value: val}, nil
}

func unmapBinaryExpr(fields values.Value) (*ast.BinaryExpr, error) {
	opVal, err := RequireField(fields, "binary-expr", "op")
	if err != nil {
		return nil, err
	}
	op, err := tokenFromSymbol(opVal, "binary-expr", "op")
	if err != nil {
		return nil, err
	}

	xVal, err := RequireField(fields, "binary-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	yVal, err := RequireField(fields, "binary-expr", "y")
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
	opVal, err := RequireField(fields, "unary-expr", "op")
	if err != nil {
		return nil, err
	}
	op, err := tokenFromSymbol(opVal, "unary-expr", "op")
	if err != nil {
		return nil, err
	}

	xVal, err := RequireField(fields, "unary-expr", "x")
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
	funVal, err := RequireField(fields, "call-expr", "fun")
	if err != nil {
		return nil, err
	}
	fun, err := unmapExpr(funVal)
	if err != nil {
		return nil, err
	}

	argsVal, err := RequireField(fields, "call-expr", "args")
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
	xVal, err := RequireField(fields, "selector-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	selVal, err := RequireField(fields, "selector-expr", "sel")
	if err != nil {
		return nil, err
	}
	sel, err := RequireString(selVal, "selector-expr", "sel")
	if err != nil {
		return nil, err
	}

	return &ast.SelectorExpr{X: x, Sel: ast.NewIdent(sel)}, nil
}

func unmapIndexListExpr(fields values.Value) (*ast.IndexListExpr, error) {
	xVal, err := RequireField(fields, "index-list-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}
	indicesVal, err := RequireField(fields, "index-list-expr", "indices")
	if err != nil {
		return nil, err
	}
	indices, err := unmapExprList(indicesVal)
	if err != nil {
		return nil, err
	}
	return &ast.IndexListExpr{X: x, Indices: indices}, nil
}

func unmapIndexExpr(fields values.Value) (*ast.IndexExpr, error) {
	xVal, err := RequireField(fields, "index-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	indexVal, err := RequireField(fields, "index-expr", "index")
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
	xVal, err := RequireField(fields, "star-expr", "x")
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
	xVal, err := RequireField(fields, "paren-expr", "x")
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
	typeVal, err := RequireField(fields, "composite-lit", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	eltsVal, err := RequireField(fields, "composite-lit", "elts")
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
	keyVal, err := RequireField(fields, "kv-expr", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valFieldVal, err := RequireField(fields, "kv-expr", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valFieldVal)
	if err != nil {
		return nil, err
	}

	return &ast.KeyValueExpr{Key: key, Value: val}, nil
}

func unmapSliceExpr(fields values.Value) (*ast.SliceExpr, error) {
	xVal, err := RequireField(fields, "slice-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	lowVal, err := RequireField(fields, "slice-expr", "low")
	if err != nil {
		return nil, err
	}
	low, err := unmapExpr(lowVal)
	if err != nil {
		return nil, err
	}

	highVal, err := RequireField(fields, "slice-expr", "high")
	if err != nil {
		return nil, err
	}
	high, err := unmapExpr(highVal)
	if err != nil {
		return nil, err
	}

	maxVal, err := RequireField(fields, "slice-expr", "max")
	if err != nil {
		return nil, err
	}
	sliceMax, err := unmapExpr(maxVal)
	if err != nil {
		return nil, err
	}

	slice3Val, err := RequireField(fields, "slice-expr", "slice3")
	if err != nil {
		return nil, err
	}
	b, ok := slice3Val.(*values.Boolean)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: slice-expr field 'slice3' must be boolean, got %T", slice3Val)
	}

	return &ast.SliceExpr{X: x, Low: low, High: high, Max: sliceMax, Slice3: b.Value}, nil
}

func unmapEllipsis(fields values.Value) (*ast.Ellipsis, error) {
	eltVal, err := RequireField(fields, "ellipsis", "elt")
	if err != nil {
		return nil, err
	}
	elt, err := unmapExpr(eltVal)
	if err != nil {
		return nil, err
	}
	return &ast.Ellipsis{Elt: elt}, nil
}

func unmapTypeAssertExpr(fields values.Value) (*ast.TypeAssertExpr, error) {
	xVal, err := RequireField(fields, "type-assert-expr", "x")
	if err != nil {
		return nil, err
	}
	x, err := unmapExpr(xVal)
	if err != nil {
		return nil, err
	}

	typeVal, err := RequireField(fields, "type-assert-expr", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	return &ast.TypeAssertExpr{X: x, Type: typ}, nil
}

func unmapFuncLit(fields values.Value) (*ast.FuncLit, error) {
	typeVal, err := RequireField(fields, "func-lit", "type")
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

	bodyVal, err := RequireField(fields, "func-lit", "body")
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
