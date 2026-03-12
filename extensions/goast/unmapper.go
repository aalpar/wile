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
	case "go-stmt":
		return unmapGoStmt(fields)
	case "defer-stmt":
		return unmapDeferStmt(fields)
	case "send-stmt":
		return unmapSendStmt(fields)
	case "labeled-stmt":
		return unmapLabeledStmt(fields)
	case "switch-stmt":
		return unmapSwitchStmt(fields)
	case "type-switch-stmt":
		return unmapTypeSwitchStmt(fields)
	case "case-clause":
		return unmapCaseClause(fields)
	case "select-stmt":
		return unmapSelectStmt(fields)
	case "comm-clause":
		return unmapCommClause(fields)

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
	case "index-list-expr":
		return unmapIndexListExpr(fields)
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
	case "type-assert-expr":
		return unmapTypeAssertExpr(fields)
	case "slice-expr":
		return unmapSliceExpr(fields)
	case "ellipsis":
		return unmapEllipsis(fields)

	// Types
	case "chan-type":
		return unmapChanType(fields)
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

	// Error recovery — bad nodes cannot be unmapped back to valid Go source.
	case "bad-expr":
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: bad-expr cannot be unmapped (represents a parse error)")
	case "bad-stmt":
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: bad-stmt cannot be unmapped (represents a parse error)")
	case "bad-decl":
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: bad-decl cannot be unmapped (represents a parse error)")

	case "unknown":
		goType, _ := GetField(fields, "go-type")
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
	if IsFalse(v) {
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
	if IsFalse(v) {
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

// unmapList traverses a Scheme proper list, applying convert to each element.
func unmapList[T any](v values.Value, convert func(values.Value) (T, error), what string) ([]T, error) {
	if IsFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of %s, got %T", what, v)
	}
	var result []T
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of %s, got %T", what, tuple)
		}
		item, err := convert(pair.Car())
		if err != nil {
			return nil, err
		}
		result = append(result, item)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of %s, got improper cdr %T", what, pair.Cdr())
		}
		tuple = cdr
	}
	return result, nil
}

// unmapExprList converts a Scheme list of expressions to []ast.Expr.
func unmapExprList(v values.Value) ([]ast.Expr, error) {
	if IsFalse(v) {
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of expressions, got %T", tuple)
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list in expression list")
		}
		tuple = cdr
	}
	return result, nil
}

// unmapStmtList converts a Scheme list of statements to []ast.Stmt.
func unmapStmtList(v values.Value) ([]ast.Stmt, error) {
	if IsFalse(v) {
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of statements, got %T", tuple)
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: improper list in statement list")
		}
		tuple = cdr
	}
	return result, nil
}

// unmapStringList extracts a list of Go strings from a Scheme list of strings.
func unmapStringList(v values.Value, nodeType, fieldName string) ([]string, error) {
	if IsFalse(v) {
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: %s field '%s' expected proper list, got %T", nodeType, fieldName, tuple)
		}
		s, err := RequireString(pair.Car(), nodeType, fieldName)
		if err != nil {
			return nil, err
		}
		result = append(result, s)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: %s field '%s' improper list", nodeType, fieldName)
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
	name, err := RequireSymbol(v, nodeType, fieldName)
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
