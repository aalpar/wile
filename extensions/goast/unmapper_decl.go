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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got %T", tuple)
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got improper cdr %T", pair.Cdr())
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of specs, got %T", tuple)
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of specs, got improper cdr %T", pair.Cdr())
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
