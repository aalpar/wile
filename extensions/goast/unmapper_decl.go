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
	nameVal, err := RequireField(fields, "file", "name")
	if err != nil {
		return nil, err
	}
	name, err := RequireString(nameVal, "file", "name")
	if err != nil {
		return nil, err
	}
	declsVal, err := RequireField(fields, "file", "decls")
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
	if IsFalse(v) {
		return nil, nil
	}
	tuple, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: expected list of declarations, got %T", v)
	}
	var decls []ast.Decl
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got %T", tuple)
		}
		// Skip comment-group entries (standalone comments interleaved by mapper).
		if sexpTag(pair.Car()) == "comment-group" {
			cdr, ok := pair.Cdr().(values.Tuple)
			if !ok {
				return nil, werr.WrapForeignErrorf(errMalformedGoAST,
					"goast: improper list in declarations")
			}
			tuple = cdr
			continue
		}
		n, err := unmapNode(pair.Car())
		if err != nil {
			return nil, err
		}
		d, ok := n.(ast.Decl)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected declaration, got %T", n)
		}
		decls = append(decls, d)
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected proper list of declarations, got improper cdr %T", pair.Cdr())
		}
		tuple = cdr
	}
	return decls, nil
}

// --- Declarations ---

func unmapFuncDecl(fields values.Value) (*ast.FuncDecl, error) {
	nameVal, err := RequireField(fields, "func-decl", "name")
	if err != nil {
		return nil, err
	}
	name, err := RequireString(nameVal, "func-decl", "name")
	if err != nil {
		return nil, err
	}

	recvVal, err := RequireField(fields, "func-decl", "recv")
	if err != nil {
		return nil, err
	}
	var recv *ast.FieldList
	if !IsFalse(recvVal) {
		recv, err = unmapFieldListValue(recvVal, "func-decl", "recv")
		if err != nil {
			return nil, err
		}
	}

	typeVal, err := RequireField(fields, "func-decl", "type")
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

	bodyVal, err := RequireField(fields, "func-decl", "body")
	if err != nil {
		return nil, err
	}
	var body *ast.BlockStmt
	if !IsFalse(bodyVal) {
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
	tokVal, err := RequireField(fields, "gen-decl", "tok")
	if err != nil {
		return nil, err
	}
	tok, err := tokenFromSymbol(tokVal, "gen-decl", "tok")
	if err != nil {
		return nil, err
	}
	specsVal, err := RequireField(fields, "gen-decl", "specs")
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
	return unmapList(v, func(elem values.Value) (ast.Spec, error) {
		n, err := unmapNode(elem)
		if err != nil {
			return nil, err
		}
		s, ok := n.(ast.Spec)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: expected spec, got %T", n)
		}
		return s, nil
	}, "specs")
}

// --- Specs ---

func unmapImportSpec(fields values.Value) (*ast.ImportSpec, error) {
	nameVal, err := RequireField(fields, "import-spec", "name")
	if err != nil {
		return nil, err
	}
	var name *ast.Ident
	if !IsFalse(nameVal) {
		s, err := RequireString(nameVal, "import-spec", "name")
		if err != nil {
			return nil, err
		}
		name = ast.NewIdent(s)
	}

	pathVal, err := RequireField(fields, "import-spec", "path")
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
	namesVal, err := RequireField(fields, "value-spec", "names")
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

	typeVal, err := RequireField(fields, "value-spec", "type")
	if err != nil {
		return nil, err
	}
	typ, err := unmapExpr(typeVal)
	if err != nil {
		return nil, err
	}

	valsVal, err := RequireField(fields, "value-spec", "values")
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
	nameVal, err := RequireField(fields, "type-spec", "name")
	if err != nil {
		return nil, err
	}
	name, err := RequireString(nameVal, "type-spec", "name")
	if err != nil {
		return nil, err
	}
	typeVal, err := RequireField(fields, "type-spec", "type")
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
