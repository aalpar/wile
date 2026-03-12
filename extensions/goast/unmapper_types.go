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

func unmapChanType(fields values.Value) (*ast.ChanType, error) {
	dirVal, err := RequireField(fields, "chan-type", "dir")
	if err != nil {
		return nil, err
	}
	dir, err := chanDirFromSymbol(dirVal)
	if err != nil {
		return nil, err
	}

	valueVal, err := RequireField(fields, "chan-type", "value")
	if err != nil {
		return nil, err
	}
	val, err := unmapExpr(valueVal)
	if err != nil {
		return nil, err
	}

	return &ast.ChanType{Dir: dir, Value: val}, nil
}

// chanDirFromSymbol converts a Scheme symbol to ast.ChanDir.
func chanDirFromSymbol(v values.Value) (ast.ChanDir, error) {
	name, err := RequireSymbol(v, "chan-type", "dir")
	if err != nil {
		return 0, err
	}
	switch name {
	case "send":
		return ast.SEND, nil
	case "recv":
		return ast.RECV, nil
	case "both":
		return ast.SEND | ast.RECV, nil
	default:
		return 0, werr.WrapForeignErrorf(errMalformedGoAST,
			"goast: chan-type field 'dir' unknown direction '%s'", name)
	}
}

func unmapArrayType(fields values.Value) (*ast.ArrayType, error) {
	lenVal, err := RequireField(fields, "array-type", "len")
	if err != nil {
		return nil, err
	}
	length, err := unmapExpr(lenVal)
	if err != nil {
		return nil, err
	}

	eltVal, err := RequireField(fields, "array-type", "elt")
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
	keyVal, err := RequireField(fields, "map-type", "key")
	if err != nil {
		return nil, err
	}
	key, err := unmapExpr(keyVal)
	if err != nil {
		return nil, err
	}

	valFieldVal, err := RequireField(fields, "map-type", "value")
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
	fieldsVal, err := RequireField(fields, "struct-type", "fields")
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
	methodsVal, err := RequireField(fields, "interface-type", "methods")
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
	paramsVal, err := RequireField(fields, "func-type", "params")
	if err != nil {
		return nil, err
	}
	params, err := unmapFieldListValue(paramsVal, "func-type", "params")
	if err != nil {
		return nil, err
	}

	resultsVal, err := RequireField(fields, "func-type", "results")
	if err != nil {
		return nil, err
	}
	var results *ast.FieldList
	if !IsFalse(resultsVal) {
		results, err = unmapFieldListValue(resultsVal, "func-type", "results")
		if err != nil {
			return nil, err
		}
	}

	return &ast.FuncType{Params: params, Results: results}, nil
}

func unmapField(fields values.Value) (*ast.Field, error) {
	namesVal, err := RequireField(fields, "field", "names")
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

	typeVal, err := RequireField(fields, "field", "type")
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

	tagVal, ok := GetField(fields, "tag")
	if ok && !IsFalse(tagVal) {
		tagNode, err := unmapNode(tagVal)
		if err != nil {
			return nil, err
		}
		tagLit, ok := tagNode.(*ast.BasicLit)
		if !ok {
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: field 'tag' expected basic-lit, got %T", tagNode)
		}
		f.Tag = tagLit
	}

	return f, nil
}

// unmapFieldListValue converts a Scheme list of field nodes to *ast.FieldList.
func unmapFieldListValue(v values.Value, nodeType, fieldName string) (*ast.FieldList, error) {
	if IsFalse(v) {
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: %s field '%s' expected proper list, got %T", nodeType, fieldName, tuple)
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
			return nil, werr.WrapForeignErrorf(errMalformedGoAST,
				"goast: %s field '%s' improper list", nodeType, fieldName)
		}
		tuple = cdr
	}
	return &ast.FieldList{List: fields}, nil
}
