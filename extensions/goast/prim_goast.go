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
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"strings"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// parseOpts extracts mapper options from a variadic rest-arg list of symbols.
func parseOpts(rest values.Value, fset *token.FileSet) (*mapperOpts, parser.Mode) {
	opts := &mapperOpts{fset: fset}
	var mode parser.Mode

	tuple, ok := rest.(values.Tuple)
	if !ok {
		return opts, mode
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		s, ok := pair.Car().(*values.Symbol)
		if ok {
			switch s.Key {
			case "positions":
				opts.positions = true
			case "comments":
				opts.comments = true
				mode |= parser.ParseComments
			}
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return opts, mode
}

// PrimGoParseFile implements (go-parse-file filename . options).
// Parses a Go source file from disk and returns an s-expression AST.
func PrimGoParseFile(mc *machine.MachineContext) error {
	filename, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-parse-file")
	if err != nil {
		return err
	}

	err = security.Check(mc.Context(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   filename.Value,
	})
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	opts, mode := parseOpts(mc.Arg(1), fset)

	f, parseErr := parser.ParseFile(fset, filename.Value, nil, mode)
	if parseErr != nil {
		return werr.WrapForeignErrorf(errGoParseError,
			"go-parse-file: %s: %s", filename.Value, parseErr)
	}

	mc.SetValue(mapNode(f, opts))
	return nil
}

// PrimGoParseString implements (go-parse-string source . options).
// Parses a Go source string as a file and returns an s-expression AST.
func PrimGoParseString(mc *machine.MachineContext) error {
	source, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-parse-string")
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	opts, mode := parseOpts(mc.Arg(1), fset)

	f, parseErr := parser.ParseFile(fset, "source.go", source.Value, mode)
	if parseErr != nil {
		return werr.WrapForeignErrorf(errGoParseError,
			"go-parse-string: %s", parseErr)
	}

	mc.SetValue(mapNode(f, opts))
	return nil
}

// PrimGoParseExpr implements (go-parse-expr source).
// Parses a single Go expression and returns an s-expression AST.
func PrimGoParseExpr(mc *machine.MachineContext) error {
	source, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-parse-expr")
	if err != nil {
		return err
	}

	expr, parseErr := parser.ParseExpr(source.Value)
	if parseErr != nil {
		return werr.WrapForeignErrorf(errGoParseError,
			"go-parse-expr: %s", parseErr)
	}

	opts := &mapperOpts{}
	mc.SetValue(mapNode(expr, opts))
	return nil
}

// PrimGoFormat implements (go-format ast).
// Converts an s-expression AST back to formatted Go source.
func PrimGoFormat(mc *machine.MachineContext) error {
	astVal := mc.Arg(0)

	n, err := unmapNode(astVal)
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	var buf strings.Builder
	err = printer.Fprint(&buf, fset, n)
	if err != nil {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"go-format: printer error: %s", err)
	}

	formatted, fmtErr := format.Source([]byte(buf.String()))
	if fmtErr != nil {
		// format.Source can fail on partial ASTs. Return unformatted.
		mc.SetValue(values.NewString(buf.String()))
		return nil //nolint:nilerr // intentional: fall back to unformatted output
	}

	mc.SetValue(values.NewString(string(formatted)))
	return nil
}

// PrimGoNodeType implements (go-node-type ast).
// Returns the tag symbol of an AST node.
func PrimGoNodeType(mc *machine.MachineContext) error {
	astVal := mc.Arg(0)

	pair, ok := astVal.(*values.Pair)
	if !ok {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"go-node-type: expected tagged alist, got %T", astVal)
	}
	tagSym, ok := pair.Car().(*values.Symbol)
	if !ok {
		return werr.WrapForeignErrorf(errMalformedGoAST,
			"go-node-type: expected symbol tag, got %T", pair.Car())
	}

	mc.SetValue(values.NewSymbol(tagSym.Key))
	return nil
}
