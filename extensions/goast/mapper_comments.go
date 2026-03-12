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
)

// collectAttached returns the set of comment groups that are referenced
// by Doc or Comment fields on AST nodes. Any group in file.Comments
// NOT in this set is a standalone comment.
func collectAttached(f *ast.File) map[*ast.CommentGroup]bool {
	attached := make(map[*ast.CommentGroup]bool)
	if f.Doc != nil {
		attached[f.Doc] = true
	}
	for _, d := range f.Decls {
		switch dd := d.(type) {
		case *ast.FuncDecl:
			if dd.Doc != nil {
				attached[dd.Doc] = true
			}
			collectFieldListAttached(dd.Recv, attached)
			if dd.Type != nil {
				collectFieldListAttached(dd.Type.Params, attached)
				collectFieldListAttached(dd.Type.Results, attached)
			}
		case *ast.GenDecl:
			if dd.Doc != nil {
				attached[dd.Doc] = true
			}
			for _, s := range dd.Specs {
				collectSpecAttached(s, attached)
			}
		}
	}
	return attached
}

func collectSpecAttached(s ast.Spec, attached map[*ast.CommentGroup]bool) {
	switch ss := s.(type) {
	case *ast.TypeSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
		collectTypeAttached(ss.Type, attached)
	case *ast.ValueSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
	case *ast.ImportSpec:
		if ss.Doc != nil {
			attached[ss.Doc] = true
		}
		if ss.Comment != nil {
			attached[ss.Comment] = true
		}
	}
}

func collectTypeAttached(t ast.Expr, attached map[*ast.CommentGroup]bool) {
	switch tt := t.(type) {
	case *ast.StructType:
		collectFieldListAttached(tt.Fields, attached)
	case *ast.InterfaceType:
		collectFieldListAttached(tt.Methods, attached)
	case *ast.FuncType:
		collectFieldListAttached(tt.Params, attached)
		collectFieldListAttached(tt.Results, attached)
	}
}

// collectFieldListAttached marks Doc/Comment on each field in a FieldList.
func collectFieldListAttached(fl *ast.FieldList, attached map[*ast.CommentGroup]bool) {
	if fl == nil {
		return
	}
	for _, f := range fl.List {
		if f.Doc != nil {
			attached[f.Doc] = true
		}
		if f.Comment != nil {
			attached[f.Comment] = true
		}
	}
}

// mapDeclsWithStandalone interleaves declarations with standalone comment
// groups in source order. Standalone groups are emitted as
// (comment-group (text . ("// ..."))) entries.
func mapDeclsWithStandalone(f *ast.File, opts *mapperOpts) []values.Value {
	attached := collectAttached(f)
	var entries []values.Value
	ci := 0
	for _, d := range f.Decls {
		// Emit standalone comments before this declaration.
		for ci < len(f.Comments) && f.Comments[ci].Pos() < d.Pos() {
			if !attached[f.Comments[ci]] {
				entries = append(entries, Node("comment-group",
					Field("text", commentGroupToStrings(f.Comments[ci]))))
			}
			ci++
		}
		entries = append(entries, mapNode(d, opts))
	}
	// Emit remaining standalone comments after all declarations.
	for ci < len(f.Comments) {
		if !attached[f.Comments[ci]] {
			entries = append(entries, Node("comment-group",
				Field("text", commentGroupToStrings(f.Comments[ci]))))
		}
		ci++
	}
	return entries
}
