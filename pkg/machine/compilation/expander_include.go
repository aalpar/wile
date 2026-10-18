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

package compilation

// expander_include.go splices (include ...) and (include-ci ...) at expand
// time. R7RS §4.1.7: the included expressions occur in place of the include
// form, so the expander must see them as the includer's own forms; before this
// the form passed through expandUnchanged and the COMPILER read the file, which
// left every later form of the unit expanded, analysed and boxed without it.

import (
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// expandIncludeForm expands (include filename ...).
func (p *ExpanderTimeContinuation) expandIncludeForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandIncludeImpl(sym, expr, false)
}

// expandIncludeCiForm expands (include-ci filename ...): include, with each file
// read as if it began with #!fold-case.
func (p *ExpanderTimeContinuation) expandIncludeCiForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandIncludeImpl(sym, expr, true)
}

// expandIncludeImpl reads each named file, expands its forms in the includer's
// environment, and returns them as one (begin form ...).
//
// A form of the wrong shape (no filename, a non-string, an improper list) is
// returned unchanged for the validator to report, as every other arity-guarded
// expander here does. So is any include in an environment with no file resolver,
// but there nothing reports it: the compiler falls back to an OS resolver and
// includes the file through CompileInclude, with the pre-splice semantics.
//
// Three properties are load-bearing:
//
//   - The included forms carry the include head's scope set. Scopes live on
//     identifiers, so without the stamp no binding local to the includer (a let
//     variable, a body-local macro) has scopes that are a subset of the file's
//     identifiers', and the file cannot see it.
//
//   - The load stack is pushed AFTER the read and popped after the forms are
//     expanded, on a child expander's ctx, so a relative include nested in the
//     file resolves against the file's directory. The child exists because ctx is
//     a field copied by value (newChildExpander): there is nowhere else for the
//     derived ctx to live.
//
//   - The result head is a FRESH-scope begin that drops the include head's scopes
//     (spliceBegin).
func (p *ExpanderTimeContinuation) expandIncludeImpl(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, foldCase bool) (syntax.SyntaxValue, error) {
	r := p.env.FileResolver()
	if r == nil {
		return formUnchanged(sym, expr)
	}
	names, ok := includeFileNames(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}

	var spliced []syntax.SyntaxValue
	for _, name := range names {
		expanded, err := p.expandIncludedFile(sym, r, name, expr.SourceContext(), foldCase)
		if err != nil {
			return nil, wrapSourcedError(expr.SourceContext(), err)
		}
		spliced = append(spliced, expanded...)
	}

	return spliceBegin(sym, "include", spliced), nil
}

// spliceBegin returns (begin form ...) for forms spliced in place of the form
// whose head is sym. It is the one place a splice builds its result, shared by
// include and cond-expand.
//
// The head carries a FRESH scope labelled label, as wrapInBegin's does, and drops
// sym's scopes. The head is dispatched by the binding it resolves to, so a bare
// begin symbol would be a call to 9 under a user's (define begin 9), and would
// resolve to a local begin under (let ((begin ...)) (include ...)).
func spliceBegin(sym *syntax.SyntaxSymbol, label string, forms []syntax.SyntaxValue) syntax.SyntaxValue {
	srcCtx := sym.SourceContext()
	head := syntax.NewSyntaxSymbol("begin", srcCtx.WithoutScopes()).
		AddScope(syntax.NewScopeWithLabel(label))
	return syntax.NewSyntaxCons(head, syntax.SyntaxList(srcCtx, forms...), srcCtx)
}

// expandIncludedFile reads one file and expands its forms on a child expander
// whose ctx carries the load stack with the file pushed.
func (p *ExpanderTimeContinuation) expandIncludedFile(sym *syntax.SyntaxSymbol, r FileResolver, name string, src *syntax.SourceContext, foldCase bool) ([]syntax.SyntaxValue, error) {
	forms, filePath, err := readIncludedForms(p.ctx, p.env, r, name, src, foldCase)
	if err != nil {
		return nil, err
	}

	for i, form := range forms {
		for s := range sym.Scopes().All() {
			form = syntax.AddScopeToSyntax(form, s)
		}
		forms[i] = p.pruneUseSiteScopes(form)
	}

	ctx := p.ctx
	stack := resolver.SelectLoadStack(ctx)
	if stack == nil {
		stack = sourceload.NewLoadStack()
		ctx = sourceload.WithLoadStack(ctx, stack)
	}
	stack.Push(filePath)
	defer stack.Pop()

	child := p.newChildExpander(p.env)
	child.ctx = ctx
	child.libraryScope = p.libraryScope
	expanded, err := child.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "include: failed to expand %s", name)
	}
	return expanded, nil
}

// includeFileNames returns the filename strings of an include form's operands.
// It reports false for no operands, a non-string operand or an improper list.
func includeFileNames(args syntax.SyntaxValue) ([]string, bool) {
	var names []string
	rest := args
	for {
		pair, ok := asNonEmptyPair(rest)
		if !ok {
			break
		}
		s, ok := pair.SyntaxCar().Unwrap().(*values.String)
		if !ok {
			return nil, false
		}
		names = append(names, s.Value)
		rest = pair.SyntaxCdr()
	}
	if !syntax.IsSyntaxEmptyList(rest) {
		return nil, false
	}
	return names, len(names) > 0
}
