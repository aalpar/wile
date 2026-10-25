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
	"context"
	"errors"
	"fmt"
	"strings"

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
//     file resolves against the file's directory; the same ctx records the file
//     on the include chain, so a cycle is refused (enterIncludedFile). The child
//     exists because ctx is a field copied by value (newChildExpander): there is
//     nowhere else for the derived ctx to live.
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
	if len(forms) == 0 {
		// An empty splice (an empty file, an empty clause) is a void, not no value:
		// (list (cond-expand (r7rs))) must see one operand.
		forms = []syntax.SyntaxValue{syntax.NewSyntaxObject(values.Void, srcCtx)}
	}
	head := syntax.NewSyntaxSymbol("begin", srcCtx.WithoutScopes()).
		AddScope(syntax.NewScopeWithLabel(label))
	return syntax.NewSyntaxCons(head, syntax.SyntaxList(srcCtx, forms...), srcCtx)
}

// expandIncludedFile reads one file and expands its forms on a child expander
// whose ctx carries the load stack with the file pushed and the include chain
// with the file recorded.
func (p *ExpanderTimeContinuation) expandIncludedFile(sym *syntax.SyntaxSymbol, r FileResolver, name string, src *syntax.SourceContext, foldCase bool) ([]syntax.SyntaxValue, error) {
	forms, filePath, err := readIncludedForms(p.ctx, p.env, r, name, src, foldCase)
	if err != nil {
		return nil, err
	}
	ctx, err := enterIncludedFile(p.ctx, sym.Key(), includeEntry{
		path:        filePath,
		guard:       p.depthGuard,
		macroOutput: p.depthGuard.macroOutput,
		literal:     literalIncludeAt(p.ctx, p.depthGuard),
	})
	if err != nil {
		return nil, err
	}

	// The splice leaves no include form for the compiler to wrap an error in, so
	// the include is recorded as the forms' origin, as a macro use is: an error
	// raised on any of them names the include, and a macro used in the file
	// chains to it (pushOrigin reads the parent off the use site).
	origin := pushOrigin(sym.Key(), sym.SourceContext(), src)
	for i, form := range forms {
		form = syntax.FlipScopeStampOrigin(form, nil, origin, nil)
		for s := range sym.Scopes().All() {
			form = syntax.AddScopeToSyntax(form, s)
		}
		forms[i] = p.pruneUseSiteScopes(form)
	}

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
		return nil, includeExpandFailure(ctx, sym.Key(), name, err)
	}
	return expanded, nil
}

// includeDepthError is the expansion depth guard's failure under a chain of
// includes, rendered once. A macro-produced include that never stops producing
// itself is not a refused cycle (enterIncludedFile), so the depth guard ends it;
// wrapped once per level, the error grew by one "include: failed to expand"
// layer per nested include, megabytes for the default depth. Each include
// level returns the inner includeDepthError itself, dropping the layers between
// two levels, which repeat the same frames.
type includeDepthError struct {
	err error
}

func (p *includeDepthError) Error() string {
	return p.err.Error()
}

func (p *includeDepthError) Unwrap() error {
	return p.err
}

// includeChainSummaryMax bounds how many files includeExpandFailure names.
const includeChainSummaryMax = 6

// includeExpandFailure wraps err, the failure to expand the forms of the file
// name that form included under ctx. A depth-guard failure becomes one
// includeDepthError naming the depth of the include chain and its files
// (outermost and innermost, bounded); one already made by a nested include is
// returned as is.
func includeExpandFailure(ctx context.Context, form, name string, err error) error {
	var deep *includeDepthError
	if errors.As(err, &deep) {
		return deep
	}
	if !errors.Is(err, werr.ErrExpandDepthExceeded) {
		return werr.WrapForeignErrorf(err, "%s: failed to expand %s", form, name)
	}
	entries := ctxChainValues[includeEntry](ctx, includeChainKey{})
	paths := make([]string, 0, includeChainSummaryMax+1)
	for i, e := range entries {
		half := includeChainSummaryMax / 2
		if len(entries) > includeChainSummaryMax && i == half {
			paths = append(paths, fmt.Sprintf("... %d more ...", len(entries)-includeChainSummaryMax))
		}
		if len(entries) > includeChainSummaryMax && i >= half && i < len(entries)-half {
			continue
		}
		paths = append(paths, e.path)
	}
	return &includeDepthError{err: werr.WrapForeignErrorWithCause(werr.ErrExpandDepthExceeded, err,
		"%s: %d nested includes (%s)", form, len(entries), strings.Join(paths, " -> "))}
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
