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

import (
	"bufio"
	"context"
	"errors"
	"io"
	"io/fs"

	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

func findFile(p *CompileTimeContinuation, ctctx CompileTimeCallContext, path string) (fs.File, string, error) {
	return p.fileResolver.ResolveAndOpen(p.includeAuthorizerContext(ctctx.Context()), path)
}

// includeAuthorizerContext installs the COMPILING namespace's effective
// authorizer on ctx when nothing has installed one already.
//
// resolver.SelectAuthorizer prefers the ctx authorizer and otherwise falls back
// to the root env the resolver captured at engine construction. LoadLibrary was
// the only production installer, so an (include …) compiled into a child
// namespace resolved under ROOT policy — the same substitution execNS fixes at
// run time, at the other end of the pipeline.
//
// Only when ctx carries none, and that direction is load-bearing: an include
// nested inside a library load must stay under the authorizer LoadLibrary
// installed for that chain. Overwriting it with the compiling env's would
// WEAKEN the policy mid-load, which is the opposite of the fix.
func (p *CompileTimeContinuation) includeAuthorizerContext(ctx context.Context) context.Context {
	if security.FromContext(ctx) != nil {
		return ctx
	}
	if p.env == nil {
		return ctx
	}
	ns := p.env.Namespace()
	if ns == nil {
		return ctx
	}
	auth := ns.EffectiveAuthorizer()
	if auth == nil {
		return ctx
	}
	return security.WithAuthorizer(ctx, auth)
}

// CompileInclude compiles an include expression.
// It reads and compiles all forms from the specified files in order.
// Each form is expanded and compiled in the current environment.
func (p *CompileTimeContinuation) CompileInclude(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.compileIncludeImpl(ctctx, expr, false)
}

// compileIncludeImpl is the shared implementation for include and include-ci.
// It uses letrec* semantics so that forward references work within included files.
// When foldCase is true (include-ci), the included file is read with R7RS §2.1
// case folding enabled, as if it began with #!fold-case.
//
// R7RS §5.3.2: Internal definitions use letrec* semantics where all defined
// variables are in scope at the start of the body.
func (p *CompileTimeContinuation) compileIncludeImpl(ctctx CompileTimeCallContext, expr syntax.SyntaxValue, foldCase bool) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	// SyntaxForEach rather than a hand-rolled cdr descent. The callback IS the
	// per-iteration closure the two defers below need, so the immediately-invoked
	// function this replaces is gone rather than merely renamed; and the improper
	// tail arrives as a RETURN VALUE, so the "not a list" refusal is a test on it
	// instead of a branch buried in the descent. The loop guard that went with
	// that descent was dead either way — (*SyntaxPair).IsEmptyList() returns false
	// unconditionally, so termination was always the cdr break.
	tail, err := rest.SyntaxForEach(ctctx.Context(), func(_ context.Context, _ int, _ bool, car syntax.SyntaxValue) error {
		// Get the file name
		fn, ok := car.Unwrap().(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a string but got a %T", car)
		}

		// Find and open the file
		file, filePath, err := findFile(p, ctctx, fn.Value)
		if err != nil {
			return werr.WrapForeignErrorf(err, "include")
		}
		defer file.Close() //nolint:errcheck

		// Push to stack after successful open, pop on exit, so a nested relative
		// path inside the included file resolves against ITS directory rather
		// than the includer's. resolver.SelectLoadStack returns the same object
		// the FS/OS resolvers read from, which is what keeps the push target and
		// the read target identical.
		//
		// A top-level (include …) outside any load carries no stack, so this
		// creates one and installs it on the ctx used for the rest of THIS file
		// — the same get-or-create PrimLoad performs. Without it a chain of
		// includes could not resolve past its first hop: the include compiler
		// would push nowhere and the resolver would read nothing. `inner` is a
		// copy, so the derived ctx does not escape this file's extent.
		inner := ctctx
		stack := resolver.SelectLoadStack(inner.ctx)
		if stack == nil {
			stack = sourceload.NewLoadStack()
			inner.ctx = sourceload.WithLoadStack(inner.ctx, stack)
		}
		stack.Push(filePath)
		defer stack.Pop()

		// Create parser for the file
		reader := bufio.NewReader(file)
		fileParser := parser.NewParserWithFile(p.env, true, reader, filePath)
		// include-ci reads the file case-insensitively (R7RS §2.1 fold-case),
		// folding identifiers to lowercase as they are read.
		fileParser.SetFoldCase(foldCase)

		// Read all forms from the file first, then process them with letrec* semantics
		var forms []syntax.SyntaxValue
		for {
			stx, readErr := fileParser.ReadSyntax(inner.ctx)
			if readErr != nil {
				if errors.Is(readErr, io.EOF) {
					break
				}
				return werr.WrapForeignErrorf(readErr, "include: error reading %q", fn.Value)
			}
			forms = append(forms, stx)
		}

		// Process forms with letrec* semantics: pre-declare all bindings first
		return p.processFormsWithLetrecSemantics(inner, forms, "include "+fn.Value)
	})
	if err != nil {
		return err
	}
	if !syntax.IsSyntaxEmptyList(tail) {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a list, got %T", tail)
	}
	return nil
}

// processFormsWithLetrecSemantics processes a slice of forms with letrec* semantics.
// It pre-declares all define bindings before compiling the forms.
//
// For define-syntax forms, the transformer is compiled immediately during expansion
// so that subsequent forms can use the macro during their expansion.
// R7RS §5.3: Internal define-syntax forms must be processed before expanding
// subsequent body expressions.
//
// errContext identifies the call site for error messages (e.g. "include", "library").
//
// When compiling inside a library, forms is rewritten IN PLACE to stamp each form
// with the library scope; pass a slice you own.
func (p *CompileTimeContinuation) processFormsWithLetrecSemantics(ctctx CompileTimeCallContext, forms []syntax.SyntaxValue, errContext string) error {
	// Flatt §3.3: when including inside a library, stamp all forms with the
	// library scope so that bindings and references carry the same scope set
	// as forms in the library's (begin ...) body. Without this, included
	// bindings have empty scopes while (begin ...) bindings carry the library
	// scope, violating the invariant that include is textual insertion.
	if p.libraryScope != nil {
		for i, form := range forms {
			forms[i] = syntax.AddScopeToSyntax(form, p.libraryScope)
		}
	}

	// Pass 1: Expand all forms, compiling define-syntax as encountered
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env, p.evaluator)
	expander.libraryScope = p.libraryScope
	expandedForms, err := expander.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: error expanding forms", errContext)
	}

	// Pre-declare all define bindings for letrec* semantics
	for _, expanded := range expandedForms {
		p.predeclareDefineBinding(expanded)
	}

	// Pass 2: Compile all forms (define-syntax already compiled, will be skipped).
	// Only the last form inherits the caller's tail position; all others are NotInTail.
	for i, expanded := range expandedForms {
		exprCtx := ctctx.NotInTail()
		if i == len(expandedForms)-1 {
			exprCtx = ctctx
		}
		compileErr := p.CompileExpression(exprCtx, expanded)
		if compileErr != nil {
			return werr.WrapForeignErrorf(compileErr, "%s: error compiling form", errContext)
		}
	}

	return nil
}

// predeclareDefineBinding pre-creates a binding for a define form.
// This enables forward references within library bodies and included files.
// See letrec_semantics.go for the shared pattern documentation.
func (p *CompileTimeContinuation) predeclareDefineBinding(v syntax.SyntaxValue) {
	nameSym := extractDefineName(v)
	if nameSym == nil {
		return
	}
	name := nameSym.Unwrap().(*values.Symbol)
	predeclareBinding(p.env, name, nameSym.Scopes(), nameSym.SourceContext())
}

// CompileIncludeCi compiles an include-ci expression (R7RS §4.1.7 / §5.6).
// It is identical to include except that each included file is read with case
// folding enabled, as if the file began with a #!fold-case directive.
func (p *CompileTimeContinuation) CompileIncludeCi(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.compileIncludeImpl(ctctx, expr, true)
}
