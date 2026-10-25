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
	"path"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

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
//
// It is a free function so the expander can install the same authorizer without
// a CompileTimeContinuation.
func includeAuthorizerContext(ctx context.Context, env *environment.EnvironmentFrame) context.Context {
	if security.FromContext(ctx) != nil {
		return ctx
	}
	if env == nil {
		return ctx
	}
	ns := env.Namespace()
	if ns == nil {
		return ctx
	}
	auth := ns.EffectiveAuthorizer()
	if auth == nil {
		return ctx
	}
	return security.WithAuthorizer(ctx, auth)
}

// lexicalIncludeContext returns ctx with a load stack whose top is the file that
// holds the include form, so a relative path resolves against THAT file's
// directory (the Racket model) rather than against whatever file the load stack
// happens to hold when the form is finally compiled. The two differ for a form
// the splice does not expand in place, such as the body of an eval-when: the
// include's own expansion has already popped the file by then. (cond-expand
// splices at expand time since P5.8 and no longer needs this.)
//
// It returns ctx unchanged when
//   - the form has no source file (REPL, eval, string input), or the file is a
//     bare name with no directory: the load stack (or, with none, the search
//     paths) is the better authority there, and it is what a top-level include
//     has always had;
//   - ctx already carries a non-empty load stack and the file is a RELATIVE
//     path. A relative SourceContext.File is often a caller's diagnostic label
//     (EvalProgram, EvalMultipleWithSource, CheckProgram, ParseWithSource), not a
//     location on disk; an absolute one names a real file. A live stack is the
//     authority an embedder set (Engine.ContextWithLoadPath) or a load chain
//     built, and a label must not override it.
//
// KNOWN LIMITATION: with a virtual-FS resolver (relative paths are real there),
// a live stack AND a form compiled after its file was popped, the stack rule
// still applies and a relative include nested under eval-when resolves
// against the stack's top.
//
// The derived stack is a copy, so no push escapes into ctx.
func lexicalIncludeContext(ctx context.Context, src *syntax.SourceContext) context.Context {
	if src == nil || src.File == "" {
		return ctx
	}
	dir := path.Dir(src.File)
	if dir == "." || dir == "" {
		return ctx
	}
	existing := resolver.SelectLoadStack(ctx)
	if existing != nil && existing.Depth() > 0 && !filepath.IsAbs(src.File) {
		return ctx
	}
	stack := sourceload.NewLoadStack()
	if existing != nil {
		for _, pth := range existing.Paths() {
			stack.Push(pth)
		}
	}
	stack.Push(src.File)
	return sourceload.WithLoadStack(ctx, stack)
}

// readIncludedForms resolves name through r under the compiling namespace's
// authorizer, reads every form of the file, and returns them with the resolved
// path. The file is closed before it returns. foldCase reads the file as if it
// began with #!fold-case (include-ci, R7RS §2.1).
//
// It neither pushes the load stack nor processes the forms: the caller owns
// both, because the push must follow the open (a nested relative path resolves
// against the included file) and must outlive the processing.
//
// src is the source context of the include form (or of its filename operand);
// name resolves against the directory of src's file when it has one
// (lexicalIncludeContext).
func readIncludedForms(ctx context.Context, env *environment.EnvironmentFrame, r FileResolver, name string, src *syntax.SourceContext, foldCase bool) ([]syntax.SyntaxValue, string, error) {
	file, filePath, err := r.ResolveAndOpen(includeAuthorizerContext(lexicalIncludeContext(ctx, src), env), name)
	if err != nil {
		return nil, "", werr.WrapForeignErrorf(err, "include")
	}
	defer file.Close() //nolint:errcheck

	fileParser := parser.NewParserWithFile(env, true, bufio.NewReader(file), filePath)
	// include-ci reads the file case-insensitively (R7RS §2.1 fold-case),
	// folding identifiers to lowercase as they are read.
	fileParser.SetFoldCase(foldCase)

	var forms []syntax.SyntaxValue
	for {
		stx, readErr := fileParser.ReadSyntax(ctx)
		if readErr != nil {
			if errors.Is(readErr, io.EOF) {
				break
			}
			return nil, "", werr.WrapForeignErrorf(readErr, "include: error reading %q", name)
		}
		forms = append(forms, stx)
	}
	return forms, filePath, nil
}

// includeEntry is one file on the include chain: the files being included on
// one causal chain, carried on ctx (ctxChain) so a sibling include, a finished
// include and a concurrent SRFI-18 thread never observe each other's entries.
// The load stack cannot answer the same question: a thread inherits its
// starter's *LoadStack (the CLI installs one for -f), so two threads including
// the same file would each find the other's push.
type includeEntry struct {
	path string
	// guard and macroInvocations are the expander state the file's forms are
	// expanded under: the run's depth guard and its macroInvocations count at
	// entry.
	// guard is nil for a file the compiler includes (a declaration-level
	// include, an include-library-declarations file), whose forms a fresh
	// expander takes from count zero.
	guard            *expandDepthGuard
	macroInvocations int
	// literal reports that the include that entered this file was written in
	// the file before it on the chain, not produced, passed through or
	// expanded by a macro.
	literal bool
}

// includeChainKey is the context key for the include chain.
type includeChainKey struct{}

// innermostInclude returns the innermost entry of ctx's include chain.
func innermostInclude(ctx context.Context) (includeEntry, bool) {
	node, _ := ctx.Value(includeChainKey{}).(*ctxChain[includeEntry])
	if node == nil {
		return includeEntry{}, false
	}
	return node.v, true
}

// literalIncludeAt reports whether an include the expander meets under guard
// was written in the innermost file of ctx's include chain: no macro invocation
// has begun since that file was entered, so the include is neither a macro's
// template, nor an argument a macro passed through, nor one a transformer
// expanded with expand (any of which could stop).
// With no file on the chain there is nothing to cycle through, and the answer
// does not matter.
func literalIncludeAt(ctx context.Context, guard *expandDepthGuard) bool {
	in, ok := innermostInclude(ctx)
	if !ok {
		return true
	}
	if in.guard == nil {
		return guard.macroInvocations == 0
	}
	return in.guard == guard && guard.macroInvocations == in.macroInvocations
}

// enterIncludedFile returns ctx recording entry, the file the including form
// (named by form) has just read, on the include chain, or refuses with
// ErrIncludeCycle when the include can never stop: the file is already on the
// chain, and it and every include after it, this one included, is literal.
// Such a file re-reads into the same includes forever. A cycle through a
// macro-produced include is not refused, since the macro can stop producing it
// (a phase-1 counter does, as in Racket and Chez); the expansion depth guard
// bounds one that does not (includeExpandFailure). The error names the files of
// the cycle, outermost first.
//
// Paths compare cleaned; resolver output already is.
func enterIncludedFile(ctx context.Context, form string, entry includeEntry) (context.Context, error) {
	entry.path = filepath.Clean(entry.path)
	if !entry.literal {
		return pushCtxChain(ctx, includeChainKey{}, entry), nil
	}
	entries := ctxChainValues[includeEntry](ctx, includeChainKey{})
	// The literal run: the longest suffix whose entries were entered literally,
	// plus the entry before it, whose own entry does not matter.
	start := len(entries) - 1
	for start > 0 && entries[start].literal {
		start--
	}
	for i := max(start, 0); i < len(entries); i++ {
		if entries[i].path != entry.path {
			continue
		}
		var chain []string
		for _, e := range entries[i:] {
			chain = append(chain, e.path)
		}
		chain = append(chain, entry.path)
		return nil, werr.WrapForeignErrorf(werr.ErrIncludeCycle, "%s: %s", form, strings.Join(chain, " -> "))
	}
	return pushCtxChain(ctx, includeChainKey{}, entry), nil
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
	form := "include"
	if foldCase {
		form = "include-ci"
	}
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	// SyntaxForEach rather than a hand-rolled cdr descent. The callback IS the
	// per-iteration closure the defer below needs, so the immediately-invoked
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

		// Find, open and read the whole file. The stack push below comes after
		// this, so the file resolves against the includer's directory.
		forms, filePath, err := readIncludedForms(ctctx.Context(), p.env, p.fileResolver, fn.Value, expr.SourceContext(), foldCase)
		if err != nil {
			return err
		}

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
		// The compiler meets a literal include only below a file it entered
		// itself: a library declaration is never a macro's output.
		in, nested := innermostInclude(inner.ctx)
		inner.ctx, err = enterIncludedFile(inner.ctx, form, includeEntry{path: filePath, literal: !nested || in.guard == nil})
		if err != nil {
			return err
		}
		stack := resolver.SelectLoadStack(inner.ctx)
		if stack == nil {
			stack = sourceload.NewLoadStack()
			inner.ctx = sourceload.WithLoadStack(inner.ctx, stack)
		}
		stack.Push(filePath)
		defer stack.Pop()

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
	nameSym := extractDefineName(p.env, nil, v)
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
