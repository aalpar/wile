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
	"errors"
	"io"
	"io/fs"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine/compilation/resolver"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

const (
	// SchemeIncludePathEnv is the environment variable name for the Scheme include path
	SchemeIncludePathEnv = resolver.SchemeIncludePathEnv
)

func findFile(p *CompileTimeContinuation, ctctx CompileTimeCallContext, path string) (fs.File, string, error) {
	return p.fileResolver.ResolveAndOpen(ctctx.Context(), path)
}

// CompileInclude compiles an include expression.
// It reads and compiles all forms from the specified files in order.
// Each form is expanded and compiled in the current environment.
func (p *CompileTimeContinuation) CompileInclude(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.compileIncludeImpl(ctctx, expr, false)
}

// compileIncludeImpl is the shared implementation for include and include-ci.
// It uses letrec* semantics so that forward references work within included files.
//
// R7RS §5.3.2: Internal definitions use letrec* semantics where all defined
// variables are in scope at the start of the body.
func (p *CompileTimeContinuation) compileIncludeImpl(ctctx CompileTimeCallContext, expr syntax.SyntaxValue, _ bool) error {
	rest, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	for !syntax.IsSyntaxEmptyList(rest) {
		// Get the file name
		car := rest.SyntaxCar()
		next := car
		fn, ok := next.Unwrap().(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a string but got a %T", next)
		}

		// Process file in closure to ensure defer runs after each iteration
		err := func() error {
			// Find and open the file
			file, filePath, err := findFile(p, ctctx, fn.Value)
			if err != nil {
				return werr.WrapForeignErrorf(err, "include")
			}
			defer file.Close() //nolint:errcheck

			// Push to stack after successful open, pop on exit.
			stack := p.env.LoadPathStack()
			if stack != nil {
				pushErr := stack.Push(filePath)
				if pushErr != nil {
					return werr.WrapForeignErrorf(pushErr, "include: push load path for %q", fn.Value)
				}
				defer stack.Pop()
			}

			// Create parser for the file
			reader := bufio.NewReader(file)
			fileParser := parser.NewParserWithFile(p.env, true, reader, filePath)

			// Read all forms from the file first, then process them with letrec* semantics
			var forms []syntax.SyntaxValue
			for {
				stx, readErr := fileParser.ReadSyntax(ctctx.ctx)
				if readErr != nil {
					if errors.Is(readErr, io.EOF) {
						break
					}
					return werr.WrapForeignErrorf(readErr, "include: error reading %q", fn.Value)
				}
				forms = append(forms, stx)
			}

			// Process forms with letrec* semantics: pre-declare all bindings first
			err = p.processFormsWithLetrecSemantics(ctctx, forms, "include "+fn.Value)
			if err != nil {
				return err
			}

			return nil
		}()
		if err != nil {
			return err
		}

		// Move to next filename
		cdr := rest.SyntaxCdr()
		nextPair, cdrOk := cdr.(*syntax.SyntaxPair)
		if !cdrOk {
			if syntax.IsSyntaxEmptyList(cdr) {
				break
			}
			return werr.WrapForeignErrorf(werr.ErrNotAPair, "include: expected a list, got %T", cdr)
		}
		rest = nextPair
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

// CompileIncludeCi compiles an include-ci expression.
func (p *CompileTimeContinuation) CompileIncludeCi(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return werr.WrapForeignErrorf(werr.ErrInvalidSyntax,
		"include-ci: case-insensitive includes not yet supported")
}
