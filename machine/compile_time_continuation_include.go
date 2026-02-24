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

package machine

import (
	"bufio"
	"errors"
	"io"
	"io/fs"
	"os"
	"path/filepath"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

const (
	// SchemeIncludePathEnv is the environment variable name for the Scheme include path
	SchemeIncludePathEnv = "SCHEME_INCLUDE_PATH"
)

func findFile(p *CompileTimeContinuation, _ CompileTimeCallContext, path string) (fs.File, string, error) {
	stack := p.env.LoadPathStack()
	includePath := os.Getenv(SchemeIncludePathEnv)
	var fallbackDirs []string
	if includePath != "" {
		fallbackDirs = filepath.SplitList(includePath)
	}

	absPath, err := environment.ResolveFile(stack, path, fallbackDirs)
	if err != nil {
		return nil, "", err
	}

	f, err := os.Open(absPath)
	if err != nil {
		return nil, "", err
	}
	return f, absPath, nil
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
		return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list of filenames, got %T", expr)
	}
	for !syntax.IsSyntaxEmptyList(rest) {
		// Get the file name
		car := rest.SyntaxCar()
		next := car
		fn, ok := next.Unwrap().(*values.String)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a string but got a %T", next)
		}

		// Process file in closure to ensure defer runs after each iteration
		err := func() error {
			// Find and open the file
			file, filePath, err := findFile(p, ctctx, fn.Value)
			if err != nil {
				return values.WrapForeignErrorf(err, "include")
			}
			defer file.Close() //nolint:errcheck

			// Push to stack after successful open, pop on exit
			stack := p.env.LoadPathStack()
			if stack != nil {
				pushErr := stack.Push(filePath)
				if pushErr != nil {
					return pushErr
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
					return values.WrapForeignErrorf(readErr, "include: error reading %q", fn.Value)
				}
				forms = append(forms, stx)
			}

			// Process forms with letrec* semantics: pre-declare all bindings first
			err = p.processFormsWithLetrecSemantics(ctctx, forms, fn.Value)
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
			return values.WrapForeignErrorf(values.ErrNotAPair, "include: expected a list, got %T", cdr)
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
func (p *CompileTimeContinuation) processFormsWithLetrecSemantics(ctctx CompileTimeCallContext, forms []syntax.SyntaxValue, filename string) error {
	// Pass 1: Expand all forms, compiling define-syntax as encountered
	expander := NewExpanderTimeContinuation(ctctx.ctx, p.env)
	expandedForms, err := expander.ExpandBodyWithDefineSyntax(forms)
	if err != nil {
		return values.WrapForeignErrorf(err, "include: error expanding forms from %q", filename)
	}

	// Pre-declare all define bindings for letrec* semantics
	for _, expanded := range expandedForms {
		p.predeclareDefineBinding(expanded)
	}

	// Pass 2: Compile all forms (define-syntax already compiled, will be skipped)
	for _, expanded := range expandedForms {
		compileErr := p.CompileExpression(ctctx, expanded)
		if compileErr != nil {
			return values.WrapForeignErrorf(compileErr, "include: error compiling form from %q", filename)
		}
	}

	return nil
}

// predeclareDefineBinding pre-creates a binding for a define form.
// This enables forward references within library bodies and included files.
func (p *CompileTimeContinuation) predeclareDefineBinding(v syntax.SyntaxValue) {
	pair, ok := v.(*syntax.SyntaxPair)
	if !ok {
		return
	}

	car := pair.SyntaxCar()
	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return
	}

	keyword := sym.Unwrap().(*values.Symbol).Key
	if keyword != "define" {
		return
	}

	// Extract the name being defined
	cdr := pair.SyntaxCdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return
	}

	second := cdrPair.SyntaxCar()
	var nameSym *syntax.SyntaxSymbol

	switch s := second.(type) {
	case *syntax.SyntaxSymbol:
		// (define name expr)
		nameSym = s
	case *syntax.SyntaxPair:
		// (define (name params...) body...)
		nameExpr := s.SyntaxCar()
		ns, ok := nameExpr.(*syntax.SyntaxSymbol)
		if ok {
			nameSym = ns
		}
	}

	if nameSym == nil {
		return
	}

	// Pre-create the binding
	name := p.env.InternSymbol(nameSym.Unwrap().(*values.Symbol))
	symbolScopes := nameSym.Scopes()

	p.bindSymbolWithScopes(name, symbolScopes)
}

// bindSymbolWithScopes creates a binding for the given symbol with the specified scopes.
// If a binding already exists, it updates the scopes if possible. This is used for pre-declaring
// define bindings with the correct scopes for hygiene.
func (p *CompileTimeContinuation) bindSymbolWithScopes(name *values.Symbol, scopes []*syntax.Scope) {
	if p.env.LocalEnvironment() != nil {
		_, _ = p.env.MaybeCreateLocalBindingWithScopes(name, environment.BindingTypeVariable, scopes, nil)
	} else {
		gi, created := p.env.MaybeCreateOwnGlobalBinding(name, environment.BindingTypeVariable)
		if !created {
			binding := p.env.GetGlobalBinding(gi)
			if binding != nil && scopes != nil {
				binding.SetScopes(scopes)
			}
		}
	}
}

// CompileIncludeCi compiles an include-ci expression.
// It reads and compiles all forms from the specified files in order,
// treating symbols as case-insensitive (folded to lowercase).
func (p *CompileTimeContinuation) CompileIncludeCi(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// TODO: implement case-insensitive parsing by creating parser with caseInsensitive=true
	// For now, just use the regular include implementation
	return p.compileIncludeImpl(ctctx, expr, true)
}
