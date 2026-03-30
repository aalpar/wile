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

// Package runtime provides the core API for embedding Wile Scheme in Go applications.
//
// This package exposes the essential functions for compiling and executing Scheme code:
//
//   - [Compile] transforms syntax into executable templates
//   - [Run] executes compiled templates
//   - [Load] reads and evaluates Scheme code from an io.Reader
//
// # Basic Usage
//
// To evaluate Scheme code from a string:
//
//	env, _ := bootstrap.NewNamespaceFrameTiny(ctx)
//	reader := strings.NewReader(`(+ 1 2)`)
//	err := runtime.Load(ctx, env, reader, "example.scm")
//
// # Creating Environments
//
// Use [github.com/aalpar/wile/internal/bootstrap.NewNamespaceFrameTiny]
// to create a top-level environment with all standard bindings.
package runtime

import (
	"bufio"
	"context"
	"errors"
	"io"
	"path/filepath"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/werr"
)

// Compile expands and compiles a syntax expression into an executable template.
//
// The returned [machine.NativeTemplate] can be executed multiple times with [Run].
// This is useful when the same code needs to be evaluated repeatedly.
func Compile(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	expanded, err := machine.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).ExpandExpression(expr)
	if err != nil {
		return nil, werr.WrapForeignErrorWithCause(werr.ErrExpansion, err, "expansion error")
	}

	// Use inTail=false for top-level expressions
	cctx := machine.NewCompileTimeCallContext(ctx, false)
	err = machine.NewCompiletimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, werr.WrapForeignErrorWithCause(werr.ErrCompilation, err, "compilation error")
	}

	return tpl, nil
}

// Run executes a compiled template and returns the result values.
//
// The template is executed in the context of the given environment. Any definitions
// or side effects will modify the environment.
func Run(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame) (machine.MultipleValues, error) {
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(ctx, cont)
	err := mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}
	return mc.GetValues(), nil
}

// Load reads and evaluates Scheme expressions from a reader into the environment.
//
// All expressions are read, wrapped in a (begin ...) form, compiled, and executed.
// This is the primary way to load Scheme libraries or configuration files.
//
// The filename parameter is used for error messages and source location tracking.
// Pass an empty string if the source has no associated filename.
func Load(ctx context.Context, env *environment.EnvironmentFrame, r io.Reader, filename string) error {
	// Push file path onto LoadPathStack so (include ...) can resolve relative paths.
	// Mirrors PrimLoad in internal/extensions/eval/prim_eval.go:111-116.
	if filename != "" {
		absPath, absErr := filepath.Abs(filename)
		if absErr != nil {
			return werr.WrapForeignErrorf(absErr, "load: cannot resolve path %q", filename)
		}
		stack := env.LoadPathStack()
		if stack != nil {
			pushErr := stack.Push(absPath)
			if pushErr != nil {
				return werr.WrapForeignErrorf(pushErr, "load: cannot push load path %q", absPath)
			}
			defer stack.Pop()
		}
	}

	p := parser.NewParserWithFile(env, true, bufio.NewReader(r), filename)

	// Collect all expressions from the reader
	var exprs []syntax.SyntaxValue
	for {
		stx, err := p.ReadSyntax(ctx)
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return werr.WrapForeignErrorf(err, "parse error in %s", filename)
		}
		exprs = append(exprs, stx)
	}

	// Nothing to do if empty
	if len(exprs) == 0 {
		return nil
	}

	// Wrap in (begin ...) if multiple expressions
	var programStx syntax.SyntaxValue
	if len(exprs) == 1 {
		programStx = exprs[0]
	} else {
		sctx := syntax.NewZeroValueSourceContext()
		beginSym := syntax.NewSyntaxSymbol("begin", sctx)
		allExprs := make([]syntax.SyntaxValue, 0, len(exprs)+1)
		allExprs = append(allExprs, beginSym)
		allExprs = append(allExprs, exprs...)
		programStx = syntax.SyntaxList(sctx, allExprs...)
	}

	// Compile and run
	tpl, err := Compile(ctx, env, programStx)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrCompilation, err, "load %s", filename)
	}

	_, err = Run(ctx, tpl, env)
	if err != nil {
		return err
	}

	return nil
}
