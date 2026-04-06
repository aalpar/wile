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

// Package repl provides an interactive Read-Eval-Print Loop for Scheme.
//
//nolint:errcheck // REPL output doesn't need error handling
package repl

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/werr"

	"github.com/ergochat/readline"
)

// REPL is an interactive Read-Eval-Print Loop.
type REPL struct {
	env         *environment.EnvironmentFrame
	debugCtx    *DebugContext
	metaHandler *MetaCommandHandler
	completer   *SchemeCompleter
	docProvider DocProvider
	historyFile string
	prompt      string
	contPrompt  string
	out         io.Writer
	errOut      io.Writer
}

// Option configures a REPL.
type Option func(*REPL)

// WithHistoryFile sets the history file path.
func WithHistoryFile(path string) Option {
	return func(r *REPL) {
		r.historyFile = path
	}
}

// WithPrompt sets the primary prompt.
func WithPrompt(prompt string) Option {
	return func(r *REPL) {
		r.prompt = prompt
	}
}

// WithContinuationPrompt sets the continuation prompt for multi-line input.
func WithContinuationPrompt(prompt string) Option {
	return func(r *REPL) {
		r.contPrompt = prompt
	}
}

// WithOutput sets the output writer.
func WithOutput(w io.Writer) Option {
	return func(r *REPL) {
		r.out = w
	}
}

// WithErrorOutput sets the error output writer.
func WithErrorOutput(w io.Writer) Option {
	return func(r *REPL) {
		r.errOut = w
	}
}

// WithDocProvider sets the documentation provider for the ,doc command.
func WithDocProvider(dp DocProvider) Option {
	return func(r *REPL) {
		r.docProvider = dp
	}
}

// New creates a new REPL with the given environment and options.
func New(env *environment.EnvironmentFrame, opts ...Option) *REPL {
	r := &REPL{
		env:         env,
		debugCtx:    NewDebugContext(),
		historyFile: defaultHistoryFile(),
		prompt:      "> ",
		contPrompt:  "  ",
		out:         os.Stdout,
		errOut:      os.Stderr,
	}
	for _, opt := range opts {
		opt(r)
	}
	r.metaHandler = NewMetaCommandHandler(r.env, r.debugCtx, r.docProvider)
	r.completer = NewSchemeCompleter(r.env, r.metaHandler.Commands())
	return r
}

// Run starts the REPL with readline support, falling back to simple mode if needed.
func (p *REPL) Run(ctx context.Context) error {
	rl, err := readline.NewFromConfig(&readline.Config{
		Prompt:          p.prompt,
		InterruptPrompt: "^C",
		EOFPrompt:       "",
		HistoryFile:     p.historyFile,
		HistoryLimit:    1000,
		AutoComplete:    p.completer,
	})
	if err != nil {
		fmt.Fprintf(p.errOut, "Warning: readline initialization failed (%v), using simple REPL\n", err)
		return p.RunSimple(ctx)
	}
	defer rl.Close() //nolint:errcheck

	// Set up break callback
	p.debugCtx.Debugger().OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
		p.debugCtx.SetCurrentMC(mc)
		if bp != nil {
			fmt.Fprintf(p.out, "\nBreakpoint %d hit", bp.ID)
			source := mc.CurrentSource()
			if source != nil {
				fmt.Fprintf(p.out, " at %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Fprintln(p.out)
		} else {
			fmt.Fprint(p.out, "\nStepped")
			source := mc.CurrentSource()
			if source != nil {
				fmt.Fprintf(p.out, " to %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Fprintln(p.out)
		}
	})

	var inputBuffer strings.Builder

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}

		line, err := rl.ReadLine()
		if err != nil {
			if errors.Is(err, readline.ErrInterrupt) {
				// Ctrl-C: clear current input and continue
				inputBuffer.Reset()
				rl.SetPrompt(p.prompt)
				continue
			}
			if errors.Is(err, io.EOF) {
				// Ctrl-D: exit
				fmt.Fprintln(p.out)
				return nil
			}
			// Other error
			fmt.Fprintf(p.errOut, "Error reading input: %v\n", err)
			continue
		}

		// Check for meta-commands before parsing as Scheme
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, ",") && inputBuffer.Len() == 0 {
			p.metaHandler.Handle(trimmed, p.out)
			continue
		}

		// Accumulate input
		if inputBuffer.Len() > 0 {
			inputBuffer.WriteString("\n")
		}
		inputBuffer.WriteString(line)

		// Try to parse the accumulated input
		input := inputBuffer.String()
		rdr := strings.NewReader(input)
		parser := parser.NewParser(p.env, true, rdr)

		stx, parseErr := parser.ReadSyntax(ctx)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				// Incomplete expression - prompt for more input
				rl.SetPrompt(p.contPrompt)
				continue
			}
			// Parse error - display and reset
			fmt.Fprintf(p.errOut, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			rl.SetPrompt(p.prompt)
			continue
		}

		// Successfully parsed - evaluate
		inputBuffer.Reset()
		rl.SetPrompt(p.prompt)

		// Compile
		tpl, compileErr := compile(ctx, p.env, stx)
		if compileErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", compileErr)
			continue
		}

		// Run with debugger
		mv, runErr := p.runWithDebugger(ctx, tpl)
		if runErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
			continue
		}

		// Print result (unless void)
		if !mv.IsVoid() {
			fmt.Fprintln(p.out, mv.SchemeString())
		}
	}
}

// RunSimple runs a basic REPL without readline support.
func (p *REPL) RunSimple(ctx context.Context) error {
	fmt.Fprint(p.out, p.prompt)
	reader := newLineReader(os.Stdin)
	var inputBuffer strings.Builder

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
		}

		line, err := reader.ReadLine()
		if err != nil {
			if errors.Is(err, io.EOF) {
				fmt.Fprintln(p.out)
				return nil
			}
			fmt.Fprintf(p.errOut, "Error reading input: %v\n", err)
			continue
		}

		inputBuffer.WriteString(line)
		inputBuffer.WriteString("\n")
		input := inputBuffer.String()
		rdr := strings.NewReader(input)
		parser := parser.NewParser(p.env, true, rdr)

		stx, parseErr := parser.ReadSyntax(ctx)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				fmt.Fprint(p.out, p.contPrompt)
				continue
			}
			fmt.Fprintf(p.errOut, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		inputBuffer.Reset()

		tpl, compileErr := compile(ctx, p.env, stx)
		if compileErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", compileErr)
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		mv, runErr := run(ctx, tpl, p.env)
		if runErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		if !mv.IsVoid() {
			fmt.Fprintf(p.out, "%s\n", mv.SchemeString())
		}
		fmt.Fprint(p.out, p.prompt)
	}
}

// Debugger returns the REPL's debugger for external configuration.
func (p *REPL) Debugger() *machine.Debugger {
	return p.debugCtx.Debugger()
}

func (p *REPL) runWithDebugger(ctx context.Context, tpl *machine.NativeTemplate) (machine.MultipleValues, error) {
	cont := machine.NewMachineContinuation(nil, tpl, p.env)
	mc := machine.NewMachineContext(ctx, cont)
	mc.SetDebugger(p.debugCtx.Debugger())
	err := mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}
	return mc.GetValues(), nil
}

// compile expands and compiles a syntax expression into an executable template.
func compile(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	expanded, err := compilation.NewExpanderTimeContinuation(ctx, env, machine.NewVMMacroEvaluator()).ExpandExpression(expr)
	if err != nil {
		return nil, werr.WrapForeignErrorWithCause(werr.ErrExpansion, err, "expansion error")
	}

	cctx := compilation.NewCompileTimeCallContext(ctx, false)
	err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, werr.WrapForeignErrorWithCause(werr.ErrCompilation, err, "compilation error")
	}

	return tpl, nil
}

// run executes a compiled template and returns the result values.
func run(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame) (machine.MultipleValues, error) {
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(ctx, cont)
	err := mc.RunWithEscapeHandling()
	if err != nil {
		return nil, err
	}
	return mc.GetValues(), nil
}

// isIncompleteInput checks if the parse error indicates incomplete input.
func isIncompleteInput(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, io.EOF) {
		return true
	}
	errStr := err.Error()
	return strings.Contains(errStr, "unexpected EOF") ||
		strings.Contains(errStr, "unterminated") ||
		strings.Contains(errStr, "unclosed")
}

// defaultHistoryFile returns the default history file path.
func defaultHistoryFile() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return home + "/.wile_history"
}

// lineReader wraps a bufio.Reader for line-by-line reading.
type lineReader struct {
	r *strings.Builder
	f io.Reader
}

func newLineReader(r io.Reader) *lineReader {
	return &lineReader{
		r: &strings.Builder{},
		f: r,
	}
}

func (p *lineReader) ReadLine() (string, error) {
	buf := make([]byte, 1)
	for {
		n, err := p.f.Read(buf)
		if err != nil {
			return "", err
		}
		if n == 0 {
			continue
		}
		if buf[0] == '\n' {
			line := p.r.String()
			p.r.Reset()
			return line, nil
		}
		p.r.WriteByte(buf[0])
	}
}
