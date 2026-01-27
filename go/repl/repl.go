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

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/syntax"

	"github.com/ergochat/readline"
)

// REPL is an interactive Read-Eval-Print Loop.
type REPL struct {
	env         *environment.EnvironmentFrame
	debugCtx    *DebugContext
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
	return r
}

// Run starts the REPL with readline support, falling back to simple mode if needed.
func (r *REPL) Run(ctx context.Context) error {
	rl, err := readline.NewFromConfig(&readline.Config{
		Prompt:          r.prompt,
		InterruptPrompt: "^C",
		EOFPrompt:       "",
		HistoryFile:     r.historyFile,
		HistoryLimit:    1000,
	})
	if err != nil {
		// Fall back to simple REPL if readline fails
		return r.RunSimple(ctx)
	}
	defer rl.Close() //nolint:errcheck

	// Set up break callback
	r.debugCtx.Debugger().OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
		r.debugCtx.SetCurrentMC(mc)
		if bp != nil {
			fmt.Fprintf(r.out, "\nBreakpoint %d hit", bp.ID)
			source := mc.CurrentSource()
			if source != nil {
				fmt.Fprintf(r.out, " at %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Fprintln(r.out)
		} else {
			fmt.Fprint(r.out, "\nStepped")
			source := mc.CurrentSource()
			if source != nil {
				fmt.Fprintf(r.out, " to %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Fprintln(r.out)
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
			if err == readline.ErrInterrupt {
				// Ctrl-C: clear current input and continue
				inputBuffer.Reset()
				rl.SetPrompt(r.prompt)
				continue
			}
			if err == io.EOF {
				// Ctrl-D: exit
				fmt.Fprintln(r.out)
				return nil
			}
			// Other error
			fmt.Fprintf(r.errOut, "Error reading input: %v\n", err)
			continue
		}

		// Check for debug commands before parsing as Scheme
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, ",") && inputBuffer.Len() == 0 {
			r.debugCtx.HandleDebugCommand(trimmed, r.out)
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
		p := parser.NewParser(r.env, true, rdr)

		stx, parseErr := p.ReadSyntax(ctx)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				// Incomplete expression - prompt for more input
				rl.SetPrompt(r.contPrompt)
				continue
			}
			// Parse error - display and reset
			fmt.Fprintf(r.errOut, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			rl.SetPrompt(r.prompt)
			continue
		}

		// Successfully parsed - evaluate
		inputBuffer.Reset()
		rl.SetPrompt(r.prompt)

		// Compile
		tpl, compileErr := r.compile(stx)
		if compileErr != nil {
			fmt.Fprintf(r.errOut, "Exception: %v\n", compileErr)
			continue
		}

		// Run with debugger
		mv, runErr := r.runWithDebugger(ctx, tpl)
		if runErr != nil {
			fmt.Fprintf(r.errOut, "Exception: %v\n", runErr)
			continue
		}

		// Print result (unless void)
		if !mv.IsVoid() {
			fmt.Fprintln(r.out, mv.SchemeString())
		}
	}
}

// RunSimple runs a basic REPL without readline support.
func (r *REPL) RunSimple(ctx context.Context) error {
	fmt.Fprint(r.out, r.prompt)
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
			if err == io.EOF {
				fmt.Fprintln(r.out)
				return nil
			}
			fmt.Fprintf(r.errOut, "Error reading input: %v\n", err)
			continue
		}

		inputBuffer.WriteString(line)
		inputBuffer.WriteString("\n")
		input := inputBuffer.String()
		rdr := strings.NewReader(input)
		p := parser.NewParser(r.env, true, rdr)

		stx, parseErr := p.ReadSyntax(ctx)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				fmt.Fprint(r.out, r.contPrompt)
				continue
			}
			fmt.Fprintf(r.errOut, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			fmt.Fprint(r.out, r.prompt)
			continue
		}

		inputBuffer.Reset()

		tpl, compileErr := r.compile(stx)
		if compileErr != nil {
			fmt.Fprintf(r.errOut, "Exception: %v\n", compileErr)
			fmt.Fprint(r.out, r.prompt)
			continue
		}

		mv, runErr := r.run(ctx, tpl)
		if runErr != nil {
			fmt.Fprintf(r.errOut, "Exception: %v\n", runErr)
			fmt.Fprint(r.out, r.prompt)
			continue
		}

		if !mv.IsVoid() {
			fmt.Fprintf(r.out, "%s\n", mv.SchemeString())
		}
		fmt.Fprint(r.out, r.prompt)
	}
}

// Debugger returns the REPL's debugger for external configuration.
func (r *REPL) Debugger() *machine.Debugger {
	return r.debugCtx.Debugger()
}

func (r *REPL) compile(expr syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	ectx := machine.NewExpandTimeCallContext()
	stx1, err := machine.NewExpanderTimeContinuation(r.env).ExpandExpression(ectx, expr)
	if err != nil {
		return nil, fmt.Errorf("expansion error: %w", err)
	}

	// Use inTail=false for top-level expressions
	cctx := machine.NewCompileTimeCallContext(false, true, r.env)
	err = machine.NewCompiletimeContinuation(tpl, r.env).CompileExpression(cctx, stx1)
	if err != nil {
		return nil, fmt.Errorf("compilation error: %w", err)
	}
	return tpl, nil
}

func (r *REPL) run(ctx context.Context, tpl *machine.NativeTemplate) (machine.MultipleValues, error) {
	return r.runWithDebugger(ctx, tpl)
}

func (r *REPL) runWithDebugger(ctx context.Context, tpl *machine.NativeTemplate) (machine.MultipleValues, error) {
	cont := machine.NewMachineContinuation(nil, tpl, r.env)
	mc := machine.NewMachineContext(ctx, cont)
	mc.SetDebugger(r.debugCtx.Debugger())
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

func (lr *lineReader) ReadLine() (string, error) {
	buf := make([]byte, 1)
	for {
		n, err := lr.f.Read(buf)
		if err != nil {
			return "", err
		}
		if n == 0 {
			continue
		}
		if buf[0] == '\n' {
			line := lr.r.String()
			lr.r.Reset()
			return line, nil
		}
		lr.r.WriteByte(buf[0])
	}
}
