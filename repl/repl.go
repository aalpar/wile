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

//nolint:errcheck // REPL output doesn't need error handling
package repl

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	wile "github.com/aalpar/wile"
	"github.com/aalpar/wile/machine"

	"github.com/ergochat/readline"
)

// REPL is an interactive Read-Eval-Print Loop.
type REPL struct {
	eng         *wile.Engine
	debugCtx    *DebugContext
	metaHandler *MetaCommandHandler
	completer   *Completer
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

// WithDebugContext sets an externally-created debug context.
func WithDebugContext(dc *DebugContext) Option {
	return func(r *REPL) {
		r.debugCtx = dc
	}
}

// WithCompleter sets an externally-created completer.
func WithCompleter(c *Completer) Option {
	return func(r *REPL) {
		r.completer = c
	}
}

// New creates a new REPL with the given engine and options.
func New(eng *wile.Engine, opts ...Option) *REPL {
	r := &REPL{
		eng:         eng,
		historyFile: defaultHistoryFile(),
		prompt:      "> ",
		contPrompt:  "  ",
		out:         os.Stdout,
		errOut:      os.Stderr,
	}
	for _, opt := range opts {
		opt(r)
	}
	if r.debugCtx == nil {
		r.debugCtx = NewDebugContext()
	}
	if r.metaHandler == nil {
		var metaOpts []MetaOption
		if r.docProvider != nil {
			metaOpts = append(metaOpts, WithMetaDocProvider(r.docProvider))
		}
		r.metaHandler = NewMetaCommandHandler(eng, metaOpts...)
		r.metaHandler.SetDebugContext(r.debugCtx)
	}
	if r.completer == nil {
		r.completer = NewCompleter(eng, r.metaHandler.Commands())
	}
	return r
}

// Run starts the REPL with readline support, falling back to simple mode if needed.
func (p *REPL) Run(ctx context.Context) error {
	// Attach debugger to engine so Engine.Run picks it up.
	p.eng.SetDebugger(p.debugCtx.Debugger())

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

		expr, parseErr := p.eng.ReadExpression(ctx, rdr)
		if parseErr != nil {
			if wile.IsIncompleteInput(parseErr) {
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
		cc, compileErr := p.eng.Compile(ctx, expr)
		if compileErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", compileErr)
			continue
		}

		// Run
		val, runErr := p.eng.Run(ctx, cc)
		if runErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
			continue
		}

		// Print result (unless void)
		if !val.IsVoid() {
			fmt.Fprintln(p.out, val.SchemeString())
		}
	}
}

// RunSimple runs a basic REPL without readline support.
func (p *REPL) RunSimple(ctx context.Context) error {
	// Attach debugger to engine so Engine.Run picks it up.
	p.eng.SetDebugger(p.debugCtx.Debugger())

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

		expr, parseErr := p.eng.ReadExpression(ctx, rdr)
		if parseErr != nil {
			if wile.IsIncompleteInput(parseErr) {
				fmt.Fprint(p.out, p.contPrompt)
				continue
			}
			fmt.Fprintf(p.errOut, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		inputBuffer.Reset()

		cc, compileErr := p.eng.Compile(ctx, expr)
		if compileErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", compileErr)
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		val, runErr := p.eng.Run(ctx, cc)
		if runErr != nil {
			fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
			fmt.Fprint(p.out, p.prompt)
			continue
		}

		if !val.IsVoid() {
			fmt.Fprintf(p.out, "%s\n", val.SchemeString())
		}
		fmt.Fprint(p.out, p.prompt)
	}
}

// Debugger returns the REPL's debugger for external configuration.
func (p *REPL) Debugger() *machine.Debugger {
	return p.debugCtx.Debugger()
}

// defaultHistoryFile returns the default history file path.
func defaultHistoryFile() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return home + "/.wile_history"
}

// lineReader wraps a reader for line-by-line reading.
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
