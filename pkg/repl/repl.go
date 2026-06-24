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
	"os/signal"
	"strings"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

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
	version     string
	in          io.Reader
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

// WithInput sets the input reader used by the simple (non-readline) loop.
// Defaults to os.Stdin. The readline loop reads from the terminal directly
// and is unaffected by this option.
func WithInput(in io.Reader) Option {
	return func(r *REPL) {
		r.in = in
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

// WithVersion sets the version string shown by the ,version meta-command.
func WithVersion(version string) Option {
	return func(r *REPL) {
		r.version = version
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
		in:          os.Stdin,
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
		if r.version != "" {
			metaOpts = append(metaOpts, WithMetaVersion(r.version))
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
	if p.eng == nil {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "repl: engine is required")
	}
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

	// Own SIGINT for the duration of the loop. At the idle prompt readline runs
	// the terminal in raw mode (ISIG off), so Ctrl-C there is delivered as
	// readline.ErrInterrupt with no OS signal — this handler only fires for an
	// interrupt that arrives while a form is evaluating, where it cancels just
	// that evaluation (see evalAndPrint) instead of tearing down the REPL.
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)
	defer signal.Stop(sigCh)

	// Set up break callback
	p.debugCtx.Debugger().OnBreak(func(state values.DebugState, bp *wile.BreakpointInfo) {
		p.debugCtx.SetCurrentState(state)
		if bp != nil {
			fmt.Fprintf(p.out, "\nBreakpoint %d hit", bp.ID)
			loc := state.CurrentLocation()
			if loc != nil {
				fmt.Fprintf(p.out, " at %s:%d:%d", loc.File, loc.Line, loc.Column)
			}
			fmt.Fprintln(p.out)
		} else {
			fmt.Fprint(p.out, "\nStepped")
			loc := state.CurrentLocation()
			if loc != nil {
				fmt.Fprintf(p.out, " to %s:%d:%d", loc.File, loc.Line, loc.Column)
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
			p.metaHandler.Handle(ctx, trimmed, p.out)
			continue
		}

		// Accumulate input
		if inputBuffer.Len() > 0 {
			inputBuffer.WriteString("\n")
		}
		inputBuffer.WriteString(line)

		// Parse every complete expression in the accumulated input. A trailing
		// incomplete form keeps the buffer and prompts for continuation; an
		// otherwise-complete line evaluates all of its forms (not just the
		// first), so a pasted multi-form line is never silently truncated.
		exprs, parseErr := p.eng.ReadExpressions(ctx, strings.NewReader(inputBuffer.String()))
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

		// Successfully parsed - evaluate each form in order.
		inputBuffer.Reset()
		rl.SetPrompt(p.prompt)

		for _, expr := range exprs {
			p.evalAndPrint(ctx, sigCh, expr)
		}
	}
}

// RunSimple runs a basic REPL without readline support.
func (p *REPL) RunSimple(ctx context.Context) error {
	if p.eng == nil {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "repl: engine is required")
	}
	// Attach debugger to engine so Engine.Run picks it up.
	p.eng.SetDebugger(p.debugCtx.Debugger())

	// Own SIGINT so a Ctrl-C while a form is evaluating cancels just that
	// evaluation (see evalAndPrint) rather than tearing down the REPL.
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)
	defer signal.Stop(sigCh)

	fmt.Fprint(p.out, p.prompt)
	reader := newLineReader(p.in)
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

		exprs, parseErr := p.eng.ReadExpressions(ctx, strings.NewReader(inputBuffer.String()))
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
		for _, expr := range exprs {
			p.evalAndPrint(ctx, sigCh, expr)
		}
		fmt.Fprint(p.out, p.prompt)
	}
}

// evalAndPrint compiles and runs one parsed expression, printing its non-void
// result. Compile and run errors are reported to errOut but do not abort the
// caller's loop: when several forms arrive on one line each is attempted, so a
// failure in one never silently swallows the forms that follow it.
//
// The form runs under its own cancellable child of ctx. A SIGINT arriving on
// sigCh while the form evaluates cancels only that child — the eval unwinds and
// control returns to the prompt with the loop's ctx untouched, so the REPL
// survives Ctrl-C during a long computation instead of exiting. The loop's ctx
// is only cancelled by genuine process termination (SIGTERM), which the caller
// detects on the next iteration.
func (p *REPL) evalAndPrint(ctx context.Context, sigCh <-chan os.Signal, expr *wile.Expression) {
	// Drop any interrupt buffered between evals so it can't cancel this one
	// before it starts.
	select {
	case <-sigCh:
	default:
	}

	evalCtx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Watch for an interrupt for the lifetime of this eval. close(done) (the
	// later-registered, first-running defer) retires the watcher on the normal
	// path; on interrupt the watcher cancels evalCtx and the eval unwinds.
	done := make(chan struct{})
	go func() {
		select {
		case <-sigCh:
			cancel()
		case <-done:
		}
	}()
	defer close(done)

	cc, compileErr := p.eng.Compile(evalCtx, expr)
	if compileErr != nil {
		fmt.Fprintf(p.errOut, "Exception: %v\n", compileErr)
		return
	}
	val, runErr := p.eng.Run(evalCtx, cc)
	if runErr != nil {
		// An interrupt cancels evalCtx but not ctx; report it as ^C and return
		// to the prompt rather than printing a raw "context canceled". When ctx
		// itself is done (SIGTERM), fall through so the caller's loop exits.
		if errors.Is(runErr, context.Canceled) && ctx.Err() == nil {
			fmt.Fprintln(p.errOut, "^C")
			return
		}
		fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
		return
	}
	if !val.IsVoid() {
		fmt.Fprintf(p.out, "%s\n", val.SchemeString())
	}
}

// Debugger returns the REPL's debugger for external configuration.
func (p *REPL) Debugger() *wile.Debugger {
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
