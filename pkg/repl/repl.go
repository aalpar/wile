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

	// The suspending callback owns the interactive break prompt. The render-only
	// callback stays installed as the fallback for stops that cannot suspend —
	// a breakpoint inside load or eval, whose freshly compiled template runs on
	// a sub-context that does not carry the break boundary.
	p.debugCtx.Debugger().OnBreak(p.renderBreak)
	p.debugCtx.Debugger().OnBreakSuspend(p.breakVerdict(func() (string, error) {
		rl.SetPrompt(debugPrompt)
		line, lerr := rl.ReadLine()
		rl.SetPrompt(p.prompt)
		return line, lerr
	}))

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

		p.evalForms(ctx, exprs)
	}
}

// RunSimple runs a basic REPL without readline support.
func (p *REPL) RunSimple(ctx context.Context) error {
	if p.eng == nil {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "repl: engine is required")
	}
	// Attach debugger to engine so Engine.Run picks it up.
	p.eng.SetDebugger(p.debugCtx.Debugger())

	reader := newLineReader(p.in)
	// The break prompt shares this reader so a suspended evaluation consumes the
	// same input stream, not a second one opened over the same fd.
	p.debugCtx.Debugger().OnBreak(p.renderBreak)
	p.debugCtx.Debugger().OnBreakSuspend(p.breakVerdict(func() (string, error) {
		fmt.Fprint(p.out, debugPrompt)
		return reader.ReadLine()
	}))

	fmt.Fprint(p.out, p.prompt)
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
		p.evalForms(ctx, exprs)
		fmt.Fprint(p.out, p.prompt)
	}
}

// evalForms evaluates one input line's worth of parsed forms in order, owning
// SIGINT only for the duration of evaluation. Scoping the handler here — rather
// than across the whole read-eval loop — keeps Ctrl-C at the *idle* prompt
// behaving normally: the readline loop clears the line via readline.ErrInterrupt
// (raw mode, no OS signal), and the simple loop's cooked-mode terminal retains
// its default SIGINT action (terminate) instead of a suppressed no-op. While a
// form evaluates, the interrupt cancels just that evaluation (see evalAndPrint).
func (p *REPL) evalForms(ctx context.Context, exprs []*wile.Expression) {
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)
	defer signal.Stop(sigCh)

	for _, expr := range exprs {
		p.evalAndPrint(ctx, sigCh, expr)
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

// debugPrompt is the prompt shown while execution is suspended at a break.
const debugPrompt = "(dbg) "

// breakCommandHelp lists what the break prompt accepts. Everything else,
// including a bare Scheme expression, is refused: evaluating user code from a
// suspended VM would re-enter the machine the break is holding still.
const breakCommandHelp = "only ,continue ,step ,next ,finish ,backtrace ,where are available at a break prompt"

// breakVerdict builds the suspension handler for one REPL entry point. readLine
// supplies that entry point's own line source — readline's in Run, the shared
// lineReader in RunSimple — so the break prompt reads from the same input the
// REPL does rather than opening a second one. It also owns showing debugPrompt,
// because readline draws its own prompt and the simple loop does not.
//
// EOF is BreakAbandon: at a break there is no further input, so the only
// remaining choice is to discard the computation (its dynamic-wind after-thunks
// still run) and return to the caller.
func (p *REPL) breakVerdict(readLine func() (string, error)) func(values.DebugState, *wile.BreakpointInfo) wile.BreakAction {
	return func(state values.DebugState, bp *wile.BreakpointInfo) wile.BreakAction {
		p.renderBreak(state, bp)
		for {
			line, err := readLine()
			if err != nil {
				fmt.Fprintln(p.out)
				return wile.BreakAbandon
			}
			token := strings.TrimPrefix(strings.TrimSpace(line), ",")
			name, known := canonicalDebugCommand(token)
			if !known {
				fmt.Fprintln(p.out, breakCommandHelp)
				continue
			}
			switch name {
			case "continue":
				return wile.BreakContinue
			case "step":
				return wile.BreakStep
			case "next":
				return wile.BreakNext
			case "finish":
				return wile.BreakFinish
			case "backtrace":
				// Both read the debugger's snapshot of this break, which is the
				// same state this callback was handed.
				p.debugCtx.cmdBacktrace(nil, p.out)
			case "where":
				p.debugCtx.cmdWhere(nil, p.out)
			default:
				fmt.Fprintln(p.out, breakCommandHelp)
			}
		}
	}
}

// renderBreak prints the one-line banner announcing a stop.
func (p *REPL) renderBreak(state values.DebugState, bp *wile.BreakpointInfo) {
	if bp != nil {
		fmt.Fprintf(p.out, "\nBreakpoint %d hit", bp.ID)
	} else {
		fmt.Fprint(p.out, "\nStepped")
	}
	loc := state.CurrentLocation()
	if loc != nil {
		if bp != nil {
			fmt.Fprintf(p.out, " at %s:%d:%d", loc.File, loc.Line, loc.Column)
		} else {
			fmt.Fprintf(p.out, " to %s:%d:%d", loc.File, loc.Line, loc.Column)
		}
	}
	fmt.Fprintln(p.out)
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
		// Process the returned byte before the error: io.Reader permits a reader
		// to return n > 0 together with io.EOF, and handling err first would drop
		// that final byte and truncate the last line.
		if n > 0 {
			if buf[0] == '\n' {
				line := p.r.String()
				p.r.Reset()
				return line, nil
			}
			p.r.WriteByte(buf[0])
		}
		if err != nil {
			// A final line with no trailing newline is still a line: flush what
			// is buffered before propagating EOF, or the last form is silently
			// swallowed. The next call sees an empty buffer and returns EOF.
			if errors.Is(err, io.EOF) && p.r.Len() > 0 {
				line := p.r.String()
				p.r.Reset()
				return line, nil
			}
			return "", err
		}
	}
}
