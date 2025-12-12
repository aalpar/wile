// Copyright 2025 Aaron Alpar
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

package main

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/runtime"
	"wile/syntax"
	"strings"

	"github.com/ergochat/readline"
	"github.com/jessevdk/go-flags"
)

type Options struct {
	File        string `short:"f" long:"file" description:"Scheme file to run"`
	LibraryPath string `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)"`
	Version     bool   `short:"v" long:"version" description:"Print version information and exit"`
}

var opts Options

const (
	// SchemeLibraryPathEnv is the environment variable for library search paths
	SchemeLibraryPathEnv = "SCHEME_LIBRARY_PATH"
	// Version is the current version of the Scheme interpreter
	Version = "0.1.0"
)

func compile(env *environment.EnvironmentFrame, expr syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)

	ectx := machine.NewExpandTimeCallContext()
	stx1, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, expr)
	if err != nil {
		return nil, fmt.Errorf("expansion error: %w", err)
	}

	// Use inTail=false for top-level expressions. Top-level is NOT tail position
	// because there's no outer function to return to.
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, stx1)
	if err != nil {
		return nil, fmt.Errorf("compilation error: %w", err)
	}
	return tpl, err
}

func run(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame) (machine.MultipleValues, error) {
	return runWithDebugger(ctx, tpl, env, nil)
}

func runWithDebugger(ctx context.Context, tpl *machine.NativeTemplate, env *environment.EnvironmentFrame, debugger *machine.Debugger) (machine.MultipleValues, error) {
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(cont)
	if debugger != nil {
		mc.SetDebugger(debugger)
	}
	err := mc.Run(ctx)
	if err != nil {
		return nil, err
	}
	q := mc.GetValues()
	return q, err
}

// initLibraryRegistry creates and configures the library registry with search paths.
// Search path order (highest priority first):
//  1. -L command line flag paths
//  2. SCHEME_LIBRARY_PATH environment variable paths
//  3. Default paths (".", "./lib")
func initLibraryRegistry(_ context.Context) *machine.LibraryRegistry {
	registry := machine.NewLibraryRegistry()

	// Add environment variable paths (after defaults)
	if envPath := os.Getenv(SchemeLibraryPathEnv); envPath != "" {
		for _, p := range strings.Split(envPath, string(os.PathListSeparator)) {
			if p != "" {
				registry.AddSearchPath(p)
			}
		}
	}

	// Add command line paths (highest priority, added last so they're first)
	if opts.LibraryPath != "" {
		for _, p := range strings.Split(opts.LibraryPath, string(os.PathListSeparator)) {
			if p != "" {
				registry.AddSearchPath(p)
			}
		}
	}

	return registry
}

func main() {
	var fin io.RuneReader
	var fd *os.File

	parser := flags.NewParser(&opts, flags.Default)
	parser.Name = "scheme"
	parser.Usage = "[OPTIONS] [FILE]"

	args, err := parser.Parse()
	if err != nil {
		if flagsErr, ok := err.(*flags.Error); ok && flagsErr.Type == flags.ErrHelp {
			os.Exit(0)
		}
		os.Exit(1)
	}

	if opts.Version {
		fmt.Printf("Wile Scheme %s\n", Version)
		os.Exit(0)
	}

	// Handle positional argument as file if --file not specified
	if opts.File == "" && len(args) > 0 {
		opts.File = args[0]
	}

	env, err0 := runtime.NewTopLevelEnvironmentFrameTiny()
	if err0 != nil {
		Failf(err0, "Cannot create top-level environment")
	}

	// Initialize library registry with search paths and attach to environment
	registry := initLibraryRegistry(nil)
	env.SetLibraryRegistry(registry)

	// Set up the library environment factory (avoids import cycle)
	machine.LibraryEnvFactory = runtime.NewTopLevelEnvironmentFrameTiny
	// read evaluate loop
	ctx := context.Background()
	// include file if any
	if opts.File != "" {
		var err1 error
		log.Printf("reading file %q", opts.File)
		fd, err1 = os.Open(opts.File)
		if err1 != nil {
			Failf(err1, "Cannot open file %s", opts.File)
		}
		fin = bufio.NewReader(fd)
		runFile(ctx, env, fin, opts.File)
		return
	}
	// interactive REPL
	runREPL(ctx, env)
}

// runFile processes a Scheme file, exiting on errors
func runFile(ctx context.Context, env *environment.EnvironmentFrame, fin io.RuneReader, filename string) {
	p := parser.NewParserWithFile(env, fin, filename)
	stx, err := p.ReadSyntax(nil)
	for err == nil {
		tpl, err2 := compile(env, stx)
		if err2 != nil {
			Failf(err2, "Cannot compile expression")
		}
		mv, err2 := run(ctx, tpl, env)
		if errors.Is(err2, machine.ErrMachineHalt) {
			Printf("%s\n", mv.SchemeString())
		} else if err2 != nil {
			Failf(err2)
		}
		stx, err = p.ReadSyntax(nil)
	}
	if !errors.Is(err, io.EOF) {
		Failf(err)
	}
}

// runREPL runs an interactive Read-Eval-Print Loop with line editing
func runREPL(ctx context.Context, env *environment.EnvironmentFrame) {
	rl, err := readline.NewFromConfig(&readline.Config{
		Prompt:          "> ",
		InterruptPrompt: "^C",
		EOFPrompt:       "",
		HistoryFile:     getHistoryFile(),
		HistoryLimit:    1000,
	})
	if err != nil {
		// Fall back to simple REPL if readline fails
		runSimpleREPL(ctx, env)
		return
	}
	defer rl.Close() //nolint:errcheck

	// Create debug context
	debugCtx := NewDebugContext()

	// Set up break callback
	debugCtx.Debugger().OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
		debugCtx.SetCurrentMC(mc)
		if bp != nil {
			fmt.Printf("\nBreakpoint %d hit", bp.ID)
			if source := mc.CurrentSource(); source != nil {
				fmt.Printf(" at %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Println()
		} else {
			fmt.Print("\nStepped")
			if source := mc.CurrentSource(); source != nil {
				fmt.Printf(" to %s:%d:%d", source.File, source.Start.Line(), source.Start.Column())
			}
			fmt.Println()
		}
	})

	var inputBuffer strings.Builder

	for {
		line, err := rl.ReadLine()
		if err != nil {
			if err == readline.ErrInterrupt {
				// Ctrl-C: clear current input and continue
				inputBuffer.Reset()
				rl.SetPrompt("> ")
				continue
			}
			if err == io.EOF {
				// Ctrl-D: exit
				fmt.Println()
				return
			}
			// Other error
			fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
			continue
		}

		// Check for debug commands before parsing as Scheme
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, ",") && inputBuffer.Len() == 0 {
			debugCtx.HandleDebugCommand(trimmed)
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
		p := parser.NewParser(env, rdr)

		stx, parseErr := p.ReadSyntax(nil)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				// Incomplete expression - prompt for more input
				rl.SetPrompt("  ")
				continue
			}
			// Parse error - display and reset
			fmt.Fprintf(os.Stderr, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			rl.SetPrompt("> ")
			continue
		}

		// Successfully parsed - evaluate
		inputBuffer.Reset()
		rl.SetPrompt("> ")

		// Compile
		tpl, compileErr := compile(env, stx)
		if compileErr != nil {
			fmt.Fprintf(os.Stderr, "Exception: %v\n", compileErr)
			continue
		}

		// Run with debugger
		mv, runErr := runWithDebugger(ctx, tpl, env, debugCtx.Debugger())
		if runErr != nil {
			fmt.Fprintf(os.Stderr, "Exception: %v\n", runErr)
			continue
		}

		// Print result (unless void)
		if !mv.IsVoid() {
			fmt.Println(mv.SchemeString())
		}
	}
}

// runSimpleREPL is a fallback REPL without readline support
func runSimpleREPL(ctx context.Context, env *environment.EnvironmentFrame) {
	Printf("> ")
	reader := bufio.NewReader(os.Stdin)
	var inputBuffer strings.Builder

	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				fmt.Println()
				return
			}
			fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
			continue
		}

		inputBuffer.WriteString(line)
		input := inputBuffer.String()
		rdr := strings.NewReader(input)
		p := parser.NewParser(env, rdr)

		stx, parseErr := p.ReadSyntax(nil)
		if parseErr != nil {
			if isIncompleteInput(parseErr) {
				Printf("  ")
				continue
			}
			fmt.Fprintf(os.Stderr, "Exception: %v\n", parseErr)
			inputBuffer.Reset()
			Printf("> ")
			continue
		}

		inputBuffer.Reset()

		tpl, compileErr := compile(env, stx)
		if compileErr != nil {
			fmt.Fprintf(os.Stderr, "Exception: %v\n", compileErr)
			Printf("> ")
			continue
		}

		mv, runErr := run(ctx, tpl, env)
		if runErr != nil {
			fmt.Fprintf(os.Stderr, "Exception: %v\n", runErr)
			Printf("> ")
			continue
		}

		if !mv.IsVoid() {
			Printf("%s\n", mv.SchemeString())
		}
		Printf("> ")
	}
}

// isIncompleteInput checks if the parse error indicates incomplete input
// (e.g., unclosed parenthesis, unterminated string)
func isIncompleteInput(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, io.EOF) {
		return true
	}
	// Check for common incomplete input errors
	errStr := err.Error()
	return strings.Contains(errStr, "unexpected EOF") ||
		strings.Contains(errStr, "unterminated") ||
		strings.Contains(errStr, "unclosed")
}

// getHistoryFile returns the path for the REPL history file
func getHistoryFile() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return home + "/.wile_history"
}

func Printf(fmtstr string, args ...interface{}) {
	_, err := fmt.Fprintf(os.Stdout, fmtstr, args...)
	if err != nil {
		os.Exit(EX_IOERR)
	}
	os.Stdout.Sync() //nolint:errcheck
}

func Failf(err error, messes ...string) {
	mess := strings.Join(messes, ": ")
	if err != nil {
		mess = strings.Join([]string{err.Error(), mess}, ": ")
	}
	if mess == "" {
		os.Exit(EX_OK)
	}
	_, err0 := fmt.Fprintf(os.Stderr, "Error: %s\n", mess)
	if err0 != nil {
		os.Exit(EX_IOERR)
	}
	os.Exit(1)
}
