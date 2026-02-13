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

package main

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
	goruntime "runtime"
	"strings"
	"syscall"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/repl"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/runtime"

	"github.com/jessevdk/go-flags"
)

type Options struct {
	File        []string `short:"f" long:"file" description:"Scheme file(s) to load (can be repeated)"`
	Interactive bool     `short:"i" long:"interactive" description:"Enter REPL after loading file(s)"`
	LibraryPath string   `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)"`
	Version     bool     `short:"V" long:"version" description:"Print version information and exit"`
	Quiet       bool     `short:"q" long:"quiet" description:"Suppress informational messages"`
}

var (
	// BuildSHA is the git SHA of the current build
	BuildSHA string
	// BuildVersion is the current version of the Scheme interpreter
	BuildVersion string
	opts         Options
)

const (
	// SchemeLibraryPathEnv is the environment variable for library search paths
	SchemeLibraryPathEnv = "SCHEME_LIBRARY_PATH"
)

// initLibraryRegistry creates and configures the library registry with search paths.
// Search path order (highest priority first):
//  1. -L command line flag paths
//  2. SCHEME_LIBRARY_PATH environment variable paths
//  3. Default paths (".", "./lib")
func initLibraryRegistry(_ context.Context) *machine.LibraryRegistry {
	registry := machine.NewLibraryRegistry()

	// Add environment variable paths (after defaults)
	envPath := os.Getenv(SchemeLibraryPathEnv)
	if envPath != "" {
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

func setupSignals(quiet bool) {
	sigChan := make(chan os.Signal, 1)
	// Notify the channel about SIGQUIT signals
	signal.Notify(sigChan, syscall.SIGQUIT)

	go func() {
		for range sigChan {
			// Allocate a buffer and write all goroutine stacks to it
			stacktrace := make([]byte, 1<<20)
			length := goruntime.Stack(stacktrace, true) // `true` means dump all goroutines
			for length >= len(stacktrace) {
				stacktrace = make([]byte, len(stacktrace)*2)
				length = goruntime.Stack(stacktrace, true)
			}
			fmt.Fprintln(os.Stderr, "=== GOROUTINE STACK DUMP ===")
			fmt.Fprintln(os.Stderr, string(stacktrace[:length]))
			fmt.Fprintln(os.Stderr, "=== END OF DUMP ===")
			// The program continues running after this point
		}
	}()

	if !quiet {
		fmt.Fprintln(os.Stderr, "Program running, send SIGQUIT (Ctrl+\\) to dump stacks.")
	}
}

func main() {
	var fin io.RuneReader

	parser := flags.NewParser(&opts, flags.Default)
	parser.Name = "scheme"
	parser.Usage = "[OPTIONS] [FILE]"

	args, err := parser.Parse()
	if err != nil {
		flagsErr, ok := err.(*flags.Error)
		if ok && flagsErr.Type == flags.ErrHelp {
			os.Exit(0)
		}
		os.Exit(1)
	}

	if opts.Version {
		fmt.Printf("Wile Scheme %s (%s)\n", BuildVersion, BuildSHA)
		os.Exit(0)
	}

	// Handle positional argument as file if --file not specified
	if len(opts.File) == 0 && len(args) > 0 {
		opts.File = append(opts.File, args[0])
	}

	env, err0 := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err0 != nil {
		Failf(err0, "Cannot create top-level environment")
	}

	// Initialize library registry with search paths and attach to environment
	registry := initLibraryRegistry(context.TODO())
	env.SetLibraryRegistry(registry)

	// Set up the library environment factory (avoids import cycle)
	// Use NewLibraryEnvironmentFrame which shares the TopLevelEnvironment for symbol identity
	machine.LibraryEnvFactory = bootstrap.NewLibraryEnvironmentFrame
	// read evaluate loop
	ctx := context.Background()
	// Load files if any
	if len(opts.File) > 0 {
		for i, filename := range opts.File {
			if !opts.Quiet {
				log.Printf("reading file %q", filename)
			}
			descriptor, err := os.Open(filename)
			if err != nil {
				Failf(err, "Cannot open file %s", filename)
			}
			func(fn string, fd *os.File) {
				defer func() {
					_ = fd.Close()
				}()
				isLastFile := i == len(opts.File)-1
				if opts.Interactive || !isLastFile {
					// Load file silently (all files in interactive mode, or non-last files in batch mode)
					err = runtime.Load(ctx, env, fd, fn)
					if err != nil {
						Failf(err)
					}
				} else {
					// Run last file (print results) and exit in non-interactive mode
					fin = bufio.NewReader(fd)
					runFile(ctx, env, fin, fn)
				}
			}(filename, descriptor)
		}
	}
	// Only enter REPL if no files were provided OR interactive mode was requested
	if len(opts.File) == 0 || opts.Interactive {
		setupSignals(opts.Quiet)
		runREPL(ctx, env)
	}
}

// runFile processes a Scheme file, exiting on errors.
// All top-level expressions are wrapped in a single (begin ...) form to enable
// proper R7RS continuation semantics across expression boundaries.
func runFile(ctx context.Context, env *environment.EnvironmentFrame, fin io.RuneReader, filename string) {
	p := parser.NewParserWithFile(env, true, fin, filename)

	// Collect all expressions from the file
	var exprs []syntax.SyntaxValue
	stx, err := p.ReadSyntax(context.TODO())
	for err == nil {
		exprs = append(exprs, stx)
		stx, err = p.ReadSyntax(context.TODO())
	}
	if !errors.Is(err, io.EOF) {
		Failf(err)
	}

	// If no expressions, nothing to do
	if len(exprs) == 0 {
		return
	}

	// If only one expression, run it directly (no need for begin wrapper)
	var programStx syntax.SyntaxValue
	if len(exprs) == 1 {
		programStx = exprs[0]
	} else {
		// Wrap all expressions in (begin expr1 expr2 ... exprN)
		// This ensures all expressions share a single continuation chain,
		// enabling proper R7RS continuation semantics across expression boundaries.
		sctx := syntax.NewZeroValueSourceContext()
		beginSym := syntax.NewSyntaxSymbol("begin", sctx)
		allExprs := make([]syntax.SyntaxValue, 0, len(exprs)+1)
		allExprs = append(allExprs, beginSym)
		allExprs = append(allExprs, exprs...)
		programStx = syntax.SyntaxList(sctx, allExprs...)
	}

	// Compile and run the single wrapped expression
	tpl, err2 := runtime.Compile(ctx, env, programStx)
	if err2 != nil {
		Failf(err2, "Cannot compile expression")
	}
	mv, err2 := runtime.Run(ctx, tpl, env)
	// Print result for normal completion; don't print void results
	if err2 == nil {
		if !mv.IsVoid() {
			Printf("%s\n", mv.SchemeString())
		}
	} else {
		Failf(err2)
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, env *environment.EnvironmentFrame) {
	r := repl.New(env)
	err := r.Run(ctx)
	if err != nil {
		Failf(err, "REPL error")
	}
}

func Printf(fmtstr string, args ...any) {
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
