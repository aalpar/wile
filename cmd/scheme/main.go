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
	"path/filepath"
	goruntime "runtime"
	"runtime/debug"
	"runtime/pprof"
	"strings"
	"syscall"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/repl"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/runtime"

	"github.com/jessevdk/go-flags"
)

type Options struct {
	Eval        []string `short:"e" long:"eval" description:"Evaluate Scheme expression (repeatable)"`
	File        []string `short:"f" long:"file" description:"Scheme file(s) to load (can be repeated)"`
	Interactive bool     `short:"i" long:"interactive" description:"Enter REPL after loading file(s)"`
	LibraryPath string   `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)"`
	Version     bool     `short:"V" long:"version" description:"Print version information and exit"`
	Quiet       bool     `short:"q" long:"quiet" description:"Suppress informational messages"`
	CPUProfile  string   `long:"cpuprofile" description:"Write CPU profile to file"`
	MemProfile  string   `long:"memprofile" description:"Write memory profile to file"`
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

// resolveVersion returns the version and SHA for display. Ldflags values
// take priority; debug.ReadBuildInfo fills in when go install is used.
func resolveVersion() (version, sha string) {
	version = BuildVersion
	sha = BuildSHA

	if version != "" && sha != "" {
		return version, sha
	}

	info, ok := debug.ReadBuildInfo()
	if !ok {
		return version, sha
	}

	if version == "" && info.Main.Version != "" && info.Main.Version != "(devel)" {
		version = info.Main.Version
	}

	if sha == "" {
		for _, s := range info.Settings {
			if s.Key == "vcs.revision" && len(s.Value) >= 7 {
				sha = s.Value[:7]
				break
			}
		}
	}

	return version, sha
}

// initLibraryRegistry creates and configures the library registry with search paths.
// Search path order (highest priority first):
//  1. -L command line flag paths
//  2. SCHEME_LIBRARY_PATH environment variable paths
//  3. Default paths (".", "./lib")
func initLibraryRegistry(_ context.Context) *machine.LibraryRegistry {
	registry := machine.NewLibraryRegistry()

	// Add environment variable paths (after defaults).
	// Reverse-iterate so left-to-right order is preserved after prepending.
	envPath := os.Getenv(SchemeLibraryPathEnv)
	if envPath != "" {
		parts := strings.Split(envPath, string(os.PathListSeparator))
		for i := len(parts) - 1; i >= 0; i-- {
			if parts[i] != "" {
				registry.PrependSearchPath(parts[i])
			}
		}
	}

	// Add command line paths (highest priority, added last so they're first).
	// Reverse-iterate so left-to-right order is preserved after prepending.
	if opts.LibraryPath != "" {
		parts := strings.Split(opts.LibraryPath, string(os.PathListSeparator))
		for i := len(parts) - 1; i >= 0; i-- {
			if parts[i] != "" {
				registry.PrependSearchPath(parts[i])
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
	var fin *bufio.Reader

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
		v, s := resolveVersion()
		fmt.Printf("Wile Scheme %s (%s)\n", v, s)
		os.Exit(0)
	}

	// Handle positional argument as file if --file not specified.
	// Remaining positional args after the filename are script arguments.
	// positionalFile tracks whether the file came from a positional arg
	// (i.e. shebang execution context) vs -f flag (pure Scheme source).
	var scriptArgs []string
	positionalFile := false
	if len(opts.File) == 0 && len(args) > 0 {
		opts.File = append(opts.File, args[0])
		scriptArgs = args[1:]
		positionalFile = true
	} else {
		scriptArgs = args
	}

	// Set (command-line) to [script-name, script-args...] per R7RS §6.14
	if len(opts.File) > 0 {
		cmdLine := make([]string, 0, 1+len(scriptArgs))
		cmdLine = append(cmdLine, opts.File[len(opts.File)-1])
		cmdLine = append(cmdLine, scriptArgs...)
		system.SetCommandLine(cmdLine)
	}

	// CPU profiling
	if opts.CPUProfile != "" {
		f, err := os.Create(opts.CPUProfile)
		if err != nil {
			Failf(err, "Cannot create CPU profile")
		}
		defer func() {
			_ = f.Close()
		}()
		err = pprof.StartCPUProfile(f)
		if err != nil {
			Failf(err, "Cannot start CPU profile")
		}
		defer pprof.StopCPUProfile()
	}

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	env, primRegistry, err0 := bootstrap.NewTopLevelWithRegistry(ctx)
	if err0 != nil {
		Failf(err0, "Cannot create top-level environment")
	}

	// Initialize library registry with search paths and attach to environment
	libRegistry := initLibraryRegistry(ctx)
	env.SetLibraryRegistry(libRegistry)

	// Set up the library environment factory on the TopLevelEnvironment.
	// Uses NewLibraryEnvironmentFrame which shares the TopLevelEnvironment for symbol identity.
	env.TopLevelEnv().SetLibraryEnvFactory(bootstrap.NewLibraryEnvironmentFrame)
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
				if opts.Interactive || !isLastFile || len(opts.Eval) > 0 {
					// Load file silently when:
					// - interactive mode (all files loaded before REPL)
					// - not the last file (earlier files are always silent)
					// - -e expressions present (files are setup, -e is the main program)
					err = runtime.Load(ctx, env, fd, fn)
					if err != nil {
						Failf(err)
					}
				} else {
					// Run last file (print results) and exit in non-interactive mode
					fin = bufio.NewReader(fd)
					runFile(ctx, env, fin, fn, positionalFile)
				}
			}(filename, descriptor)
		}
	}

	// Evaluate -e expressions after files are loaded
	if len(opts.Eval) > 0 {
		runEval(ctx, env, opts.Eval)
	}

	// Enter REPL if no files and no evals were provided, or interactive mode was requested
	if (len(opts.File) == 0 && len(opts.Eval) == 0) || opts.Interactive {
		setupSignals(opts.Quiet)
		runREPL(ctx, env, primRegistry)
	}

	// Memory profiling (written at exit after all work is done)
	if opts.MemProfile != "" {
		f, err := os.Create(opts.MemProfile)
		if err != nil {
			Failf(err, "Cannot create memory profile")
		}
		defer func() {
			_ = f.Close()
		}()
		goruntime.GC()
		err = pprof.WriteHeapProfile(f)
		if err != nil {
			Failf(err, "Cannot write memory profile")
		}
	}
}

// runFile processes a Scheme file, exiting on errors.
// All top-level expressions are wrapped in a single (begin ...) form to enable
// proper R7RS continuation semantics across expression boundaries.
// When shebang is true, a leading #! line is skipped if present.
func runFile(ctx context.Context, env *environment.EnvironmentFrame, fin *bufio.Reader, filename string, shebang bool) {
	// Skip shebang line: only for files executed as programs (positional arg),
	// not for files loaded via -f which should be pure Scheme source.
	if shebang {
		peek, err := fin.Peek(2)
		if err == nil && peek[0] == '#' && peek[1] == '!' {
			_, _ = fin.ReadString('\n')
		}
	}

	// Push file path onto LoadPathStack so (include ...) can resolve relative paths.
	absPath, absErr := filepath.Abs(filename)
	if absErr == nil {
		stack := env.LoadPathStack()
		if stack != nil {
			pushErr := stack.Push(absPath)
			if pushErr == nil {
				defer stack.Pop()
			}
		}
	}

	p := parser.NewParserWithFile(env, true, fin, filename)

	// Collect all expressions from the file
	var exprs []syntax.SyntaxValue
	stx, err := p.ReadSyntax(ctx)
	for err == nil {
		exprs = append(exprs, stx)
		stx, err = p.ReadSyntax(ctx)
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
	if err2 != nil {
		Failf(err2)
	}
	if !mv.IsVoid() {
		Printf("%s\n", mv.SchemeString())
	}
}

// runEval evaluates expressions supplied via -e flags.
// All expressions are parsed together, wrapped in a single (begin ...) form,
// and compiled/run as one unit — same continuation semantics as file execution.
func runEval(ctx context.Context, env *environment.EnvironmentFrame, exprs []string) {
	combined := strings.Join(exprs, "\n")
	fin := strings.NewReader(combined)
	p := parser.NewParserWithFile(env, true, fin, "<eval>")

	var stxExprs []syntax.SyntaxValue
	stx, err := p.ReadSyntax(ctx)
	for err == nil {
		stxExprs = append(stxExprs, stx)
		stx, err = p.ReadSyntax(ctx)
	}
	if !errors.Is(err, io.EOF) {
		Failf(err)
	}

	if len(stxExprs) == 0 {
		return
	}

	var programStx syntax.SyntaxValue
	if len(stxExprs) == 1 {
		programStx = stxExprs[0]
	} else {
		sctx := syntax.NewZeroValueSourceContext()
		beginSym := syntax.NewSyntaxSymbol("begin", sctx)
		allExprs := make([]syntax.SyntaxValue, 0, len(stxExprs)+1)
		allExprs = append(allExprs, beginSym)
		allExprs = append(allExprs, stxExprs...)
		programStx = syntax.SyntaxList(sctx, allExprs...)
	}

	tpl, err2 := runtime.Compile(ctx, env, programStx)
	if err2 != nil {
		Failf(err2, "Cannot compile expression")
	}
	mv, err2 := runtime.Run(ctx, tpl, env)
	if err2 != nil {
		Failf(err2)
	}
	if !mv.IsVoid() {
		Printf("%s\n", mv.SchemeString())
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, env *environment.EnvironmentFrame, primRegistry *registry.Registry) {
	docProv := repl.NewRegistryDocProvider(primRegistry)
	r := repl.New(env, repl.WithDocProvider(docProv))
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
