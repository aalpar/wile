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

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/internal/repl"
	"github.com/aalpar/wile/stdlib"

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

// buildLibraryPaths collects library search paths from environment variables
// and command-line flags. Paths from -L take priority over SCHEME_LIBRARY_PATH.
func buildLibraryPaths() []string {
	var paths []string

	envPath := os.Getenv(SchemeLibraryPathEnv)
	if envPath != "" {
		for p := range strings.SplitSeq(envPath, string(os.PathListSeparator)) {
			if p != "" {
				paths = append(paths, p)
			}
		}
	}

	if opts.LibraryPath != "" {
		var cmdPaths []string
		for p := range strings.SplitSeq(opts.LibraryPath, string(os.PathListSeparator)) {
			if p != "" {
				cmdPaths = append(cmdPaths, p)
			}
		}
		// Command-line paths have higher priority (prepend before env paths)
		paths = append(cmdPaths, paths...)
	}

	return paths
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
			closeErr := f.Close()
			if closeErr != nil {
				Failf(closeErr, "Cannot close CPU profile")
			}
		}()
		err = pprof.StartCPUProfile(f)
		if err != nil {
			Failf(err, "Cannot start CPU profile")
		}
		defer pprof.StopCPUProfile()
	}

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	libPaths := buildLibraryPaths()
	eng, err0 := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(libPaths...),
	)
	if err0 != nil {
		Failf(err0, "Cannot create engine")
	}
	defer func() {
		closeErr := eng.Close()
		if closeErr != nil {
			fmt.Fprintf(os.Stderr, "Warning: engine close error: %v\n", closeErr)
		}
	}()
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
					content, readErr := io.ReadAll(bufio.NewReader(fd))
					if readErr != nil {
						Failf(readErr, "Cannot read file %s", fn)
					}
					absPath, absErr := filepath.Abs(fn)
					if absErr != nil {
						Failf(absErr, "cannot resolve path")
					}
					code := "(begin " + string(content) + "\n)"
					loadErr := eng.WithLoadPath(absPath, func() error {
						expr, parseErr := eng.ParseWithSource(ctx, code, fn)
						if parseErr != nil {
							return parseErr
						}
						compiled, compileErr := eng.Compile(ctx, expr)
						if compileErr != nil {
							return compileErr
						}
						_, runErr := eng.Run(ctx, compiled)
						return runErr
					})
					if loadErr != nil {
						Failf(loadErr)
					}
				} else {
					// Run last file (print results) and exit in non-interactive mode
					fin = bufio.NewReader(fd)
					runFile(ctx, eng, fin, fn, positionalFile)
				}
			}(filename, descriptor)
		}
	}

	// Evaluate -e expressions after files are loaded
	if len(opts.Eval) > 0 {
		runEval(ctx, eng, opts.Eval)
	}

	// Enter REPL if no files and no evals were provided, or interactive mode was requested
	if (len(opts.File) == 0 && len(opts.Eval) == 0) || opts.Interactive {
		setupSignals(opts.Quiet)
		runREPL(ctx, eng)
	}

	// Memory profiling (written at exit after all work is done)
	if opts.MemProfile != "" {
		f, err := os.Create(opts.MemProfile)
		if err != nil {
			Failf(err, "Cannot create memory profile")
		}
		defer func() {
			closeErr := f.Close()
			if closeErr != nil {
				Failf(closeErr, "Cannot close memory profile")
			}
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
func runFile(ctx context.Context, eng *wile.Engine, fin *bufio.Reader, filename string, shebang bool) {
	// Skip shebang line: only for files executed as programs (positional arg),
	// not for files loaded via -f which should be pure Scheme source.
	if shebang {
		peek, err := fin.Peek(2)
		if err == nil && peek[0] == '#' && peek[1] == '!' {
			_, _ = fin.ReadString('\n')
		}
	}

	content, readErr := io.ReadAll(fin)
	if readErr != nil {
		Failf(readErr, "Cannot read file %s", filename)
	}

	if len(content) == 0 {
		return
	}

	absPath, absErr := filepath.Abs(filename)
	if absErr != nil {
		Failf(absErr, "cannot resolve path")
	}

	// Wrap in (begin ...) so all defines are mutually recursive and all
	// expressions share a single continuation chain. Using a space (not
	// newline) after "begin" avoids shifting source line numbers.
	code := "(begin " + string(content) + "\n)"

	var result wile.Value
	loadErr := eng.WithLoadPath(absPath, func() error {
		expr, parseErr := eng.ParseWithSource(ctx, code, filename)
		if parseErr != nil {
			return parseErr
		}
		compiled, compileErr := eng.Compile(ctx, expr)
		if compileErr != nil {
			return compileErr
		}
		var runErr error
		result, runErr = eng.Run(ctx, compiled)
		return runErr
	})
	if loadErr != nil {
		Failf(loadErr)
	}

	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runEval evaluates expressions supplied via -e flags.
// All expressions are joined and evaluated together via EvalMultipleWithSource.
func runEval(ctx context.Context, eng *wile.Engine, exprs []string) {
	combined := strings.Join(exprs, "\n")
	result, err := eng.EvalMultipleWithSource(ctx, combined, "<eval>")
	if err != nil {
		Failf(err)
	}
	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, eng *wile.Engine) {
	docProv := repl.NewRegistryDocProvider(eng.Registry())
	r := repl.New(eng.Environment(), repl.WithDocProvider(docProv))
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
