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

	"github.com/aalpar/wile/coverage"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/repl"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	"github.com/jessevdk/go-flags"
)

type Options struct {
	Eval         []string `short:"e" long:"eval" description:"Evaluate Scheme expression (repeatable)"`
	File         []string `short:"f" long:"file" description:"Scheme file(s) to load (can be repeated)"`
	Interactive  bool     `short:"i" long:"interactive" description:"Enter REPL after loading file(s)"`
	LibraryPath  string   `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)"`
	Version      bool     `short:"V" long:"version" description:"Print version information and exit"`
	Quiet        bool     `short:"q" long:"quiet" description:"Suppress informational messages"`
	MCP          bool     `long:"mcp" description:"Start as MCP server on stdio"`
	MCPTimeout   float64  `long:"mcp-timeout" description:"Default eval timeout in seconds for MCP mode (0 = no timeout)" default:"30"`
	CPUProfile   string   `long:"cpuprofile" description:"Write CPU profile to file"`
	MemProfile   string   `long:"memprofile" description:"Write memory profile to file"`
	MutexProfile string   `long:"mutexprofile" description:"Write mutex contention profile to file"`
	BlockProfile string   `long:"blockprofile" description:"Write goroutine blocking profile to file"`
	Cover        string   `long:"cover" description:"Write Scheme-level coverage report to file (Go cover format)"`
	CoverStdlib  bool     `long:"cover-stdlib" description:"Include stdlib files in --cover output (default excludes scheme/, wile/, srfi/)"`
	CoverSummary string   `long:"cover-summary" description:"Write human-readable coverage summary to file"`
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

// formatVersion renders the human-readable version line. The SHA is omitted
// when empty so the output never reads "Wile Scheme <version> ()".
func formatVersion(version, sha string) string {
	if version == "" {
		version = "(unknown version)"
	}
	if sha == "" {
		return fmt.Sprintf("Wile Scheme %s", version)
	}
	return fmt.Sprintf("Wile Scheme %s (%s)", version, sha)
}

// versionString resolves and formats the version line shared by the
// --version flag, the REPL startup header, and the ,version meta-command.
func versionString() string {
	return formatVersion(resolveVersion())
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
		fmt.Println(versionString())
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

	// --mcp is exclusive: check conflicts before any defer is registered.
	if opts.MCP && (len(opts.Eval) > 0 || len(opts.File) > 0 || opts.Interactive) {
		fmt.Fprintln(os.Stderr, "Error: --mcp cannot be combined with -e, -f, or -i")
		os.Exit(1)
	}
	if opts.MCPTimeout < 0 {
		fmt.Fprintln(os.Stderr, "Error: --mcp-timeout must be non-negative")
		os.Exit(1)
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

	// Mutex and block profiling must be armed before any work runs: unlike CPU
	// profiling (start/stop bracketing), these accumulate samples globally at the
	// configured rate and are snapshotted at exit. Fraction/rate 1 captures every
	// event, which is appropriate for the short, deliberate profiling runs these
	// flags target (e.g. attributing pool-mutex contention under parallel load).
	if opts.MutexProfile != "" {
		goruntime.SetMutexProfileFraction(1)
	}
	if opts.BlockProfile != "" {
		goruntime.SetBlockProfileRate(1)
	}

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	if opts.MCP {
		err := doMCP(ctx, opts.MCPTimeout)
		if err != nil {
			Failf(err, "MCP server error")
		}
		return
	}

	var coverageCollector *coverage.Collector
	if opts.Cover != "" || opts.CoverSummary != "" {
		coverageCollector = coverage.NewCollector()
	}

	libPaths := buildLibraryPaths()
	engineOpts := []wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(libPaths...),
	}
	if coverageCollector != nil {
		engineOpts = append(engineOpts, wile.WithCoverage(coverageCollector))
	}

	// An interactive session (the REPL, or -i after loading files/-e) uses a MUTABLE
	// top level: the REPL is Wile's interaction environment (Chez model), where
	// redefining a binding across input lines must work. Pure batch execution (a file
	// or -e program with no -i) keeps the immutable default so the frame-reclaim
	// optimizer's top-level payoff applies to the compiled program. This mirrors the
	// runREPL entry condition below; positional script args were already folded into
	// opts.File above, so `wile foo.scm` is correctly treated as batch.
	enterREPL := (len(opts.File) == 0 && len(opts.Eval) == 0) || opts.Interactive
	if enterREPL {
		engineOpts = append(engineOpts, wile.WithMutableTopLevel())
	}

	eng, err0 := wile.NewEngine(ctx, engineOpts...)
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
					loadErr := eng.WithLoadPath(absPath, func() error {
						_, evalErr := eng.EvalProgram(ctx, string(content), fn)
						return evalErr
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
		if !opts.Quiet {
			Printf("%s\n", versionString())
		}
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

	// Mutex/block profiling (written at exit after all work is done)
	if opts.MutexProfile != "" {
		writeNamedProfile("mutex", opts.MutexProfile)
	}
	if opts.BlockProfile != "" {
		writeNamedProfile("block", opts.BlockProfile)
	}

	// Coverage reporting (written at exit after all work is done)
	if coverageCollector != nil {
		if opts.Cover != "" {
			err := writeCoverageFile(opts.Cover, coverageCollector, opts.CoverStdlib)
			if err != nil {
				Failf(err, "writing coverage file")
			}
		}
		if opts.CoverSummary != "" {
			err := writeSummaryFile(opts.CoverSummary, coverageCollector, opts.CoverStdlib)
			if err != nil {
				Failf(err, "writing coverage summary")
			}
		}
	}
}

func writeCoverageFile(path string, col *coverage.Collector, includeStdlib bool) error {
	f, err := os.Create(path)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrFileOpen, err, "writeCoverageFile: create %s", path)
	}
	defer func() {
		_ = f.Close()
	}()
	if includeStdlib {
		return coverage.WriteGoCoverIncludingStdlib(f, col)
	}
	return coverage.WriteGoCover(f, col)
}

func writeSummaryFile(path string, col *coverage.Collector, includeStdlib bool) error {
	f, err := os.Create(path)
	if err != nil {
		return werr.WrapForeignErrorWithCause(werr.ErrFileOpen, err, "writeSummaryFile: create %s", path)
	}
	defer func() {
		_ = f.Close()
	}()
	if includeStdlib {
		return coverage.WriteSummaryIncludingStdlib(f, col)
	}
	return coverage.WriteSummary(f, col)
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

	// EvalProgram wraps the file in a single (begin ...) unit so all top-level
	// defines are mutually visible and share one continuation chain; run with the
	// file's directory on the load path so relative include/load resolves.
	var result wile.Value
	loadErr := eng.WithLoadPath(absPath, func() error {
		var evalErr error
		result, evalErr = eng.EvalProgram(ctx, string(content), filename)
		return evalErr
	})
	if loadErr != nil {
		Failf(loadErr)
	}

	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runEval evaluates expressions supplied via -e flags.
// The -e expressions are joined and evaluated as ONE compilation unit via
// EvalProgram, exactly like runFile: mutually-recursive defines and
// redefine-within-the-batch work, and the -e path does not diverge from file
// execution under the immutable top-level default (B2).
func runEval(ctx context.Context, eng *wile.Engine, exprs []string) {
	result, err := eng.EvalProgram(ctx, strings.Join(exprs, "\n"), "<eval>")
	if err != nil {
		Failf(err)
	}
	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, eng *wile.Engine) {
	reg, ok := eng.Environment().Namespace().Registry().(*registry.Registry)
	if !ok {
		Failf(nil, "internal error: namespace registry has unexpected type")
	}
	docProv := repl.NewRegistryDocProvider(reg, eng)
	r := repl.New(eng, repl.WithDocProvider(docProv), repl.WithVersion(versionString()))
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

// writeNamedProfile snapshots a runtime profile registered with pprof (e.g.
// "mutex", "block") to path. Called at exit so the snapshot reflects all work
// done. The named profiles must be armed earlier via SetMutexProfileFraction /
// SetBlockProfileRate; this only writes the accumulated samples.
func writeNamedProfile(name, path string) {
	p := pprof.Lookup(name)
	if p == nil {
		Failf(werr.ErrInternal, fmt.Sprintf("Unknown profile %q", name))
	}
	f, err := os.Create(path)
	if err != nil {
		Failf(err, fmt.Sprintf("Cannot create %s profile", name))
	}
	defer func() {
		closeErr := f.Close()
		if closeErr != nil {
			Failf(closeErr, fmt.Sprintf("Cannot close %s profile", name))
		}
	}()
	err = p.WriteTo(f, 0)
	if err != nil {
		Failf(err, fmt.Sprintf("Cannot write %s profile", name))
	}
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
