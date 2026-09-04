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
	"sync"
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

// Options is the CLI flag surface.
//
// Every value-bearing field carries unquote:"false". go-flags v1.6.1 otherwise
// runs unquoteIfPossible (convert.go:360-365) on each option value from
// parseOption (parser.go:547-548), which strips a leading '"' and applies Go
// string-escape rules: -e '"hi"' arrived as the symbol hi, path flags rejected
// real files whose names start with a quote, and --strict silently accepted
// '"core"'. Positional arguments never reach parseOption and are unaffected.
//
// A new value-bearing flag MUST carry the tag; bool fields take no value and
// must not.
type Options struct {
	Eval         []string `short:"e" long:"eval" description:"Evaluate Scheme expression (repeatable)" unquote:"false"`
	File         []string `short:"f" long:"file" description:"Scheme file(s) to load (can be repeated)" unquote:"false"`
	Interactive  bool     `short:"i" long:"interactive" description:"Enter REPL after loading file(s)"`
	Check        bool     `long:"check" description:"Parse and compile without executing; report errors and exit"`
	LibraryPath  string   `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)" unquote:"false"`
	Version      bool     `short:"V" long:"version" description:"Print version information and exit"`
	Quiet        bool     `short:"q" long:"quiet" description:"Suppress informational messages"`
	Strict       string   `long:"strict" description:"Narrow the visible top level: 'core' binds only the core surface, 'no-bindings' binds nothing (everything, car included, must be imported)" choice:"core" choice:"no-bindings" unquote:"false"`
	MCP          bool     `long:"mcp" description:"Start as MCP server on stdio"`
	MCPTimeout   float64  `long:"mcp-timeout" description:"Default eval timeout in seconds for MCP mode (0 = no caller-supplied deadline, bounded by the server maximum)" default:"30" unquote:"false"`
	CPUProfile   string   `long:"cpuprofile" description:"Write CPU profile to file" unquote:"false"`
	MemProfile   string   `long:"memprofile" description:"Write memory profile to file" unquote:"false"`
	MutexProfile string   `long:"mutexprofile" description:"Write mutex contention profile to file" unquote:"false"`
	BlockProfile string   `long:"blockprofile" description:"Write goroutine blocking profile to file" unquote:"false"`
	Cover        string   `long:"cover" description:"Write Scheme-level coverage report to file (Go cover format)" unquote:"false"`
	CoverStdlib  bool     `long:"cover-stdlib" description:"Include stdlib files in --cover output (default excludes scheme/, wile/, srfi/)"`
	CoverSummary string   `long:"cover-summary" description:"Write human-readable coverage summary to file" unquote:"false"`
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
	parser.Name = "wile"
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
	if opts.MCP && (len(opts.Eval) > 0 || len(opts.File) > 0 || opts.Interactive || opts.Check) {
		fmt.Fprintln(os.Stderr, "Error: --mcp cannot be combined with -e, -f, -i, or --check")
		os.Exit(1)
	}

	// --check means "do not execute"; -i means "execute, then hand me a REPL".
	// With no input at all it would fall through to the REPL, which is the one
	// thing --check must never do, so require something to check.
	if opts.Check {
		if opts.Interactive {
			fmt.Fprintln(os.Stderr, "Error: --check cannot be combined with -i")
			os.Exit(1)
		}
		if len(opts.File) == 0 && len(opts.Eval) == 0 {
			fmt.Fprintln(os.Stderr, "Error: --check requires a file or -e expression")
			os.Exit(1)
		}
	}
	if opts.MCPTimeout < 0 {
		fmt.Fprintln(os.Stderr, "Error: --mcp-timeout must be non-negative")
		os.Exit(1)
	}

	// CPU profiling
	var cpuProfile *os.File
	if opts.CPUProfile != "" {
		f, err := os.Create(opts.CPUProfile)
		if err != nil {
			Failf(err, "Cannot create CPU profile")
		}
		err = pprof.StartCPUProfile(f)
		if err != nil {
			Failf(err, "Cannot start CPU profile")
		}
		cpuProfile = f
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

	var coverageCollector *coverage.Collector
	if opts.Cover != "" || opts.CoverSummary != "" {
		coverageCollector = coverage.NewCollector()
	}

	// Every profile and report is written by writeRunReports, reached three
	// ways. The deferred call covers the --mcp and --check returns. The explicit
	// call at the bottom of main runs ahead of the deferred engine Close, so the
	// heap snapshot still sees the engine. The system extension's exit hook
	// covers a program that ends in (exit): that terminates through os.Exit,
	// which skips both of the others, and every test suite ends that way. The
	// Once makes whichever arrivals come later no-ops.
	var finishOnce sync.Once
	finishRun := func() {
		finishOnce.Do(func() {
			writeRunReports(cpuProfile, coverageCollector)
		})
	}
	defer finishRun()
	system.SetExitHook(finishRun)

	// Cancel on SIGTERM only. SIGINT (Ctrl-C) is deliberately NOT routed into
	// this context: the interactive REPL owns SIGINT so a Ctrl-C during eval
	// cancels just that evaluation and returns to the prompt instead of
	// cancelling this context and tearing the whole session down (see
	// pkg/repl). In non-interactive batch runs no SIGINT handler is installed,
	// so Ctrl-C keeps its default action of terminating the process.
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGTERM)
	defer cancel()

	if opts.MCP {
		err := doMCP(ctx, opts.MCPTimeout)
		if err != nil {
			Failf(err, "MCP server error")
		}
		return
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

	// --strict narrows the VISIBLE top level; the KitchenSink registry above is
	// untouched, so everything withheld is still one (import …) away.
	//
	// The flag takes a REQUIRED value, so both --strict=core and --strict core
	// work. It deliberately has no optional-value form: go-flags refuses to
	// consume a space-separated value for a flag declared optional, so a
	// valueless --strict would have made `wile --strict no-bindings script.scm`
	// silently select core and swallow "no-bindings" as a positional file.
	//
	// go-flags validates the value against the choice tags before this point, so
	// the default arm is reachable only when the flag was not given at all —
	// which the "" case names explicitly rather than leaving to a bare default.
	switch opts.Strict {
	case "":
	case "core":
		engineOpts = append(engineOpts, wile.WithStrictNamespace())
	case "no-bindings":
		engineOpts = append(engineOpts, wile.WithoutAmbientBindings())
	default:
		Failf(werr.ErrEngineInit, "unrecognized --strict value %q", opts.Strict)
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
	if opts.Check {
		runCheck(ctx, eng, opts.File, opts.Eval, positionalFile)
		return
	}

	// Load files if any
	if len(opts.File) > 0 {
		for i, filename := range opts.File {
			if !opts.Quiet {
				log.Printf("reading file %q", displayName(filename))
			}
			descriptor, closeSource, err := openScriptFile(filename)
			if err != nil {
				// The os error already reads "open <path>: ..." — adding the
				// filename again would print it twice.
				Fail(err)
			}
			func(fn string, fd *os.File, closeFn func() error) {
				defer func() {
					_ = closeFn()
				}()
				isLastFile := i == len(opts.File)-1
				if opts.Interactive || !isLastFile || len(opts.Eval) > 0 {
					// Load file silently when:
					// - interactive mode (all files loaded before REPL)
					// - not the last file (earlier files are always silent)
					// - -e expressions present (files are setup, -e is the main program)
					//
					// This branch is shared with -f, so the shebang decision is
					// positionalFile's, exactly as it is for runFile below.
					silent := bufio.NewReader(fd)
					skipShebang(silent, positionalFile)
					content, readErr := io.ReadAll(silent)
					if readErr != nil {
						Failf(readErr, "Cannot read file %s", fn)
					}
					absPath, absErr := filepath.Abs(fn)
					if absErr != nil {
						Failf(absErr, "cannot resolve path")
					}
					loadCtx, ctxErr := eng.ContextWithLoadPath(ctx, absPath)
					if ctxErr != nil {
						Fail(ctxErr)
					}
					_, evalErr := eng.EvalProgram(loadCtx, string(content), fn)
					if evalErr != nil {
						Fail(evalErr)
					}
				} else {
					// Run last file (print results) and exit in non-interactive mode
					fin = bufio.NewReader(fd)
					runFile(ctx, eng, fin, fn, positionalFile)
				}
			}(filename, descriptor, closeSource)
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

	finishRun()
}

// writeRunReports finalizes every profile and coverage report the flags asked
// for, after all work is done. It is the one end-of-run writer; the finishRun
// comment in main lists the paths that reach it. The CPU profile is stopped
// first so the report writing is not itself sampled.
func writeRunReports(cpuProfile *os.File, col *coverage.Collector) {
	if cpuProfile != nil {
		pprof.StopCPUProfile()
		closeErr := cpuProfile.Close()
		if closeErr != nil {
			Failf(closeErr, "Cannot close CPU profile")
		}
	}

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

	if opts.MutexProfile != "" {
		writeNamedProfile("mutex", opts.MutexProfile)
	}
	if opts.BlockProfile != "" {
		writeNamedProfile("block", opts.BlockProfile)
	}

	if col == nil {
		return
	}
	if opts.Cover != "" {
		err := writeCoverageFile(opts.Cover, col, opts.CoverStdlib)
		if err != nil {
			Failf(err, "writing coverage file")
		}
	}
	if opts.CoverSummary != "" {
		err := writeSummaryFile(opts.CoverSummary, col, opts.CoverStdlib)
		if err != nil {
			Failf(err, "writing coverage summary")
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

// stdinFilename is the conventional argument that names standard input as the
// program source, following the Unix "-" idiom used by cat, sh, and friends.
const stdinFilename = "-"

// evalSourceName labels -e expressions in diagnostics. Shared by the run and
// check paths so a -e program reports the same location either way.
const evalSourceName = "<eval>"

// openScriptFile opens a Scheme source for execution. The filename "-" reads the
// program from standard input; the returned closer is then a no-op so stdin is
// left open for the running process. For any other name it opens the file and
// returns its Close method.
func openScriptFile(filename string) (*os.File, func() error, error) {
	if filename == stdinFilename {
		return os.Stdin, func() error {
			return nil
		}, nil
	}
	fd, err := os.Open(filename)
	if err != nil {
		return nil, nil, err
	}
	return fd, fd.Close, nil
}

// displayName renders a script source name for human-facing messages, mapping
// the "-" stdin sentinel to a readable label.
func displayName(filename string) string {
	if filename == stdinFilename {
		return "<stdin>"
	}
	return filename
}

// skipShebang consumes a leading #! line when shebang is true and one is
// present. Only files executed as programs (a positional argument) get the
// skip; files loaded via -f are pure Scheme source and keep the #! so it fails
// to parse, which is deliberate and documented.
//
// Every path that reads a script source must go through this, including the
// silent-load branch: that branch is shared between positional files and -f,
// so the caller decides via shebang rather than the branch peeking
// unconditionally.
func skipShebang(fin *bufio.Reader, shebang bool) {
	if !shebang {
		return
	}
	peek, err := fin.Peek(2)
	if err != nil || peek[0] != '#' || peek[1] != '!' {
		return
	}
	_, _ = fin.ReadString('\n')
}

// runFile processes a Scheme file, exiting on errors.
// All top-level expressions are wrapped in a single (begin ...) form to enable
// proper R7RS continuation semantics across expression boundaries.
// When shebang is true, a leading #! line is skipped if present.
func runFile(ctx context.Context, eng *wile.Engine, fin *bufio.Reader, filename string, shebang bool) {
	skipShebang(fin, shebang)

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
	loadCtx, ctxErr := eng.ContextWithLoadPath(ctx, absPath)
	if ctxErr != nil {
		Fail(ctxErr)
	}
	result, evalErr := eng.EvalProgram(loadCtx, string(content), filename)
	if evalErr != nil {
		Fail(evalErr)
	}

	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runCheck compiles every input without executing it, reporting the first
// failure and exiting 1, or exiting 0 when everything compiles clean.
//
// It is a mode of its own rather than a branch inside the run paths because
// every way those paths differ is a difference about executing: which file
// prints its result, which files load silently, and whether a REPL follows.
// Checking treats every file and every -e expression the same way. Routing here
// once is also what keeps the silent multi-file load path from running a program
// under --check — that path is a second EvalProgram call site, and patching only
// runFile would miss it.
//
// Files are checked in order against one engine, so a later file resolves names
// an earlier file defined, matching how execution would see them.
func runCheck(ctx context.Context, eng *wile.Engine, files []string, evals []string, shebang bool) {
	for _, filename := range files {
		descriptor, closeSource, openErr := openScriptFile(filename)
		if openErr != nil {
			// The os error already reads "open <path>: ..." — adding the
			// filename again would print it twice.
			Fail(openErr)
		}
		func(fn string, fd *os.File, closeFn func() error) {
			defer func() {
				_ = closeFn()
			}()
			checkFile(ctx, eng, bufio.NewReader(fd), fn, shebang)
		}(filename, descriptor, closeSource)
	}

	if len(evals) > 0 {
		// Joined into one unit exactly as runEval does, so -e under --check
		// reports what -e without it would have compiled.
		checkErr := eng.CheckProgram(ctx, strings.Join(evals, "\n"), evalSourceName)
		if checkErr != nil {
			Fail(checkErr)
		}
	}
}

// checkFile compiles one source without executing it. The file's directory goes
// on the load path because include resolves at compile time and so is reached by
// checking.
func checkFile(ctx context.Context, eng *wile.Engine, fin *bufio.Reader, filename string, shebang bool) {
	skipShebang(fin, shebang)

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

	loadCtx, ctxErr := eng.ContextWithLoadPath(ctx, absPath)
	if ctxErr != nil {
		Fail(ctxErr)
	}
	checkErr := eng.CheckProgram(loadCtx, string(content), filename)
	if checkErr != nil {
		Fail(checkErr)
	}
}

// runEval evaluates expressions supplied via -e flags.
// The -e expressions are joined and evaluated as ONE compilation unit via
// EvalProgram, exactly like runFile: mutually-recursive defines and
// redefine-within-the-batch work, and the -e path does not diverge from file
// execution under the immutable top-level default (B2).
func runEval(ctx context.Context, eng *wile.Engine, exprs []string) {
	result, err := eng.EvalProgram(ctx, strings.Join(exprs, "\n"), evalSourceName)
	if err != nil {
		Fail(err)
	}
	if result != nil && !result.IsVoid() {
		Printf("%s\n", result.SchemeString())
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, eng *wile.Engine) {
	reg, ok := eng.Environment().Namespace().Registry().(*registry.PrimitiveRegistry)
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
		Failf(werr.ErrInternal, "Unknown profile %q", name)
	}
	f, err := os.Create(path)
	if err != nil {
		Failf(err, "Cannot create %s profile", name)
	}
	defer func() {
		closeErr := f.Close()
		if closeErr != nil {
			Failf(closeErr, "Cannot close %s profile", name)
		}
	}()
	err = p.WriteTo(f, 0)
	if err != nil {
		Failf(err, "Cannot write %s profile", name)
	}
}

// Failf prints "Error: <err>: <formatted message>" to stderr and exits 1.
//
// The (format string, args ...any) shape is deliberate: it makes Failf a printf
// wrapper that `go vet` classifies and checks. The former (messes ...string)
// signature accepted "Cannot open file %s", filename as two message *parts*,
// silently printing a literal %s and the filename twice, and vet could not see
// it. Use Fail for the no-message case.
func Failf(err error, format string, args ...any) {
	fail(err, fmt.Sprintf(format, args...))
}

// Fail prints "Error: <err>" and exits 1. A nil err with no message exits
// EX_OK, so fail's joined-empty case can never emit a bare "Error: ".
func Fail(err error) {
	fail(err, "")
}

func fail(err error, mess string) {
	// Drop the empty parts so a bare Fail(err) never emits a dangling ": ".
	var parts []string
	if err != nil {
		parts = append(parts, err.Error())
	}
	if mess != "" {
		parts = append(parts, mess)
	}
	joined := strings.Join(parts, ": ")
	if joined == "" {
		os.Exit(EX_OK)
	}
	_, err0 := fmt.Fprintf(os.Stderr, "Error: %s\n", joined)
	if err0 != nil {
		os.Exit(EX_IOERR)
	}
	os.Exit(1)
}
