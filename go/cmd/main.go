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
	"os/signal"
	gruntime "runtime"
	"strings"
	"syscall"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/repl"
	"wile/runtime"
	"wile/syntax"

	"github.com/jessevdk/go-flags"
)

type Options struct {
	File        string `short:"f" long:"file" description:"Scheme file to run"`
	LibraryPath string `short:"L" long:"library-path" description:"Library search path (colon-separated, prepended to SCHEME_LIBRARY_PATH)"`
	Version     bool   `short:"v" long:"version" description:"Print version information and exit"`
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
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(ctx, cont)
	err := mc.Run()
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

func setupSignals() {
	sigChan := make(chan os.Signal, 1)
	// Notify the channel about SIGQUIT signals
	signal.Notify(sigChan, syscall.SIGQUIT)

	go func() {
		for range sigChan {
			// Allocate a buffer and write all goroutine stacks to it
			stacktrace := make([]byte, 1<<20)
			length := gruntime.Stack(stacktrace, true) // `true` means dump all goroutines
			for length >= len(stacktrace) {
				stacktrace = make([]byte, len(stacktrace)*2)
				length = gruntime.Stack(stacktrace, true)
			}
			fmt.Println("=== GOROUTINE STACK DUMP ===")
			fmt.Println(string(stacktrace[:length]))
			fmt.Println("=== END OF DUMP ===")
			// The program continues running after this point
		}
	}()

	// ... rest of your program ...
	fmt.Println("Program running, send SIGQUIT (Ctrl+\\\\) to dump stacks.")
}

func main() {
	setupSignals()
	var fin io.RuneReader
	var fd *os.File

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
		fmt.Printf("Wile Scheme %s\n", BuildVersion)
		os.Exit(0)
	}

	// Handle positional argument as file if --file not specified
	if opts.File == "" && len(args) > 0 {
		opts.File = args[0]
	}

	env, err0 := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
	if err0 != nil {
		Failf(err0, "Cannot create top-level environment")
	}

	// Initialize library registry with search paths and attach to environment
	registry := initLibraryRegistry(context.TODO())
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
	// interactive REPL using the repl package
	runREPL(ctx, env)
}

// runFile processes a Scheme file, exiting on errors
func runFile(ctx context.Context, env *environment.EnvironmentFrame, fin io.RuneReader, filename string) {
	p := parser.NewParserWithFile(env, true, fin, filename)
	stx, err := p.ReadSyntax(context.TODO())
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
		stx, err = p.ReadSyntax(context.TODO())
	}
	if !errors.Is(err, io.EOF) {
		Failf(err)
	}
}

// runREPL runs an interactive Read-Eval-Print Loop using the repl package
func runREPL(ctx context.Context, env *environment.EnvironmentFrame) {
	r := repl.New(env)
	if err := r.Run(ctx); err != nil {
		Failf(err, "REPL error")
	}
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
