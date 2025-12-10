package main

import (
	"bufio"
	"context"
	"errors"
	"flag"
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
)

var (
	paths       []string
	fnm         string
	libraryPath string
)

const (
	// SchemeLibraryPathEnv is the environment variable for library search paths
	SchemeLibraryPathEnv = "SCHEME_LIBRARY_PATH"
)

func init() {
	flag.StringVar(&fnm, "file", "", "File to include")
	flag.StringVar(&libraryPath, "L", "", "Library search path (colon-separated, prepended to search paths)")
}

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
	mc := machine.NewMachineContext(cont)
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
func initLibraryRegistry() *machine.LibraryRegistry {
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
	if libraryPath != "" {
		for _, p := range strings.Split(libraryPath, string(os.PathListSeparator)) {
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
	flag.Parse()
	env, err0 := runtime.NewTopLevelEnvironmentFrameTiny()
	if err0 != nil {
		Failf(err0, "Cannot create top-level environment")
	}

	// Initialize library registry with search paths and attach to environment
	registry := initLibraryRegistry()
	env.SetLibraryRegistry(registry)

	// Set up the library environment factory (avoids import cycle)
	machine.LibraryEnvFactory = runtime.NewTopLevelEnvironmentFrameTiny
	// read evaluate loop
	ctx := context.Background()
	// include file if any
	if fnm != "" {
		var err1 error
		log.Printf("reading file %q", fnm)
		fd, err1 = os.Open(fnm)
		if err1 != nil {
			Failf(err1, "Cannot open file %s", fnm)
		}
		fin = bufio.NewReader(fd)
		runFile(ctx, env, fin)
		return
	}
	// interactive REPL
	runREPL(ctx, env)
}

// runFile processes a Scheme file, exiting on errors
func runFile(ctx context.Context, env *environment.EnvironmentFrame, fin io.RuneReader) {
	p := parser.NewParser(env, fin)
	stx, err := p.ReadSyntax()
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
		stx, err = p.ReadSyntax()
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
	defer rl.Close()

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

		// Accumulate input
		if inputBuffer.Len() > 0 {
			inputBuffer.WriteString("\n")
		}
		inputBuffer.WriteString(line)

		// Try to parse the accumulated input
		input := inputBuffer.String()
		rdr := strings.NewReader(input)
		p := parser.NewParser(env, rdr)

		stx, parseErr := p.ReadSyntax()
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

		// Run
		mv, runErr := run(ctx, tpl, env)
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

		stx, parseErr := p.ReadSyntax()
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
	os.Stdout.Sync()
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
