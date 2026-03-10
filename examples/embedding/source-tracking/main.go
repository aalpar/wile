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

// source-tracking demonstrates the WithSource API for embedding Wile
// with per-operation source location tracking.
//
// When Scheme code is evaluated with EvalWithSource, EvalMultipleWithSource,
// or CompileWithSource, the source name (typically a filename) is attached
// to every parsed expression. If a runtime error occurs, the RuntimeError
// includes the source location and a VM stack trace.
//
// Run with: go run ./examples/embedding/source-tracking/main.go
package main

import (
	"context"
	"errors"
	"fmt"
	"log"

	"github.com/aalpar/wile"
)

func main() {
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	if err != nil {
		log.Fatal(err)
	}

	// -----------------------------------------------------------------------
	// 1. EvalWithSource: error messages include the source name
	// -----------------------------------------------------------------------
	fmt.Println("--- EvalWithSource: error with source location ---")

	_, err = engine.EvalWithSource(ctx,
		`(error "something went wrong" 42)`,
		"config.scm")

	var rtErr *wile.RuntimeError
	if errors.As(err, &rtErr) {
		fmt.Printf("  Source:    %s\n", rtErr.Source)
		fmt.Printf("  Message:   %s\n", rtErr.Message)
		fmt.Printf("  Condition: %s\n", rtErr.Condition.SchemeString())
		if rtErr.StackTrace != "" {
			fmt.Printf("  Stack:\n%s\n", rtErr.StackTrace)
		}
	}
	fmt.Println()

	// -----------------------------------------------------------------------
	// 2. EvalMultipleWithSource: multiple expressions, one source
	// -----------------------------------------------------------------------
	fmt.Println("--- EvalMultipleWithSource: define + call ---")

	result, err := engine.EvalMultipleWithSource(ctx, `
		(define (greet name)
		  (string-append "Hello, " name "!"))
		(greet "Wile")
	`, "greeter.scm")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("  Result: %s\n", result.SchemeString())
	fmt.Println()

	// -----------------------------------------------------------------------
	// 3. CompileWithSource: compile once with source, run multiple times
	// -----------------------------------------------------------------------
	fmt.Println("--- CompileWithSource: compile once, run many ---")

	_, err = engine.EvalWithSource(ctx,
		`(define n 0)`,
		"setup.scm")
	if err != nil {
		log.Fatal(err)
	}

	compiled, err := engine.CompileWithSource(ctx,
		`(* n n)`,
		"compute.scm")
	if err != nil {
		log.Fatal(err)
	}

	for _, val := range []int64{3, 7, 11} {
		err = engine.Define("n", wile.NewInteger(val))
		if err != nil {
			log.Fatal(err)
		}
		result, err = engine.Run(ctx, compiled)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("  n=%d: (* n n) => %s\n", val, result.SchemeString())
	}
	fmt.Println()

	// -----------------------------------------------------------------------
	// 4. Different sources produce distinct error locations
	// -----------------------------------------------------------------------
	fmt.Println("--- Different sources produce distinct locations ---")

	sources := []struct {
		name string
		code string
	}{
		{"validation.scm", `(error "invalid input")`},
		{"policy.scm", `(error "access denied")`},
	}

	for _, src := range sources {
		_, err = engine.EvalWithSource(ctx, src.code, src.name)
		if errors.As(err, &rtErr) {
			fmt.Printf("  [%s] %s\n", rtErr.Source, rtErr.Condition.SchemeString())
		}
	}
	fmt.Println()

	// -----------------------------------------------------------------------
	// 5. Without source: errors show no file location
	// -----------------------------------------------------------------------
	fmt.Println("--- Without source: no file in error ---")

	_, err = engine.Eval(ctx, `(error "anonymous error")`)
	if errors.As(err, &rtErr) {
		if rtErr.Source == "" {
			fmt.Println("  Source is empty (no file tracked)")
		}
		fmt.Printf("  Condition: %s\n", rtErr.Condition.SchemeString())
	}
}
