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

// stack-trace demonstrates the VM stack trace captured when a runtime error
// escapes to the embedder, with emphasis on the sub-context spanning added in
// CaptureStackTrace (the "<foreign-call boundary>" frame).
//
// A trace is built by walking the continuation chain. Two things make the
// output worth studying:
//
//  1. Tail calls leave no frame. Wile is properly tail-recursive, so a chain of
//     tail calls collapses to a single frame. To see depth you need non-tail
//     calls — here each call is wrapped in (+ 1 _) so the caller's frame must
//     survive to receive the return value.
//
//  2. Go primitives that call back into Scheme (a parameter converter, eval,
//     an exception thunk) run that Scheme in a *sub-context*. The walk hops the
//     Go boundary via parentMC and inserts a synthetic "<foreign-call boundary>"
//     frame at the crossing, so the outer Scheme frames that led into the Go
//     primitive are not lost.
//
// Both programs below are wrapped in a single (begin ...) so they parse as one
// expression: that keeps every define and the call site live on one continuation
// chain. (EvalMultiple* compiles each top-level form independently, which would
// drop the very parent frames this example is about.)
//
// Run with: go run ./examples/embedding/stack-trace/main.go
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
	// 1. Single-context trace: three non-tail frames, no boundary.
	//
	//    a -> b -> c, each non-tail; c calls (car x) on a number and errors.
	//    The trace reads c, b, a — innermost first — all in one context.
	// -----------------------------------------------------------------------
	fmt.Println("--- 1. Single context (non-tail calls): plain 3-frame trace ---")

	singleContext := `(begin
  (define (a x) (+ 1 (b x)))
  (define (c x) (+ 1 (car x)))   ; car on a number -> error
  (define (b x) (+ 1 (c x)))
  (a 42))`

	printTrace(ctx, engine, singleContext, "calls.scm")

	// -----------------------------------------------------------------------
	// 2. Boundary-spanning trace: Scheme -> Go primitive -> Scheme.
	//
	//    make-parameter runs its converter (deep) inside a sub-context. deep
	//    errors, and the trace captured there hops parentMC back out to the
	//    Scheme frame (mid) that invoked make-parameter, inserting a
	//    "<foreign-call boundary>" frame at the Go crossing.
	// -----------------------------------------------------------------------
	fmt.Println("--- 2. Across a Go primitive (make-parameter converter): boundary frame ---")

	boundary := `(begin
  (define (deep x) (+ 1 (car x)))     ; converter body; errors on a number
  (define (mid v) (+ 1 (make-parameter v deep)))
  (mid 7))`

	printTrace(ctx, engine, boundary, "converter.scm")
}

// printTrace evaluates a single (begin ...) expression expected to raise, and
// prints the VM stack trace carried by the resulting RuntimeError. The source
// name is attached via ParseWithSource so each frame reports a file:line:column.
func printTrace(ctx context.Context, engine *wile.Engine, code string, source string) {
	expr, err := engine.ParseWithSource(ctx, code, source)
	if err != nil {
		log.Fatal(err)
	}

	_, err = engine.Eval(ctx, expr)

	var rtErr *wile.RuntimeError
	if !errors.As(err, &rtErr) {
		fmt.Printf("  (expected a runtime error, got: %v)\n\n", err)
		return
	}

	fmt.Printf("  error: %s\n", rtErr.Condition.SchemeString())
	if rtErr.Source != "" {
		fmt.Printf("  raised at: %s\n", rtErr.Source)
	}
	if rtErr.StackTrace != "" {
		fmt.Printf("%s\n", rtErr.StackTrace)
	}
}
