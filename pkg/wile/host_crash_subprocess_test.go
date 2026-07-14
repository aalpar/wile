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

package wile_test

// Host-crash regression suite.
//
// The defects covered here are Go *fatal errors* — "stack overflow", "concurrent
// map read and map write", out-of-memory — not panics. recover() cannot catch a
// fatal error; the runtime prints it and kills the process. So a test that says
// "this no longer kills the host" cannot be written as an in-process assertion:
// if the defect is present, the assertion never runs, because the test binary
// itself is the host that dies.
//
// The harness below re-execs the test binary as a child with a single test
// selected and WILE_CRASH_CHILD set. The child runs the payload; the parent
// asserts the child exited cleanly. A regression turns into a non-zero exit and
// a signal, which IS observable, instead of taking the whole run down with it.
//
// Every payload drives the public pkg/wile.Engine API, because that is the
// contract an embedder depends on.

import (
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// crashChildEnv gates the payload. Absent, a payload test re-execs itself;
// present, it runs the payload for real.
const crashChildEnv = "WILE_CRASH_CHILD"

// runsInChild reports whether this process is the re-exec'd child.
func runsInChild() bool {
	return os.Getenv(crashChildEnv) == "1"
}

// requireCleanChildRun re-execs the calling test in a child process and asserts
// the child completed without dying. It returns true in the parent (meaning:
// the assertion has been made, stop here) and false in the child (meaning: run
// the payload).
//
// A defect regression manifests as a fatal error, which kills the child with a
// non-zero exit and leaves "fatal error:" on stderr. Both are asserted, because
// a payload that failed an ordinary assertion also exits non-zero, and the two
// must not be confused with each other.
func requireCleanChildRun(t *testing.T) bool {
	t.Helper()
	if runsInChild() {
		return false
	}

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^"+t.Name()+"$", "-test.v")
	cmd.Env = append(os.Environ(), crashChildEnv+"=1")
	out, err := cmd.CombinedOutput()
	text := string(out)

	qt.Assert(t, ctx.Err(), qt.IsNil,
		qt.Commentf("child did not terminate within the timeout — the payload is "+
			"looping or exhausting memory, which is the very defect under test.\n%s", text))
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("child exited non-zero; the host crashed or the payload's own "+
			"assertions failed.\n%s", text))
	qt.Assert(t, strings.Contains(text, "fatal error:"), qt.IsFalse,
		qt.Commentf("child hit an unrecoverable Go fatal error.\n%s", text))
	return true
}

// evalInFreshEngine runs src on a new KitchenSink engine and returns whatever
// the engine reports. A host crash never reaches the return: the process dies.
func evalInFreshEngine(t *testing.T, src string) error {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()
	_, err = eng.EvalMultiple(ctx, src)
	return err
}

// TestHostCrash_CyclicRecordEqual pins reviews/2026-07-13 record.go:100.
// equal? on a self-referential record recursed on the Go stack until the host
// died. Equal is now iterative and its visited set closes the cycle.
func TestHostCrash_CyclicRecordEqual(t *testing.T) {
	if requireCleanChildRun(t) {
		return
	}
	err := evalInFreshEngine(t, `
		(define-record-type <node> (make-node next) node? (next node-next set-node-next!))
		(define a (make-node #f))
		(define b (make-node #f))
		(set-node-next! a a)
		(set-node-next! b b)
		(display (equal? a a))
		(display (equal? a b))
	`)
	qt.Assert(t, err, qt.IsNil)
}

// TestHostCrash_CyclicHashtableEqual pins reviews/2026-07-13 hashtable.go:62.
func TestHostCrash_CyclicHashtableEqual(t *testing.T) {
	if requireCleanChildRun(t) {
		return
	}
	err := evalInFreshEngine(t, `
		(define h (make-hashtable))
		(hashtable-set! h 'self h)
		(display (equal? h h))
	`)
	qt.Assert(t, err, qt.IsNil)
}

// TestHostCrash_CyclicBoxWrite pins reviews/2026-07-13 box.go:64. A cycle
// reachable through a Box overflowed the host stack in every writer verb; the
// bound is a datum label (write/display) or "..." (write-simple).
func TestHostCrash_CyclicBoxWrite(t *testing.T) {
	if requireCleanChildRun(t) {
		return
	}
	err := evalInFreshEngine(t, `
		(define b (box 1))
		(set-box! b b)
		(write b)
		(newline)
		(display b)
		(newline)
		(write-simple b)
		(newline)
		(write-shared b)
		(newline)
	`)
	qt.Assert(t, err, qt.IsNil)
}

// TestHostCrash_CircularApply pins reviews/2026-07-13 machine_context.go:459.
// apply on a circular list spread elements onto the eval stack forever, ignoring
// ctx, maxStackSize and maxCallDepth, and OOM'd the host. It must now report an
// error instead.
func TestHostCrash_CircularApply(t *testing.T) {
	if requireCleanChildRun(t) {
		return
	}
	err := evalInFreshEngine(t, `
		(define xs (list 1 2 3))
		(set-cdr! (cddr xs) xs)
		(apply + xs)
	`)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("apply over a circular list must be an error, not an OOM"))
}

// TestHostCrash_DeepCarNestEqual pins the traversal owner itself: a car-nest far
// deeper than the Go stack tolerates. This is the case that recursion could not
// survive at any depth bound, and the one Equal's heap worklist exists for.
func TestHostCrash_DeepCarNestEqual(t *testing.T) {
	if requireCleanChildRun(t) {
		return
	}
	err := evalInFreshEngine(t, `
		(define (nest n acc) (if (= n 0) acc (nest (- n 1) (list acc))))
		(define a (nest 1000000 '()))
		(define b (nest 1000000 '()))
		(display (equal? a b))
	`)
	qt.Assert(t, err, qt.IsNil)
}
