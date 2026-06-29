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

package core_test

// Generative fuzzing of the continuation machinery with a crash-oracle (item 3 of
// plans/2026-06-28-continuation-testing-enhancement.md).
//
// The two latest bugs in the reification+flip arc — a tail-promoted-op nil-deref and a
// runaway-loop hang — were both found by hand-poking unusual shapes. This generator does
// that exhaustively: it builds thousands of small, syntactically-valid, BOUNDED
// continuation programs from the grammar {literal, var, call/cc, dynamic-wind, guard,
// call-with-values, call-with-exit, parameterize, if, +, list, bounded multi-shot
// resume} and runs each under a hard timeout.
//
// The oracle is FAILURE-shaped, not value-shaped — no reference implementation needed:
//   - a Go-level panic ("recovered panic" / "invalid memory address" / "nil pointer"),
//   - a "no prompt found" continuation-routing failure, or
//   - a timeout (the VM wedged on legal input)
// is a BUG. A normal Scheme value, or a CATCHABLE Scheme error (a raised condition, a
// type error), is fine — the VM behaving correctly on a nonsensical-but-legal program.
// Deterministic silent-WRONG-value bugs are out of scope here (no reference oracle) —
// that net is the golden corpus + cross-scheme differential; this fuzzer's job is to
// surface crashes, routing failures, and hangs across shapes those can't enumerate.
//
// Boundedness: the only set! the grammar emits is the multi-shot production, which
// stores a captured continuation and re-invokes it under a fixed counter (the stored
// continuation is kept out of scope, so nothing else can re-invoke it unboundedly).
// Together with bounded depth every program terminates quickly, so a timeout still
// means a genuine VM hang, not a legitimately-infinite program. The generator also
// never emits abort-current-continuation or composable continuations to a foreign tag,
// so any "no prompt found" is a routing bug, not a user error. The seed is fixed, so
// any failure prints a one-line reproduction.

import (
	"context"
	"errors"
	"fmt"
	"math/rand"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

type contGen struct {
	rng *rand.Rand
	ctr int
}

func (g *contGen) fresh(prefix string) string {
	g.ctr++
	return fmt.Sprintf("%s%d", prefix, g.ctr)
}

// scopeVar is an in-scope binding; callable vars (call/cc continuations and
// call-with-exit escapes) may be invoked, others only referenced.
type scopeVar struct {
	name     string
	callable bool
}

var fuzzAtoms = []string{"1", "2", "3", "#t", "#f", "'sym", "'()", `"s"`}

func (g *contGen) atom(scope []scopeVar) string {
	// Sometimes reference an in-scope variable; if it is callable, sometimes invoke it
	// (an escape / resume) — the load-bearing stressor.
	if len(scope) > 0 && g.rng.Intn(2) == 0 {
		v := scope[g.rng.Intn(len(scope))]
		if v.callable && g.rng.Intn(2) == 0 {
			return "(" + v.name + " " + fuzzAtoms[g.rng.Intn(len(fuzzAtoms))] + ")"
		}
		return v.name
	}
	return fuzzAtoms[g.rng.Intn(len(fuzzAtoms))]
}

func (g *contGen) expr(depth int, scope []scopeVar) string {
	if depth <= 0 || g.rng.Intn(4) == 0 {
		return g.atom(scope)
	}
	switch g.rng.Intn(11) {
	case 0: // call/cc — introduces a callable continuation
		k := g.fresh("kc")
		body := g.expr(depth-1, append(scope, scopeVar{k, true}))
		return "(call/cc (lambda (" + k + ") " + body + "))"
	case 1: // dynamic-wind
		return "(dynamic-wind (lambda () " + g.expr(depth-1, scope) + ") (lambda () " +
			g.expr(depth-1, scope) + ") (lambda () " + g.expr(depth-1, scope) + "))"
	case 2: // guard
		e := g.fresh("ec")
		handler := g.expr(depth-1, append(scope, scopeVar{e, false}))
		return "(guard (" + e + " (#t " + handler + ")) " + g.expr(depth-1, scope) + ")"
	case 3: // call-with-values
		args := g.fresh("av")
		return "(call-with-values (lambda () " + g.expr(depth-1, scope) + ") (lambda " + args + " " +
			g.atom(append(scope, scopeVar{args, false})) + "))"
	case 4: // call-with-exit — introduces a callable escape
		e := g.fresh("ex")
		return "(call-with-exit (lambda (" + e + ") " + g.expr(depth-1, append(scope, scopeVar{e, true})) + "))"
	case 5: // raise (a catchable condition, not a crash)
		return "(raise " + fuzzAtoms[g.rng.Intn(len(fuzzAtoms))] + ")"
	case 6: // if
		return "(if " + g.expr(depth-1, scope) + " " + g.expr(depth-1, scope) + " " + g.expr(depth-1, scope) + ")"
	case 7: // values combinator
		return "(call-with-values (lambda () (values " + g.atom(scope) + " " + g.atom(scope) + ")) list)"
	case 8: // parameterize
		p := g.fresh("pc")
		return "(let ((" + p + " (make-parameter 0))) (parameterize ((" + p + " " + g.atom(scope) + ")) " +
			g.expr(depth-1, append(scope, scopeVar{p, false})) + "))"
	case 9: // multi-shot resume — store a call/cc continuation and re-invoke it a
		// BOUNDED number of times. `middle` (re-run on every shot) is where boundaries
		// get re-entered, so this exercises a continuation reinstated through reified
		// call-with-values / dynamic-wind / guard frames repeatedly. The stored kbox is
		// deliberately kept OUT of scope so only the counter drives re-invocation — the
		// program always terminates, and a timeout is still a genuine VM hang.
		kbox := g.fresh("mk")
		cnt := g.fresh("mn")
		limit := 2 + g.rng.Intn(2) // 2 or 3 shots
		middle := g.expr(depth-1, append(scope, scopeVar{cnt, false}))
		return fmt.Sprintf("(let ((%s #f) (%s 0)) "+
			"(call/cc (lambda (k) (set! %s k))) "+
			"%s "+
			"(set! %s (+ %s 1)) "+
			"(if (< %s %d) (%s #f)) "+
			"%s)",
			kbox, cnt, kbox, middle, cnt, cnt, cnt, limit, kbox, cnt)
	default: // list combinator (forces two sub-evaluations)
		return "(list " + g.expr(depth-1, scope) + " " + g.expr(depth-1, scope) + ")"
	}
}

// isVMCrash reports whether err is a VM-level failure (Go panic, routing failure, or
// hang) rather than a legitimate catchable Scheme condition.
func isVMCrash(err error) (string, bool) {
	if err == nil {
		return "", false
	}
	if errors.Is(err, context.DeadlineExceeded) {
		return "timeout/hang", true
	}
	msg := err.Error()
	for _, marker := range []string{"recovered panic", "invalid memory address", "nil pointer", "no prompt found"} {
		if strings.Contains(msg, marker) {
			return marker, true
		}
	}
	return "", false
}

// TestContinuationFuzz generates and runs a large batch of bounded continuation programs;
// none may crash the VM, route to a missing prompt, or hang. A failure prints the exact
// program for one-line reproduction. Deterministic seed -> reproducible.
func TestContinuationFuzz(t *testing.T) {
	const (
		seed     = int64(0xC0FFEE)
		programs = 2500
		maxDepth = 5
	)
	g := &contGen{rng: rand.New(rand.NewSource(seed))}
	crashes := 0
	multiShot := 0
	for i := range programs {
		code := g.expr(maxDepth, nil)
		// A stored-and-re-invoked continuation is the multi-shot resume stressor; the
		// grammar marks it by emitting set! (only the multi-shot production does).
		if strings.Contains(code, "set!") {
			multiShot++
		}
		_, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 8*time.Second)
		reason, bad := isVMCrash(err)
		if bad {
			crashes++
			t.Errorf("VM crash #%d [%s] on generated program:\n%s\nerr: %v", i, reason, code, err)
			if crashes >= 10 {
				t.Fatalf("stopping after %d crashes (seed=%#x)", crashes, seed)
			}
		}
	}
	// Guard the coverage the oracle depends on: if the grammar stops emitting multi-shot
	// programs, the crash oracle silently stops exercising re-invoked continuations (the
	// class that hid the sticky-isolatedMarks escalation bug).
	if multiShot == 0 {
		t.Errorf("generated 0 multi-shot (store-and-re-invoke) programs; the fuzzer is not exercising multi-shot resume")
	}
	t.Logf("fuzzed %d bounded continuation programs (%d multi-shot) (seed=%#x): no VM crash / no-prompt-found / hang", programs, multiShot, seed)
}
