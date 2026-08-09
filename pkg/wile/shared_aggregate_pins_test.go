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

// Pins for review 2026-08-07 wave 2, item 7 — the accepted deviation that a
// Scheme program with the threads capability can fault the host by mutating a
// shared aggregate concurrently.
//
// These four are RATCHETS, not must-fail-first gates, and that is structural:
// item 7 closes no code defect, it scopes a claim. 3a passes by construction,
// 3b passes because the reach is real and decided (design §8 Q3 = Keep), 3c's
// detector fires today because no test starts two threads against a shared
// aggregate at all, and 3d is a canary calibrated to pass now and to fail when
// the immutability plan's Phase 2 removes prim_vectors.go:77's
// ImmutableLiterals().IsImmutable(v) — the sync.Map load that is currently the
// only synchronisation anywhere on the vector-set! path.
//
// Everything touching values.Vector goes through the METHOD surface
// (NewVector/Get/Set/Length), never slice-shaped: that plan's Phase 2 converts
// Vector to a struct and compile-errors every slice-shaped use while method
// sites survive untouched. Both threaded arms use an UNTIMED thread-join!, so
// wave 3 item 16's blocking-wait rewrite does not have to touch them.

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// ---------------------------------------------------------------------------
// 3a — no profile below KitchenSink reaches make-thread.
// ---------------------------------------------------------------------------

// TestPin3aThreadsAreKitchenSinkOnly is structural: threads.Extension has
// exactly one production site, bootstrap.go:62 inside allExtensions, and the
// tiny/console/console-with-load/small arms of ProfileExtensions do not name
// it. This passes by construction and keeps passing while the reach 3b pins
// stays wide open — which is exactly why 3a alone does not guard the defect.
func TestPin3aThreadsAreKitchenSinkOnly(t *testing.T) {
	for _, profile := range []string{"tiny", "console", "console-with-load", "small"} {
		exts, err := bootstrap.ProfileExtensions(profile)
		qt.Assert(t, err, qt.IsNil)
		for _, ext := range exts {
			qt.Assert(t, ext.Name(), qt.Not(qt.Equals), threads.Extension.Name(),
				qt.Commentf("profile %q registers the threads extension", profile))
		}
	}

	kitchen, err := bootstrap.ProfileExtensions("kitchen-sink")
	qt.Assert(t, err, qt.IsNil)
	found := false
	for _, ext := range kitchen {
		if ext.Name() == threads.Extension.Name() {
			found = true
		}
	}
	qt.Assert(t, found, qt.IsTrue,
		qt.Commentf("kitchen-sink no longer registers threads; 3b's reach argument is stale"))
}

// ---------------------------------------------------------------------------
// 3b — the reach the prose now admits to, asserted as the DECIDED answer.
// ---------------------------------------------------------------------------

// TestPin3bNoAuthorizerSmallEngineReachesThreads asserts the escalation, not
// its absence. A Small engine registers no threads extension, but installs no
// authorizer either, so checkProfileWidening's auth == nil arm allows
// (environment '(wile kitchen-sink)) and make-thread is reachable through it.
//
// Design §8 Q3's standing answer is Keep, so this is the decided behaviour and
// a future re-decision must flip this test rather than pass silently.
func TestPin3bNoAuthorizerSmallEngineReachesThreads(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.Small))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	// make-thread is unbound at the engine's own top level ...
	_, err = eng.EvalMultiple(ctx, "(procedure? make-thread)")
	qt.Assert(t, err, qt.Not(qt.IsNil),
		qt.Commentf("Small now binds make-thread ambiently; 3a is stale"))

	// ... and reachable anyway, through the widened namespace.
	result, err := eng.EvalMultiple(ctx,
		"(eval '(thread? (make-thread (lambda () 42))) (environment '(wile kitchen-sink)))")
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("the no-authorizer widening path closed; SECURITY.md's carve-out and "+
			"profile_containment.go's case 1 both need re-deciding, not just re-wording"))
	qt.Assert(t, result.SchemeString(), qt.Equals, "#t")
}

// TestPin3bRestrictiveProfilesResolveAnAuthorizer is the archive's preferred
// hardening (memory/2026-06-04-security-audit-remediation.local.md:140-142),
// taken in the same sitting and in the opposite direction: under a profile
// that does install a policy, the privileged sink resolves a non-nil
// authorizer and the same widening request is refused.
func TestPin3bRestrictiveProfilesResolveAnAuthorizer(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.ConsoleWithLoad))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	_, err = eng.EvalMultiple(ctx,
		"(eval '(thread? (make-thread (lambda () 42))) (environment '(wile kitchen-sink)))")
	qt.Assert(t, err, qt.Not(qt.IsNil),
		qt.Commentf("a ConsoleWithLoad engine widened to kitchen-sink; its authorizer "+
			"was either nil or never consulted"))
	qt.Assert(t, errors.Is(err, bootstrap.ErrProfileWidensEngine), qt.IsTrue,
		qt.Commentf("refusal did not come from the profile-widening gate: %v", err))
}

// ---------------------------------------------------------------------------
// 3c / 3d — the shared-aggregate arms, run in a re-exec'd child.
// ---------------------------------------------------------------------------

// aggregateCaseEnv selects the payload in the child. The child mechanism is
// host_crash_subprocess_test.go's: a fault here is a Go fatal error, so it
// cannot be asserted in-process — the test binary IS the host that dies.
const aggregateCaseEnv = "WILE_SHARED_AGGREGATE_CASE"

// aggregateIterEnv carries N to the child.
const aggregateIterEnv = "WILE_SHARED_AGGREGATE_N"

// pin3dIterations is the N that 3d passes at, recorded so the IM-Phase-2
// comparison has a number. Two SRFI-18 threads, each performing N
// vector-set!/vector-ref pairs on one shared vector with values of two
// different Go dynamic types: 4,000,000 unsynchronised stores in ~0.4s,
// 5/5 clean at 003b3353 on darwin/arm64.
const pin3dIterations = 2000000

// pin3cIterations is smaller: the detector fires on the first conflicting
// access pair, and -race instrumentation is roughly an order of magnitude
// slower.
const pin3cIterations = 2000

// sharedAggregateCases names the payloads. The review's §8 identifies the
// class as values/vector.go:48 (Vector.Set's plain (*p)[i] = value) AND
// machine/parameter.go:80 (Parameter.SetValue's plain p.value = v), so the
// arms are parameterised over both rather than written once for Vector.
var sharedAggregateCases = []string{"scheme-vector-set", "values-vector-set", "machine-parameter-set"}

// runAggregateChild re-execs this test binary running only the helper, with
// the named payload selected, and returns its combined output and exit error.
func runAggregateChild(t *testing.T, name string, iterations int) (string, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 120*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestSharedAggregateHelper$", "-test.v")
	cmd.Env = append(os.Environ(),
		aggregateCaseEnv+"="+name,
		aggregateIterEnv+"="+strconv.Itoa(iterations))
	out, err := cmd.CombinedOutput()
	qt.Assert(t, ctx.Err(), qt.IsNil,
		qt.Commentf("child %q did not terminate within the timeout\n%s", name, out))
	return string(out), err
}

// TestPin3cRaceDetectorFiresOnSharedAggregate is the -race arm. It asserts the
// deviation is real: two threads mutating one shared aggregate with no
// synchronisation is a data race, on every aggregate in the class.
func TestPin3cRaceDetectorFiresOnSharedAggregate(t *testing.T) {
	if !raceDetectorEnabled {
		t.Skip("3c is the -race arm; run under make test-race")
	}
	for _, name := range sharedAggregateCases {
		t.Run(name, func(t *testing.T) {
			out, err := runAggregateChild(t, name, pin3cIterations)
			qt.Assert(t, strings.Contains(out, "WARNING: DATA RACE"), qt.IsTrue,
				qt.Commentf("the race detector did not fire on %q; the aggregate acquired "+
					"synchronisation, or the payload stopped sharing\n%s", name, out))
			qt.Assert(t, err, qt.Not(qt.IsNil),
				qt.Commentf("child reported a race but exited zero\n%s", out))
		})
	}
}

// TestPin3dHostSurvivesSharedVectorMutation is the non-race arm and the
// instrument for the immutability plan's Phase 2. It runs the Scheme
// vector-set! shape — the one whose only synchronisation is the sync.Map load
// at prim_vectors.go:77 — for pin3dIterations and asserts the host survives.
//
// A failure here is not "the test broke". It means a change made the tear
// materially more likely, which is the whole reason the canary exists.
func TestPin3dHostSurvivesSharedVectorMutation(t *testing.T) {
	if raceDetectorEnabled {
		t.Skip("3d is the non-race arm; under -race the detector reports this shape by design")
	}
	out, err := runAggregateChild(t, "scheme-vector-set", pin3dIterations)
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("child exited non-zero after %d iterations\n%s", pin3dIterations, out))
	qt.Assert(t, strings.Contains(out, "fatal error:"), qt.IsFalse,
		qt.Commentf("host hit an unrecoverable Go fatal error\n%s", out))
	qt.Assert(t, strings.Contains(out, "unexpected fault address"), qt.IsFalse,
		qt.Commentf("host took a SIGBUS on a torn two-word Value store\n%s", out))
}

// TestSharedAggregateHelper is the child. It runs exactly one payload and is a
// no-op when run directly, which is what keeps `go test ./...` from executing
// an unsynchronised payload in the parent's own process.
func TestSharedAggregateHelper(t *testing.T) {
	name := os.Getenv(aggregateCaseEnv)
	if name == "" {
		t.Skip("helper process; driven by the 3c/3d parents")
	}
	iterations, err := strconv.Atoi(os.Getenv(aggregateIterEnv))
	qt.Assert(t, err, qt.IsNil)

	switch name {
	case "scheme-vector-set":
		raceSchemeVectorSet(t, iterations)
	case "values-vector-set":
		raceValuesVectorSet(iterations)
	case "machine-parameter-set":
		raceMachineParameterSet(iterations)
	default:
		t.Fatalf("unknown shared-aggregate case %q", name)
	}
}

// raceSchemeVectorSet drives the deviation the way an embedder's guest script
// would: two SRFI-18 threads, one shared vector, values of two different Go
// dynamic types, an untimed thread-join!.
func raceSchemeVectorSet(t *testing.T, iterations int) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	src := `
      (define shared (make-vector 1 0))
      (define (churn value n)
        (make-thread
          (lambda ()
            (let loop ((k 0))
              (if (< k n)
                  (begin
                    (vector-set! shared 0 value)
                    (vector-ref shared 0)
                    (loop (+ k 1)))
                  'done)))))
      (define a (churn 1 ` + strconv.Itoa(iterations) + `))
      (define b (churn "x" ` + strconv.Itoa(iterations) + `))
      (thread-start! a)
      (thread-start! b)
      (thread-join! a)
      (thread-join! b)
      'ok`
	result, err := eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "ok")
}

// raceValuesVectorSet pins values/vector.go:48 directly, through the method
// surface only.
func raceValuesVectorSet(iterations int) {
	shared := values.NewVector(values.NewInteger(0))
	writes := []values.Value{values.NewInteger(1), values.NewString("x")}
	var wg sync.WaitGroup
	for _, v := range writes {
		wg.Add(1)
		go func(v values.Value) {
			defer wg.Done()
			for range iterations {
				_ = shared.Set(0, v)
				_ = shared.Get(0)
			}
		}(v)
	}
	wg.Wait()
	_ = shared.Length()
}

// raceMachineParameterSet pins machine/parameter.go:80, the other member of
// the class. (76b73ccf moved this store from the report's parameter.go:55 by
// giving Parameter a base ParameterBase field.)
func raceMachineParameterSet(iterations int) {
	shared := machine.NewParameter(values.NewInteger(0), nil, machine.MutableBase)
	writes := []values.Value{values.NewInteger(1), values.NewString("x")}
	var wg sync.WaitGroup
	for _, v := range writes {
		wg.Add(1)
		go func(v values.Value) {
			defer wg.Done()
			for range iterations {
				shared.SetValue(v)
				_ = shared.Value()
			}
		}(v)
	}
	wg.Wait()
}
