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

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// FALSIFICATION PROBE (plan "Recommended immediate next action"): does the
// parameter-based handler model (validated 4/4 in handler_mark_prototype_test.go)
// break `guard`, AND is `guard`'s value-facet truncation introduced by the handler
// change or pre-existing today?
//
// `guard` wraps (call-with-values (lambda () body) list) INSIDE
// with-exception-handler (bootstrap_macros.scm:171,185). The open-problem doc
// (claim 3 / step 4 / Alley 3's grave) asserts the handler-on-mark change and the
// value-return-frame change (claim 1) are COUPLED through guard and "must land in
// one atomic change -- you cannot validate either half alone."
//
// FINDING (measured 2026-06-26, in-tree pipeline, GOWORK=off):
//   - G1..G3 PASS: the parameter handler model preserves guard's EXCEPTION semantics
//     (catch, return-value, re-raise-to-outer) with NO regression.
//   - C2 (real builtin guard) TRUNCATES a continuation captured in its body and
//     re-invoked after return -> "2", identical to C1 (raw call-with-values). So
//     guard's value-facet truncation is PRE-EXISTING in shipped guard TODAY.
//   - G4 (my-guard over the parameter handler) truncates IDENTICALLY -> "2" == C2.
//     The handler change introduces NO NEW truncation.
//
// CONCLUSION (initial, NARROW): for a SINGLE guard, claim-3-without-claim-1 does not
// regress guard, which read as "claim 3 is separable."
//
// CORRECTION (2026-06-26, after implementing claim 3): that reading was too narrow
// and is FALSIFIED. NESTED guards regress (TestCoverageExceptionReRaise): the inner
// guard runs in the outer guard's call-with-values PRODUCER sub-context, and the
// inner guard-k (call/cc) loses the outer handler mark on restore — a general
// marks-vs-call/cc-across-sub-context limitation (plain parameterize loses its value
// across the same shape). Continuation barriers also regress (in-place handler escape
// crosses the barrier). So claim 3 is NOT separable: it must land together with
// claim 1 (call-with-values producer inline) and barrier handling. See
// memory/2026-06-26-subcontext-continuation-the-open-problem.local.md (Progress +
// step 4) and plans/2026-06-26-exception-handler-capture-impl.local.md.
// The probes below remain valid as single-guard characterization.

// myGuardDef is a faithful copy of the bootstrap `guard`/`guard-aux` macros
// (bootstrap_macros.scm:166-202) with the handler primitives swapped for the
// parameter-based prototype forms (p-weh / p-raise-continuable from
// handlerMarkPrototype). call/cc and call-with-values are the real builtins -- the
// point is to exercise the exact guard shape over the new handler storage.
const myGuardDef = `
  (define-syntax my-guard
    (syntax-rules ()
      ((my-guard (var clause ...) e1 e2 ...)
       ((call/cc
          (lambda (guard-k)
            (p-weh
             (lambda (condition)
               ((call/cc
                  (lambda (handler-k)
                    (guard-k
                     (lambda ()
                       (let ((var condition))
                         (my-guard-aux
                          (lambda ()
                            (handler-k
                             (lambda ()
                               (p-raise-continuable condition))))
                          var clause ...))))))))
             (lambda ()
               (let ((results (call-with-values (lambda () e1 e2 ...) list)))
                 (lambda () (apply values results)))))))))))
  (define-syntax my-guard-aux
    (syntax-rules (else =>)
      ((my-guard-aux re-raise var (else result ...))
       (begin result ...))
      ((my-guard-aux re-raise var (test => proc) clause ...)
       (let ((t test))
         (if t (proc t) (my-guard-aux re-raise var clause ...))))
      ((my-guard-aux re-raise var (test result ...) clause ...)
       (if test (begin result ...) (my-guard-aux re-raise var clause ...)))
      ((my-guard-aux re-raise var)
       (re-raise))))
`

// runProtoGuard assembles: parameter-handler prelude + my-guard macros + probe,
// all in one body, run through the in-tree pipeline (current branch).
func runProtoGuard(t *testing.T, probe string) (string, error) {
	t.Helper()
	code := "(let ()\n" + handlerMarkPrototype + "\n" + myGuardDef + "\n" + probe + ")"
	result, err := testhelpers.RunSchemeCode(t, code)
	if err != nil {
		return "", err
	}
	return result.SchemeString(), nil
}

// --- Part 1: does the parameter model preserve guard's EXCEPTION semantics? ---

// G1: basic catch -- a raise inside my-guard's body is caught by a matching clause.
func TestGuardCoupling_BasicCatch(t *testing.T) {
	got, err := runProtoGuard(t, `(my-guard (e (#t (list 'caught e))) (p-raise 'boom))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "(caught boom)")
}

// G2: no exception -- my-guard returns the body value.
func TestGuardCoupling_NoException(t *testing.T) {
	got, err := runProtoGuard(t, `(my-guard (e (#t 'caught)) (+ 1 2))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "3")
}

// G3: no clause matches -> re-raise escalates to the OUTER handler in the dynamic
// extent of the original raise (R7RS §4.2.7). Exercises my-guard's re-raise path
// (handler-k + p-raise-continuable) over the parameter model.
func TestGuardCoupling_ReRaiseToOuter(t *testing.T) {
	got, err := runProtoGuard(t, `
(call/cc (lambda (done)
  (p-weh
    (lambda (e) (done (list 'outer e)))
    (lambda ()
      (my-guard (e ((eq? e 'wanted) 'got-wanted))
        (p-raise 'unwanted))))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "(outer unwanted)")
}

// --- Part 2: the value-facet coupling. Each probe is the SAME count-to-3 generator
// differing only in how the captured-continuation producer is wrapped. "Survives"
// => returns (finished (1 2 3)); truncation => anything else. ---

// generatorProbe builds the count-to-3 generator around a producer expression that
// captures k and computes (+ 1 captured).
func generatorProbe(wrap string) string {
	return `
(call/cc (lambda (done)
  (let ((k #f) (count 0) (trail '()))
    (define result ` + wrap + `)
    (set! trail (cons result trail))
    (set! count (+ count 1))
    (if (< count 3)
        (k count)
        (done (list 'finished (reverse trail)))))))`
}

// C0 (positive control): plain call/cc, no boundary. MUST survive -- proves the
// generator harness correctly detects the "survives" outcome.
func TestGuardCoupling_C0_PlainSurvives(t *testing.T) {
	got, err := runProtoGuard(t, generatorProbe(`(+ 1 (call/cc (lambda (c) (set! k c) 0)))`))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "(finished (1 2 3))")
}

// fullTrail is the count-to-three generator's correct trace. Before the
// boundary-reification cluster these probes truncated to "2" (the producer ran in a
// sub-context, so a k captured inside it dropped the consumer + set!/if/done tail on
// re-entry); reifying call-with-values / call-with-exit as continuation-chain frames
// keeps that tail on the captured chain, so the generator now counts to three. These
// three cells are the permanent regression guards for that fix.
const fullTrail = "(finished (1 2 3))"

// C1: raw call-with-values, producer captures k — the producer-in-sub-context
// truncation is fixed; the generator reaches the full trace.
func TestGuardCoupling_C1_RawCallWithValues(t *testing.T) {
	got, err := runProtoGuard(t, generatorProbe(
		`(call-with-values (lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0)))) (lambda (x) x))`))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, fullTrail)
}

// C2: the REAL builtin guard (call-with-exit over call-with-values) survives
// identically to C1 once both boundaries are reified.
func TestGuardCoupling_C2_RealGuard(t *testing.T) {
	got, err := runProtoGuard(t, generatorProbe(
		`(guard (e (#t e)) (+ 1 (call/cc (lambda (c) (set! k c) 0))))`))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, fullTrail)
}

// G4: my-guard over the parameter handler survives identically to the real builtin
// guard (C2) — the handler facet adds no truncation.
func TestGuardCoupling_G4_MyGuard(t *testing.T) {
	got, err := runProtoGuard(t, generatorProbe(
		`(my-guard (e (#t e)) (+ 1 (call/cc (lambda (c) (set! k c) 0))))`))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, fullTrail)
}
