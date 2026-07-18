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

import (
	"context"
	"sort"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// nilPin records one free identifier in one bootstrap macro's template that was
// snapshotted with no global pin and no local binding at macro-definition time.
type nilPin struct {
	macro  string
	freeID string
	// runtimeBound reports whether the name resolves in the RUNTIME phase once
	// bootstrap has finished. This is the discriminator: a nil pin on a name that
	// stays unbound is a special form (correctly nil); a nil pin on a name that a
	// later bootstrap define DOES bind is a load-order accident.
	runtimeBound bool
	expandBound  bool
	compileBound bool
}

// TestBootstrapMacrosPinLateBoundReferents is a ratchet against review finding C6.
//
// `unless`'s template referenced `not`, a bootstrap PROCEDURE that loaded after the
// macros. At macro-definition time the referent was unbound, so the free-id snapshot
// took a nil pin, resolution degraded to use-site, and a user (define not ...)
// captured the macro's own identifier (R7RS 4.3.2). Nothing else enforces "which
// bootstrap file does this macro belong in", and the failure is silent, so this test
// enumerates every syntax-rules macro in the expand frame, walks each clause's
// FreeIds, and fails on any Global == nil && !HasLocalBinding whose name is
// nevertheless bound in the runtime phase after bootstrap.
//
// The expand/compile phases do NOT fail the test, but NOT because they are safe.
// The original rationale here claimed a sibling-macro reference (and -> and,
// guard -> guard-aux) "resolves in operator position during expansion, where a
// runtime define cannot reach it". That is FALSE, and measurably so:
//
//	(guard (e (else 'caught)) (raise 'x))                          => caught
//	(define-syntax guard-aux (syntax-rules () ((_ r ...) 'PWNED)))
//	(guard (e (else 'caught)) (raise 'x))                          => PWNED
//
// A top-level define-syntax reaches the expand frame and overwrites the binding
// IN PLACE (same *Binding, new value), so `guard`'s reference to its private helper
// is captured by ordinary user code. No library or import is required.
//
// Pinning does not prevent this. A pin is a *Binding pointer, so an in-place value
// overwrite defeats it; verified by reordering guard-aux above guard, which does
// pin the reference (census drops to 46) and changes nothing about the capture.
// Runtime-phase referents like `not` survive only because they live in the SEALED
// BASE while a top-level define writes to the mutable runtime child, a different
// frame. There is no sealed base for the expand phase, so bootstrap macro bindings
// are mutable in place and pins into them are not load-bearing.
//
// The census therefore certifies bootstrap LOAD ORDER for runtime-phase referents
// and nothing more. It is not evidence that expand-phase referents are safe; the
// known-capturable name today is guard-aux. Do not widen this test to assert
// expand-phase safety without first carving an immutable base for the expand phase
// (or renaming the helper beyond user reach).
//
// Run with -v to see the full census, including the unbound entries.
func TestBootstrapMacrosPinLateBoundReferents(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()

	env := eng.Environment()
	expandEnv := env.Expand()

	pins := collectNilPins(t, env, expandEnv)

	sort.Slice(pins, func(i, j int) bool {
		if pins[i].macro != pins[j].macro {
			return pins[i].macro < pins[j].macro
		}
		return pins[i].freeID < pins[j].freeID
	})

	// Partition. A nil pin on a name that a later bootstrap define binds in the
	// runtime phase is the C6 signature. The remainder is NOT a clean bill of health:
	// it mixes genuinely inert entries (special forms, template-introduced binders,
	// syntax-rules literals) with expand-phase sibling references that this test
	// cannot see and that ARE capturable — see the doc comment on guard-aux.
	defects := make([]nilPin, 0, len(pins))
	unchecked := make([]nilPin, 0, len(pins))
	for _, p := range pins {
		if p.runtimeBound {
			defects = append(defects, p)
			continue
		}
		unchecked = append(unchecked, p)
	}

	t.Logf("nil-pin census: %d total, %d runtime-bound (DEFECT), %d not runtime-bound (UNCHECKED, not proven safe)",
		len(pins), len(defects), len(unchecked))

	for _, p := range defects {
		t.Errorf("macro %q pins free identifier %q as unbound, but %q IS bound in the runtime "+
			"phase after bootstrap (expand=%v compile=%v). The macro is defined before its "+
			"referent loads, so the reference degrades to use-site resolution and a user "+
			"(define %s ...) captures it. Move the macro after the definition (see "+
			"bootstrap_macros_late.scm) or move the definition earlier.",
			p.macro, p.freeID, p.freeID, p.expandBound, p.compileBound, p.freeID)
	}

	t.Logf("--- not runtime-bound: inert (special form / binder / literal) OR an expand-phase " +
		"sibling reference this test does not check (e.g. guard-aux, which IS capturable) ---")
	byName := map[string][]string{}
	for _, p := range unchecked {
		byName[p.freeID] = append(byName[p.freeID], p.macro)
	}
	names := make([]string, 0, len(byName))
	for n := range byName {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		t.Logf("  %-28s referenced by %d macro(s): %v", n, len(byName[n]), byName[n])
	}
}

// collectNilPins walks every BindingTypeSyntax global in expandEnv, digs the
// SyntaxRulesClause set out of the transformer closure's literal pool, and returns one
// entry per nil-pinned free identifier.
func collectNilPins(t *testing.T, env, expandEnv *environment.EnvironmentFrame) []nilPin {
	t.Helper()

	q := []nilPin{}
	ge := expandEnv.GlobalEnvironment()
	if ge == nil {
		t.Fatalf("expand frame has no global environment")
	}

	macroCount := 0
	for sym := range ge.Keys() {
		key := sym
		gi := ge.GetGlobalIndex(&key)
		if gi == nil {
			continue
		}
		bnd := ge.GetOwnGlobalBinding(gi)
		if bnd == nil || bnd.BindingType() != environment.BindingTypeSyntax {
			continue
		}
		clauses := clausesOf(bnd.Value())
		if clauses == nil {
			continue
		}
		macroCount++
		for _, cl := range clauses {
			for name, res := range cl.FreeIds {
				if res == nil {
					continue
				}
				if res.Global != nil || res.HasLocalBinding {
					continue
				}
				q = append(q, nilPin{
					macro:        key.Key,
					freeID:       name,
					runtimeBound: env.GetGlobalIndex(values.NewSymbol(name)) != nil,
					expandBound:  expandEnv.GetGlobalIndex(values.NewSymbol(name)) != nil,
					compileBound: env.AtPhase(environment.PhaseCompile).GetGlobalIndex(values.NewSymbol(name)) != nil,
				})
			}
		}
	}

	t.Logf("walked %d syntax-rules macros in the expand frame", macroCount)
	if macroCount == 0 {
		t.Fatalf("no syntax-rules macros found — the walk is broken, not the bootstrap")
	}
	return q
}

// clausesOf digs the *compilation.ClausesWrapper out of a transformer closure's literal
// pool. Returns nil for a syntax binding that is not a syntax-rules macro (a primitive
// expander or a syntax-case transformer), which the caller skips.
func clausesOf(v values.Value) []*compilation.SyntaxRulesClause {
	cl, ok := v.(*machine.MachineClosure)
	if !ok {
		return nil
	}
	tpl := cl.Template()
	if tpl == nil {
		return nil
	}
	for _, lit := range tpl.Literals() {
		w, ok := lit.(*compilation.ClausesWrapper)
		if ok {
			return w.Clauses
		}
	}
	return nil
}
