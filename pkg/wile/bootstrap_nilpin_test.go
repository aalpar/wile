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
	"github.com/aalpar/wile/pkg/internal/match"
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

// TestBootstrapMacrosPinLateBoundReferents is a load-order ratchet for bootstrap macro
// free-identifier references, across BOTH the runtime and expand phases.
//
// `unless`'s template referenced `not`, a bootstrap PROCEDURE that loaded after the
// macros. At macro-definition time the referent was unbound, so the free-id snapshot
// took a nil pin (SyntaxRulesClause.FreeIds[name].Global == nil), resolution degraded to
// use-site, and a user (define not ...) captured the macro's own identifier (R7RS 4.3.2).
// The analogous expand-phase case was `guard` -> `guard-aux`: a top-level
// (define-syntax guard-aux …) captured guard's private helper.
//
// Both are now closed, so this test can certify both phases (updated 2026-07-22, the
// free-template-id-hygiene arc — plans/2026-07-22-free-template-id-hygiene-impl.local.md):
//
//   - D1 carved a sealed EXPAND base (Namespace.SealedExpandBase, phase 1): bootstrap
//     macros/expanders live there, immutable, so a top-level define-syntax shadows in the
//     mutable expand child instead of overwriting the pinned binding in place.
//   - D2 taught macro dispatch (lookupMacroBinding) to consult the pin, after the local
//     let-syntax arm and before the NextPhase/library arms.
//   - D0 reordered the sibling helpers (guard-aux, define-record-type-impl) above their
//     referencers so a DIRECT reference's pin is non-nil at macro-definition time.
//   - pinTemplateSelfReferences back-patches a template's reference to the macro's OWN name
//     (a recursive macro's self-reference — guard-aux -> guard-aux, and -> and) to the
//     just-created binding, which reorder alone cannot pin (a macro precedes itself nowhere).
//
// So a pin into a bootstrap macro/expander IS now load-bearing. The census walks every
// syntax-rules macro in the SEALED EXPAND base, walks each clause's FreeIds (keyed by
// FreeIdKey = scope-fingerprint|name — recover the bare name with match.FreeIdName before the
// discriminator lookups, or every lookup misses and the partition is vacuous), and FAILS on
// any Global == nil && !HasLocalBinding whose bare name is bound in the runtime phase (a late
// bootstrap procedure) OR in the expand phase (an unpinned sibling macro/expander or recursive
// self-reference — a real capture exposure). A neither-bound nil-pin is genuinely inert (a
// special form the expander handles, a template-introduced binder, or a syntax-rules literal
// like else/=>).
//
// Fixing a flagged entry: reorder the referent's definition above the referencing macro
// (sibling macro/expander) or into an earlier bootstrap file (runtime procedure). A
// precisely-justified exclusion (e.g. an auxiliary-syntax literal, or a referent bound only
// under a fuller profile) must be narrow and commented, not a broad skip.
//
// Run with -v to see the full census, including the inert entries.
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
	// D1 (2026-07-22): bootstrap macros now live in the sealed EXPAND base (phase 1),
	// not the mutable expand child (env.Expand()), which holds only user define-syntax.
	// Walk the sealed expand base so the census still sees the bootstrap macros.
	expandEnv := env.Namespace().SealedExpandBase()

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
		if p.runtimeBound || p.expandBound {
			defects = append(defects, p)
			continue
		}
		unchecked = append(unchecked, p)
	}

	t.Logf("nil-pin census: %d total, %d runtime/expand-bound (DEFECT), %d neither (inert: special form / binder / literal)",
		len(pins), len(defects), len(unchecked))

	for _, p := range defects {
		t.Errorf("macro %q pins free identifier %q as unbound, but %q IS bound after bootstrap "+
			"(runtime=%v expand=%v compile=%v). The macro is defined before its referent loads, "+
			"so the reference degrades to use-site resolution and a user (define %s ...) or "+
			"(define-syntax %s ...) captures it. Move the referent's definition earlier (for a "+
			"runtime procedure see bootstrap_macros_late.scm; for a sibling macro/expander "+
			"reorder it above this macro, as guard-aux was above guard).",
			p.macro, p.freeID, p.freeID, p.runtimeBound, p.expandBound, p.compileBound, p.freeID, p.freeID)
	}

	t.Logf("--- neither runtime- nor expand-bound: genuinely inert (special form handled by the " +
		"expander, template-introduced binder, or syntax-rules literal like else/=>) ---")
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
		clauses := compilation.ClausesFromClosure(bnd.Value())
		if clauses == nil {
			continue
		}
		macroCount++
		for _, cl := range clauses {
			for fkey, res := range cl.FreeIds {
				if res == nil {
					continue
				}
				if res.Global != nil || res.HasLocalBinding {
					continue
				}
				// FreeIds is keyed by FreeIdKey (scope-fingerprint|name); the discriminator
				// lookups take a bare symbol, so recover the bare name — otherwise every
				// GetGlobalIndex(NewSymbol("|name")) misses and the partition is vacuous.
				name := match.FreeIdName(fkey)
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
