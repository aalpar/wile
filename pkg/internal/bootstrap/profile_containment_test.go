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

package bootstrap

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// callerWithProfileSurface builds a Namespace standing in for an engine
// constructed at the named profile: its registry carries core plus that
// profile's extensions, and auth is its policy (nil for an un-sandboxed
// engine).
//
// Core is in the stand-in because it is in the real thing —
// initializeEnvironmentWithRegistry registers core before walking the
// extension list, so an engine's surface is never smaller than core. A
// stand-in without it would be narrower than any engine the gate ever sees.
func callerWithProfileSurface(t *testing.T, profile string, auth security.Authorizer) *environment.Namespace {
	t.Helper()
	exts, err := ProfileExtensions(profile)
	if err != nil {
		t.Fatalf("ProfileExtensions(%q): %v", profile, err)
	}
	reg := registry.NewRegistry()
	coreErr := core.AddToRegistry(reg)
	if coreErr != nil {
		t.Fatalf("AddToRegistry(core): %v", coreErr)
	}
	for _, ext := range exts {
		regErr := ext.AddToRegistry(reg)
		if regErr != nil {
			t.Fatalf("AddToRegistry(%s): %v", ext.Name(), regErr)
		}
	}
	ns := environment.NewNamespace()
	ns.SetRegistry(reg)
	ns.SetAuthorizer(auth)
	return ns
}

// permitAll is a custom policy that opts into namespace widening while still
// being an installed authorizer. It exists to show the gate is a question put
// to the policy, not a hard-coded refusal.
func permitAll() security.Authorizer {
	return security.AuthorizerFunc(func(security.AccessRequest) error {
		return nil
	})
}

// denyNamespaceOnly permits everything except namespace construction, isolating
// the new resource from the built-in authorizers' deny-unknown default.
func denyNamespaceOnly() security.Authorizer {
	return security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceNamespace {
			return security.ErrAccessDenied
		}
		return nil
	})
}

// coreRegistryPrimitiveCount is the number of registered primitives whose Go
// symbol is under github.com/aalpar/wile/pkg/registry/core in
// testdata/axis-b-manifest.scm, the reproducible source (467 rows, one per
// registered primitive).
//
// Two other counts are NOT this quantity and both gaps reconcile exactly. A
// static count of unique `Name:` literals under pkg/registry/core gives 198,
// missing the ten comparison names registered from a table rather than from a
// per-name literal (char<?…char>=?, string<?…string>=?); 198 + 10 = 208 with
// nothing in the other direction. Counting by the manifest's *file* column
// gives 178, missing the thirty core primitives whose Impl is declared in
// pkg/registry/helpers — 20 type predicates through helpers.MakeTypePredicate
// and 10 accessors/mutators through helpers/accessor.go — while their specs,
// and their symbols, are core's.
const coreRegistryPrimitiveCount = 208

// TestExtensionPrimitiveNamesCountsCore is the unit gate for review
// 2026-08-07 wave 2 item 12. An empty extension slice is exactly the tiny
// profile, and tiny is not an empty surface: it is the core registry. At
// 003b3353 this returned 0, so namesNotIn could never see a core acquisition
// and (environment '(wile tiny)) handed out 208 primitives with no capability
// question asked.
func TestExtensionPrimitiveNamesCountsCore(t *testing.T) {
	c := qt.New(t)
	names, err := extensionPrimitiveNames(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(len(names), qt.Equals, coreRegistryPrimitiveCount)
	c.Assert(names, qt.Not(qt.HasLen), 0)

	// A sample of names an embedder would recognise, so a future registry
	// churn that changes the count says which direction it moved.
	for _, name := range []string{"car", "cons", "string-append", "vector-fill!", "char<?", "string<?"} {
		_, ok := names[name]
		c.Assert(ok, qt.IsTrue, qt.Commentf("core primitive %q missing from the tiny surface", name))
	}
}

// TestCheckProfileWidening covers the three-case rule: no authorizer allows
// anything, a contained profile is allowed without consulting the policy, and
// only a widening request reaches the authorizer.
func TestCheckProfileWidening(t *testing.T) {
	tests := []struct {
		name      string
		engine    string
		auth      security.Authorizer
		requested string
		wantDeny  bool
	}{
		{
			name:      "no authorizer permits widening, preserving the documented path",
			engine:    "console",
			auth:      nil,
			requested: "kitchen-sink",
			wantDeny:  false,
		},
		{
			name:      "same profile is contained and never reaches the authorizer",
			engine:    "console",
			auth:      security.ConsoleAuthorizer(),
			requested: "console",
			wantDeny:  false,
		},
		{
			name:      "tiny is contained in every engine",
			engine:    "console",
			auth:      security.ConsoleAuthorizer(),
			requested: "tiny",
			wantDeny:  false,
		},
		{
			name:      "console is contained in console-with-load",
			engine:    "console-with-load",
			auth:      security.ConsoleAuthorizer(),
			requested: "console",
			wantDeny:  false,
		},
		{
			name:      "kitchen-sink from a console engine is refused",
			engine:    "console",
			auth:      security.ConsoleAuthorizer(),
			requested: "kitchen-sink",
			wantDeny:  true,
		},
		{
			name:      "kitchen-sink from a small engine is refused",
			engine:    "small",
			auth:      security.ConsoleWithLoadAuthorizer(),
			requested: "kitchen-sink",
			wantDeny:  true,
		},
		{
			name:      "a permissive policy may opt into widening",
			engine:    "console",
			auth:      permitAll(),
			requested: "kitchen-sink",
			wantDeny:  false,
		},
		{
			name:      "a policy may refuse namespace construction alone",
			engine:    "console",
			auth:      denyNamespaceOnly(),
			requested: "kitchen-sink",
			wantDeny:  true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			ns := callerWithProfileSurface(t, tt.engine, tt.auth)
			exts, err := ProfileExtensions(tt.requested)
			c.Assert(err, qt.IsNil)

			err = checkProfileWidening(ns, tt.requested, exts)
			if !tt.wantDeny {
				c.Assert(err, qt.IsNil)
				return
			}
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, ErrProfileWidensEngine), qt.IsTrue)
			c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
		})
	}
}

// TestCheckProfileWideningUnknownSurfaceIsNotContained pins the conservative
// reading of an engine whose registry cannot be established: containment is not
// provable, so a non-empty request must reach the authorizer rather than being
// waved through.
//
// Tiny is one of those requests. It used to be the exception here — an empty
// extension slice looked like an empty request, so it was waved through — and
// that reading was the item 12 fail-open. A profile is core plus its
// extensions, so the smallest request in the system is 208 names, and against
// an unprovable surface even tiny must be asked about.
func TestCheckProfileWideningUnknownSurfaceIsNotContained(t *testing.T) {
	c := qt.New(t)
	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.ConsoleAuthorizer())

	tinyExts, err := ProfileExtensions("tiny")
	c.Assert(err, qt.IsNil)
	err = checkProfileWidening(ns, "tiny", tinyExts)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrProfileWidensEngine), qt.IsTrue)

	consoleExts, err := ProfileExtensions("console")
	c.Assert(err, qt.IsNil)
	err = checkProfileWidening(ns, "console", consoleExts)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrProfileWidensEngine), qt.IsTrue)
}

// TestProfileFactoryRefusesUngatedExtensionReach is the test the TODO entry for
// this item specifically asks for, and it is written to be non-vacuous.
//
// The trap it avoids: asserting only that a denial happens proves nothing about
// the hazard, because the hazard is an UNGATED extension — threads and
// gointerop define no security.Check sites at all, so no authorizer denial
// would ever have fired for them. The test therefore does both halves. The
// permit arm shows make-thread really does become reachable from a Console
// engine that asks for kitchen-sink, which is the escape; the refuse arm shows
// the containment check is what stops it.
func TestProfileFactoryRefusesUngatedExtensionReach(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	makeThread := values.NewSymbol("make-thread")

	// Permit arm: this is the escape as it exists without containment. A Console
	// engine reaches a threads primitive it never registered, and no authorizer
	// is ever consulted about it, because threads has no gate sites.
	permitted := callerWithProfileSurface(t, "console", permitAll())
	ns, err := eval.ProfileFactory(ctx, permitted, "kitchen-sink", "")
	c.Assert(err, qt.IsNil)
	bound := ns.Runtime().GetBinding(makeThread, syntax.AllScopes())
	c.Assert(bound, qt.IsNotNil,
		qt.Commentf("make-thread must be reachable here, or the refuse arm below proves nothing"))

	// Refuse arm: the same request under the Console policy is stopped before
	// any namespace is built.
	confined := callerWithProfileSurface(t, "console", security.ConsoleAuthorizer())
	ns, err = eval.ProfileFactory(ctx, confined, "kitchen-sink", "")
	c.Assert(ns, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrProfileWidensEngine), qt.IsTrue)
}

// TestProfileFactoryUnaffectedWithoutAuthorizer guards the compatibility half of
// the policy: an engine that installed no authorizer keeps the documented
// widening path, so every existing embedder of (environment '(wile ...))
// behaves exactly as before.
func TestProfileFactoryUnaffectedWithoutAuthorizer(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	caller := callerWithProfileSurface(t, "console", nil)
	ns, err := eval.ProfileFactory(ctx, caller, "kitchen-sink", "")
	c.Assert(err, qt.IsNil)
	c.Assert(ns, qt.IsNotNil)
	c.Assert(ns.Runtime().GetBinding(values.NewSymbol("make-thread"), syntax.AllScopes()), qt.IsNotNil)
}
