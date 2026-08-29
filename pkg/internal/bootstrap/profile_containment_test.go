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
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strings"
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

const (
	// axisBManifestPath is the checked-in per-primitive manifest, relative to the
	// repo root. It is regenerated only under WILE_AXIS_B_UPDATE=1.
	axisBManifestPath = "testdata/axis-b-manifest.scm"
	// coreFilePrefix selects the manifest's core rows by their SOURCE FILE.
	// Unlike the Go symbol, the file survives inlining — see coreManifestNames.
	coreFilePrefix = "pkg/registry/core/"
)

// coreHelperBuiltPrimitives is the frozen second half of the core surface: the
// core primitives whose Impl comes from a shared constructor in
// pkg/registry/helpers, so the manifest records that helper's file rather than a
// file under pkg/registry/core. Sorted; kept as data because nothing in a
// manifest row identifies the REGISTERING package of a factory-built closure.
//
// The only column that ever did was the Go symbol, and it did so by accident of
// inlining: a helper constructor inlined into core's package init is attributed
// to core, and the same constructor left alone stays in helpers. Selecting on it
// made the derivation a property of the compiler, and it silently lost every row
// the compiler chose not to inline. See maskGoFunctionColumn in
// pkg/wile/audit_manifest_test.go for the measurement across three toolchains:
// the symbol moves, the source location does not.
//
// TestExtensionPrimitiveNamesIsTheCoreManifestSurface is this list's ratchet. A
// new core primitive built from a helpers constructor and not added here shows
// up there as a liveOnly name; one removed from core but left here shows up as
// manifestOnly. To regenerate, take every name in extensionPrimitiveNames(nil)
// whose manifest row's source file is not under coreFilePrefix.
var coreHelperBuiltPrimitives = []string{
	"%parameter-raw-set!", "boolean?", "box?", "bytevector?", "char->integer",
	"char?", "complex?", "continuation-mark-set?", "continuation-prompt-tag?",
	"continuation?", "equal-hash", "error-context-marks", "error-context-source",
	"error-context-stack-trace", "error-context?", "error-object-irritants",
	"error-object-message", "error-object-source", "error-object-stack-trace",
	"error-object?", "exact?", "file-error?", "hashtable-keys",
	"hashtable-mutable?", "hashtable-size", "hashtable?", "inexact?", "integer?",
	"null?", "number?", "opaque-tag", "opaque?", "pair?", "parameter?",
	"procedure?", "rational?", "read-error?", "real?", "set-box!", "string-hash",
	"string?", "symbol-hash", "symbol?", "unbox", "vector?", "void?",
}

// axisBManifestRow parses one manifest row: (name return-type (params…) symbol
// source). Capture 1 is the primitive name, capture 2 its source location. The
// leading `\(?` absorbs the extra paren on the first row, which opens the outer
// list.
var axisBManifestRow = regexp.MustCompile(`^\s*\(?\("([^"]+)" "[^"]*" \([^)]*\) "[^"]*" "([^"]*)"\)`)

// bootstrapRepoRoot returns the absolute path of the wile repo root, inferred
// from this test file's location. This package lives at pkg/internal/bootstrap,
// so the module root is three directories up. Mirrors repoRoot in
// pkg/wile/audit_manifest_test.go, which does the same with two hops.
func bootstrapRepoRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller(0) failed — cannot infer repo root")
	}
	q := filepath.Join(filepath.Dir(thisFile), "..", "..", "..")
	return q
}

// coreManifestNames derives the core primitive surface from the checked-in
// axis-B manifest: every row whose SOURCE FILE is under pkg/registry/core, plus
// the frozen coreHelperBuiltPrimitives list for the ones implemented by a shared
// constructor in pkg/registry/helpers.
//
// The selector used to be the Go symbol column, and that was a defect: the
// symbol is runtime.FuncForPC(reflect.ValueOf(spec.Impl).Pointer()).Name(),
// taken at generation time by resolveImpl in pkg/wile/audit_manifest_test.go,
// so for a factory-built closure it names whichever package the compiler chose
// to inline the factory into. Regenerating the manifest on a toolchain that
// declines that inline moved 46 core rows out of the selector at once. The
// source location does not move — maskGoFunctionColumn measured both columns
// across three toolchains — so the file is the selector, and the helper-built
// residue is data rather than a compiler artifact.
//
// The other obvious selector, a static count of unique non-test `Name:`
// literals under pkg/registry/core —
//
//	grep -ohE 'Name:[[:space:]]+"[^"]+"' pkg/registry/core/*.go | sort -u | wc -l
//
// is worse still: it misses the comparison names registered from a table rather
// than from a per-name literal (char<?…char>=?, string<?…string>=?).
//
// This couples the test to the manifest exactly as TestBuildAxisBManifest is
// coupled: adding or removing a core primitive turns this red until the
// manifest is regenerated with WILE_AXIS_B_UPDATE=1. That is the ratchet, not a
// defect — regenerate, do not patch a number back in.
func coreManifestNames(t *testing.T) values.StringSet {
	t.Helper()
	path := filepath.Join(bootstrapRepoRoot(t), axisBManifestPath)
	body, err := os.ReadFile(path)
	if err != nil {
		// Never t.Skip: the manifest is tracked, so an unreadable one is a broken
		// checkout, and skipping is how this assertion would pass vacuously.
		t.Fatalf("%s: %v", axisBManifestPath, err)
	}

	helperBuilt := values.StringSet{}
	for _, name := range coreHelperBuiltPrimitives {
		helperBuilt.Set(name)
	}

	q := values.StringSet{}
	for i, line := range strings.Split(string(body), "\n") {
		if line == "" {
			continue
		}
		m := axisBManifestRow.FindStringSubmatch(line)
		if m == nil {
			t.Fatalf("%s:%d does not parse as a manifest row: %s", axisBManifestPath, i+1, line)
		}
		if !strings.HasPrefix(m[2], coreFilePrefix) && !helperBuilt.ContainsOne(m[1]) {
			continue
		}
		q.Set(m[1])
	}
	if len(q) < 100 {
		t.Fatalf("%s yielded only %d names under %s — the source-location column's shape has changed and the derivation is silently under-counting",
			axisBManifestPath, len(q), coreFilePrefix)
	}
	return q
}

// TestExtensionPrimitiveNamesIsTheCoreManifestSurface is the unit gate for
// review 2026-08-07 wave 2 item 12. An empty extension slice is exactly the
// tiny profile, and tiny is not an empty surface: it is the core registry. At
// 003b3353 this returned 0, so namesNotIn could never see a core acquisition
// and (environment '(wile tiny)) handed out the whole core surface with no
// capability question asked.
//
// The assertion is SET equality against a derivation, not a hand-typed
// cardinality: cardinality survives a swap (one name gained, one lost) and the
// number it was compared against had no source of truth — the comment beside it
// had already rotted, claiming a manifest row count 12 too high.
//
// This is the count half of the wave-6 §8 mechanism (1) "premises become
// generated assertions". The static go/ast half — every resolved-global arm of
// CompileSymbol reaches emitCachedBindingLoad — is NOT here and is blocked on
// wave 5 cluster E; when it lands it must generalise the derived
// procedureInvokers list in pkg/wile/capture_safety_test.go (wave 4 item 10)
// rather than add a second hand derivation.
//
// The boundary this does not reach: it checks premises stated in Go and in
// TODO.md, never one stated only in a plans/ file. That residue is covered by
// `make planlint` and `make indexlint`, which CI does not run.
func TestExtensionPrimitiveNamesIsTheCoreManifestSurface(t *testing.T) {
	c := qt.New(t)
	names, err := extensionPrimitiveNames(nil)
	c.Assert(err, qt.IsNil)

	manifest := coreManifestNames(t)
	liveOnly := []string{}
	for name := range names {
		ok := manifest.ContainsOne(name)
		if !ok {
			liveOnly = append(liveOnly, name)
		}
	}
	manifestOnly := []string{}
	for name := range manifest {
		ok := names.ContainsOne(name)
		if !ok {
			manifestOnly = append(manifestOnly, name)
		}
	}
	slices.Sort(liveOnly)
	slices.Sort(manifestOnly)
	// Check, not Assert: a rename moves a name in BOTH directions at once, and
	// an Assert on the first difference aborts before the second is printed,
	// reporting an addition where the change was a rename.
	c.Check(liveOnly, qt.HasLen, 0,
		qt.Commentf("registered by core but absent from %s: %v — regenerate the manifest with WILE_AXIS_B_UPDATE=1",
			axisBManifestPath, liveOnly))
	c.Check(manifestOnly, qt.HasLen, 0,
		qt.Commentf("in %s under %s (or in coreHelperBuiltPrimitives) but no longer registered: %v",
			axisBManifestPath, coreFilePrefix, manifestOnly))

	// A sample of names an embedder would recognise, kept below the set
	// assertion as the reader-legible check: it says what this surface IS,
	// where the difference above only says what moved.
	for _, name := range []string{"car", "cons", "string-append", "vector-fill!", "char<?", "string<?"} {
		ok := names.ContainsOne(name)
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
// extensions, so the smallest request in the system is the entire core surface
// (TestExtensionPrimitiveNamesIsTheCoreManifestSurface derives it), and against
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
