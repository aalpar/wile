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

// The child-namespace sandbox: Engine.EvalIn is the only production entry where
// the EXECUTING namespace differs from the one a primitive was registered in,
// and every runtime gate used to consult the latter.
//
// MachineContext.Authorizer() read p.env.Namespace(). Inside a foreign
// primitive p.env is the apply frame, whose namespace comes from the
// *ForeignClosure's captured closureEnv — the namespace the primitive was
// REGISTERED in, which for every synthetic extension library is the engine root
// built once at construction. So a DenyAll child ran system, open-input-file and
// eval with its own authorizer recording zero requests, and
// (interaction-environment) handed sandboxed code the host's top level.
//
// The escapes are construction- and ORDER-dependent — a cold library cache mints
// against the child and answers correctly, a warm one installs root-minted
// closures — so these are a matrix rather than one case.

package wile

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
)

// allowAll is the permissive root policy these tests sit under, so that a
// denial can only have come from the child.
type allowAll struct{}

func (allowAll) Authorize(security.AccessRequest) error {
	return nil
}

// sandboxedChild builds an engine whose root permits everything (and records)
// and a child namespace that denies everything (and records).
//
// WithLibraryPaths is load-bearing: a bare child namespace has an empty store
// and no library registry, so the binding under test has to arrive by import.
// The review's own filed repro omitted it and cannot have been run as written.
func sandboxedChild(t *testing.T) (*Engine, *environment.Namespace, *recordingAuthorizer, *recordingAuthorizer) {
	t.Helper()
	rootAuth := &recordingAuthorizer{inner: allowAll{}}
	childAuth := &recordingAuthorizer{inner: security.DenyAll()}
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithLibraryPaths(t.TempDir()),
		WithAuthorizer(rootAuth))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	child := eng.Namespace().NewChildNamespace(environment.WithChildAuthorizer(childAuth))
	return eng, child, rootAuth, childAuth
}

// TestChildNamespaceAuthorizerGovernsRuntimeGates is the policy half.
//
// Each arm is a gate site reached from a different extension, and each must both
// DENY and be recorded by the CHILD's authorizer. Asserting the denial alone
// would pass against a root that happens to deny too.
func TestChildNamespaceAuthorizerGovernsRuntimeGates(t *testing.T) {
	cases := []struct {
		name string
		code string
	}{
		{name: "process:exec-shell", code: `(system "true")`},
		{name: "file:read", code: `(open-input-file "/etc/hosts")`},
		{name: "env:read", code: `(get-environment-variable "HOME")`},
		{name: "file:read directory", code: `(current-directory)`},
		{name: "code:eval", code: `(eval '(+ 1 2))`},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, child, _, childAuth := sandboxedChild(t)
			_, err := eng.EvalIn(ctx, eng.MustParse(ctx,
				`(import (wile process) (wile files) (wile eval) (wile envvars) (wile io))`), child)
			qt.Assert(t, err, qt.IsNil)

			before := len(childAuth.reqs)
			_, err = eng.EvalIn(ctx, eng.MustParse(ctx, tc.code), child)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("the child's DenyAll must govern"))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
			qt.Assert(t, len(childAuth.reqs) > before, qt.IsTrue,
				qt.Commentf("the child's authorizer was never consulted"))
		})
	}
}

// TestChildNamespaceIsItsOwnInteractionEnvironment is the target half, and it is
// the more serious one: the policy escape lets sandboxed code call a primitive,
// while this one hands it the HOST's top level to define into. Under
// WithMutableTopLevel it can redefine host procedures outright.
func TestChildNamespaceIsItsOwnInteractionEnvironment(t *testing.T) {
	ctx := context.Background()
	eng, child, _, _ := sandboxedChild(t)

	_, err := eng.EvalIn(ctx, eng.MustParse(ctx, `(import (wile introspection) (wile namespace))`), child)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalIn(ctx, eng.MustParse(ctx,
		`(begin (namespace-define! (interaction-environment) 'planted 99) 'ok)`), child)
	qt.Assert(t, err, qt.IsNil)

	// The host must not see it.
	_, err = eng.EvalMultiple(ctx, `planted`)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("sandboxed code defined into the engine's own top level"))

	// And the child must: the define went somewhere, and that somewhere is the
	// child. Without this the test would pass against a broken
	// namespace-define! that dropped the binding entirely.
	v, err := eng.EvalIn(ctx, eng.MustParse(ctx, `planted`), child)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "99")
}

// TestChildNamespaceAuthorizerGovernsSourceLoading covers the compile-time and
// load-time halves, which parts 1-3 of the fix do not reach: the resolvers read
// their authorizer off the ROOT env captured at engine construction, and
// LoadLibrary is the only production installer of a ctx authorizer. So include
// and load resolved under root policy while import was already correct.
func TestChildNamespaceAuthorizerGovernsSourceLoading(t *testing.T) {
	ctx := context.Background()
	dir := t.TempDir()
	src := filepath.Join(dir, "oracle.scm")
	qt.Assert(t, os.WriteFile(src, []byte("42\n"), 0o600), qt.IsNil)

	cases := []struct {
		name string
		code string
	}{
		{name: "include", code: `(include "` + src + `")`},
		{name: "load", code: `(load "` + src + `")`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, child, _, childAuth := sandboxedChild(t)
			_, err := eng.EvalIn(ctx, eng.MustParse(ctx, `(import (wile eval))`), child)
			qt.Assert(t, err, qt.IsNil)

			before := len(childAuth.reqs)
			_, err = eng.EvalIn(ctx, eng.MustParse(ctx, tc.code), child)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("the child's DenyAll must govern"))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
			qt.Assert(t, len(childAuth.reqs) > before, qt.IsTrue,
				qt.Commentf("the child's authorizer was never consulted"))
		})
	}
}

// TestChildNamespaceAuthorizerGovernsWarmLibraryCache pins the ORDER-dependent
// half. On a cache MISS the child's own LibraryEnvFactory mints closures against
// the child and policy is honoured; on a HIT, CopyLibraryBindingsToEnvAtPhase
// installs the root-minted ones. Same engine, same child, same program, opposite
// verdicts depending only on whether the host imported first.
//
// TestLibraryLoadUsesCallerAuthorizerOnMiss pins the miss; this is the hit.
func TestChildNamespaceAuthorizerGovernsWarmLibraryCache(t *testing.T) {
	ctx := context.Background()
	eng, child, _, childAuth := sandboxedChild(t)

	// Warm the cache from the ROOT first. This is the whole point of the arm.
	_, err := eng.EvalMultiple(ctx, `(import (wile files))`)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalIn(ctx, eng.MustParse(ctx, `(import (wile files))`), child)
	qt.Assert(t, err, qt.IsNil)

	before := len(childAuth.reqs)
	_, err = eng.EvalIn(ctx, eng.MustParse(ctx, `(open-input-file "/etc/hosts")`), child)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
	qt.Assert(t, len(childAuth.reqs) > before, qt.IsTrue,
		qt.Commentf("a warm cache installed root-minted closures"))
}

// TestChildNamespaceCannotWidenAStrictRoot is the containment direction, which
// already holds and must keep holding. EffectiveAuthorizer is root ∧ child,
// most-restrictive-wins, and EvalIn additionally overwrites a nil child
// authorizer with the engine's — so a permissive child under a strict root is
// still refused. This is the property the fix must not trade away while
// tightening the other direction.
func TestChildNamespaceCannotWidenAStrictRoot(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithLibraryPaths(t.TempDir()),
		WithAuthorizer(security.ReadOnlyWithLoad()))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close() //nolint:errcheck // test cleanup

	for _, name := range []string{"nil child authorizer", "permissive child authorizer"} {
		t.Run(name, func(t *testing.T) {
			var opts []environment.NamespaceOption
			if name == "permissive child authorizer" {
				opts = append(opts, environment.WithChildAuthorizer(allowAll{}))
			}
			child := eng.Namespace().NewChildNamespace(opts...)
			_, err := eng.EvalIn(ctx, eng.MustParse(ctx, `(import (wile process))`), child)
			qt.Assert(t, err, qt.IsNil)

			_, err = eng.EvalIn(ctx, eng.MustParse(ctx, `(system "true")`), child)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("a child must not widen its root"))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
		})
	}
}

// denyResource refuses one resource (and, when action is non-empty, only that
// action on it) and permits everything else. A child holding it keeps code:eval
// and can still import: the shape of a sandbox that trusts eval but not, say,
// the shell.
type denyResource struct {
	resource string
	action   string
}

func (p denyResource) Authorize(req security.AccessRequest) error {
	if req.Resource != p.resource {
		return nil
	}
	if p.action != "" && req.Action != p.action {
		return nil
	}
	return security.ErrAccessDenied
}

// evalTrustingChild builds a KitchenSink engine whose root permits everything
// and a child whose recorded policy is inner, with the eval, process, namespace
// and introspection libraries already imported into the child.
func evalTrustingChild(t *testing.T, libDir string, inner security.Authorizer) (*Engine, *environment.Namespace, *recordingAuthorizer) {
	t.Helper()
	ctx := context.Background()
	eng, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithLibraryPaths(libDir),
		WithAuthorizer(allowAll{}))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	childAuth := &recordingAuthorizer{inner: inner}
	child := eng.Namespace().NewChildNamespace(environment.WithChildAuthorizer(childAuth))
	_, err = eng.EvalIn(ctx, eng.MustParse(ctx,
		`(import (wile eval) (wile process) (wile namespace) (wile introspection))`), child)
	qt.Assert(t, err, qt.IsNil)
	return eng, child, childAuth
}

// TestChildNamespaceAuthorizerGovernsConstructedEnvironments pins the
// constructors. Each derived its new namespace from the one the primitive was
// REGISTERED in (the engine root), so the namespace handed to eval carried the
// root's authorizer, and a child that denies process but grants code:eval ran
// system through it.
//
// The marker file is the oracle, and the child's recorder is the second one: a
// denial alone would pass against a constructor that merely failed to bind
// system, or against a root that happened to deny too.
func TestChildNamespaceAuthorizerGovernsConstructedEnvironments(t *testing.T) {
	cases := []struct {
		name string
		env  string
	}{
		{name: "environment import spec", env: `(environment '(only (wile process) system))`},
		{name: "environment wile profile", env: `(environment '(wile kitchen-sink))`},
		{name: "scheme-report-environment", env: `(scheme-report-environment 5)`},
		{name: "null-environment", env: `(null-environment 5)`},
		{name: "make-namespace", env: `(make-namespace '(only (wile process) system))`},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng, child, childAuth := evalTrustingChild(t, t.TempDir(),
				denyResource{resource: security.ResourceProcess})

			// The import inside the eval'd body is what reaches system in the
			// report and null environments, which bind nothing of (wile process)
			// on their own.
			marker := filepath.Join(t.TempDir(), "escaped")
			code := `(eval '(begin (import (only (wile process) system)) (system "touch ` + marker + `")) ` + tc.env + `)`

			before := len(childAuth.reqs)
			_, err := eng.EvalIn(ctx, eng.MustParse(ctx, code), child)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("the child's policy must govern the constructed namespace"))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
			_, statErr := os.Stat(marker)
			qt.Assert(t, errors.Is(statErr, os.ErrNotExist), qt.IsTrue,
				qt.Commentf("system ran under the engine root's policy"))
			qt.Assert(t, len(childAuth.reqs) > before, qt.IsTrue,
				qt.Commentf("the child's authorizer was never consulted"))
		})
	}
}

// TestChildNamespaceAuthorizerGovernsConstructorImports is the load half of the
// same confusion. The import source handed to ImportSpecInto decides whose
// EffectiveAuthorizer LoadLibrary consults, and it was the registering
// namespace's runtime, so a child denied code:load loaded a file-backed library
// through any of the three primitives that share that sink, while a plain
// (import …) from the same child was refused.
func TestChildNamespaceAuthorizerGovernsConstructorImports(t *testing.T) {
	cases := []struct {
		name string
		code string
	}{
		{name: "environment", code: `(environment '(gamma))`},
		{name: "make-namespace", code: `(make-namespace '(gamma))`},
		{name: "namespace-require", code: `(namespace-require (interaction-environment) '(gamma))`},
	}
	ctx := context.Background()
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			err := writeTestFile(filepath.Join(dir, "gamma.sld"), `(define-library (gamma)
  (export gamma-val)
  (begin (define gamma-val 5)))`)
			qt.Assert(t, err, qt.IsNil)
			eng, child, childAuth := evalTrustingChild(t, dir,
				denyResource{resource: security.ResourceCode, action: security.ActionLoad})

			before := len(childAuth.reqs)
			_, err = eng.EvalIn(ctx, eng.MustParse(ctx, tc.code), child)
			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("the child's code:load denial must govern"))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
			qt.Assert(t, len(childAuth.reqs) > before, qt.IsTrue,
				qt.Commentf("the child's authorizer was never consulted"))
		})
	}
}

// TestChildNamespaceCannotWidenProfileOfAStrictRoot is the containment
// direction for (environment '(wile <profile>)), and a ratchet rather than a
// must-fail-first test: it passes on the pre-fix code, where the widening check
// read the ROOT namespace. It fails if checkProfileWidening, now handed the
// executing child, reads the child's own Authorizer() instead of
// EffectiveAuthorizer(): a permissive child would then answer namespace:create
// for a strict root.
func TestChildNamespaceCannotWidenProfileOfAStrictRoot(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx,
		WithProfile(ConsoleWithLoad),
		WithLibraryPaths(t.TempDir()))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close() //nolint:errcheck // test cleanup

	child := eng.Namespace().NewChildNamespace(environment.WithChildAuthorizer(allowAll{}))
	_, err = eng.EvalIn(ctx, eng.MustParse(ctx, `(import (wile eval))`), child)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalIn(ctx, eng.MustParse(ctx, `(environment '(wile kitchen-sink))`), child)
	qt.Assert(t, err, qt.IsNotNil, qt.Commentf("a permissive child must not widen a strict root's profile"))
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}
