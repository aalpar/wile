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

package wile

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// recoverEngineInit runs fn and returns the error it panicked with, or nil if
// it did not panic. A panic value that is not an error is re-raised: the
// contract under test is a WRAPPED sentinel, and a bare string or sentinel
// would otherwise pass silently.
func recoverEngineInit(t *testing.T, fn func()) (q error) {
	t.Helper()
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		err, ok := r.(error)
		if !ok {
			panic(r)
		}
		q = err
	}()
	fn()
	return nil
}

// namespaceConsumedOptions is the family NewEngine cannot apply on the
// WithNamespace path, because that path skips bootstrapNamespace, which is
// the only site that consumes them. Every entry must panic.
//
// ADDING A NEW OPTION: if it writes a field of engineConfig that only
// bootstrapNamespace reads, add it here AND to rejectNamespaceConsumedOptions
// in engine.go. TestWithNamespaceRejectionCoversEveryConsumedField is the
// ratchet that fails when the two drift.
func namespaceConsumedOptions() []struct {
	name string
	opt  EngineOption
} {
	return []struct {
		name string
		opt  EngineOption
	}{
		{"WithRegistry", WithRegistry(registry.NewRegistry())},
		{"WithoutCore", WithoutCore()},
		{"WithExtension", WithExtension(math.Extension)},
		{"WithProfile", WithProfile(Console)},
		{"WithAuthorizer", WithAuthorizer(security.ConsoleAuthorizer())},
		{"WithAuthorizer(nil)", WithAuthorizer(nil)},
		{"WithSandbox", WithSandbox()},
		{"WithEnv", WithEnv("WILE_K", "v")},
		{"WithEnvMap", WithEnvMap(map[string]string{"WILE_K": "v"})},
		{"WithEnvMap(nil)", WithEnvMap(nil)},
		{"WithImmutableTopLevel", WithImmutableTopLevel()},
		{"WithMutableTopLevel", WithMutableTopLevel()},
		{"WithStrictNamespace", WithStrictNamespace()},
		{"WithoutAmbientBindings", WithoutAmbientBindings()},
		{"WithDialect", WithDialect(NoMutation)},
	}
}

// TestWithNamespaceRejectsNamespaceConsumedOptions is the headline gate. Each
// option below was silently DROPPED before this change, which for the
// authorizer family meant a caller asking for a sandbox got an engine without
// one and no error.
func TestWithNamespaceRejectsNamespaceConsumedOptions(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	for _, tt := range namespaceConsumedOptions() {
		c.Run(tt.name, func(c *qt.C) {
			ns, err := NewNamespace(ctx)
			c.Assert(err, qt.IsNil)

			got := recoverEngineInit(c.TB.(*testing.T), func() {
				_, _ = NewEngine(ctx, WithNamespace(ns), tt.opt)
			})
			c.Assert(got, qt.IsNotNil, qt.Commentf("expected a panic, got none"))
			c.Assert(errors.Is(got, werr.ErrEngineInit), qt.IsTrue,
				qt.Commentf("panic value does not wrap ErrEngineInit: %v", got))

			// The diagnostic must name the offending option, or the caller
			// cannot tell which of fifteen to move.
			base := strings.TrimSuffix(strings.TrimSuffix(tt.name, "(nil)"), "(nil)")
			c.Assert(strings.Contains(got.Error(), base), qt.IsTrue,
				qt.Commentf("panic message %q does not name %q", got.Error(), base))
		})
	}
}

// TestWithNamespaceAcceptsEngineOnlyOptions is the other half: the rejection
// must not swallow options NewEngine really does apply. Without this arm the
// test above is satisfied by rejecting everything.
func TestWithNamespaceAcceptsEngineOnlyOptions(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	cases := []struct {
		name string
		opts []EngineOption
	}{
		{"no extra options", nil},
		{"WithLibraryPaths", []EngineOption{WithLibraryPaths(".")}},
		{"WithMaxCallDepth", []EngineOption{WithMaxCallDepth(100)}},
		{"WithInlineThreshold", []EngineOption{WithInlineThreshold(0)}},
		{"WithLossyConversionsAllowed", []EngineOption{WithLossyConversionsAllowed()}},
	}

	for _, tt := range cases {
		c.Run(tt.name, func(c *qt.C) {
			ns, err := NewNamespace(ctx, WithProfile(Small))
			c.Assert(err, qt.IsNil)

			opts := append([]EngineOption{WithNamespace(ns)}, tt.opts...)
			var eng *Engine
			var engErr error
			got := recoverEngineInit(c.TB.(*testing.T), func() {
				eng, engErr = NewEngine(ctx, opts...)
			})
			c.Assert(got, qt.IsNil, qt.Commentf("unexpected panic: %v", got))
			c.Assert(engErr, qt.IsNil)
			c.Assert(eng, qt.IsNotNil)
		})
	}
}

// TestWithNamespaceSandboxNoLongerSilentlyDropped pins the exact A/B from the
// filing. Arm A was already correct; arm B wrote the file. Arm B must now be
// unreachable rather than merely unsandboxed — a caller cannot end up with a
// working engine that ignored the sandbox it asked for.
func TestWithNamespaceSandboxNoLongerSilentlyDropped(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	dir := t.TempDir()

	write := func(eng *Engine, path string) error {
		_, err := eng.EvalMultiple(ctx,
			`(call-with-output-file "`+path+`" (lambda (port) (write-string "x" port)))`)
		return err
	}

	// A — the sandbox applies, and did before this change.
	pathA := filepath.Join(dir, "a.txt")
	engA, err := NewEngine(ctx, WithProfile(Small), WithSandbox())
	c.Assert(err, qt.IsNil)
	c.Assert(write(engA, pathA), qt.IsNotNil)
	_, statErr := os.Stat(pathA)
	c.Assert(os.IsNotExist(statErr), qt.IsTrue, qt.Commentf("arm A wrote the file"))

	// B — previously returned a working, UNSANDBOXED engine and nil error.
	ns, err := NewNamespace(ctx, WithProfile(Small))
	c.Assert(err, qt.IsNil)
	got := recoverEngineInit(t, func() {
		_, _ = NewEngine(ctx, WithNamespace(ns), WithSandbox())
	})
	c.Assert(got, qt.IsNotNil, qt.Commentf("arm B did not panic"))
	c.Assert(errors.Is(got, werr.ErrEngineInit), qt.IsTrue)
}

// namespaceConsumedFields are the engineConfig fields ONLY bootstrapNamespace
// reads. Every one must be covered by rejectNamespaceConsumedOptions.
var namespaceConsumedFields = []string{
	"registry",
	"extensions",
	"profileSet",
	"profileAuthorizer",
	"explicitAuthorizer",
	"explicitAuthorizerSet",
	"sandboxAuthorizer",
	"envMap",
	"envMapSet",
	"immutableTopLevel",
	"topLevelMutabilitySet",
	"strictLevel",
	"dialect",
}

// engineOnlyFields are read by NewEngine AFTER the namespace exists, so they
// apply equally to a pre-built one and must NOT be rejected.
//
// contractEnforcement is the uncomfortable member and is listed here only
// because that is the SHIPPED behavior, not because the classification is
// settled. It has three readers: applyOptionsFromConfig at the bootstrap call
// site (engine.go, skipped on this path), the same function at the
// setupLibrarySystem call site (runs), and the Engine's own copy consumed by
// RegisterFunc (runs). So WithContractEnforcement + WithNamespace applies to
// libraries and later registrations but NOT to the base environment — a
// partial, silent application of exactly the kind this file exists to stop.
// Rejecting it does not fully fix that either: passing it to NewNamespace
// covers the base environment but leaves the Engine's copy false. See TODO.md.
var engineOnlyFields = []string{
	"maxCallDepth", "callDepthSet",
	"maxParseDepth", "parseDepthSet",
	"maxExpandDepth", "expandDepthSet",
	"maxStackSize",
	"inlineThreshold", "inlineThresholdSet",
	"libraryPaths", "libraryEnabled",
	"importObserver",
	"namespace",
	"resolverFactories",
	"contractEnforcement",
	"lossyConversionsAllowed",
	"coverageCollector",
}

// TestWithNamespaceRejectionCoversEveryConsumedField is the ratchet. It does
// not check behavior; it checks that nobody added a field to engineConfig
// without deciding which side of the WithNamespace line it falls on. A new
// namespace-consumed field that skips rejectNamespaceConsumedOptions would
// otherwise reintroduce exactly the silent drop this change removes, and no
// behavioral test would notice, because the option to trigger it would be new
// too.
func TestWithNamespaceRejectionCoversEveryConsumedField(t *testing.T) {
	c := qt.New(t)

	classified := make(map[string]bool, len(namespaceConsumedFields)+len(engineOnlyFields))
	for _, name := range namespaceConsumedFields {
		classified[name] = true
	}
	for _, name := range engineOnlyFields {
		c.Assert(classified[name], qt.IsFalse, qt.Commentf("%q is in both lists", name))
		classified[name] = true
	}

	typ := reflect.TypeFor[engineConfig]()
	var unclassified []string
	for field := range typ.Fields() {
		if !classified[field.Name] {
			unclassified = append(unclassified, field.Name)
		}
	}
	c.Assert(unclassified, qt.HasLen, 0,
		qt.Commentf("engineConfig fields not classified as namespace-consumed or engine-only: %v — "+
			"decide which side each falls on, and if namespace-consumed, cover it in "+
			"rejectNamespaceConsumedOptions", unclassified))

	c.Assert(typ.NumField(), qt.Equals, len(namespaceConsumedFields)+len(engineOnlyFields),
		qt.Commentf("a classified field no longer exists on engineConfig"))
}

// TestWithNamespaceRejectionIsNotTriggeredByNewNamespace guards the caller the
// 2026-08-04 filing worried about. NewNamespace CONSUMES these options, so it
// must keep accepting every one of them.
func TestWithNamespaceRejectionIsNotTriggeredByNewNamespace(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	for _, tt := range namespaceConsumedOptions() {
		c.Run(tt.name, func(c *qt.C) {
			var ns *environment.Namespace
			var err error
			got := recoverEngineInit(c.TB.(*testing.T), func() {
				ns, err = NewNamespace(ctx, tt.opt)
			})
			c.Assert(got, qt.IsNil, qt.Commentf("NewNamespace panicked: %v", got))
			c.Assert(err, qt.IsNil)
			c.Assert(ns, qt.IsNotNil)
		})
	}
}
