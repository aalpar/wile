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
	"testing"

	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// recoverEngineInit runs fn and returns the error it panicked with, or nil if
// it did not panic. A panic value that is not an error is re-raised: a bare
// string or sentinel would otherwise pass silently.
//
// Nothing in the option family panics any more — the classification is a
// compile-time fact. The helper survives because the tests below assert the
// ABSENCE of a panic, and a raw panic would abort the run instead of failing
// the case that caused it.
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

// namespaceConsumedOptions is the family only bootstrapNamespace can apply.
// Each returns EngineOption and NOT EngineOnlyOption, so writing one at
// NewEngineWithNamespace does not compile.
//
// ADDING A NEW OPTION: if it writes a field of engineConfig that only
// bootstrapNamespace reads, wrap it in namespaceConsumedOption and add it here.
// TestNamespaceConsumedOptionsAreNotEngineOnly checks the adapter matches this
// table, and TestEveryOptionIsInExactlyOneTable checks the table is complete.
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
		{"WithExtensions", WithExtensions(math.Extension)},
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
		{"WithContractEnforcement", WithContractEnforcement()},
	}
}

// TestNewEngineWithNamespaceAcceptsEngineOnlyOptions is the arm that stops the
// classification from degenerating to "reject everything": an option NewEngine
// reads after the namespace exists applies equally to a pre-built one, and must
// still be passable there.
func TestNewEngineWithNamespaceAcceptsEngineOnlyOptions(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	cases := []struct {
		name string
		opts []EngineOnlyOption
	}{
		{"no extra options", nil},
		{"WithLibraryPaths", []EngineOnlyOption{WithLibraryPaths(".")}},
		{"WithMaxCallDepth", []EngineOnlyOption{WithMaxCallDepth(100)}},
		{"WithInlineThreshold", []EngineOnlyOption{WithInlineThreshold(0)}},
		{"WithLossyConversionsAllowed", []EngineOnlyOption{WithLossyConversionsAllowed()}},
	}

	for _, tt := range cases {
		c.Run(tt.name, func(c *qt.C) {
			ns, err := NewNamespace(ctx, WithProfile(Small))
			c.Assert(err, qt.IsNil)

			var eng *Engine
			var engErr error
			got := recoverEngineInit(c.TB.(*testing.T), func() {
				eng, engErr = NewEngineWithNamespace(ctx, ns, tt.opts...)
			})
			c.Assert(got, qt.IsNil, qt.Commentf("unexpected panic: %v", got))
			c.Assert(engErr, qt.IsNil)
			c.Assert(eng, qt.IsNotNil)
		})
	}
}

// TestPreBuiltNamespaceCarriesItsSandbox pins the A/B from the 2026-08-04
// filing. Arm A was always correct. Arm B was the defect: NewEngine(
// WithNamespace(ns), WithSandbox()) returned a working, UNSANDBOXED engine and
// a nil error, so a caller who asked to be confined was not.
//
// Arm B can no longer be written — WithSandbox is not an EngineOnlyOption — so
// what it asserts now is the MIGRATION the compiler points at: pass the sandbox
// to NewNamespace, and the namespace carries it into the engine. That is a
// stronger claim than the panic it replaces, because it proves the route the
// diagnostic recommends actually confines.
func TestPreBuiltNamespaceCarriesItsSandbox(t *testing.T) {
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

	// B — the migration. The sandbox goes to NewNamespace and travels with ns.
	pathB := filepath.Join(dir, "b.txt")
	ns, err := NewNamespace(ctx, WithProfile(Small), WithSandbox())
	c.Assert(err, qt.IsNil)
	engB, err := NewEngineWithNamespace(ctx, ns)
	c.Assert(err, qt.IsNil)
	c.Assert(write(engB, pathB), qt.IsNotNil,
		qt.Commentf("a namespace built with WithSandbox must confine the engine over it"))
	_, statErr = os.Stat(pathB)
	c.Assert(os.IsNotExist(statErr), qt.IsTrue, qt.Commentf("arm B wrote the file"))
}

// TestNewEngineWithNamespaceRejectsNilNamespace pins [nil means NONE]. As an
// option, WithNamespace(nil) meant "no namespace" and silently took the
// bootstrap path. As a positional parameter nil has no such reading: it is the
// same class of bad input as the two registry checks it joins, and answered the
// same way.
func TestNewEngineWithNamespaceRejectsNilNamespace(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngineWithNamespace(ctx, nil)
	c.Assert(eng, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
		qt.Commentf("nil namespace must wrap ErrEngineInit, got %v", err))
}

// namespaceConsumedFields are the engineConfig fields ONLY bootstrapNamespace
// reads. Every one must be written by an option in namespaceConsumedOptions.
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
	"contractEnforcement",
}

// engineOnlyFields are read after the namespace exists, so they apply equally
// to a pre-built one and are reachable from NewEngineWithNamespace.
var engineOnlyFields = []string{
	"maxCallDepth", "callDepthSet",
	"maxParseDepth", "parseDepthSet",
	"maxExpandDepth", "expandDepthSet",
	"maxStackSize",
	"inlineThreshold", "inlineThresholdSet",
	"libraryPaths", "libraryEnabled",
	"importObserver",
	"resolverFactories",
	"lossyConversionsAllowed",
	"coverageCollector",
}

// TestEngineConfigFieldsAreClassified is the ratchet. It does not check
// behavior; it checks that nobody added a field to engineConfig without
// deciding which side of the constructor split it falls on. A new
// namespace-consumed field reachable from an EngineOnlyOption would reintroduce
// exactly the silent partial application this split removes, and no behavioral
// test would notice, because the option to trigger it would be new too.
func TestEngineConfigFieldsAreClassified(t *testing.T) {
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
			"decide which side each falls on, and give the writing option the matching adapter",
			unclassified))

	c.Assert(typ.NumField(), qt.Equals, len(namespaceConsumedFields)+len(engineOnlyFields),
		qt.Commentf("a classified field no longer exists on engineConfig"))
}

// TestNewNamespaceAcceptsEveryConsumedOption guards the caller the 2026-08-04
// filing worried about. NewNamespace CONSUMES these options, so it must keep
// accepting every one of them.
func TestNewNamespaceAcceptsEveryConsumedOption(t *testing.T) {
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
