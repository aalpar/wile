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
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"slices"
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

// engineOnlyOptions is the family NewEngine reads AFTER the namespace exists.
// Each returns EngineOnlyOption and so is passable to NewEngineWithNamespace.
//
// The field is typed EngineOption, not EngineOnlyOption, deliberately: the
// wider static type is what makes TestEngineOnlyOptionsAreEngineOnly a genuine
// runtime check of the adapter rather than a compile-time tautology.
func engineOnlyOptions() []struct {
	name string
	opt  EngineOption
} {
	return []struct {
		name string
		opt  EngineOption
	}{
		{"WithMaxCallDepth", WithMaxCallDepth(100)},
		{"WithMaxParseDepth", WithMaxParseDepth(100)},
		{"WithMaxExpandDepth", WithMaxExpandDepth(100)},
		{"WithMaxStackSize", WithMaxStackSize(1 << 20)},
		{"WithInlineThreshold", WithInlineThreshold(0)},
		{"WithLibraryPaths", WithLibraryPaths(".")},
		{"WithImportObserver", WithImportObserver(func(LibraryImportEvent) {})},
		{"WithSourceFS", WithSourceFS(os.DirFS("."))},
		{"WithSourceOS", WithSourceOS()},
		{"WithCoverage", WithCoverage(nil)},
		{"WithLossyConversionsAllowed", WithLossyConversionsAllowed()},
	}
}

// TestNamespaceConsumedOptionsAreNotEngineOnly is the direct successor to the
// deleted TestWithNamespaceRejectsNamespaceConsumedOptions. That test asserted a
// panic; the panic is now a compile error, which no ordinary Go test can
// observe. This states the same invariant in the form a test CAN read: the
// option does not satisfy NewEngineWithNamespace's element type.
//
// (An external `go build` in a temp dir could assert non-compilation directly.
// Rejected: a whole toolchain invocation per option, for an invariant one type
// assertion states exactly.)
func TestNamespaceConsumedOptionsAreNotEngineOnly(t *testing.T) {
	c := qt.New(t)

	for _, tt := range namespaceConsumedOptions() {
		c.Run(tt.name, func(c *qt.C) {
			_, ok := tt.opt.(EngineOnlyOption)
			c.Assert(ok, qt.IsFalse,
				qt.Commentf("%s implements EngineOnlyOption, so it can be passed to "+
					"NewEngineWithNamespace, where nothing consumes it — it should be "+
					"wrapped in namespaceConsumedOption, not engineOnlyOption", tt.name))
		})
	}
}

// TestEngineOnlyOptionsAreEngineOnly is the converse arm. Without it the test
// above is satisfied by giving every option the namespace-consumed adapter,
// which would make NewEngineWithNamespace take no options at all.
func TestEngineOnlyOptionsAreEngineOnly(t *testing.T) {
	c := qt.New(t)

	for _, tt := range engineOnlyOptions() {
		c.Run(tt.name, func(c *qt.C) {
			_, ok := tt.opt.(EngineOnlyOption)
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("%s does not implement EngineOnlyOption, so it cannot be "+
					"passed to NewEngineWithNamespace even though the engine reads it "+
					"after the namespace exists", tt.name))
		})
	}
}

// TestEveryOptionIsInExactlyOneTable is what closes the fail-open hole the two
// tests above leave: they only check the options someone remembered to list.
// Adding WithSomethingNew with the wrong adapter passes every other test in the
// tree, because the table that would have caught it is the one that was not
// updated.
//
// So this reads the SOURCE rather than the tables: every exported With*/Without*
// in pkg/wile whose result type is EngineOption or EngineOnlyOption must appear
// in exactly one table. It is the option-level counterpart to
// TestEngineConfigFieldsAreClassified, which catches a new engineConfig FIELD;
// neither subsumes the other, because a new option over an existing field is
// invisible to the field ratchet.
//
// Follows the shape of callable_narrowing_ratchet_test.go: one ParseFile per
// .go file rather than the deprecated parser.ParseDir. _test.go files are
// included on purpose — a test-only option still has to pick an adapter.
func TestEveryOptionIsInExactlyOneTable(t *testing.T) {
	c := qt.New(t)

	listed := make(map[string]string)
	for _, tt := range namespaceConsumedOptions() {
		listed[strings.TrimSuffix(strings.TrimSuffix(tt.name, "(nil)"), "(nil)")] = "namespace-consumed"
	}
	for _, tt := range engineOnlyOptions() {
		_, dup := listed[tt.name]
		c.Assert(dup, qt.IsFalse, qt.Commentf("%s is in both tables", tt.name))
		listed[tt.name] = "engine-only"
	}

	fset := token.NewFileSet()
	entries, err := os.ReadDir(".")
	c.Assert(err, qt.IsNil)

	var declared []string
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") {
			continue
		}
		file, parseErr := parser.ParseFile(fset, entry.Name(), nil, 0)
		c.Assert(parseErr, qt.IsNil, qt.Commentf("parsing %s", entry.Name()))
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv != nil {
				continue
			}
			if !strings.HasPrefix(fn.Name.Name, "With") && !strings.HasPrefix(fn.Name.Name, "Without") {
				continue
			}
			if !fn.Name.IsExported() || !returnsAnEngineOption(fn) {
				continue
			}
			declared = append(declared, fn.Name.Name)
		}
	}

	c.Assert(len(declared) > 0, qt.IsTrue,
		qt.Commentf("found no option constructors at all — the AST filter is broken, "+
			"which would make this ratchet vacuously green"))

	var unclassified []string
	for _, name := range declared {
		_, ok := listed[name]
		if !ok {
			unclassified = append(unclassified, name)
		}
	}
	c.Assert(unclassified, qt.HasLen, 0,
		qt.Commentf("option constructors in no table: %v — add each to "+
			"namespaceConsumedOptions or engineOnlyOptions, and give it the matching "+
			"adapter (namespaceConsumedOption / engineOnlyOption)", unclassified))

	var stale []string
	for name := range listed {
		if !slices.Contains(declared, name) {
			stale = append(stale, name)
		}
	}
	slices.Sort(stale)
	c.Assert(stale, qt.HasLen, 0,
		qt.Commentf("tables name option constructors that no longer exist: %v", stale))
}

// returnsAnEngineOption reports whether fn's sole result is EngineOption or
// EngineOnlyOption. Both are package-local identifiers here, so a bare *ast.Ident
// is the only shape to match — a SandboxOption or an ApplyOption constructor
// falls out on the name, and a multi-result function falls out on the count.
func returnsAnEngineOption(fn *ast.FuncDecl) bool {
	if fn.Type.Results == nil || len(fn.Type.Results.List) != 1 {
		return false
	}
	ident, ok := fn.Type.Results.List[0].Type.(*ast.Ident)
	if !ok {
		return false
	}
	return ident.Name == "EngineOption" || ident.Name == "EngineOnlyOption"
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
