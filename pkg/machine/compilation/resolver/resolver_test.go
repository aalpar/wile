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

package resolver

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// newTestNamespace returns a Namespace with a load stack initialized.
// Tests that call LoadPathStack().Push() must use this instead of
// environment.NewNamespace() to avoid a nil dereference.
func newTestNamespace() *environment.Namespace {
	ns := environment.NewNamespace()
	ns.SetLoadPathStack(sourceload.NewLoadStack())
	return ns
}

// realDir normalizes a temp directory path to account for macOS symlinks
// (/tmp -> /private/tmp). Without this, paths from t.TempDir() and
// filepath.Abs() inside resolvers won't match.
func realDir(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(dir)
	qt.Assert(t, err, qt.IsNil)
	return resolved
}

// testSearcher is a minimal LibrarySearcher for tests that need search paths
// without importing machine/compilation.
type testSearcher struct {
	paths []string
}

func (p *testSearcher) GetSearchPaths() []string {
	return p.paths
}

// --- OSFileResolver ---

func TestOSFileResolver_EmptyPath(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	_, _, err := r.ResolveAndOpen(context.Background(), "")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "empty filename")
}

func TestOSFileResolver_AbsolutePath(t *testing.T) {
	dir := realDir(t, t.TempDir())
	absPath := filepath.Join(dir, "abs.scm")
	err := os.WriteFile(absPath, []byte("42"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), absPath)
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, absPath)
}

func TestOSFileResolver_AbsolutePathNotFound(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	_, _, err := r.ResolveAndOpen(context.Background(), "/nonexistent/path/to/file.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestOSFileResolver_RelativeViaLoadPathStack(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := os.WriteFile(filepath.Join(dir, "found.scm"), []byte("ok"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	// Push a file path in the target directory so CurrentDir() returns dir.
	stack.Push(filepath.Join(dir, "parent.scm"))
	defer stack.Pop()

	t.Setenv(SchemeIncludePathEnv, "")

	r := NewOSFileResolver(env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "found.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(dir, "found.scm"))
}

func TestOSFileResolver_RelativeViaCWDFallback(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := os.WriteFile(filepath.Join(dir, "cwd.scm"), []byte("1"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Chdir(dir)

	t.Setenv(SchemeIncludePathEnv, "")

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "cwd.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(dir, "cwd.scm"))
}

func TestOSFileResolver_RelativeViaIncludePath(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := os.WriteFile(filepath.Join(dir, "env.scm"), []byte("2"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, dir)

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "env.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(dir, "env.scm"))
}

func TestOSFileResolver_RelativeViaLibraryRegistry(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := os.WriteFile(filepath.Join(dir, "reg.scm"), []byte("3"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, "")

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{dir}})
	env := ns.Runtime()

	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "reg.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(dir, "reg.scm"))
}

func TestOSFileResolver_RelativeNotFound(t *testing.T) {
	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to a temp dir that does NOT contain the file.
	t.Chdir(t.TempDir())

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

// TestOSFileResolver_RelativeNeverReachesFilesystemRoot pins the removal of the
// "/" last-resort candidate (reviews/2026-08-07/REVIEW.md 2.1.5). A relative
// path whose first component happens to name a root-level directory must not be
// reinterpreted as an absolute host path.
func TestOSFileResolver_RelativeNeverReachesFilesystemRoot(t *testing.T) {
	fi, statErr := os.Stat("/etc/hosts")
	if statErr != nil || fi.IsDir() {
		t.Skip("no /etc/hosts on this platform")
	}

	t.Setenv(SchemeIncludePathEnv, "")
	// A CWD with no etc/ subdirectory, so "etc/hosts" is unresolvable unless
	// the resolver joins it onto the filesystem root.
	t.Chdir(t.TempDir())

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	_, resolved, err := r.ResolveAndOpen(context.Background(), "etc/hosts")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	qt.Assert(t, resolved, qt.Equals, "")
}

func TestOSFileResolver_ReturnedPathIsAbsolute(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := os.WriteFile(filepath.Join(dir, "check.scm"), []byte("x"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, dir)

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "check.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, filepath.IsAbs(resolved), qt.IsTrue)
}

func TestOSFileResolver_SecurityDenied(t *testing.T) {
	dir := realDir(t, t.TempDir())
	absPath := filepath.Join(dir, "denied.scm")
	err := os.WriteFile(absPath, []byte("secret"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.AuthorizerFunc(func(_ security.AccessRequest) error {
		return security.ErrAccessDenied
	}))
	env := ns.Runtime()
	r := NewOSFileResolver(env)

	_, _, err = r.ResolveAndOpen(context.Background(), absPath)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestOSFileResolver_SecurityAllowed(t *testing.T) {
	dir := realDir(t, t.TempDir())
	absPath := filepath.Join(dir, "allowed.scm")
	err := os.WriteFile(absPath, []byte("ok"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.AuthorizerFunc(func(_ security.AccessRequest) error {
		return nil
	}))
	env := ns.Runtime()
	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), absPath)
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, absPath)
}

func TestOSFileResolver_SecurityCheckTarget(t *testing.T) {
	dir := realDir(t, t.TempDir())
	absPath := filepath.Join(dir, "target.scm")
	err := os.WriteFile(absPath, []byte("t"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	var captured security.AccessRequest
	ns.SetAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
		captured = req
		return nil
	}))
	env := ns.Runtime()
	r := NewOSFileResolver(env)

	f, _, err := r.ResolveAndOpen(context.Background(), absPath)
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()

	qt.Assert(t, captured.Resource, qt.Equals, security.ResourceCode)
	qt.Assert(t, captured.Action, qt.Equals, security.ActionLoad)
	qt.Assert(t, captured.Target, qt.Equals, absPath)
}

func TestOSFileResolver_NoAuthorizerAllowsByDefault(t *testing.T) {
	dir := realDir(t, t.TempDir())
	absPath := filepath.Join(dir, "open.scm")
	err := os.WriteFile(absPath, []byte("default"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	// No authorizer on namespace — open by default.
	f, resolved, err := r.ResolveAndOpen(context.Background(), absPath)
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, absPath)
}

func TestOSFileResolver_FallbackPriority(t *testing.T) {
	// Library registry > SCHEME_INCLUDE_PATH > CWD.
	// Put different files in each to verify priority.
	regDir := realDir(t, t.TempDir())
	includeDir := realDir(t, t.TempDir())

	// Only create the file in the registry dir — if the resolver
	// checked SCHEME_INCLUDE_PATH or CWD first, it wouldn't find it.
	err := os.WriteFile(filepath.Join(regDir, "priority.scm"), []byte("reg"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, includeDir)

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{regDir}})
	env := ns.Runtime()

	r := NewOSFileResolver(env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "priority.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(regDir, "priority.scm"))
}

func TestOSFileResolver_LoadPathStackPriorityOverFallbacks(t *testing.T) {
	stackDir := realDir(t, t.TempDir())
	fallbackDir := realDir(t, t.TempDir())

	// File exists in both — stack dir should win.
	err := os.WriteFile(filepath.Join(stackDir, "dup.scm"), []byte("stack"), 0o644)
	qt.Assert(t, err, qt.IsNil)
	err = os.WriteFile(filepath.Join(fallbackDir, "dup.scm"), []byte("fallback"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, fallbackDir)

	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	stack.Push(filepath.Join(stackDir, "parent.scm"))
	defer stack.Pop()

	r := NewOSFileResolver(env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "dup.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(stackDir, "dup.scm"))
}

func TestOSFileResolver_DotDotResolution(t *testing.T) {
	// Simulate ../../foo.scm resolution via load path stack.
	base := realDir(t, t.TempDir())

	// Create: base/foo.scm and base/sub/deep/parent.scm
	err := os.WriteFile(filepath.Join(base, "foo.scm"), []byte("found"), 0o644)
	qt.Assert(t, err, qt.IsNil)
	deepDir := filepath.Join(base, "sub", "deep")
	err = os.MkdirAll(deepDir, 0o755)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, "")

	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	stack.Push(filepath.Join(deepDir, "parent.scm"))
	defer stack.Pop()

	r := NewOSFileResolver(env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "../../foo.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(base, "foo.scm"))
}

// --- EmbedFileResolver ---

func TestEmbedFileResolver_Found(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/core.scm": &fstest.MapFile{Data: []byte("(define x 1)")},
	}
	r := NewEmbedFileResolver(fsys)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "lib/core.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "lib/core.scm")
}

func TestEmbedFileResolver_NotFound(t *testing.T) {
	fsys := fstest.MapFS{}
	r := NewEmbedFileResolver(fsys)

	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "missing.scm")
}

func TestEmbedFileResolver_ReturnedPathIsRelative(t *testing.T) {
	fsys := fstest.MapFS{
		"bootstrap.scm": &fstest.MapFile{Data: []byte("(begin)")},
	}
	r := NewEmbedFileResolver(fsys)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "bootstrap.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, filepath.IsAbs(resolved), qt.IsFalse)
}

func TestEmbedFileResolver_IgnoresContext(t *testing.T) {
	// EmbedFileResolver doesn't perform security checks — even a
	// deny-all authorizer should not prevent access.
	fsys := fstest.MapFS{
		"safe.scm": &fstest.MapFile{Data: []byte("ok")},
	}
	r := NewEmbedFileResolver(fsys)

	ctx := security.WithAuthorizer(
		context.Background(),
		security.AuthorizerFunc(func(_ security.AccessRequest) error {
			return security.ErrAccessDenied
		}),
	)

	f, resolved, err := r.ResolveAndOpen(ctx, "safe.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "safe.scm")
}

// --- FSFileResolver ---

func TestFSFileResolver_EmptyPath(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": {Data: []byte("1")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestFSFileResolver_RejectsAbsolutePath(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": {Data: []byte("1")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "/abs/path.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "absolute")
}

func TestFSFileResolver_DirectPath(t *testing.T) {
	fsys := fstest.MapFS{"hello.scm": {Data: []byte("42")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "hello.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "hello.scm")
}

func TestFSFileResolver_RelativeToLoadPathStack(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/main.sld":   {Data: []byte("(define-library (main))")},
		"lib/helper.scm": {Data: []byte("42")},
	}
	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	stack.Push("lib/main.sld")
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "helper.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "lib/helper.scm")
}

// The per-load-chain LoadStack carried on ctx takes precedence over the shared
// per-namespace stack: a relative include resolves against the ctx stack's
// directory even when the namespace stack points elsewhere. This is the wiring
// that lets concurrent library loads resolve their includes independently.
func TestFSFileResolver_CtxStackPreferredOverEnvStack(t *testing.T) {
	fsys := fstest.MapFS{
		"a/helper.scm": {Data: []byte("1")},
		"b/helper.scm": {Data: []byte("2")},
	}
	env := newTestNamespace().Runtime()
	// Shared namespace stack points at directory "a".
	env.LoadPathStack().Push("a/main.sld")
	defer env.LoadPathStack().Pop()

	// Per-load-chain ctx stack points at directory "b".
	ctxStack := sourceload.NewLoadStack()
	ctxStack.Push("b/lib.sld")
	ctx := sourceload.WithLoadStack(context.Background(), ctxStack)

	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(ctx, "helper.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "b/helper.scm")
}

func TestFSFileResolver_ViaSearchPaths(t *testing.T) {
	fsys := fstest.MapFS{
		"vendor/util.scm": {Data: []byte("99")},
	}
	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"vendor"}})
	env := ns.Runtime()

	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "util.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "vendor/util.scm")
}

func TestFSFileResolver_NotFound(t *testing.T) {
	fsys := fstest.MapFS{}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestFSFileResolver_SecurityDenied(t *testing.T) {
	fsys := fstest.MapFS{"secret.scm": {Data: []byte("(launch-missiles)")}}
	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.DenyAll())
	env := ns.Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "secret.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestFSFileResolver_LoadPathStackPriorityOverSearchPaths(t *testing.T) {
	fsys := fstest.MapFS{
		"stack-dir/util.scm":  {Data: []byte("from-stack")},
		"search-dir/util.scm": {Data: []byte("from-search")},
	}
	ns := newTestNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"search-dir"}})
	env := ns.Runtime()

	stack := env.LoadPathStack()
	stack.Push("stack-dir/parent.scm")
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "util.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "stack-dir/util.scm")
}

func TestFSFileResolver_SearchPathPriorityOverRoot(t *testing.T) {
	fsys := fstest.MapFS{
		"util.scm":            {Data: []byte("from-root")},
		"search-dir/util.scm": {Data: []byte("from-search")},
	}
	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"search-dir"}})
	env := ns.Runtime()

	r := NewFSFileResolver(fsys, env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "util.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "search-dir/util.scm")
}

func TestFSFileResolver_FirstSearchPathWins(t *testing.T) {
	fsys := fstest.MapFS{
		"first/lib.scm":  {Data: []byte("from-first")},
		"second/lib.scm": {Data: []byte("from-second")},
	}
	ns := environment.NewNamespace()
	// Search paths in order: first, second.
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"first", "second"}})
	env := ns.Runtime()

	r := NewFSFileResolver(fsys, env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "lib.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "first/lib.scm")
}

func TestFSFileResolver_EmptySearchPathSkipped(t *testing.T) {
	fsys := fstest.MapFS{
		"real-dir/found.scm": {Data: []byte("ok")},
	}
	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"", "real-dir"}})
	env := ns.Runtime()

	r := NewFSFileResolver(fsys, env)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "found.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "real-dir/found.scm")
}

func TestFSFileResolver_DotDotTraversal(t *testing.T) {
	fsys := fstest.MapFS{
		"sibling.scm":     {Data: []byte("found")},
		"sub/current.scm": {Data: []byte("here")},
	}
	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	stack.Push("sub/current.scm")
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)
	// path.Join("sub", "../sibling.scm") cleans to "sibling.scm"
	f, resolved, err := r.ResolveAndOpen(context.Background(), "../sibling.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "sibling.scm")
}

func TestFSFileResolver_FallsThroughToRoot(t *testing.T) {
	fsys := fstest.MapFS{
		"root-only.scm": {Data: []byte("at-root")},
		"sub/other.scm": {Data: []byte("other")},
	}
	env := newTestNamespace().Runtime()
	stack := env.LoadPathStack()
	stack.Push("sub/other.scm")
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)
	// "root-only.scm" is NOT in "sub/", so load-path-stack miss,
	// no search paths, falls through to FS root
	f, resolved, err := r.ResolveAndOpen(context.Background(), "root-only.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "root-only.scm")
}

func TestFSFileResolver_NotFoundListsSearchedPaths(t *testing.T) {
	fsys := fstest.MapFS{}
	ns := newTestNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"lib", "vendor"}})
	env := ns.Runtime()

	stack := env.LoadPathStack()
	stack.Push("src/main.scm")
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)
	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	// Error should mention all searched locations
	errMsg := err.Error()
	qt.Assert(t, errMsg, qt.Contains, "src/")
	qt.Assert(t, errMsg, qt.Contains, "lib/")
	qt.Assert(t, errMsg, qt.Contains, "vendor/")
}

// --- ChainFileResolver ---

func TestChainFileResolver_FirstResolverWins(t *testing.T) {
	fs1 := fstest.MapFS{"a.scm": {Data: []byte("from-fs1")}}
	fs2 := fstest.MapFS{"a.scm": {Data: []byte("from-fs2")}}

	r := NewChainFileResolver([]environment.FileResolver{
		NewEmbedFileResolver(fs1),
		NewEmbedFileResolver(fs2),
	})

	f, resolved, err := r.ResolveAndOpen(context.Background(), "a.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "a.scm")

	// Verify we got fs1's content
	buf := make([]byte, 20)
	n, _ := f.Read(buf)
	qt.Assert(t, string(buf[:n]), qt.Equals, "from-fs1")
}

func TestChainFileResolver_FallsThrough(t *testing.T) {
	fs1 := fstest.MapFS{"only-in-fs1.scm": {Data: []byte("1")}}
	fs2 := fstest.MapFS{"only-in-fs2.scm": {Data: []byte("2")}}

	r := NewChainFileResolver([]environment.FileResolver{
		NewEmbedFileResolver(fs1),
		NewEmbedFileResolver(fs2),
	})

	// File only in fs2 — falls through fs1.
	f, resolved, err := r.ResolveAndOpen(context.Background(), "only-in-fs2.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "only-in-fs2.scm")
}

func TestChainFileResolver_AllMiss(t *testing.T) {
	fs1 := fstest.MapFS{}
	fs2 := fstest.MapFS{}

	r := NewChainFileResolver([]environment.FileResolver{
		NewEmbedFileResolver(fs1),
		NewEmbedFileResolver(fs2),
	})

	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestChainFileResolver_SecurityDenialStopsChain(t *testing.T) {
	// First resolver denies access — chain should NOT fall through.
	fsys := fstest.MapFS{"secret.scm": {Data: []byte("classified")}}
	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.DenyAll())
	env := ns.Runtime()

	backup := fstest.MapFS{"secret.scm": {Data: []byte("backup")}}

	r := NewChainFileResolver([]environment.FileResolver{
		NewFSFileResolver(fsys, env),
		NewEmbedFileResolver(backup),
	})

	_, _, err := r.ResolveAndOpen(context.Background(), "secret.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestChainFileResolver_PanicsOnEmptyResolvers(t *testing.T) {
	qt.Assert(t, func() {
		NewChainFileResolver([]environment.FileResolver{})
	}, qt.PanicMatches, `.*resolvers must not be empty.*`)
}

// --- FSFileResolver EnumerateFiles ---

func TestFSFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("(define-library (scheme base))")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("(define-library (scheme write))")},
		"chibi/test.scm":   &fstest.MapFile{Data: []byte("(define-library (chibi test))")},
		".hidden/lib.sld":  &fstest.MapFile{Data: []byte("skip")},
		"readme.txt":       &fstest.MapFile{Data: []byte("not a library")},
	}

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"."}})
	env := ns.Runtime()

	resolver := NewFSFileResolver(fsys, env)

	// Verify FSFileResolver satisfies FileEnumerator via interface variable.
	var fr environment.FileResolver = resolver
	enumerator, ok := fr.(FileEnumerator)
	c.Assert(ok, qt.IsTrue)

	files, err := enumerator.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	slices.Sort(files)
	c.Assert(files, qt.DeepEquals, []string{"chibi/test.scm", "scheme/base.sld", "scheme/write.sld"})
}

func TestFSFileResolverEnumerateWithSearchPaths(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"lib/scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"lib"}})
	env := ns.Runtime()

	resolver := NewFSFileResolver(fsys, env)

	files, err := resolver.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	slices.Sort(files)
	c.Assert(files, qt.DeepEquals, []string{"scheme/base.sld", "scheme/write.sld"})
}

func TestFSFileResolverEnumerateSldAndScm(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"scheme/base.scm": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"."}})
	env := ns.Runtime()

	resolver := NewFSFileResolver(fsys, env)

	files, err := resolver.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	// EnumerateFiles returns BOTH paths with no dedup.
	c.Assert(len(files), qt.Equals, 2)
	slices.Sort(files)
	c.Assert(files, qt.DeepEquals, []string{"scheme/base.scm", "scheme/base.sld"})
}

func TestFSFileResolverEnumerateDotSlashSearchPath(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"stdlib/lib/scheme/base.sld": &fstest.MapFile{Data: []byte("")},
	}

	// "./stdlib/lib" has a leading "./" that is invalid for fs.ValidPath.
	// EnumerateFiles must clean it to "stdlib/lib" before walking.
	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"./stdlib/lib"}})
	env := ns.Runtime()

	resolver := NewFSFileResolver(fsys, env)

	files, err := resolver.EnumerateFiles()
	c.Assert(err, qt.IsNil)
	c.Assert(files, qt.Contains, "scheme/base.sld")
}

// --- OSFileResolver EnumerateFiles ---

func TestOSFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	dir := realDir(t, t.TempDir())

	libDir := filepath.Join(dir, "libs")
	c.Assert(os.MkdirAll(filepath.Join(libDir, "scheme"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "base.sld"), []byte(""), 0o644), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "write.sld"), []byte(""), 0o644), qt.IsNil)

	c.Assert(os.MkdirAll(filepath.Join(libDir, ".hidden"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, ".hidden", "skip.sld"), []byte(""), 0o644), qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{libDir}})
	env := ns.Runtime()

	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to an empty temp dir so CWD fallback doesn't find project libraries.
	t.Chdir(t.TempDir())

	resolver := NewOSFileResolver(env)
	files, err := resolver.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	slices.Sort(files)
	c.Assert(files, qt.DeepEquals, []string{"scheme/base.sld", "scheme/write.sld"})
}

func TestOSFileResolverEnumerateEmptySearchPaths(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"/nonexistent/path"}})
	env := ns.Runtime()

	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to an empty temp dir so CWD fallback doesn't find project libraries.
	t.Chdir(t.TempDir())

	resolver := NewOSFileResolver(env)
	files, err := resolver.EnumerateFiles()
	c.Assert(err, qt.IsNil)
	c.Assert(len(files), qt.Equals, 0)
}

// --- ChainFileResolver EnumerateFiles ---

func TestChainFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	fs1 := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"my/lib.sld":      &fstest.MapFile{Data: []byte("")},
	}
	fs2 := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	ns.SetLibraryRegistry(&testSearcher{paths: []string{"."}})
	env := ns.Runtime()

	chain := NewChainFileResolver([]environment.FileResolver{
		NewFSFileResolver(fs1, env),
		NewFSFileResolver(fs2, env),
	})

	files, err := chain.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	// Results are concatenated from child resolvers (fs1 first, then fs2).
	// No dedup — scheme/base.sld appears from both.
	// Verify all paths from both children are present.
	slices.Sort(files)

	// fs1 contributes: my/lib.sld, scheme/base.sld
	// fs2 contributes: scheme/base.sld, scheme/write.sld
	// Total: 4 paths (no dedup)
	c.Assert(files, qt.DeepEquals, []string{
		"my/lib.sld",
		"scheme/base.sld",
		"scheme/base.sld",
		"scheme/write.sld",
	})
}

// --- Interface compliance ---

func TestFileResolverInterfaceCompliance(t *testing.T) {
	// Compile-time check that all types satisfy FileResolver.
	var _ environment.FileResolver = (*OSFileResolver)(nil)
	var _ environment.FileResolver = (*EmbedFileResolver)(nil)
	var _ environment.FileResolver = (*FSFileResolver)(nil)
	var _ environment.FileResolver = (*ChainFileResolver)(nil)

	// Compile-time check that enumerator types satisfy FileEnumerator.
	var _ FileEnumerator = (*FSFileResolver)(nil)
	var _ FileEnumerator = (*OSFileResolver)(nil)
	var _ FileEnumerator = (*ChainFileResolver)(nil)

	// EmbedFileResolver intentionally does NOT implement FileEnumerator.
	var embed environment.FileResolver = (*EmbedFileResolver)(nil)
	_, ok := embed.(FileEnumerator)
	qt.Assert(t, !ok, qt.IsTrue)
}
