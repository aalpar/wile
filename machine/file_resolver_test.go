package machine

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

// realDir normalizes a temp directory path to account for macOS symlinks
// (/tmp -> /private/tmp). Without this, paths from t.TempDir() and
// filepath.Abs() inside resolvers won't match.
func realDir(t *testing.T, dir string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(dir)
	qt.Assert(t, err, qt.IsNil)
	return resolved
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

	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	// Push a file path in the target directory so CurrentDir() returns dir.
	qt.Assert(t, stack.Push(filepath.Join(dir, "parent.scm")), qt.IsNil)
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

	origDir, err := os.Getwd()
	qt.Assert(t, err, qt.IsNil)
	defer os.Chdir(origDir) //nolint:errcheck

	err = os.Chdir(dir)
	qt.Assert(t, err, qt.IsNil)

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

	env := environment.NewNamespace().Runtime()
	reg := NewLibraryRegistry()
	reg.PrependSearchPath(dir)
	env.SetLibraryRegistry(reg)

	r := NewOSFileResolver(env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "reg.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, filepath.Join(dir, "reg.scm"))
}

func TestOSFileResolver_RelativeNotFound(t *testing.T) {
	t.Setenv(SchemeIncludePathEnv, "")

	// Chdir to a temp dir that does NOT contain the file.
	dir := t.TempDir()
	origDir, err := os.Getwd()
	qt.Assert(t, err, qt.IsNil)
	defer os.Chdir(origDir) //nolint:errcheck
	err = os.Chdir(dir)
	qt.Assert(t, err, qt.IsNil)

	env := environment.NewNamespace().Runtime()
	r := NewOSFileResolver(env)

	_, _, err = r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
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

	// No authorizer on context — open by default.
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

	env := environment.NewNamespace().Runtime()
	reg := NewLibraryRegistry()
	reg.PrependSearchPath(regDir)
	env.SetLibraryRegistry(reg)

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

	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push(filepath.Join(stackDir, "parent.scm")), qt.IsNil)
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

	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push(filepath.Join(deepDir, "parent.scm")), qt.IsNil)
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
	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("lib/main.sld"), qt.IsNil)
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "helper.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "lib/helper.scm")
}

func TestFSFileResolver_ViaSearchPaths(t *testing.T) {
	fsys := fstest.MapFS{
		"vendor/util.scm": {Data: []byte("99")},
	}
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("vendor")
	env.SetLibraryRegistry(libReg)

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
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("search-dir")
	env.SetLibraryRegistry(libReg)

	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("stack-dir/parent.scm"), qt.IsNil)
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
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("search-dir")
	env.SetLibraryRegistry(libReg)

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
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	// PrependSearchPath prepends, so add in reverse order
	libReg.PrependSearchPath("second")
	libReg.PrependSearchPath("first")
	env.SetLibraryRegistry(libReg)

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
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("real-dir")
	libReg.PrependSearchPath("")
	env.SetLibraryRegistry(libReg)

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
	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("sub/current.scm"), qt.IsNil)
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
	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("sub/other.scm"), qt.IsNil)
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
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("vendor")
	libReg.PrependSearchPath("lib")
	env.SetLibraryRegistry(libReg)

	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("src/main.scm"), qt.IsNil)
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

	r := NewChainFileResolver([]FileResolver{
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

	r := NewChainFileResolver([]FileResolver{
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

	r := NewChainFileResolver([]FileResolver{
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

	r := NewChainFileResolver([]FileResolver{
		NewFSFileResolver(fsys, env),
		NewEmbedFileResolver(backup),
	})

	_, _, err := r.ResolveAndOpen(context.Background(), "secret.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestChainFileResolver_PanicsOnEmptyResolvers(t *testing.T) {
	qt.Assert(t, func() {
		NewChainFileResolver([]FileResolver{})
	}, qt.PanicMatches, `.*resolvers must not be empty.*`)
}

// --- Interface compliance ---

func TestFileResolverInterfaceCompliance(t *testing.T) {
	// Compile-time check that all types satisfy the interface.
	var _ FileResolver = (*OSFileResolver)(nil)
	var _ FileResolver = (*EmbedFileResolver)(nil)
	var _ FileResolver = (*FSFileResolver)(nil)
	var _ FileResolver = (*ChainFileResolver)(nil)
}
