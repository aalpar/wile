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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// recordingAuthorizer wraps an authorizer and remembers every request, so a
// test can assert not just the decision but that the gate was consulted at all.
// The finding these tests pin was invisible to decision-only assertions: the
// authorizer recorded zero requests because there was no request to record.
type recordingAuthorizer struct {
	inner security.Authorizer
	reqs  []security.AccessRequest
}

func (p *recordingAuthorizer) Authorize(req security.AccessRequest) error {
	p.reqs = append(p.reqs, req)
	return p.inner.Authorize(req)
}

func (p *recordingAuthorizer) streamTargets() []string {
	var q []string
	for _, r := range p.reqs {
		if r.Resource == security.ResourceStream {
			q = append(q, r.Action+":"+r.Target)
		}
	}
	return q
}

// codeLoadRequests returns the recorded code:load requests, in order.
func (p *recordingAuthorizer) codeLoadRequests() []security.AccessRequest {
	var q []security.AccessRequest
	for _, r := range p.reqs {
		if r.Resource == security.ResourceCode && r.Action == security.ActionLoad {
			q = append(q, r)
		}
	}
	return q
}

// TestVirtualSourceIsNotGatedAgainstProcessCWD pins the end of the CWD
// coincidence. A file served by WithSourceFS is named by a path that only its
// fs.FS understands ("evil.scm"), and a path-confining authorizer resolves such
// a name against the PROCESS working directory. With the CWD inside the
// confinement root the untrusted program was admitted and ran; with the CWD
// outside it was refused for a reason that had nothing to do with the file.
func TestVirtualSourceIsNotGatedAgainstProcessCWD(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	trusted, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	untrusted, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(untrusted, "evil.scm"), []byte("(define evil-ran 42)"), 0o644)
	c.Assert(err, qt.IsNil)

	rec := &recordingAuthorizer{inner: security.FilesystemRoot(trusted)}
	engine, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithAuthorizer(rec),
		WithSourceFS(os.DirFS(untrusted)))
	c.Assert(err, qt.IsNil)
	defer engine.Close() //nolint:errcheck // test cleanup

	t.Chdir(trusted)

	_, err = engine.EvalMultiple(ctx, `(include "evil.scm") evil-ran`)
	c.Assert(err, qt.IsNotNil, qt.Commentf("a virtual path is not a path under the root"))
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)

	// The gate must have been consulted with the path the fs.FS was asked for,
	// verbatim — the missing assertion that let the CWD coincidence hide.
	//
	// EVERY consultation, not the first: a single unlabelled request is enough
	// to put the verdict back on the process working directory, and inspecting
	// only reqs[0] is what let exactly that survive on the library cache-hit
	// path (see TestLibraryCacheHitReauthorizesWithItsOwnSource).
	reqs := rec.codeLoadRequests()
	c.Assert(len(reqs) > 0, qt.IsTrue, qt.Commentf("no code:load request was recorded"))
	for i, r := range reqs {
		c.Assert(r.Target, qt.Equals, "evil.scm", qt.Commentf("request %d", i))
		c.Assert(r.TargetSource, qt.Equals, security.SourceVirtualFS, qt.Commentf("request %d", i))
	}
}

// TestLibraryCacheHitReauthorizesWithItsOwnSource pins that the re-authorization
// a cache hit performs asks the SAME question the resolver asked on the miss.
//
// The hit gate manufactures its request from the recorded source path. Omitting
// TargetSource made a virtual library path be judged as an OS path, which
// containedInRoot resolves against the process working directory — the exact
// coincidence the resolver gate stopped depending on, reintroduced one layer up.
// It broke the opt-in authorizer this branch ships for embedders serving a
// virtual stdlib: (import (alpha)) failed on the FIRST import from any normal
// CWD, because building the export index has already loaded the library, so the
// import is a hit.
func TestLibraryCacheHitReauthorizesWithItsOwnSource(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	root, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	virtual, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(virtual, "alpha.sld"),
		[]byte("(define-library (alpha) (export a) (begin (define a 42)))"), 0o644)
	c.Assert(err, qt.IsNil)

	// A CWD deliberately outside the confinement root: under the defect the
	// verdict flipped with this choice, which is what made it a coincidence.
	outside, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	t.Chdir(outside)

	rec := &recordingAuthorizer{inner: security.FilesystemRootWithVirtualSources(root)}
	engine, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithAuthorizer(rec),
		WithSourceFS(os.DirFS(virtual)),
		WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)
	defer engine.Close() //nolint:errcheck // test cleanup

	v, err := engine.EvalMultiple(ctx, `(import (alpha)) a`)
	c.Assert(err, qt.IsNil, qt.Commentf("the opt-in variant must serve a virtual library"))
	c.Assert(v.SchemeString(), qt.Equals, "42")

	// At least two: the resolver's, and the cache hit's. Every one of them names
	// the virtual source, or one of them is judging a virtual path as a host one.
	reqs := rec.codeLoadRequests()
	c.Assert(len(reqs) >= 2, qt.IsTrue,
		qt.Commentf("want a resolver request and a cache-hit request, got %d", len(reqs)))
	for i, r := range reqs {
		c.Assert(r.TargetSource, qt.Equals, security.SourceVirtualFS,
			qt.Commentf("request %d (%q) lost its source", i, r.Target))
	}
}

// TestSourceChainFallsThroughAVirtualDenial is the end-to-end form of the chain
// protocol: the documented WithSourceFS + WithSourceOS pairing must keep working
// under a path-confining authorizer.
//
// Since the resolver authorizes a candidate before stat'ing it, a name absent
// from the fs.FS is refused rather than reported missing, and FilesystemRoot
// refuses every virtual target by construction. Treating that refusal as final
// made the OS half of the chain unreachable: an (include …) of a file sitting
// inside the confinement root reported "access denied".
func TestSourceChainFallsThroughAVirtualDenial(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	trusted, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	virtual, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(trusted, "lib.scm"), []byte("(define chain-ran 7)"), 0o644)
	c.Assert(err, qt.IsNil)

	engine, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithAuthorizer(security.FilesystemRoot(trusted)),
		WithSourceFS(os.DirFS(virtual)),
		WithSourceOS())
	c.Assert(err, qt.IsNil)
	defer engine.Close() //nolint:errcheck // test cleanup

	t.Chdir(trusted)

	v, err := engine.EvalMultiple(ctx, `(include "lib.scm") chain-ran`)
	c.Assert(err, qt.IsNil, qt.Commentf("the permitted host copy must still be reachable"))
	c.Assert(v.SchemeString(), qt.Equals, "7")

	// The policy is not weakened by the fall-through: a host file OUTSIDE the
	// root is still refused, and the refusal is not reported as an absence.
	outside, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(outside, "outside.scm"), []byte("(define escaped 1)"), 0o644)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, fmt.Sprintf(`(include %q)`,
		filepath.Join(outside, "outside.scm")))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("got: %v", err))
}

// TestHostStdioIsGated pins the stream gate (reviews/2026-08-07/REVIEW.md 2.1.1).
// A Console engine under DenyAll used to read the host's stdin and write its
// stdout with the authorizer never consulted.
func TestHostStdioIsGated(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	rec := &recordingAuthorizer{inner: security.DenyAll()}
	engine, err := NewEngine(ctx, WithProfile(Console), WithAuthorizer(rec), WithSandbox())
	c.Assert(err, qt.IsNil)

	c.Assert(rec.streamTargets(), qt.DeepEquals,
		[]string{"read:stdin", "write:stdout", "write:stderr"})

	_, err = engine.EvalMultiple(ctx, `(display "PWNED")`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	_, err = engine.EvalMultiple(ctx, `(read-line)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrPortClosed), qt.IsTrue)

	// The refusal is observable from Scheme rather than silent.
	v, err := engine.EvalMultiple(ctx, `(output-port-open? (current-output-port))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "#f")

	v, err = engine.EvalMultiple(ctx, `(output-port-open? (current-error-port))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "#f")

	// A denied stream must not leave the program without a working port: an
	// in-memory port opened by the program itself is unaffected.
	v, err = engine.EvalMultiple(ctx,
		`(let ((p (open-output-string))) (display "ok" p) (get-output-string p))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, `"ok"`)
}

// TestProfileStreamsStayOpen pins the other direction: the shipped profiles and
// the WithSandbox modifier keep the host streams, which is what wile.Console
// documents ("stdin/stdout/stderr available").
func TestProfileStreamsStayOpen(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		name string
		opts []EngineOption
	}{
		{"console", []EngineOption{WithProfile(Console)}},
		{"console+sandbox", []EngineOption{WithProfile(Console), WithSandbox()}},
		{"console-with-load", []EngineOption{WithProfile(ConsoleWithLoad)}},
		{"kitchen-sink", []EngineOption{WithProfile(KitchenSink)}},
		{"read-only", []EngineOption{WithProfile(Console), WithAuthorizer(security.ReadOnly())}},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := NewEngine(ctx, tc.opts...)
			c.Assert(err, qt.IsNil)

			v, err := engine.EvalMultiple(ctx, `(output-port-open? (current-output-port))`)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "#t")

			v, err = engine.EvalMultiple(ctx, `(input-port-open? (current-input-port))`)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, "#t")
		})
	}
}

// TestStreamGateIsPerStream pins that the three streams are decided
// independently, so a policy can hand out stdout without handing out stdin.
func TestStreamGateIsPerStream(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceStream && req.Target == security.StreamStdin {
			return security.ErrAccessDenied
		}
		return nil
	})

	engine, err := NewEngine(ctx, WithProfile(Console), WithAuthorizer(auth))
	c.Assert(err, qt.IsNil)

	v, err := engine.EvalMultiple(ctx, `(input-port-open? (current-input-port))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "#f")

	v, err = engine.EvalMultiple(ctx, `(output-port-open? (current-output-port))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "#t")
}
