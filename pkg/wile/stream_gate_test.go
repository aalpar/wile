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
	reqs := rec.codeLoadRequests()
	c.Assert(len(reqs) > 0, qt.IsTrue, qt.Commentf("no code:load request was recorded"))
	c.Assert(reqs[0].Target, qt.Equals, "evil.scm")
	c.Assert(reqs[0].TargetSource, qt.Equals, security.SourceVirtualFS)
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
