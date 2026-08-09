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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/security"
)

// End-to-end gate for review 2026-08-07 wave 2 item 12.
//
// THE VACUITY TRAP: without WithExtension(eval.Extension) these tests pass for
// the wrong reason. A WithoutCore() engine has no `eval` and no `environment`
// binding, so the acquisition cannot even be attempted and a "was refused"
// assertion is satisfied by an unbound-variable error. The fail-open needs a
// reachable eval to demonstrate.

// evalOnlyRecorder is an authorizer that permits code:eval and nothing else,
// wrapped so the test can assert which requests the gates actually issued.
func evalOnlyRecorder() *recordingAuthorizer {
	inner := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if req.Resource == security.ResourceCode && req.Action == security.ActionEval {
			return nil
		}
		return security.ErrAccessDenied
	})
	return &recordingAuthorizer{inner: inner}
}

// namespaceRequests projects the recorder down to the namespace:create
// requests it saw, as "action:target".
func namespaceRequests(rec *recordingAuthorizer) []string {
	var q []string
	for _, r := range rec.reqs {
		if r.Resource == security.ResourceNamespace {
			q = append(q, r.Action+":"+r.Target)
		}
	}
	return q
}

// TestProfileNamespaceDoesNotHandOutCoreUngated is the end-to-end gate. At
// 003b3353 the form SUCCEEDED, returning 1; the recorder held three code:eval
// requests and zero namespace:create, and string-append and vector-fill! were
// acquired the same way. extensionPrimitiveNames walked only the extension
// list, so a request for tiny looked like a request for nothing.
func TestProfileNamespaceDoesNotHandOutCoreUngated(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	rec := evalOnlyRecorder()
	eng, err := NewEngine(ctx,
		WithoutCore(),
		WithExtension(eval.Extension),
		WithAuthorizer(rec))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	// The engine really is core-less: car is unbound at its own top level.
	_, err = eng.EvalMultiple(ctx, "(car (cons 1 2))")
	c.Assert(err, qt.IsNotNil, qt.Commentf("WithoutCore() engine still binds car; the gate below proves nothing"))

	_, err = eng.EvalMultiple(ctx, "(eval '(car (cons 1 2)) (environment '(wile tiny)))")
	c.Assert(err, qt.IsNotNil, qt.Commentf("core acquired through (environment '(wile tiny)) with no capability question"))
	c.Assert(errors.Is(err, bootstrap.ErrProfileWidensEngine), qt.IsTrue,
		qt.Commentf("refusal did not come from the profile-widening gate: %v", err))
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue)

	c.Assert(namespaceRequests(rec), qt.DeepEquals, []string{"create:tiny"},
		qt.Commentf("the widening gate must be consulted exactly once, with the profile name as target"))
}

// TestProfileNamespaceCoreAcquisitionIsPerRequest pins trap (b): the fail-open
// was per-REQUEST, not per-engine. A core-less engine asking for kitchen-sink
// was already correctly denied at 003b3353 — kitchen-sink's extensions supply
// names the engine lacks — and must stay denied for the same reason.
func TestProfileNamespaceCoreAcquisitionIsPerRequest(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	rec := evalOnlyRecorder()
	eng, err := NewEngine(ctx,
		WithoutCore(),
		WithExtension(eval.Extension),
		WithAuthorizer(rec))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	_, err = eng.EvalMultiple(ctx,
		"(eval '(thread? (make-thread (lambda () 42))) (environment '(wile kitchen-sink)))")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, bootstrap.ErrProfileWidensEngine), qt.IsTrue)
	c.Assert(namespaceRequests(rec), qt.DeepEquals, []string{"create:kitchen-sink"})
}
