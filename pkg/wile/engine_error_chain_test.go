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

	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// TestErrorChain_SecurityDenial_PreservesErrAccessDenied verifies that the full
// wrapping chain from ErrAccessDenied through goErrorToSchemeException to
// RuntimeError preserves errors.Is reachability.
func TestErrorChain_SecurityDenial_PreservesErrAccessDenied(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithExtension(files.Extension),
		WithAuthorizer(security.DenyAll()),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// security.Check happens before the file is opened, so no actual file needed.
	_, err = eng.Eval(ctx, eng.MustParse(ctx, `(open-input-file "any-path")`))
	c.Assert(err, qt.IsNotNil)

	// Outermost error must be a RuntimeError.
	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
	}

	// ErrAccessDenied must be reachable through the full wrapping chain.
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("errors.Is chain broken; full error: %v", err))
}

// TestErrorChain_CallDepthExceeded verifies that ErrCallDepthExceeded is
// reachable through the RuntimeError wrapping chain.
func TestErrorChain_CallDepthExceeded(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithMaxCallDepth(5))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.Eval(ctx, eng.MustParse(ctx, `(letrec ((f (lambda (n) (+ 1 (f n))))) (f 0))`))
	c.Assert(err, qt.IsNotNil)

	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
	}

	c.Assert(errors.Is(err, werr.ErrCallDepthExceeded), qt.IsTrue,
		qt.Commentf("errors.Is chain broken; full error: %v", err))
}
