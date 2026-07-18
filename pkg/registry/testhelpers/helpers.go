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

// Package testhelpers provides shared test infrastructure for Scheme primitive tests.
//
// All functions use the internal pipeline (parser → expander → compiler → VM)
// with a full environment including all extensions. This matches the environment
// available in the standalone scheme binary.
package testhelpers

import (
	"context"
	"fmt"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/values"
)

// NewBootstrappedNamespace builds a fully bootstrapped namespace and returns it for
// frame-topology assertions (e.g. sealed-base vs mutable-runtime placement). Mirrors
// RunSchemeCode's setup but exposes the *Namespace, which RunSchemeCode does not.
func NewBootstrappedNamespace(t *testing.T) *environment.Namespace {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	qt.Assert(t, err, qt.IsNil)
	return env.Namespace()
}

// panicNonError wraps a recovered non-error panic value as an error.
type panicNonError struct {
	v any
}

func (p panicNonError) Error() string {
	return fmt.Sprintf("panic (non-error): %v", p.v)
}

// RunSchemeCode parses and runs Scheme source code string using a fresh
// environment with all core primitives and extensions loaded.
func RunSchemeCode(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.Background())
	if err != nil {
		return nil, err
	}
	return RunSchemeCodeWithEnv(t, env, code)
}

// SchemeCodeTestCase is the common struct for table-driven tests that run Scheme code
// and compare against an expected value.
type SchemeCodeTestCase struct {
	Name     string
	Code     string
	Expected values.Value
}

// SchemeCodeErrorTestCase is the common struct for table-driven tests that run Scheme code
// and expect an error (or just verify execution without checking the result).
type SchemeCodeErrorTestCase struct {
	Name string
	Code string
}

// RunSchemeCodeExpectError runs code and expects an error (including panics).
func RunSchemeCodeExpectError(t *testing.T, code string) (err error) {
	t.Helper()
	defer func() {
		r := recover()
		if r != nil {
			e, ok := r.(error)
			if ok {
				err = e
			} else {
				err = panicNonError{v: r}
			}
		}
	}()
	_, err = RunSchemeCode(t, code)
	if err == nil {
		t.Errorf("expected error but got none for: %s", code)
	}
	return err
}

// RunSchemeCodeExpectTrue is a shorthand for boolean true result.
func RunSchemeCodeExpectTrue(t *testing.T, code string) {
	t.Helper()
	result, err := RunSchemeCode(t, code)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != values.TrueValue {
		t.Errorf("expected #t but got %v for: %s", result, code)
	}
}

// RunSchemeCodeExpectFalse is a shorthand for boolean false result.
func RunSchemeCodeExpectFalse(t *testing.T, code string) {
	t.Helper()
	result, err := RunSchemeCode(t, code)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result != values.FalseValue {
		t.Errorf("expected #f but got %v for: %s", result, code)
	}
}

// RunSchemeCodeWithTimeout runs code with a timeout to prevent infinite loops.
// Uses context.WithTimeout for proper cooperative cancellation - the VM loop
// checks ctx.Done() on each iteration and exits cleanly when cancelled.
func RunSchemeCodeWithTimeout(t *testing.T, code string, timeout time.Duration) (values.Value, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	return RunSchemeCodeWithContext(ctx, t, code)
}

// RunSchemeCodeWithContext parses and runs Scheme source code with the given context.
// The context enables cancellation/timeout - the VM loop checks ctx.Done() on each iteration.
func RunSchemeCodeWithContext(ctx context.Context, t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(ctx)
	if err != nil {
		return nil, err
	}
	return RunSchemeCodeWithEnvAndContext(ctx, t, env, code)
}

// RunSchemeCodeExpectCatchable asserts that evaluating code raises a condition
// that Scheme `guard` can catch — not an uncatchable host-boundary panic and not
// a normal return. It wraps code in `(eq? 'wile-caught (guard (e (#t 'wile-caught)) code))`
// and requires the result to be #t. A Go panic that escapes RunResumable
// (recover() != nil here) fails the test, distinguishing "raised a catchable
// condition" from "panicked"; a non-nil error means the condition escaped guard
// (uncatchable). Distinct from RunSchemeCodeExpectError, which scores a recovered
// panic as a satisfied error.
func RunSchemeCodeExpectCatchable(t *testing.T, code string) {
	t.Helper()
	defer func() {
		r := recover()
		if r != nil {
			t.Fatalf("primitive panicked (uncatchable) for %q: %v", code, r)
		}
	}()
	guarded := fmt.Sprintf("(eq? 'wile-caught (guard (e (#t 'wile-caught)) %s))", code)
	result, err := RunSchemeCode(t, guarded)
	if err != nil {
		t.Fatalf("condition escaped guard (uncatchable) for %q: %v", code, err)
	}
	if result != values.TrueValue {
		t.Fatalf("expected a caught condition (#t) for %q, got %v", code, result)
	}
}
