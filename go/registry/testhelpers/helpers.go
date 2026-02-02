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
package testhelpers

import (
	"context"
	"testing"
	"time"

	"github.com/aalpar/wile/go/values"
	"github.com/aalpar/wile/go/wile"
)

// RunSchemeCode parses and runs Scheme source code string using the default Engine.
func RunSchemeCode(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	engine, err := wile.NewEngine()
	if err != nil {
		return nil, err
	}
	result, err := engine.Eval(context.Background(), code)
	if err != nil {
		return nil, err
	}
	return unwrapValue(result), nil
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
		if r := recover(); r != nil {
			// Panic was expected, convert to error
			if e, ok := r.(error); ok {
				err = e
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
	engine, err := wile.NewEngine()
	if err != nil {
		return nil, err
	}
	result, err := engine.Eval(ctx, code)
	if err != nil {
		return nil, err
	}
	return unwrapValue(result), nil
}

// unwrapValue extracts the underlying values.Value from wile.Value.
func unwrapValue(v wile.Value) values.Value {
	if v == nil {
		return nil
	}
	return v.Internal()
}
