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

package compilation

import (
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
)

// removableMutators are the in-place mutation primitives a dialect (e.g. wile's
// NoMutation) may remove. Kept in sync with the canonical set in
// pkg/wile/dialect_nomutation.go, duplicated here because the layering forbids
// compilation importing pkg/wile. TestInlineHOFTemplatesDeclareMutatorRequirements
// uses it to guard the soundness property below.
var removableMutators = []string{
	"set-car!", "set-cdr!", "vector-set!", "vector-fill!", "vector-copy!",
	"string-set!", "list-set!", "bytevector-u8-set!", "bytevector-copy!",
	"hashtable-set!", "hashtable-delete!", "hashtable-clear!", "set-box!",
}

// TestInlineHOFTemplatesDeclareMutatorRequirements is the drift guard for the
// requirements gate: any inline-HOF template whose body calls a removable mutation
// primitive MUST declare that primitive in spec.requires. Otherwise a dialect that
// removes the primitive would leave the HOF stamped and emit an unbound reference at
// every inlined call site (the bug the gate fixes) instead of de-optimizing cleanly.
// A future template that adds a mutator without declaring it trips this test.
func TestInlineHOFTemplatesDeclareMutatorRequirements(t *testing.T) {
	for name, spec := range inlineHOFSpecs {
		for _, m := range removableMutators {
			if !strings.Contains(spec.template, m) {
				continue
			}
			if !slices.Contains(spec.requires, m) {
				t.Errorf("inline-HOF %q template calls %q but does not declare it in requires %v",
					name, m, spec.requires)
			}
		}
	}
}

// TestInlineHOFRequiresAreLive guards the opposite drift: every primitive declared in
// requires must actually appear in the template, so a stale requirement can't silently
// suppress an inlining that is in fact valid.
func TestInlineHOFRequiresAreLive(t *testing.T) {
	for name, spec := range inlineHOFSpecs {
		for _, req := range spec.requires {
			if !strings.Contains(spec.template, req) {
				t.Errorf("inline-HOF %q declares require %q not used by its template", name, req)
			}
		}
	}
}

// TestValidateInlineHOFCallbackIndex covers the build-time guard that an inline-HOF
// spec's stamped callback index names a real parameter of its template. The live
// inlineHOFSpecs never trip it (every callback is parameter 0 of a multi-param
// template), so the error path is exercised here rather than at engine init —
// guarding a future mis-authored spec whose index would land the CaptureSafe stamp
// on the wrong argument.
func TestValidateInlineHOFCallbackIndex(t *testing.T) {
	tcs := []struct {
		name          string
		callbackParam int
		requiredCount int
		wantErr       bool
	}{
		{"in-range-first", 0, 2, false},
		{"in-range-last", 2, 3, false},
		{"out-of-range-equal", 2, 2, true},
		{"out-of-range-over", 3, 2, true},
		{"negative", -1, 2, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := validateInlineHOFCallbackIndex("test-hof", tc.callbackParam, tc.requiredCount)
			if !tc.wantErr {
				if err != nil {
					t.Errorf("callbackParam %d of %d: unexpected error %v", tc.callbackParam, tc.requiredCount, err)
				}
				return
			}
			if err == nil {
				t.Fatalf("callbackParam %d of %d: want error, got nil", tc.callbackParam, tc.requiredCount)
			}
			if !errors.Is(err, werr.ErrEngineInit) {
				t.Errorf("want ErrEngineInit, got %v", err)
			}
		})
	}
}
