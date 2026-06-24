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
	"strings"
	"testing"
)

// TestRuntimeErrorSourceProvenance verifies that a runtime error carries
// :line:col provenance in RuntimeError.Source whether or not the program has a
// file name. A nameless EvalMultiple program (File == "") previously got an empty
// Source even though the position survived in the stack trace; a named program
// must keep its "file:line:col" form unchanged.
func TestRuntimeErrorSourceProvenance(t *testing.T) {
	tcs := []struct {
		name       string
		source     string // "" => EvalMultiple (nameless); else EvalMultipleWithSource
		wantPrefix string
	}{
		{
			name:       "nameless eval emits :line:col",
			source:     "",
			wantPrefix: ":1:",
		},
		{
			name:       "named source keeps file:line:col",
			source:     "prog.scm",
			wantPrefix: "prog.scm:1:",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()

			// car of a non-pair compiles fine but raises at runtime.
			const code = "(car 5)"
			if tc.source == "" {
				_, err = eng.EvalMultiple(ctx, code)
			} else {
				_, err = eng.EvalMultipleWithSource(ctx, code, tc.source)
			}
			if err == nil {
				t.Fatal("expected runtime error")
			}
			var re *RuntimeError
			if !errors.As(err, &re) {
				t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
			}
			if !strings.HasPrefix(re.Source, tc.wantPrefix) {
				t.Errorf("RuntimeError.Source = %q, want prefix %q", re.Source, tc.wantPrefix)
			}
		})
	}
}
