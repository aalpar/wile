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
	"testing"

	"github.com/aalpar/wile/pkg/werr"
)

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
