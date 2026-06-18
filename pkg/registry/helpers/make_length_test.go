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

package helpers

import (
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
)

func TestValidateMakeLength(t *testing.T) {
	tcs := []struct {
		name    string
		n       int64
		wantErr bool
	}{
		{"negative", -1, true},
		{"zero", 0, false},
		{"small", 100, false},
		{"at max", MaxMakeLength, false},
		{"over max", MaxMakeLength + 1, true},
		{"absurd (audit example)", 9999999999, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := ValidateMakeLength(tc.n, "make-test")
			if tc.wantErr {
				if !errors.Is(err, werr.ErrInvalidArgument) {
					t.Fatalf("n=%d: want ErrInvalidArgument, got: %v", tc.n, err)
				}
				return
			}
			if err != nil {
				t.Fatalf("n=%d: want nil, got: %v", tc.n, err)
			}
		})
	}
}
