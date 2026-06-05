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

package security

import "testing"

func TestConfinementRootOf(t *testing.T) {
	tcs := []struct {
		name     string
		auth     Authorizer
		wantRoot string
		wantOK   bool
	}{
		{"console", ConsoleAuthorizer(), "/tmp", true},
		{"console-with-load", ConsoleWithLoadAuthorizer(), "/tmp", true},
		{"filesystem-root", FilesystemRoot("/app/data"), "/app/data", true},
		{"deny-all", DenyAll(), "", false},
		{"read-only", ReadOnly(), "", false},
		{"composite unwraps to member root", All(ReadOnly(), FilesystemRoot("/srv")), "/srv", true},
		{"nil", nil, "", false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			root, ok := ConfinementRootOf(tc.auth)
			if ok != tc.wantOK || root != tc.wantRoot {
				t.Fatalf("ConfinementRootOf(%s) = (%q, %v), want (%q, %v)",
					tc.name, root, ok, tc.wantRoot, tc.wantOK)
			}
		})
	}
}
