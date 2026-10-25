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
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestCancelledAtLeavesOutAMissingLocation: a cancelled body's failure with no
// location renders the failure alone, not ", at  (...)".
func TestCancelledAtLeavesOutAMissingLocation(t *testing.T) {
	for _, tc := range []struct {
		name  string
		cause error
		want  string
	}{
		{"escape", &machine.ErrExceptionEscape{Condition: values.NewNativeError("thread-sleep!: cancelled")}, " (thread-sleep!: cancelled)"},
		{"scheme error", machine.NewSchemeError("Run: cancelled at the VM poll", nil, ""), " (Run: cancelled at the VM poll)"},
		{"neither", werr.ErrOperationCancelled, ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, cancelledAt(tc.cause), qt.Equals, tc.want)
		})
	}
}
