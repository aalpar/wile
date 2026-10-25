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

package values_test

import (
	"testing"
	"unsafe"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestSourceContextFitsItsSizeClass is a ratchet on the reader's largest
// allocation: every syntax node carries a SourceContext, which is ~43% of the
// bytes a large file's parse allocates. 96 is a Go size class and 97 to 112
// bytes allocate from the 112 class, so one more 8-byte field costs 16 bytes per
// node. SourceIndexes' int32 line and column are what paid for FileIsPath; a
// field added here has to find its room the same way, or argue for the class.
func TestSourceContextFitsItsSizeClass(t *testing.T) {
	qt.Assert(t, unsafe.Sizeof(values.SourceIndexes{}), qt.Equals, uintptr(16))
	qt.Assert(t, unsafe.Sizeof(values.SourceContext{}) <= 96, qt.IsTrue,
		qt.Commentf("sizeof(SourceContext) = %d", unsafe.Sizeof(values.SourceContext{})))
}
