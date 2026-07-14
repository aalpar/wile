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

package machine

import (
	"reflect"
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestSliceCarriersAreNotValues pins the specific regression this package is prone to:
// a slice-backed VM container quietly acquiring a values.Value conformance.
//
// The GENERAL contract — every values.Value implementor is Go-comparable — is enforced
// module-wide by values.TestValue_AllImplementorsAreGoComparable, which type-checks all
// seven implementing packages with go/types and needs no roster. This file used to carry
// a hand-maintained roster of 7 exemplars plus a test named ...CoverPackage that scanned
// nothing; the package declares ~50 values.Value types (Operation embeds values.Value, so
// every Operation* is one), so the roster covered 14% of what it claimed. It is gone.
//
// What remains here is the part go/types cannot say: these two types must not be Values
// AT ALL. Both are slice-shaped VM containers with value receivers, not Scheme data:
//
//   - Operations ([]Operation) — the compiler's opcode list
//   - MultipleValues ([]values.Value) — the VM's multi-return carrier
//
// Both once implemented values.Value for container convenience, and the cost was paid by
// the equality core: values.Equal could not assume its operands were comparable, and
// values.EqIdentity — a bare `a == b` on the eq? hot path — was a latent host crash.
// Re-adding SchemeString/IsVoid/EqualTo(values.Value) to either would make them
// non-comparable Values again and re-open it. Note that MultipleValues still carries
// SchemeString and IsVoid for diagnostics, so it is ONE method away.
//
// A third violator, boxedValues, genuinely IS a Value (OperationBoxValues puts it in the
// value register) and was fixed by making it pointer-shaped rather than by removing the
// conformance. The module-wide test guards it.
//
// The contract itself is stated on values.Value's doc comment.
func TestSliceCarriersAreNotValues(t *testing.T) {
	valueIface := reflect.TypeFor[values.Value]()

	for _, nonValue := range []any{Operations{}, MultipleValues{}} {
		rt := reflect.TypeOf(nonValue)
		qt.Assert(t, rt.Implements(valueIface), qt.IsFalse,
			qt.Commentf("%s must NOT implement values.Value: it is a slice type with value "+
				"receivers, so boxing it into a Value interface makes eq? panic with "+
				"\"comparing uncomparable type\". It is a VM container, not a Scheme datum. "+
				"See the type's doc comment.", rt))
	}
}
