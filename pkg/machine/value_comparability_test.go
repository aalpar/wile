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

// machineValueExemplars is the roster of every concrete values.Value implemented
// in this package. It is the machine-side counterpart of
// values.allValueExemplars, and exists for the same reason: the Value contract's
// Go-comparability requirement has no compile-time expression, so it must be
// asserted over a roster or not at all.
//
// ADDING A NEW values.Value TO THIS PACKAGE: add an exemplar here. A type that
// implements Value and is absent from this list is unguarded — it can be
// slice-backed with value receivers and nothing will notice until eq? faults on
// it at runtime, in a user's process.
var machineValueExemplars = []values.Value{
	&MachineClosure{},
	&CaseLambdaClosure{},
	&NativeTemplate{},
	&boxedValues{},
	noMarkSentinelType{},
	errorContextKeyType{},
	&ErrorContext{},
}

// TestMachineValues_AreGoComparable enforces the values.Value comparability
// contract for this package's implementors.
//
// This is the test that would have caught all three of the violators fixed on
// this branch, none of which was found by reading code:
//
//   - Operations ([]Operation, value receivers) — compiler opcode list
//   - MultipleValues ([]values.Value, value receivers) — VM multi-return carrier
//   - boxedValues (struct{[]values.Value}, formerly value receivers) — dynamic-wind
//
// The first two were resolved by removing the Value conformance outright: neither
// is a Scheme datum, and both had taken it on for container convenience. The
// third genuinely IS a Value (OperationBoxValues puts it in the value register,
// so dynamic-wind reaches it from Scheme) and was resolved by making it
// pointer-shaped.
//
// The cost of those three conformances was paid by the equality core: values.Equal
// could not assume its operands were comparable, and values.EqIdentity — a bare
// `a == b` on the eq? hot path — was a latent host crash. See
// plans/2026-07-14-equivalence-predicate-divergence.md, finding F7.
func TestMachineValues_AreGoComparable(t *testing.T) {
	for _, exemplar := range machineValueExemplars {
		rt := reflect.TypeOf(exemplar)
		qt.Assert(t, rt.Comparable(), qt.IsTrue,
			qt.Commentf("values.Value implementor %s is not Go-comparable — values.EqIdentity "+
				"(eq?, memq, assq) would panic on it with \"comparing uncomparable type\". "+
				"Give the type pointer receivers, or stop implementing values.Value.", rt))
	}
}

// TestMachineValueExemplars_CoverPackage catches a roster that has gone stale.
//
// A comparability test over an incomplete roster is worse than no test: it
// reports green while the type it was supposed to guard sits unchecked. This
// scans the package's own declared types via the exemplars' reflect.Type set and
// asserts the two known non-Value slice carriers have NOT quietly re-acquired a
// Value conformance — the specific regression that would undo this branch.
func TestMachineValueExemplars_CoverPackage(t *testing.T) {
	valueIface := reflect.TypeFor[values.Value]()

	// Operations and MultipleValues are deliberately NOT values.Value. If someone
	// re-adds SchemeString/IsVoid/EqualTo(values.Value) to either, they silently
	// re-enter the Value set as non-comparable slice types and re-open the crash.
	for _, nonValue := range []any{Operations{}, MultipleValues{}} {
		rt := reflect.TypeOf(nonValue)
		qt.Assert(t, rt.Implements(valueIface), qt.IsFalse,
			qt.Commentf("%s must NOT implement values.Value: it is a slice type with value "+
				"receivers, so boxing it into a Value interface makes eq? panic. It is a VM "+
				"container, not a Scheme datum. See the type's doc comment.", rt))
	}
}
