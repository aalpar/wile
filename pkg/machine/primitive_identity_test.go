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

package machine_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// TestPrimitiveIdentityIsByPointer pins both directions the mechanism rests on.
//
// Two closures stamped from ONE token agree — that is the whole point: the
// registry mints a separate *ForeignClosure per environment, and this is what
// lets a recognizer see one primitive across them.
//
// Two tokens minted with the SAME name do not agree. That is what makes an
// embedder registering their own equal-hash fail closed, where a name compare
// would have accepted it and hashed with the wrong function.
func TestPrimitiveIdentityIsByPointer(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	newStamped := func(identity *machine.PrimitiveIdentity) *machine.ForeignClosure {
		q := machine.NewForeignClosure(env, 1, false, func(machine.CallContext) error {
			return nil
		})
		q.SetIdentity(identity)
		return q
	}

	equalHash := machine.NewPrimitiveIdentity("equal-hash")
	sealedCopy := newStamped(equalHash)
	libraryCopy := newStamped(equalHash)
	qt.Assert(t, machine.IdentityOf(sealedCopy), qt.Equals, machine.IdentityOf(libraryCopy),
		qt.Commentf("two environments' copies of one primitive must agree"))
	qt.Assert(t, sealedCopy == libraryCopy, qt.IsFalse,
		qt.Commentf("and must still be distinct objects, or the test proves nothing"))

	impostor := machine.NewPrimitiveIdentity("equal-hash")
	qt.Assert(t, machine.IdentityOf(newStamped(impostor)), qt.Not(qt.Equals), machine.IdentityOf(sealedCopy),
		qt.Commentf("same name must not mean same identity"))
	qt.Assert(t, equalHash.Name(), qt.Equals, "equal-hash")
}

// TestIdentityOfAnswersNoneForEverythingElse pins the fail-closed direction of
// IdentityOf. All three shapes below answer nil, so comparing against a real
// token refuses them without any separate type check — the property
// PrimMakeHashtable's single-line guard depends on.
func TestIdentityOfAnswersNoneForEverythingElse(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	stamped := machine.NewForeignClosure(env, 1, false, func(machine.CallContext) error {
		return nil
	})
	identity := machine.NewPrimitiveIdentity("stamped")
	stamped.SetIdentity(identity)

	unstamped := machine.NewForeignClosure(env, 1, false, func(machine.CallContext) error {
		return nil
	})

	cases := []struct {
		name  string
		value values.Value
		want  *machine.PrimitiveIdentity
	}{
		{"a stamped foreign closure", stamped, identity},
		{"a foreign closure whose spec declared none", unstamped, nil},
		{"a non-procedure", values.NewInteger(1), nil},
		// A bytecode closure is the shape a Scheme (lambda …) reaches
		// PrimMakeHashtable as, and it can carry no identity at all.
		{"a machine closure", machine.NewClosureWithTemplate(machine.NewNativeTemplate(0, 0, false), env), nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, machine.IdentityOf(tc.value), qt.Equals, tc.want)
		})
	}
}
