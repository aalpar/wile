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
	"github.com/aalpar/wile/pkg/values"
)

// PrimitiveIdentity is the stable identity of one registered primitive, shared by
// every ForeignClosure built from that primitive's spec.
//
// It exists because a primitive has no single closure object. A library
// environment is an island (environment.Namespace.NewChildRuntime roots it at a
// sealed base of its OWN, so nothing in it reaches the engine's frames at any
// phase), so the library env factory re-applies the whole registry into it
// and mints a SECOND closure for every primitive; a program that imports
// (scheme base) then holds that copy while the engine's sealed base still holds
// the first. Both are the same primitive and behave identically, and a pointer
// compare says they are not.
//
// So Go code asking "is this value the registered equal-hash?" compares
// identities, not pointers. The question a pointer answers — "is this the sealed
// base's copy of it?" — is the wrong one, and stops being answerable at all once
// the visible surface narrows (WithStrictNamespace, or a dialect's
// PrimitiveRemover) so that the sealed base no longer holds the primitive.
//
// Identity is by POINTER on the token itself, so two tokens with the same name are
// distinct. That is the fail-closed direction: an embedder registering their own
// equal-hash gets their own token (or none) and is refused, where a name compare
// would have accepted it and hashed with the wrong function. There is deliberately
// no way to construct a token from Scheme.
//
// This is NOT a Binding Identity violation. That invariant forbids deciding two
// IDENTIFIERS denote the same variable by comparing SPELLINGS; nothing is resolved
// by spelling here. The name below is diagnostic only — never compared.
type PrimitiveIdentity struct {
	name string
}

// NewPrimitiveIdentity mints a fresh identity. Call it ONCE per primitive, at
// package scope: the returned pointer is the identity, so a second call for the
// same primitive produces a token that matches nothing.
func NewPrimitiveIdentity(name string) *PrimitiveIdentity {
	return &PrimitiveIdentity{name: name}
}

// Name returns the primitive's name, for diagnostics. It is not the identity —
// see the type doc.
func (p *PrimitiveIdentity) Name() string {
	return p.name
}

// IdentityOf returns v's primitive identity, or nil if v is not a closure built
// from a spec that declared one. nil means NONE: a Scheme procedure, a primitive
// with no declared identity, and a non-procedure are all indistinguishable here,
// which is what makes an identity compare against a non-nil token fail closed.
func IdentityOf(v values.Value) *PrimitiveIdentity {
	fc, ok := v.(*ForeignClosure)
	if !ok {
		return nil
	}
	return fc.Identity()
}
