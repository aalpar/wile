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

package core

import (
	"github.com/aalpar/wile/pkg/machine"
)

// The four primitives the hashtable surface must RECOGNIZE rather than merely
// call: make-hashtable takes (equal-hash, equal?) as arguments and has to decide
// whether they are the built-in pair, and hashtable-equivalence-function /
// hashtable-hash-function hand a procedure back and have to pick the caller's own
// binding for it.
//
// They are declared together, in one place, because the two directions must agree:
// whatever the accessors return must be what make-hashtable accepts. Splitting
// them across hashes.go and equality.go would let one drift.
//
// Minted once — for three of them, at package scope here. machine.PrimitiveIdentity
// is identified by POINTER, so a second machine.NewPrimitiveIdentity("equal?")
// elsewhere is a DIFFERENT primitive as far as recognition is concerned — that is
// the property that makes an embedder's own equal? fail closed instead of being
// accepted by name. See that type for why the closure pointer cannot be used instead.
//
// eq? is the exception: it is also a promoted-op descriptor's identity, and those
// descriptors live in pkg/machine, which cannot import this package. So its one
// token is minted there and ALIASED here. Re-minting it would give the hashtable
// surface a token no closure carries, and (make-hashtable eq? ...) would silently
// stop recognizing eq? — recognizedValue (prim_hashtables.go) pointer-compares
// against this variable.
var (
	identityEqualHash = machine.NewPrimitiveIdentity("equal-hash")
	identityEqualQ    = machine.NewPrimitiveIdentity("equal?")
	identityEqQ       = machine.IdentityEqQ
	identityEqvQ      = machine.NewPrimitiveIdentity("eqv?")
)
