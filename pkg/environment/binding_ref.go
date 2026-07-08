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

package environment

// BindingRefKind discriminates the two halves of the binding domain a
// BindingRef can name. The zero value is intentionally invalid so a zero
// BindingRef is never == a resolved one.
type BindingRefKind uint8

const (
	// BindingRefInvalid is the zero value: not a reference to any binding.
	BindingRefInvalid BindingRefKind = iota
	// BindingRefLocal names a resolved local binding by frame + slot.
	BindingRefLocal
	// BindingRefGlobal names a global binding symbolically, by symbol Key.
	BindingRefGlobal
)

// BindingRef names any binding — a resolved local slot, or a symbolic global
// that need not exist yet. All fields are comparable, so BindingRef is usable
// as a Go map key. Exactly one arm is meaningful, selected by kind.
//
// The global arm is deliberately symbolic (a Key string, not a resolved
// GlobalIndex): a top-level (set! …)/(define …) is named during validation,
// before the compiler creates the corresponding global binding, so no frame
// slot exists yet. This is the local-or-global identity used to track "was
// this binding mutated / is it an inline candidate" within one compilation
// unit.
//
// BindingRef is NOT a substitute for GlobalIndex. GlobalIndex carries
// Env *GlobalEnvironmentFrame for cross-library macro hygiene (definition-site
// resolution); the symbolic global arm here drops that, so two hygiene-distinct
// top-level bindings of the same name share a ref. That over-match is safe for
// a conservative mutation/stability set (it can only forfeit an optimization,
// never wrongly apply one), but it means BindingRef must not be used where
// cross-library binding identity matters.
type BindingRef struct {
	kind   BindingRefKind
	local  BindingID // meaningful iff kind == BindingRefLocal
	global string    // symbol Key; meaningful iff kind == BindingRefGlobal
}

// LocalRef returns a BindingRef naming a resolved local binding.
func LocalRef(bid BindingID) BindingRef {
	return BindingRef{kind: BindingRefLocal, local: bid}
}

// GlobalRef returns a BindingRef naming a global binding symbolically, by the
// symbol's Key. The named binding need not exist yet.
func GlobalRef(key string) BindingRef {
	return BindingRef{kind: BindingRefGlobal, global: key}
}

// IsValid reports whether the reference names a binding (local or global).
func (p BindingRef) IsValid() bool {
	return p.kind != BindingRefInvalid
}

// IsLocal reports whether the reference names a local binding.
func (p BindingRef) IsLocal() bool {
	return p.kind == BindingRefLocal
}

// IsGlobal reports whether the reference names a global binding.
func (p BindingRef) IsGlobal() bool {
	return p.kind == BindingRefGlobal
}
