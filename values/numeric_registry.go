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

package values

import (
	"sync"

	"github.com/aalpar/wile/werr"
)

// NumericTypeSpec records the cold-path properties of one numeric kind.
//
// Each of the seven concrete numeric types registers exactly one spec via
// registerNumericSpec() in its init() function. Fields are unexported;
// callers use the getter methods.
//
// The three function fields are non-nil invariants enforced by
// registerNumericSpec — bottom-of-chain kinds bind an identity
// simplifyDown rather than nil.
type NumericTypeSpec struct {
	schemeName    string
	simplifyDown  func(Number) Number
	toFloat64     func(Number) (float64, error)
	toComplex128  func(Number) complex128
	isAlwaysExact bool
}

// SchemeName returns the Scheme type name for this numeric kind (e.g. "integer").
func (s *NumericTypeSpec) SchemeName() string {
	return s.schemeName
}

// SimplifyDown reduces n one step toward a simpler type. Returns n unchanged
// if no simpler representation exists. The cross-kind BigComplex/Complex
// shortcuts live in Simplify() itself, not here.
func (s *NumericTypeSpec) SimplifyDown(n Number) Number {
	return s.simplifyDown(n)
}

// ToFloat64 converts n to float64. Returns ErrNotAReal for Complex and
// BigComplex (per Q-i=C3; FFI float64 handling is a separate concern).
func (s *NumericTypeSpec) ToFloat64(n Number) (float64, error) {
	return s.toFloat64(n)
}

// ToComplex128 converts n to complex128. Universal across all 7 kinds.
func (s *NumericTypeSpec) ToComplex128(n Number) complex128 {
	return s.toComplex128(n)
}

// IsAlwaysExact reports whether every value of this kind is exact.
// BigComplex returns false; per-instance exactness is determined by
// BigComplex.IsExact() (called by ExactnessOf).
func (s *NumericTypeSpec) IsAlwaysExact() bool {
	return s.isAlwaysExact
}

// numericRegistry holds one spec per NumericKind, indexed by kind.
var numericRegistry [numKinds]NumericTypeSpec

// registryFilled tracks which kinds have been registered.
var registryFilled [numKinds]bool

var registryMu sync.Mutex
var registryOnce sync.Once

// registerNumericSpec registers the spec for the given kind. Called from
// each numeric type's init() function. Panics if the same kind is registered
// twice (catches accidental duplicate init registrations).
//
// kind is passed positionally — there is no Kind field on the spec — so
// mismatches between the caller's intent and the spec are impossible.
func registerNumericSpec(kind NumericKind, spec NumericTypeSpec) {
	if spec.schemeName == "" {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: schemeName must not be empty for kind %d", kind))
	}
	if spec.simplifyDown == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: simplifyDown must not be nil for kind %d", kind))
	}
	if spec.toFloat64 == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: toFloat64 must not be nil for kind %d", kind))
	}
	if spec.toComplex128 == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: toComplex128 must not be nil for kind %d", kind))
	}
	registryMu.Lock()
	defer registryMu.Unlock()
	if registryFilled[kind] {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: duplicate registration for kind %d", kind))
	}
	numericRegistry[kind] = spec
	registryFilled[kind] = true
}

// validateNumericSpecs checks that all entries in the provided arrays are
// complete. Exposed as a package-internal function so tests can call it with
// crafted bad state (the live sync.Once is already consumed by the time
// tests run, so testing completeness validation requires a separate entry point).
func validateNumericSpecs(specs [numKinds]NumericTypeSpec, filled [numKinds]bool) {
	for k := range numKinds {
		if !filled[k] {
			panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
				"validateNumericSpecs: kind %d not registered", k))
		}
		if specs[k].schemeName == "" {
			panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
				"validateNumericSpecs: schemeName empty for kind %d", k))
		}
		if specs[k].simplifyDown == nil || specs[k].toFloat64 == nil || specs[k].toComplex128 == nil {
			panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
				"validateNumericSpecs: nil function field for kind %d", k))
		}
	}
}

// ensureNumericRegistryInit validates that all numKinds have been registered.
// Uses sync.Once so the validation scan runs at most once per process.
// Panics with ErrNumericRegistry if any kind is missing.
func ensureNumericRegistryInit() {
	registryOnce.Do(func() {
		validateNumericSpecs(numericRegistry, registryFilled)
	})
}

// Lookup returns the NumericTypeSpec for the given kind.
// Calls ensureNumericRegistryInit on every access (cold path only).
func Lookup(kind NumericKind) *NumericTypeSpec {
	ensureNumericRegistryInit()
	return &numericRegistry[kind]
}
