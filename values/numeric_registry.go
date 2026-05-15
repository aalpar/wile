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
	"math/big"

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
	isAlwaysExact bool

	// toFloat64WithAccuracy replaces the previous toFloat64 field.
	// Per-kind dispatch returns the float64 result, the big.Accuracy
	// (Below/Exact/Above), and an isReal flag (false iff input was
	// Complex/BigComplex with non-zero imaginary part — the imaginary
	// information is dropped). Always non-nil for every kind.
	toFloat64WithAccuracy func(Number) (float64, big.Accuracy, bool)

	// toComplex128WithAccuracy replaces the previous toComplex128 field.
	// Returns a Complex128Result struct (field-named to prevent
	// realAcc/imagAcc swap bugs at call sites). Always non-nil for every
	// kind. For real-only kinds, res.ImagAcc is trivially big.Exact.
	toComplex128WithAccuracy func(Number) Complex128Result
}

// SchemeName returns the Scheme type name for this numeric kind (e.g. "integer").
func (p *NumericTypeSpec) SchemeName() string {
	return p.schemeName
}

// SimplifyDown reduces n to the simplest in-kind representation in a single
// call (multi-step descents are inlined per-kind: e.g. BigFloat→BigInteger→Integer).
// Returns n unchanged if no simpler representation exists. The cross-kind
// BigComplex/Complex shortcuts live in Simplify() itself, not here.
func (p *NumericTypeSpec) SimplifyDown(n Number) Number {
	return p.simplifyDown(n)
}

// ToFloat64WithAccuracy dispatches via the registered closure for the kind.
// Returns (value, accuracy, isReal). The accuracy slot is Below/Exact/Above
// per Go big.Accuracy semantics. isReal is false iff the input was a
// Complex/BigComplex with non-zero imaginary part (the imaginary component
// is dropped; callers should use ToComplex128WithAccuracy for full fidelity).
func (p *NumericTypeSpec) ToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	return p.toFloat64WithAccuracy(n)
}

// ToComplex128WithAccuracy dispatches via the registered closure for the kind.
// Returns a Complex128Result with per-component accuracy. For real-only
// inputs, res.ImagAcc is big.Exact.
func (p *NumericTypeSpec) ToComplex128WithAccuracy(n Number) Complex128Result {
	return p.toComplex128WithAccuracy(n)
}

// IsAlwaysExact reports whether every value of this kind is exact.
// BigComplex returns false; per-instance exactness is determined by
// BigComplex.IsExact() (called by ExactnessOf).
func (p *NumericTypeSpec) IsAlwaysExact() bool {
	return p.isAlwaysExact
}

// numericRegistry holds one spec per NumericKind, indexed by kind.
var numericRegistry [numKinds]NumericTypeSpec

// registryFilled tracks which kinds have been registered.
var registryFilled [numKinds]bool

// registerNumericSpec registers the spec for the given kind. Called from
// each numeric type's init() function. Panics if the same kind is registered
// twice (catches accidental duplicate init registrations).
//
// kind is passed positionally — there is no Kind field on the spec — so
// mismatches between the caller's intent and the spec are impossible.
//
// Go init phase is single-goroutine (Go spec §init), so no mutex is required.
func registerNumericSpec(kind NumericKind, spec NumericTypeSpec) {
	if spec.schemeName == "" {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: schemeName must not be empty for kind %d", kind))
	}
	if spec.simplifyDown == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: simplifyDown must not be nil for kind %d (%s)", kind, spec.schemeName))
	}
	if spec.toFloat64WithAccuracy == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: toFloat64WithAccuracy must not be nil for kind %d (%s)", kind, spec.schemeName))
	}
	if spec.toComplex128WithAccuracy == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: toComplex128WithAccuracy must not be nil for kind %d (%s)", kind, spec.schemeName))
	}
	if registryFilled[kind] {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"registerNumericSpec: duplicate registration for kind %d (existing=%q, new=%q)",
			kind, numericRegistry[kind].schemeName, spec.schemeName))
	}
	numericRegistry[kind] = spec
	registryFilled[kind] = true
}

// validateNumericSpecs checks that all entries in the provided arrays are
// complete. Exposed as a package-internal function so tests can call it with
// crafted bad state; the live registry is validated eagerly by the init()
// at the bottom of this file.
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
		if specs[k].simplifyDown == nil || specs[k].toFloat64WithAccuracy == nil || specs[k].toComplex128WithAccuracy == nil {
			panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
				"validateNumericSpecs: nil function field for kind %d (%s)", k, specs[k].schemeName))
		}
	}
}

// The eager package-init validation lives in validate_numeric_registry.go,
// which sorts after all per-type files (integer.go, big_integer.go,
// big_float.go, big_complex.go, complex.go, float.go, rational.go). Go init
// order is lexical by filename; placing the validation in a 'v'-prefixed
// file is the only way to guarantee it runs after rational.go's init.

// Lookup returns the NumericTypeSpec for the given kind. Renamed from
// LookupNumericSpec (PR #752) — the registry is the only Lookup site in
// values/, so the prefix was redundant. All internal call sites updated.
// Bounds-checked: out-of-range kind panics with ErrNumericRegistry rather
// than producing a Go runtime "index out of range" panic. Consulted by the
// cold-path helpers (Simplify, ExactnessOf, NumberToFloat64, NumberToComplex128).
// NumberToFloat64/NumberToComplex128 are also reached from the IEEE 754
// special-value guard inside the arithmetic dispatch closures; those fire
// only when a Float operand is Inf/NaN, not on every arithmetic op.
func Lookup(kind NumericKind) *NumericTypeSpec {
	if int(kind) >= int(numKinds) {
		panic(werr.WrapForeignErrorf(werr.ErrNumericRegistry,
			"Lookup: kind %d out of range [0,%d)", kind, numKinds))
	}
	return &numericRegistry[kind]
}
