// Copyright 2025 Aaron Alpar
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
	"context"
	"testing"

	"wile/environment"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestAllFeatures(t *testing.T) {
	features := AllFeatures()

	// Should contain base R7RS features
	qt.Assert(t, contains(features, "r7rs"), qt.IsTrue)
	qt.Assert(t, contains(features, "wile"), qt.IsTrue)
	qt.Assert(t, contains(features, "exact-closed"), qt.IsTrue)
	qt.Assert(t, contains(features, "ratios"), qt.IsTrue)
	qt.Assert(t, contains(features, "ieee-float"), qt.IsTrue)
	qt.Assert(t, contains(features, "full-unicode"), qt.IsTrue)

	// Should contain platform-specific features
	qt.Assert(t, contains(features, "little-endian"), qt.IsTrue)

	// Should not be empty
	qt.Assert(t, len(features) > 0, qt.IsTrue)
}

func TestIsFeatureSupported(t *testing.T) {
	// Base features
	qt.Assert(t, IsFeatureSupported("r7rs"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("wile"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("exact-closed"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("ratios"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("ieee-float"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("full-unicode"), qt.IsTrue)

	// Unsupported features
	qt.Assert(t, IsFeatureSupported("r6rs"), qt.IsFalse)
	qt.Assert(t, IsFeatureSupported("nonexistent"), qt.IsFalse)
}

func TestPlatformFeatures(t *testing.T) {
	features := platformFeatures()

	// Should contain endianness
	hasEndian := contains(features, "little-endian") || contains(features, "big-endian")
	qt.Assert(t, hasEndian, qt.IsTrue)

	// Should contain some OS feature
	qt.Assert(t, len(features) > 0, qt.IsTrue)
}

func TestFeatureIdentifier_IsSatisfied(t *testing.T) {
	req := NewFeatureIdentifier("r7rs")
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)

	req2 := NewFeatureIdentifier("nonexistent")
	qt.Assert(t, req2.IsSatisfied(nil), qt.IsFalse)
}

func TestLibraryRequirement_IsSatisfied(t *testing.T) {
	// Create a library registry
	registry := NewLibraryRegistry()

	// Library not loaded and file doesn't exist
	req := NewLibraryRequirement(NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, req.IsSatisfied(registry), qt.IsFalse)

	// Nil registry
	qt.Assert(t, req.IsSatisfied(nil), qt.IsFalse)
}

func TestAndRequirement_IsSatisfied(t *testing.T) {
	// All requirements satisfied
	req := NewAndRequirement(
		NewFeatureIdentifier("r7rs"),
		NewFeatureIdentifier("wile"),
	)
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)

	// One requirement not satisfied
	req2 := NewAndRequirement(
		NewFeatureIdentifier("r7rs"),
		NewFeatureIdentifier("nonexistent"),
	)
	qt.Assert(t, req2.IsSatisfied(nil), qt.IsFalse)

	// Empty and (all satisfied)
	req3 := NewAndRequirement()
	qt.Assert(t, req3.IsSatisfied(nil), qt.IsTrue)
}

func TestOrRequirement_IsSatisfied(t *testing.T) {
	// At least one requirement satisfied
	req := NewOrRequirement(
		NewFeatureIdentifier("r7rs"),
		NewFeatureIdentifier("nonexistent"),
	)
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)

	// No requirements satisfied
	req2 := NewOrRequirement(
		NewFeatureIdentifier("nonexistent1"),
		NewFeatureIdentifier("nonexistent2"),
	)
	qt.Assert(t, req2.IsSatisfied(nil), qt.IsFalse)

	// Empty or (none satisfied)
	req3 := NewOrRequirement()
	qt.Assert(t, req3.IsSatisfied(nil), qt.IsFalse)
}

func TestNotRequirement_IsSatisfied(t *testing.T) {
	// Not of unsupported feature
	req := NewNotRequirement(NewFeatureIdentifier("nonexistent"))
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)

	// Not of supported feature
	req2 := NewNotRequirement(NewFeatureIdentifier("r7rs"))
	qt.Assert(t, req2.IsSatisfied(nil), qt.IsFalse)
}

func TestElseRequirement_IsSatisfied(t *testing.T) {
	req := NewElseRequirement()
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)
}

func TestComplexFeatureRequirements(t *testing.T) {
	// (and r7rs (or wile nonexistent))
	req := NewAndRequirement(
		NewFeatureIdentifier("r7rs"),
		NewOrRequirement(
			NewFeatureIdentifier("wile"),
			NewFeatureIdentifier("nonexistent"),
		),
	)
	qt.Assert(t, req.IsSatisfied(nil), qt.IsTrue)

	// (and r7rs (not (or r6rs r5rs)))
	req2 := NewAndRequirement(
		NewFeatureIdentifier("r7rs"),
		NewNotRequirement(
			NewOrRequirement(
				NewFeatureIdentifier("r6rs"),
				NewFeatureIdentifier("r5rs"),
			),
		),
	)
	qt.Assert(t, req2.IsSatisfied(nil), qt.IsTrue)

	// (and (not r7rs) wile) - should be false
	req3 := NewAndRequirement(
		NewNotRequirement(NewFeatureIdentifier("r7rs")),
		NewFeatureIdentifier("wile"),
	)
	qt.Assert(t, req3.IsSatisfied(nil), qt.IsFalse)
}

// Helper function to check if a slice contains a string
func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// Tests moved from coverage_additional_test.go
// TestAllPlatformFeatures tests platformFeatures function
func TestAllPlatformFeatures(t *testing.T) {
	features := AllFeatures()
	qt.Assert(t, len(features) > 0, qt.IsTrue)

	// Check for some expected features
	found := false
	for _, f := range features {
		if f == "r7rs" {
			found = true
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestPlatformFeaturesAdditional tests the platformFeatures function additional paths
func TestPlatformFeaturesAdditional(t *testing.T) {
	features := platformFeatures()
	// Should contain at least r7rs and some platform features
	qt.Assert(t, len(features) > 0, qt.IsTrue)
}

// TestCondExpandWithElse tests cond-expand with else clause
func TestCondExpandWithElse(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with else clause (should be supported since else is always true)
	sv := parseSchemeExpr(t, env, `(cond-expand (else 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCondExpandR7RS tests cond-expand with r7rs feature
func TestCondExpandR7RS(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with r7rs feature (should always be present)
	sv := parseSchemeExpr(t, env, `(cond-expand (r7rs 100) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(100))
}

// TestCondExpandAnd tests cond-expand with and requirement
func TestCondExpandAnd(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((and r7rs) 200) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(200))
}

// TestCondExpandOr tests cond-expand with or requirement
func TestCondExpandOr(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((or r7rs nonexistent-feature) 300) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(300))
}

// TestCondExpandNot tests cond-expand with not requirement
func TestCondExpandNot(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(cond-expand ((not nonexistent-feature) 400) (else 0))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(400))
}

// TestFeatureRequirementIsSatisfied tests various feature requirements
func TestFeatureRequirementIsSatisfied(t *testing.T) {
	registry := NewLibraryRegistry()

	// FeatureIdentifier
	r7rsFeat := NewFeatureIdentifier("r7rs")
	qt.Assert(t, r7rsFeat.IsSatisfied(registry), qt.IsTrue)

	nonExistent := NewFeatureIdentifier("nonexistent-feature")
	qt.Assert(t, nonExistent.IsSatisfied(registry), qt.IsFalse)

	// AndRequirement with single true (variadic function)
	andReq := NewAndRequirement(r7rsFeat)
	qt.Assert(t, andReq.IsSatisfied(registry), qt.IsTrue)

	// AndRequirement with one false
	andReqFail := NewAndRequirement(r7rsFeat, nonExistent)
	qt.Assert(t, andReqFail.IsSatisfied(registry), qt.IsFalse)

	// OrRequirement
	orReq := NewOrRequirement(nonExistent, r7rsFeat)
	qt.Assert(t, orReq.IsSatisfied(registry), qt.IsTrue)

	// NotRequirement
	notReq := NewNotRequirement(nonExistent)
	qt.Assert(t, notReq.IsSatisfied(registry), qt.IsTrue)

	// Library requirement
	libReq := NewLibraryRequirement(NewLibraryName("nonexistent", "lib"))
	qt.Assert(t, libReq.IsSatisfied(registry), qt.IsFalse)
}

// TestAllFeaturesFunction tests AllFeatures returns expected features
func TestAllFeaturesFunction(t *testing.T) {
	features := AllFeatures()
	qt.Assert(t, len(features) > 0, qt.IsTrue)

	// Should include r7rs
	found := false
	for _, f := range features {
		if f == "r7rs" {
			found = true
			break
		}
	}
	qt.Assert(t, found, qt.IsTrue)
}

// TestIsFeatureSupportedFunction tests IsFeatureSupported function
func TestIsFeatureSupportedFunction(t *testing.T) {
	qt.Assert(t, IsFeatureSupported("r7rs"), qt.IsTrue)
	qt.Assert(t, IsFeatureSupported("nonexistent-feature"), qt.IsFalse)
}

// TestFeatureRequirementTypes tests FeatureRequirement type creation
func TestFeatureRequirementTypes(t *testing.T) {
	// Test different feature requirement types
	feat := NewFeatureIdentifier("r7rs")
	qt.Assert(t, feat, qt.IsNotNil)

	andReq := NewAndRequirement(feat)
	qt.Assert(t, andReq, qt.IsNotNil)

	orReq := NewOrRequirement(feat)
	qt.Assert(t, orReq, qt.IsNotNil)

	notReq := NewNotRequirement(feat)
	qt.Assert(t, notReq, qt.IsNotNil)

	libReq := NewLibraryRequirement(NewLibraryName("scheme", "base"))
	qt.Assert(t, libReq, qt.IsNotNil)
}

// TestPlatformFeaturesCoverage tests platformFeatures function coverage
func TestPlatformFeaturesCoverage(t *testing.T) {
	features := platformFeatures()
	// Check for some expected platform-specific features
	// Should have darwin or linux or windows
	hasDarwin := false
	hasLinux := false
	hasWindows := false
	for _, f := range features {
		if f == "darwin" {
			hasDarwin = true
		}
		if f == "linux" {
			hasLinux = true
		}
		if f == "windows" {
			hasWindows = true
		}
	}
	// At least one platform should be present
	qt.Assert(t, hasDarwin || hasLinux || hasWindows, qt.IsTrue)
}

// TestAllFeaturesContainsPlatformFeatures tests that AllFeatures includes platform features
func TestAllFeaturesContainsPlatformFeatures(t *testing.T) {
	features := AllFeatures()
	pf := platformFeatures()

	// All platform features should be in AllFeatures
	for _, f := range pf {
		found := false
		for _, af := range features {
			if af == f {
				found = true
				break
			}
		}
		qt.Assert(t, found, qt.IsTrue, qt.Commentf("platform feature %q not found", f))
	}
}

// TestNewFeatureRequirementConstructors tests constructor functions for FeatureRequirement
func TestNewFeatureRequirementConstructors(t *testing.T) {
	// Test NewFeatureIdentifier
	fi := NewFeatureIdentifier("r7rs")
	qt.Assert(t, fi, qt.IsNotNil)

	// Test NewLibraryRequirement
	lr := NewLibraryRequirement(NewLibraryName("scheme", "base"))
	qt.Assert(t, lr, qt.IsNotNil)

	// Test NewAndRequirement
	ar := NewAndRequirement(fi, lr)
	qt.Assert(t, ar, qt.IsNotNil)

	// Test NewOrRequirement
	or := NewOrRequirement(fi, lr)
	qt.Assert(t, or, qt.IsNotNil)

	// Test NewNotRequirement
	nr := NewNotRequirement(fi)
	qt.Assert(t, nr, qt.IsNotNil)

	// Test NewElseRequirement
	er := NewElseRequirement()
	qt.Assert(t, er, qt.IsNotNil)
}
