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
	"testing"

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
