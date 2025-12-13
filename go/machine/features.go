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

// features.go implements R7RS feature detection for cond-expand.
//
// cond-expand allows conditional compilation based on implementation features.
// Features are checked at compile/expansion time, not runtime.
//
// Example:
//   (cond-expand
//     (r7rs (do-r7rs-thing))
//     (else (do-fallback)))
//
// Reference: R7RS Section 4.2.1

import (
	"runtime"
)

// ImplementationName is the name of this Scheme implementation.
const ImplementationName = "wile"

// supportedFeatures lists the feature identifiers this implementation supports.
// These are checked by cond-expand feature requirements.
var supportedFeatures = []string{
	// R7RS compliance
	"r7rs",

	// Implementation identifier
	"wile",

	// Numeric features
	"exact-closed", // exact arithmetic is closed under +, -, *
	"ratios",       // rational numbers are supported
	"ieee-float",   // IEEE 754 floating point

	// Unicode
	"full-unicode", // full Unicode support for characters and strings
}

// platformFeatures returns platform-specific feature identifiers.
func platformFeatures() []string {
	var features []string

	// Operating system
	switch runtime.GOOS {
	case "darwin":
		features = append(features, "darwin", "macosx", "posix", "unix")
	case "linux":
		features = append(features, "linux", "posix", "unix")
	case "windows":
		features = append(features, "windows")
	case "freebsd", "openbsd", "netbsd":
		features = append(features, runtime.GOOS, "bsd", "posix", "unix")
	default:
		// Unknown OS - check for Unix-like
		if runtime.GOOS != "windows" {
			features = append(features, "posix", "unix")
		}
	}

	// Architecture / byte order
	// Go's runtime doesn't directly expose endianness, but we can infer from arch
	switch runtime.GOARCH {
	case "amd64", "386":
		features = append(features, "little-endian", "x86-family")
	case "arm64", "arm":
		features = append(features, "little-endian")
	default:
		// Assume little-endian for most modern architectures
		features = append(features, "little-endian")
	}

	return features
}

// AllFeatures returns all supported feature identifiers.
func AllFeatures() []string {
	features := make([]string, len(supportedFeatures))
	copy(features, supportedFeatures)
	features = append(features, platformFeatures()...)
	return features
}

// IsFeatureSupported checks if a feature identifier is supported.
func IsFeatureSupported(feature string) bool {
	for _, f := range AllFeatures() {
		if f == feature {
			return true
		}
	}
	return false
}

// FeatureRequirement represents a parsed cond-expand feature requirement.
// Feature requirements can be:
//   - A symbol (feature identifier)
//   - (library <library-name>) - check if library is available
//   - (and <req> ...) - all requirements must be satisfied
//   - (or <req> ...) - at least one requirement must be satisfied
//   - (not <req>) - requirement must NOT be satisfied
type FeatureRequirement interface {
	// IsSatisfied returns true if this requirement is satisfied.
	// The registry parameter is used to check library availability.
	IsSatisfied(registry *LibraryRegistry) bool
}

// featureIdentifier is a simple feature requirement.
type featureIdentifier struct {
	name string
}

func (f *featureIdentifier) IsSatisfied(registry *LibraryRegistry) bool {
	return IsFeatureSupported(f.name)
}

// libraryRequirement checks if a library is available.
type libraryRequirement struct {
	name LibraryName
}

func (f *libraryRequirement) IsSatisfied(registry *LibraryRegistry) bool {
	if registry == nil {
		return false
	}
	// Check if library is already loaded
	if registry.Lookup(f.name) != nil {
		return true
	}
	// Check if library file exists
	_, err := registry.FindLibraryFile(f.name)
	return err == nil
}

// andRequirement is satisfied if all sub-requirements are satisfied.
type andRequirement struct {
	requirements []FeatureRequirement
}

func (f *andRequirement) IsSatisfied(registry *LibraryRegistry) bool {
	for _, req := range f.requirements {
		if !req.IsSatisfied(registry) {
			return false
		}
	}
	return true
}

// orRequirement is satisfied if at least one sub-requirement is satisfied.
type orRequirement struct {
	requirements []FeatureRequirement
}

func (f *orRequirement) IsSatisfied(registry *LibraryRegistry) bool {
	for _, req := range f.requirements {
		if req.IsSatisfied(registry) {
			return true
		}
	}
	return false
}

// notRequirement is satisfied if the sub-requirement is NOT satisfied.
type notRequirement struct {
	requirement FeatureRequirement
}

func (f *notRequirement) IsSatisfied(registry *LibraryRegistry) bool {
	return !f.requirement.IsSatisfied(registry)
}

// elseRequirement is always satisfied (used for else clause).
type elseRequirement struct{}

func (f *elseRequirement) IsSatisfied(registry *LibraryRegistry) bool {
	return true
}

// NewFeatureIdentifier creates a feature identifier requirement.
func NewFeatureIdentifier(name string) FeatureRequirement {
	return &featureIdentifier{name: name}
}

// NewLibraryRequirement creates a library requirement.
func NewLibraryRequirement(name LibraryName) FeatureRequirement {
	return &libraryRequirement{name: name}
}

// NewAndRequirement creates an and requirement.
func NewAndRequirement(reqs ...FeatureRequirement) FeatureRequirement {
	return &andRequirement{requirements: reqs}
}

// NewOrRequirement creates an or requirement.
func NewOrRequirement(reqs ...FeatureRequirement) FeatureRequirement {
	return &orRequirement{requirements: reqs}
}

// NewNotRequirement creates a not requirement.
func NewNotRequirement(req FeatureRequirement) FeatureRequirement {
	return &notRequirement{requirement: req}
}

// NewElseRequirement creates an else requirement (always satisfied).
func NewElseRequirement() FeatureRequirement {
	return &elseRequirement{}
}
