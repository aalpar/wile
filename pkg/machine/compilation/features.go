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

package compilation

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
	"context"
	"runtime"
	"slices"
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
	features := slices.Clone(supportedFeatures)
	features = append(features, platformFeatures()...)
	return features
}

// IsFeatureSupported checks if a feature identifier is supported.
func IsFeatureSupported(feature string) bool {
	return slices.Contains(AllFeatures(), feature)
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
	// The registry parameter is used to check if a library is already loaded.
	// The resolver parameter is used to check if a library file exists
	// (via the FileResolver chain, supporting both OS and virtual fs.FS).
	// The load parameter, when non-nil, lets a (library X) requirement attempt
	// a real load (so a .sld that resolves on disk but fails to import counts
	// as unsatisfied); when nil, library availability falls back to a
	// file-existence check via the resolver.
	IsSatisfied(ctx context.Context, registry *LibraryRegistry, resolver FileResolver, load LibraryLoadProbe) bool
}

// LibraryLoadProbe attempts to actually load a library by name, returning true
// iff the load (parse + compile + execute + register) succeeds. A successful
// probe caches the library in the registry, so the subsequent real import is
// free. cond-expand's (library X) requirement uses this so importability — not
// mere file presence — decides the clause (R7RS §4.2.1, plan item 5F/P6).
type LibraryLoadProbe func(name LibraryName) bool

// featureIdentifier is a simple feature requirement.
type featureIdentifier struct {
	name string
}

func (p *featureIdentifier) IsSatisfied(_ context.Context, _ *LibraryRegistry, _ FileResolver, _ LibraryLoadProbe) bool {
	return IsFeatureSupported(p.name)
}

// libraryRequirement checks if a library is available.
type libraryRequirement struct {
	name LibraryName
}

func (p *libraryRequirement) IsSatisfied(ctx context.Context, registry *LibraryRegistry, resolver FileResolver, load LibraryLoadProbe) bool {
	if registry != nil && registry.Lookup(p.name) != nil {
		return true
	}
	// Preferred: attempt a real load so a .sld that resolves on disk but fails
	// to import (missing dependency, body compile error) counts as unsatisfied.
	// A successful probe caches the library, so the later real import is free.
	if load != nil {
		return load(p.name)
	}
	// Fallback (no loader threaded in, e.g. in unit tests): the library counts
	// as available if its file merely resolves via the FileResolver chain.
	if resolver == nil {
		return false
	}
	f, _, err := ResolveLibraryFile(ctx, resolver, p.name)
	if err != nil {
		return false
	}
	f.Close() //nolint:errcheck
	return true
}

// andRequirement is satisfied if all sub-requirements are satisfied.
type andRequirement struct {
	requirements []FeatureRequirement
}

func (p *andRequirement) IsSatisfied(ctx context.Context, registry *LibraryRegistry, resolver FileResolver, load LibraryLoadProbe) bool {
	for _, req := range p.requirements {
		if !req.IsSatisfied(ctx, registry, resolver, load) {
			return false
		}
	}
	return true
}

// orRequirement is satisfied if at least one sub-requirement is satisfied.
type orRequirement struct {
	requirements []FeatureRequirement
}

func (p *orRequirement) IsSatisfied(ctx context.Context, registry *LibraryRegistry, resolver FileResolver, load LibraryLoadProbe) bool {
	for _, req := range p.requirements {
		if req.IsSatisfied(ctx, registry, resolver, load) {
			return true
		}
	}
	return false
}

// notRequirement is satisfied if the sub-requirement is NOT satisfied.
type notRequirement struct {
	requirement FeatureRequirement
}

func (p *notRequirement) IsSatisfied(ctx context.Context, registry *LibraryRegistry, resolver FileResolver, load LibraryLoadProbe) bool {
	return !p.requirement.IsSatisfied(ctx, registry, resolver, load)
}

// elseRequirement is always satisfied (used for else clause).
type elseRequirement struct{}

func (*elseRequirement) IsSatisfied(_ context.Context, _ *LibraryRegistry, _ FileResolver, _ LibraryLoadProbe) bool {
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
