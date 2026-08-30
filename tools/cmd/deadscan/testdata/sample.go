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

// Package sample is the fixture every deadscan test measures. It sits under
// testdata/ so the go tool ignores it as a build target, and it is type-checked
// directly by the tests rather than loaded through go/packages.
//
// Each declaration below exists to exercise one classification, and the tests
// name which:
//
//   - Reachable / reachableCaller — an ordinary live symbol.
//   - Orphan                      — dead, referenced by nothing.
//   - Leaf / LeafCaller           — dead as a pair; Leaf is reachable only from
//     LeafCaller, which is itself dead, so Leaf is cluster-only rather than a
//     standalone deletion.
//   - KindFirst / KindSecond      — an iota block, where deleting a member
//     renumbers the rest.
//   - Checker.Check               — pinned by a satisfaction assertion, and
//     called by nothing.
//   - Checker.Helper              — NOT in the asserted interface, so the
//     assertion must not pin it.
//   - FaultError.Error            — pinned by the universe error protocol.
//   - Zero / FaultError.Is        — the propagation case: Zero is read only by
//     Is, which is pinned, so death must not reach Zero.
package sample

// Kind is an iota-driven enum: its members are positional.
type Kind int

const (
	// KindFirst is the zero value.
	KindFirst Kind = iota
	// KindSecond follows it, and would renumber if KindFirst were removed.
	KindSecond
)

// Zero is read only from KindOf, whose method is pinned by the error protocol.
const Zero = 0

// Reachable is called from reachableCaller, an unexported production function.
func Reachable() int {
	return 1
}

func reachableCaller() int {
	return Reachable()
}

// Orphan is referenced by nothing at all.
func Orphan() int {
	return 2
}

// Leaf is called only from LeafCaller, which is itself dead.
func Leaf() int {
	return 3
}

// LeafCaller is dead, and is the only caller of Leaf.
func LeafCaller() int {
	return Leaf()
}

// Validator is the interface the assertion below names.
type Validator interface {
	Check() bool
}

// Checker satisfies Validator, and nothing ever calls Check.
type Checker struct{}

var _ Validator = (*Checker)(nil)

// Check is required by Validator, so the assertion pins it.
func (p *Checker) Check() bool {
	return true
}

// Helper is not part of Validator and must not be pinned by the assertion.
func (p *Checker) Helper() bool {
	return false
}

// FaultError implements the universe error interface.
type FaultError struct{}

// Error is dispatched through error, which lives in no package scope.
func (p *FaultError) Error() string {
	return "fault"
}

// KindOf is reached only through the error protocol pin on Error, and is the
// only reader of Zero.
func (p *FaultError) Is(target error) bool {
	return Zero == 0 && target != nil
}
