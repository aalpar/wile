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

package wile_test

import (
	"os"
	"strings"
	"testing"
)

// Prose gates for review 2026-08-07 wave 2, items 11 and 7. Both items close a
// documentation defect rather than a code defect, so the gate is the sentence
// itself: these assertions all failed at 003b3353 and pass only because that
// commit's SECURITY.md was corrected.
//
// A prose gate is weaker than a behavioural one — the test and the fix are the
// same sentence. The behavioural half lives in shared_aggregate_pins_test.go,
// which asserts the reach the prose now admits to, so a silent re-decision
// flips a test rather than passing.

// normalizeProse collapses every run of whitespace to one space, so an
// assertion is about the words and not about where the paragraph was wrapped.
func normalizeProse(s string) string {
	return strings.Join(strings.Fields(s), " ")
}

// securityDoc reads SECURITY.md from the module root, two levels above this
// package.
func securityDoc(t *testing.T) string {
	t.Helper()
	b, err := os.ReadFile("../../SECURITY.md")
	if err != nil {
		t.Fatalf("read SECURITY.md: %v", err)
	}
	return string(b)
}

// docSection returns the text between a heading and the next heading at any
// level, failing the test when the heading is absent.
func docSection(t *testing.T, doc, heading string) string {
	t.Helper()
	_, rest, found := strings.Cut(doc, heading)
	if !found {
		t.Fatalf("SECURITY.md: heading %q not found", heading)
	}
	section, _, found := strings.Cut(rest, "\n#")
	if !found {
		return rest
	}
	return section
}

// postureBullet returns the "- **<lead>." bullet of the Runtime posture list.
func postureBullet(t *testing.T, doc, lead string) string {
	t.Helper()
	section := docSection(t, doc, "### Runtime posture")
	_, rest, found := strings.Cut(section, "- **"+lead)
	if !found {
		t.Fatalf("SECURITY.md: Runtime posture has no %q bullet", lead)
	}
	bullet, _, found := strings.Cut(rest, "\n- **")
	if !found {
		return normalizeProse(rest)
	}
	return normalizeProse(bullet)
}

// TestSecurityDocScopesMemorySafety is the gate for item 7's restatement of
// SECURITY.md:75-77. At 003b3353 the bullet read "No CGo and no `unsafe` in
// production code, so Wile inherits Go's memory safety" with no qualifier of
// any kind.
func TestSecurityDocScopesMemorySafety(t *testing.T) {
	bullet := postureBullet(t, securityDoc(t), "Pure Go")
	stale := "so Wile inherits Go's memory safety"
	if strings.Contains(bullet, stale) {
		t.Errorf("SECURITY.md Pure Go bullet still claims memory safety unscoped: %q", stale)
	}
	for _, want := range []string{"threads capability", "authorizer"} {
		if !strings.Contains(bullet, want) {
			t.Errorf("SECURITY.md Pure Go bullet does not name %q; the memory-safety claim must be scoped to both the threads capability and an installed authorizer", want)
		}
	}
}

// TestSecurityDocStatesHostFaultConsequence is the gate for item 7's
// correction of SECURITY.md:86-92, which is 2026-07-17-review-remediation-impl
// D1(B)'s shipped text. At 003b3353 it bounded the consequence of a racing
// store at a lost update ("This matches R7RS and SRFI-18, neither of which
// promises atomic mutation"); the measured consequence is an uncatchable
// runtime.throw that kills the host process.
func TestSecurityDocStatesHostFaultConsequence(t *testing.T) {
	bullet := postureBullet(t, securityDoc(t), "One engine per goroutine")
	stale := "the `atomic` primitives. This matches R7RS and SRFI-18, neither of which promises atomic mutation."
	if strings.Contains(bullet, stale) {
		t.Error("SECURITY.md still bounds the non-atomic-store consequence at a lost update; a torn two-word Value store is a host memory-safety violation")
	}
	for _, want := range []string{"runtime.throw", "host process"} {
		if !strings.Contains(bullet, want) {
			t.Errorf("SECURITY.md non-atomic-stores bullet does not name %q", want)
		}
	}
}

// TestSecurityDocNamesNoAuthorizerCarveOut is the gate for item 11. At
// 003b3353 the two-layer paragraph at :96-98 asserted the intersection
// property unconditionally, with no occurrence of "no authorizer" and no
// pointer to the widening section anywhere before the profile table.
func TestSecurityDocNamesNoAuthorizerCarveOut(t *testing.T) {
	section := docSection(t, securityDoc(t), "### Two-layer authorization")
	head, _, found := strings.Cut(section, "| Profile")
	if !found {
		t.Fatal("SECURITY.md: Two-layer authorization section has no profile table")
	}
	preamble := normalizeProse(head)
	if !strings.Contains(preamble, "no authorizer") {
		t.Error("SECURITY.md's two-layer paragraph does not name the no-authorizer case, for which the intersection property is false")
	}
	if !strings.Contains(preamble, "#profile-namespaces-widen-the-surface") {
		t.Error("SECURITY.md's two-layer paragraph does not point at docs/security/sandboxing.md's widening section")
	}
}
