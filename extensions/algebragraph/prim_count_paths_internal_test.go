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

package algebragraph

import (
	"strings"
	"testing"

	"github.com/aalpar/wile/values"
)

// TestExtractNumNodes_RejectsOversized pins the allocation bound at the
// argument chokepoint. The kernel allocates O(numNodes) slots (the dominant
// term is counts []*big.Int — one new(big.Int) per node), so a num-nodes of
// 2^40 from a public caller OOMs the host. extractNumNodes only returns the
// count without allocating, so probing maxNodes+1 here is safe even before the
// guard exists — and isolates the allocation bound from the int64-range and
// sign checks.
func TestExtractNumNodes_RejectsOversized(t *testing.T) {
	_, err := extractNumNodes(values.NewInteger(int64(maxNodes)+1), "count-paths-in-dag")
	if err == nil {
		t.Fatalf("extractNumNodes: expected error for maxNodes+1, got nil")
	}
	const want = "too many nodes"
	if !strings.Contains(err.Error(), want) {
		t.Errorf("error %q does not contain %q", err.Error(), want)
	}

	// maxNodes itself must still be accepted (no allocation here —
	// extractNumNodes only returns the count).
	n, err := extractNumNodes(values.NewInteger(int64(maxNodes)), "count-paths-in-dag")
	if err != nil {
		t.Errorf("extractNumNodes: maxNodes must be accepted, got error: %v", err)
	}
	if n != int(maxNodes) {
		t.Errorf("extractNumNodes: got %d, want %d", n, int(maxNodes))
	}
}
