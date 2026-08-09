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

package values_test

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// childEnvVar re-enters this test binary as the renderer. The subprocess prints
// the renderings and exits; the parent compares them.
const childEnvVar = "WILE_HASHTABLE_RENDER_CHILD"

// renderProcesses is how many children the gate forks. The disorder it guards
// against is a PER-PROCESS seeded sync.Map.Range walk, so every distinct order
// this can see costs one fork; a loop inside one process sees exactly one order
// and passes vacuously. Twelve was enough to show eight distinct orders in eight
// runs before the fix, so it is not a marginal sample.
const renderProcesses = 12

// buildEqTableRendering builds an eq table whose keys all RENDER alike but are
// distinct objects, and returns its rendering.
//
// The discriminator is the table KIND, not the entry count. An eqv table hashes
// by content (Hashtable.hashKey), so N equal-content strings collapse into one
// bucket and render in insertion order at any N — deterministic before the fix
// and after it. An eq table hashes by pointer, spreading them across buckets
// whose Range order is per-process seeded. Sizes are irrelevant: the eq table
// varied at N=3 and N=500 alike.
func buildEqTableRendering(t *testing.T) string {
	t.Helper()
	h := values.NewHashtable(values.HashtableEq)
	for i := range 60 {
		// A fresh *String per key: equal content, distinct identity, so eq? keeps
		// them apart while SchemeString cannot.
		err := h.Set(values.NewString("a"), values.NewInteger(int64(i)))
		qt.Assert(t, err, qt.IsNil)
	}
	qt.Assert(t, h.Size(), qt.Equals, 60)
	q, err := values.WriteValueToString(h)
	qt.Assert(t, err, qt.IsNil)
	return q
}

// buildRecordTableRendering is the record half: distinct records of one type all
// render #<record:point>, so they collide in the comparator exactly as the
// strings above do.
func buildRecordTableRendering(t *testing.T) string {
	t.Helper()
	rt := values.NewRecordType(values.NewSymbol("point"),
		[]*values.Symbol{values.NewSymbol("x"), values.NewSymbol("y")})
	h := values.NewHashtable(values.HashtableEq)
	for i := range 20 {
		rec, err := values.NewRecord(rt, []values.Value{values.NewInteger(int64(i)), values.NewInteger(int64(i))})
		qt.Assert(t, err, qt.IsNil)
		err = h.Set(rec, values.NewInteger(int64(i)))
		qt.Assert(t, err, qt.IsNil)
	}
	q, err := values.WriteValueToString(h)
	qt.Assert(t, err, qt.IsNil)
	return q
}

// buildEqvTableRendering is the CONTROL: the same keys in an eqv table. It was
// already deterministic before the fix and must stay so.
func buildEqvTableRendering(t *testing.T) string {
	t.Helper()
	h := values.NewHashtable(values.HashtableEqv)
	for i := range 60 {
		err := h.Set(values.NewString("a"), values.NewInteger(int64(i)))
		qt.Assert(t, err, qt.IsNil)
	}
	q, err := values.WriteValueToString(h)
	qt.Assert(t, err, qt.IsNil)
	return q
}

// TestHashtableRenderingIsIdenticalAcrossProcesses forks renderProcesses copies
// of this test binary and requires every one to produce byte-identical output.
//
// Identity across processes is the assertion, not "N distinct outputs": the
// pre-fix counts are themselves random and pinning one would make the gate
// flaky in the direction that matters least.
func TestHashtableRenderingIsIdenticalAcrossProcesses(t *testing.T) {
	if os.Getenv(childEnvVar) != "" {
		fmt.Println(buildEqTableRendering(t))
		fmt.Println(buildRecordTableRendering(t))
		fmt.Println(buildEqvTableRendering(t))
		return
	}

	c := qt.New(t)
	outputs := make([]string, 0, renderProcesses)
	for i := range renderProcesses {
		cmd := exec.CommandContext(t.Context(), os.Args[0], "-test.run=^"+t.Name()+"$", "-test.v=false")
		cmd.Env = append(os.Environ(), childEnvVar+"="+strconv.Itoa(i))
		out, err := cmd.CombinedOutput()
		c.Assert(err, qt.IsNil, qt.Commentf("child %d: %s", i, out))
		outputs = append(outputs, renderedLines(string(out)))
	}

	for i := 1; i < len(outputs); i++ {
		c.Assert(outputs[i], qt.Equals, outputs[0],
			qt.Commentf("process %d rendered a different order than process 0", i))
	}
	c.Assert(strings.Count(outputs[0], "\n"), qt.Equals, 2,
		qt.Commentf("expected three renderings, got %q", outputs[0]))
}

// renderedLines drops the testing framework's own chatter (PASS, ok, coverage)
// so only the three rendering lines are compared.
func renderedLines(out string) string {
	var q []string
	for line := range strings.SplitSeq(out, "\n") {
		if strings.HasPrefix(line, "#hash(") {
			q = append(q, line)
		}
	}
	return strings.Join(q, "\n")
}
