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

// PIN, not a gate: checkStackSize is inlined at exactly its eight call sites in
// pkg/machine, so the push opcodes keep the unlimited-stack fast path (a compare and
// a return) without a call.
//
// Mutation that reddens it: inline reportStackOverflow's fmt.Sprintf body into
// checkStackSize. gc then prints `cannot inline (*MachineContext).checkStackSize`,
// no call site inlines, and the count is 0. Raising its cost past the budget of 80
// has the same effect. Adding a ninth call site without a matching edit here fails
// the other way, which is intended: the number is a census, update it with the site.
//
// The assertion counts `inlining call to (*MachineContext).checkStackSize` in
// `go build -gcflags=-m=2` output. That is strictly stronger than asserting
// `can inline`, which says only that the callee is under budget: it also catches a
// CALLER that stops inlining a still-inlinable callee, which is the harm the
// performance row describes. It also avoids a trap: `can inline` is not equivalent
// to "within budget as the author meant" (the same run prints `can inline
// (*MachineContext).applyParameter.func1 with cost 130`).
//
// What it does not cover:
//
//   - The format. -m=2 output is not a stable interface. If the wording changes the
//     count drops to 0 and the test fails loudly, which is acceptable; the fix is to
//     update the match string, not to loosen the test.
//   - The build configuration. The subprocess builds the package plain, without
//     -race, -cover or -N -l, whatever the enclosing `go test` was given, so the
//     count describes the shipped optimizer, not an instrumented build.
//   - Which eight. The census is a number, not a list of sites; moving a call
//     between functions does not change it.

package machine

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

const checkStackSizeInlineSites = 8

func TestCheckStackSizeIsInlinedAtEveryCallSitePin(t *testing.T) {
	c := qt.New(t)

	cmd := exec.Command("go", "build", "-gcflags=-m=2", "-o", os.DevNull, ".")
	// A clean environment for the knobs that change inlining: an enclosing
	// `go test -race` or a GOFLAGS carrying -gcflags would change the count.
	cmd.Env = append(os.Environ(), "GOFLAGS=", "GOWORK=off")
	out, err := cmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("go build -gcflags=-m=2 failed:\n%s", out))

	got := strings.Count(string(out), "inlining call to (*MachineContext).checkStackSize")
	c.Assert(got, qt.Equals, checkStackSizeInlineSites,
		qt.Commentf("checkStackSize must be inlined at exactly %d sites. A lower count "+
			"means it left the inline budget or a caller stopped inlining it; a higher "+
			"count means a new call site was added without updating this census (saw %d).",
			checkStackSizeInlineSites, got))
}
