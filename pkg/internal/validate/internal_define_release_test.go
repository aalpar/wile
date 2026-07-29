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

package validate

import (
	"testing"
)

// TestInternalDefineFrameReleasable covers the internal-define spelling of a
// local recursive binding group — the one that actually occurs in the corpus
// (255 internal defines in benchmarks/larceny/src/compiler.scm alone, against
// zero explicit letrec forms).
//
// As with the letrec twin, these assertions live at the predicate rather than at
// allocation slope: when the group co-induction is broken the unsafe member keeps
// allocating, so the measured slope never drops below a floor and the alloc probe
// reports success.
func TestInternalDefineFrameReleasable(t *testing.T) {
	t.Run("mutual recursion over capture-safe primitives", func(t *testing.T) {
		env := envWithImported(t, "=", "-")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineFn("od", call(symRef("ev"), call(symRef("-"), symRef("i"))))
		body := []ValidatedExpr{ev, od, call(symRef("ev"), symRef("n"))}
		if !InternalDefineFrameReleasable(ev, body, env) {
			t.Error("mutually recursive internal defines must be releasable — the shape " +
				"and hazard are identical to the letrec form, only the spelling differs")
		}
	})

	// THE MUTANT THIS KILLS: verifying only the define asked about. `ev` looks
	// clean on its own; clearing its call to `od` is an assumption about `od`.
	t.Run("a sibling captures the continuation", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "call/cc")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineFn("od", call(symRef("call/cc"), symRef("k")))
		body := []ValidatedExpr{ev, od, call(symRef("ev"), symRef("n"))}
		if InternalDefineFrameReleasable(ev, body, env) {
			t.Error("a capturing sibling must refuse the whole group — verifying only ev " +
				"leaves the assumption about od standing on itself")
		}
	})

	// THE MUTANT THIS KILLS: dropping the membership check. The compiler supplies
	// the group from one of three predeclaration sites; if it ever supplies a
	// STALE body, an unrelated define sharing a sibling's name would be recorded
	// as evidence for a call to a different procedure. A stale body does not
	// contain d, so this converts that class of plumbing error into a refusal.
	t.Run("a body that does not contain the define", func(t *testing.T) {
		env := envWithImported(t, "=", "-")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineFn("od", call(symRef("ev"), call(symRef("-"), symRef("i"))))
		// A group that looks right and even binds the same names, but is not the
		// body ev was declared in.
		other := defineFn("od", call(symRef("ev"), symRef("i")))
		foreign := []ValidatedExpr{other, od}
		if InternalDefineFrameReleasable(ev, foreign, env) {
			t.Error("a define must be proven against the body that declared it; accepting " +
				"a foreign group lets same-name evidence describe a different procedure")
		}
	})

	t.Run("a sibling calls a procedure-invoking callee", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "map", "proc", "xs")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineFn("od", call(symRef("map"), symRef("proc"), symRef("xs")))
		body := []ValidatedExpr{ev, od}
		if InternalDefineFrameReleasable(ev, body, env) {
			t.Error("map invokes an unknown callback, which could capture the continuation " +
				"that pins the frame")
		}
	})

	t.Run("the define creates an escaping closure", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "cons")
		ev := defineFn("ev",
			call(symRef("cons"), lam(symRef("i"))),
			call(symRef("od"), symRef("i")))
		od := defineFn("od", call(symRef("ev"), call(symRef("-"), symRef("i"))))
		body := []ValidatedExpr{ev, od}
		if InternalDefineFrameReleasable(ev, body, env) {
			t.Error("a closure created in this define's own body parents the very frame " +
				"being released")
		}
	})

	// The escape clause is per-define, mirroring the letrec form: a sibling's
	// escaping closure parents the SIBLING's frame, and that sibling is refused
	// when it is itself compiled.
	t.Run("a sibling's escaping closure does not refuse this define", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "cons")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineFn("od",
			call(symRef("cons"), lam(symRef("i"))),
			call(symRef("ev"), symRef("i")))
		body := []ValidatedExpr{ev, od}
		if !InternalDefineFrameReleasable(ev, body, env) {
			t.Error("ev's frame is not parented by a closure od creates")
		}
		if InternalDefineFrameReleasable(od, body, env) {
			t.Error("od itself creates the escaping closure and must be refused")
		}
	})

	t.Run("a value-form sibling is not assumed safe", func(t *testing.T) {
		env := envWithImported(t, "=", "-", "h")
		ev := defineFn("ev", call(symRef("od"), call(symRef("-"), symRef("i"))))
		od := defineVal("od", symRef("h"))
		body := []ValidatedExpr{ev, od}
		if InternalDefineFrameReleasable(ev, body, env) {
			t.Error("od is bound to whatever h denotes — possibly a capturing procedure — " +
				"so withInternalDefines records no evidence and the call must refuse")
		}
	})

	t.Run("a value-form define is never itself releasable", func(t *testing.T) {
		env := envWithImported(t, "=", "-")
		od := defineVal("od", symRef("h"))
		body := []ValidatedExpr{od}
		if InternalDefineFrameReleasable(od, body, env) {
			t.Error("a value-form define binds no procedure and has no frame to release")
		}
	})
}
