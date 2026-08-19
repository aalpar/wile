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

// boxing.go is Pass 2 of flat-closure conversion: which free-vector slots must
// hold a shared cell rather than a copied value, and the letrec carve-out that
// keeps a self-recursive lambda out of one.
//
// A flat closure copies its free variables' VALUES. Copying is invisible until
// somebody writes, so exactly the captured-and-assigned variables need the
// indirection back.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
)

// assignedSlotKey names a local slot ABSOLUTELY: the compile-time frame that
// owns it, and its index in that frame.
//
// Absolute, not (slot, depth), because the assigned set is shared down the whole
// compile and a depth is relative to whoever is asking. Re-basing a depth at
// every frame descent would be one more thing to keep in step with
// compileBody's and CompileValidatedLet's env switches; an owning-frame pointer
// simply does not move.
//
// The frame pointer is a stable identity in a way a *environment.Binding is not:
// EnsureLocalBinding appends to the frame's []Binding, which can reallocate the
// slice, but never the *EnvironmentFrame itself.
type assignedSlotKey struct {
	frame *environment.EnvironmentFrame
	slot  int
}

// absoluteSlot converts a (slot, depth) resolved against env into the
// frame-absolute key, by walking depth lexical parents. Returns ok=false when
// the chain is shorter than the depth, which would mean the index did not come
// from this frame.
func absoluteSlot(env *environment.EnvironmentFrame, li *environment.LocalIndex) (assignedSlotKey, bool) {
	owner := env
	for range li.Up() {
		if owner == nil {
			return assignedSlotKey{}, false
		}
		owner = owner.Parent()
	}
	if owner == nil {
		return assignedSlotKey{}, false
	}
	return assignedSlotKey{frame: owner, slot: li.Over()}, true
}

// recordAssignedSlots adds every set!-targeted local slot in body to p's shared
// assigned set, resolving each target against p.env.
//
// WHY THE SET IS SEEDED AT THE FRAME, NOT AT THE CAPTURE. "Assigned" is a
// property of the VARIABLE, so it has to be decided where the variable is bound
// — a lambda cannot see a set! that sits in its enclosing body, and that set! is
// exactly as invisible to a value-copying closure as one it can see. The
// depth-qualified formulation this replaces got both halves wrong:
// `(let ((a 0)) ((lambda (x) (set! a x)) 7) a)` reports depth 0 because the
// lambda does not ESCAPE, and
// `(let ((a 0)) (let ((f (lambda () a))) (set! a 1) f))` reports depth 0 for the
// plainer reason that its set! is inside no lambda at all.
//
// WalkBindingRefs walks the whole subtree, so one call at a frame covers every
// set! below it; a nested frame's own call is a refinement for the names IT
// binds, not a repetition.
//
// Two over-approximations, both stated rather than papered over. A set! whose
// target is bound by a frame that does not exist yet (a nested let's own
// binding, at the moment an enclosing lambda seeds) resolves outward and can
// mark a same-spelled outer slot; and a set! whose target is shadowed between
// here and its binder resolves to the shadowed binding. Both cost one
// indirection on a slot that did not need it. The opposite error — missing a
// genuine assignment — is a wrong answer, and is what the nested-frame seeding
// exists to prevent.
func (p *CompileTimeContinuation) recordAssignedSlots(body []validate.ValidatedExpr) {
	if p.env == nil {
		return
	}
	p.ensureAssignedSlots()
	for _, e := range body {
		validate.WalkBindingRefs(e, func(sym *syntax.SyntaxSymbol, role validate.RefRole, _ int) {
			if role != validate.RefSetBangTarget {
				return
			}
			li := p.env.GetLocalIndex(sym.Sym, syntax.ScopesOf(sym.Scopes()))
			if li == nil {
				return
			}
			k, ok := absoluteSlot(p.env, li)
			if !ok {
				return
			}
			p.assignedSlots[k] = true
		})
	}
}

// ensureAssignedSlots allocates the shared assigned set on first use.
//
// Allocated lazily but shared eagerly: compileBody calls this before handing the
// map to the child continuation, so one map serves a whole compile even when
// the outermost frame binds nothing that is ever set!.
func (p *CompileTimeContinuation) ensureAssignedSlots() {
	if p.assignedSlots == nil {
		p.assignedSlots = make(map[assignedSlotKey]bool)
	}
}

// markBoxedFreeVars sets freeVar.boxed for every free variable that is both
// captured and assigned, and returns the updated layout.
//
// CAPTURED is fvs — Pass 1 already answered it, and a nested lambda's free set
// is a subset of this one by the transitive closure in collectFreeVars.
//
// ASSIGNED is p.assignedSlots, seeded at every frame-creating site by
// recordAssignedSlots. Both conjuncts are boundary-free: neither consults a
// closure-nesting count.
func (p *CompileTimeContinuation) markBoxedFreeVars(fvs []freeVar) []freeVar {
	if len(fvs) == 0 || len(p.assignedSlots) == 0 || p.env == nil {
		return fvs
	}
	for i := range fvs {
		li := environment.NewLocalIndex(fvs[i].key.slot, fvs[i].key.depth)
		k, ok := absoluteSlot(p.env, li)
		if !ok {
			continue
		}
		fvs[i].boxed = p.assignedSlots[k]
	}
	return fvs
}

// letrecTier classifies how a letrec binding's forward reference is realised
// under flat closures.
type letrecTier int

const (
	// tierBoxed (T1) is the general case: the binding is captured by a SIBLING
	// init, or referenced while an init is being evaluated rather than only
	// from inside a closure. The value genuinely does not exist yet, so the
	// closure must share a cell rather than copy one.
	tierBoxed letrecTier = iota

	// tierSelfPatch (T2) is a lambda referencing its OWN binding, with nothing
	// else in the group referring to it before its init completes. No box: the
	// value is available the instant the closure is constructed, so the emitter
	// pushes a placeholder for the self slot and OpMakeClosure overwrites it
	// with the closure it just built. No bytecode runs in between, so the
	// placeholder window is not observable — the closure cannot be applied
	// before it exists.
	//
	// Boxing this converts a resolved definition order into a permanent
	// indirection on an inner loop. schelog's unify1 is exactly this shape and
	// is unification's inner loop.
	tierSelfPatch

	// tierMutual (T3) is mutual recursion among lambdas. Boxed FOR NOW. A
	// group-wide fixpoint back-patch is possible and is DELIBERATELY DEFERRED:
	// it is a group-level protocol with a real ordering obligation (no init may
	// EVALUATE a sibling reference before the patch, only capture it) and needs
	// its own proof. T2 is a local decision with no such obligation. Revisit
	// only if a profile names mutual recursion.
	tierMutual
)

// String names the tier for test failures and diagnostics.
func (p letrecTier) String() string {
	switch p {
	case tierBoxed:
		return "boxed"
	case tierSelfPatch:
		return "self-patch"
	case tierMutual:
		return "mutual"
	}
	return "unknown"
}

// letrecBindingTier classifies the i-th binding of a letrec / letrec* group.
//
// PRECONDITION: v.Kind must be recursive (letrec family, which includes the
// desugared named let). In a plain let or let* the bindings are not in each
// other's inits, so there is no forward reference to classify and every answer
// here would describe outer bindings instead.
//
// The three questions, in the order that makes the answers disjoint:
//
//  1. Does a SIBLING init reference binding i? Then i is captured before its own
//     init runs. If i's init reciprocates, that is mutual recursion (T3);
//     otherwise it is an ordinary forward reference (T1).
//  2. Is binding i referenced OUTSIDE a closure body by any init — including its
//     own? Then the reference is evaluated during initialization, when the value
//     really does not exist (T1).
//  3. Otherwise: is i's own init a lambda that references i? Then the only
//     reader is a closure that cannot run before it exists (T2).
//
// Identity is decided the usual way: the name Key narrows, ScopesCompatible
// decides. Shadowing inside an init is deliberately NOT modelled — a shadowed
// same-name reference counts as a reference, which can only move a binding
// toward tierBoxed, the conservative direction.
func letrecBindingTier(v *validate.ValidatedLet, i int) letrecTier {
	if v == nil || i < 0 || i >= len(v.Bindings) {
		return tierBoxed
	}
	self := v.Bindings[i].Name
	for j := range v.Bindings {
		if j == i {
			continue
		}
		if !initReferences(v.Bindings[j].Init, self, false) {
			continue
		}
		if initReferences(v.Bindings[i].Init, v.Bindings[j].Name, false) {
			return tierMutual
		}
		return tierBoxed
	}
	for j := range v.Bindings {
		if initReferences(v.Bindings[j].Init, self, true) {
			return tierBoxed
		}
	}
	_, isLambda := v.Bindings[i].Init.(*validate.ValidatedLambda)
	if isLambda && initReferences(v.Bindings[i].Init, self, false) {
		return tierSelfPatch
	}
	return tierBoxed
}

// initReferences reports whether expr references the binder name. When
// outsideClosureOnly, references that sit inside a closure body are ignored —
// the "is it read during initialization?" question, as opposed to "is it read at
// all?".
//
// The closure boundary is WalkSubExprs's RoleClosureBody, which marks the body
// of every lambda, case-lambda clause and function-form define. That is the
// honest boundary: WalkBindingRefs's depth counts only ESCAPING closures, and an
// immediately-applied lambda's body is not an escaping closure but is still not
// evaluated where it is written.
func initReferences(expr validate.ValidatedExpr, name *syntax.SyntaxSymbol, outsideClosureOnly bool) bool {
	if expr == nil || name == nil {
		return false
	}
	found := false
	var walk func(e validate.ValidatedExpr, inClosure bool)
	walk = func(e validate.ValidatedExpr, inClosure bool) {
		if found || e == nil {
			return
		}
		if outsideClosureOnly && inClosure {
			return
		}
		sym, ok := e.(*validate.ValidatedSymbol)
		if ok {
			if sameBinder(name, sym.Symbol) {
				found = true
			}
			return
		}
		setBang, ok := e.(*validate.ValidatedSetBang)
		if ok && sameBinder(name, setBang.Name) {
			found = true
			return
		}
		validate.WalkSubExprs(e, func(child validate.ValidatedExpr, role validate.ChildRole) {
			walk(child, inClosure || role == validate.RoleClosureBody)
		})
	}
	walk(expr, false)
	return found
}

// sameBinder reports whether ref denotes the binding introduced by binder: the
// spelling narrows, the scope-set subset test decides.
func sameBinder(binder, ref *syntax.SyntaxSymbol) bool {
	if binder == nil || ref == nil {
		return false
	}
	if binder.Sym.Key != ref.Sym.Key {
		return false
	}
	return syntax.ScopesCompatible(binder.Scopes(), ref.Scopes())
}

// countBoxedFreeVars returns how many slots of a layout are boxed.
func countBoxedFreeVars(fvs []freeVar) int {
	q := 0
	for _, fv := range fvs {
		if fv.boxed {
			q++
		}
	}
	return q
}

// freeVarBoxedFlags projects a layout to the parallel boxed vector recorded on
// the template, or nil when nothing is boxed — the overwhelmingly common case,
// and one worth not allocating for.
func freeVarBoxedFlags(fvs []freeVar) []bool {
	if countBoxedFreeVars(fvs) == 0 {
		return nil
	}
	q := make([]bool, len(fvs))
	for i, fv := range fvs {
		q[i] = fv.boxed
	}
	return q
}
