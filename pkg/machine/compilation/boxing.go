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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
)

// boxedSlotKey names a local slot ABSOLUTELY: the compile-time frame that owns
// it, and its index in that frame.
//
// Absolute, not (slot, depth), because the boxing decision is made once at the
// binder and consulted from arbitrarily deep inside, where a depth means
// something else. Re-basing at every frame descent would be one more thing to
// keep in step with compileBody's and CompileValidatedLet's env switches; an
// owning-frame pointer simply does not move.
//
// The frame pointer is a stable identity in a way a *environment.Binding is not:
// EnsureLocalBinding appends to the frame's []Binding, which can reallocate the
// slice, but never the *EnvironmentFrame itself.
type boxedSlotKey struct {
	frame *environment.EnvironmentFrame
	slot  int
}

// absoluteSlot converts a (slot, depth) resolved against env into the
// frame-absolute key, by walking depth lexical parents. Returns ok=false when
// the chain is shorter than the depth, which would mean the index did not come
// from this frame.
func absoluteSlot(env *environment.EnvironmentFrame, li *environment.LocalIndex) (boxedSlotKey, bool) {
	owner := env
	for range li.Up() {
		if owner == nil {
			return boxedSlotKey{}, false
		}
		owner = owner.Parent()
	}
	if owner == nil {
		return boxedSlotKey{}, false
	}
	return boxedSlotKey{frame: owner, slot: li.Over()}, true
}

// refPosition selects which occurrences of a binder a reference scan counts.
type refPosition int

const (
	// refAnywhere counts every reference.
	refAnywhere refPosition = iota
	// refOutsideClosure counts only references evaluated where they are
	// written — outside the body of any lambda, case-lambda clause or
	// function-form define.
	refOutsideClosure
	// refInsideClosure counts only references inside such a body. That IS
	// "captured": a closure over the binder exists and will read it.
	refInsideClosure
)

// binderIsBoxed reports whether the variable bound by name must live in a box
// rather than in the slot directly: it is CAPTURED by some closure in scope AND
// ASSIGNED by some set! in scope.
//
// Both conjuncts are decided syntactically, over the binder's own scope, by
// BINDER IDENTITY — the spelling narrows, ScopesCompatible decides. Deciding it
// at the binder rather than at the capture is what makes it correct: a lambda
// cannot see a set! that sits in its ENCLOSING body, and that set! is exactly as
// invisible to a value-copying closure as one it can see. The depth-qualified
// formulation this replaces missed both directions —
// `(let ((a 0)) ((lambda (x) (set! a x)) 7) a)` reports depth 0 because the
// lambda does not ESCAPE, and
// `(let ((a 0)) (let ((f (lambda () a))) (set! a 1) f))` reports depth 0 for the
// plainer reason that its set! is inside no lambda at all.
//
// Over-approximation, stated: a reference shadowed between here and this binder
// still satisfies the subset test and counts. Boxing a slot that did not need it
// costs one indirection. The opposite error is a wrong answer — and cannot
// happen, because a genuine reference necessarily carries the binder's scopes as
// a subset, which is the same relation GetLocalIndex resolves by.
func binderIsBoxed(name *syntax.SyntaxSymbol, scope []validate.ValidatedExpr) bool {
	if name == nil {
		return false
	}
	captured := false
	assigned := false
	for _, e := range scope {
		if !captured {
			captured = referencesBinder(e, name, refInsideClosure)
		}
		if !assigned {
			assigned = assignsBinder(e, name)
		}
		if captured && assigned {
			return true
		}
	}
	return false
}

// markBoxedBinders decides, for each binder in binders, whether it is boxed over
// scope; records the boxed ones in p.boxedSlots by absolute slot; and returns
// their LocalIndexes in binder order so the caller can emit OpBoxSlot.
//
// The binder must already exist in p.env — this resolves it there, which is what
// keeps the emit site, every read, and every write agreeing on one slot: all
// three go through GetLocalIndex against the same frame.
func (p *CompileTimeContinuation) markBoxedBinders(
	binders []*syntax.SyntaxSymbol,
	scope []validate.ValidatedExpr,
) []*environment.LocalIndex {
	if p.env == nil || len(binders) == 0 {
		return nil
	}
	var q []*environment.LocalIndex
	for _, b := range binders {
		if b == nil || !binderIsBoxed(b, scope) {
			continue
		}
		li := p.env.GetLocalIndex(b.Sym, syntax.ScopesOf(b.Scopes()))
		if li == nil {
			continue
		}
		k, ok := absoluteSlot(p.env, li)
		if !ok {
			continue
		}
		p.ensureBoxedSlots()
		p.boxedSlots[k] = true
		q = append(q, li)
	}
	return q
}

// ensureBoxedSlots allocates the shared verdict map on first use.
//
// Allocated lazily but shared eagerly: compileBody calls this before handing the
// map to the child continuation, so one map serves a whole compile even when the
// outermost frame boxes nothing.
func (p *CompileTimeContinuation) ensureBoxedSlots() {
	if p.boxedSlots == nil {
		p.boxedSlots = make(map[boxedSlotKey]bool)
	}
}

// localIsBoxed reports whether the slot li names, resolved against p.env, holds
// a box. Every read, write and free-vector site asks this one question, so the
// three cannot disagree.
func (p *CompileTimeContinuation) localIsBoxed(li *environment.LocalIndex) bool {
	if len(p.boxedSlots) == 0 || p.env == nil || li == nil {
		return false
	}
	k, ok := absoluteSlot(p.env, li)
	if !ok {
		return false
	}
	return p.boxedSlots[k]
}

// emitBoxSlots emits one OpBoxSlot per index, installing the cells before any
// closure in scope can observe the slots.
func (p *CompileTimeContinuation) emitBoxSlots(lis []*environment.LocalIndex) {
	for _, li := range lis {
		p.AppendOperations(machine.NewOperationBoxSlot(li))
	}
}

// emitLocalStore emits the write form the slot needs: a plain store, or a store
// through the box when the slot holds one.
func (p *CompileTimeContinuation) emitLocalStore(li *environment.LocalIndex) {
	if p.localIsBoxed(li) {
		p.AppendOperations(machine.NewOperationStoreThroughBox(li))
		return
	}
	p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
}

// emitLocalLoad emits the read form the slot needs: a plain load, followed by an
// unbox when the slot holds a box.
func (p *CompileTimeContinuation) emitLocalLoad(li *environment.LocalIndex) {
	p.AppendOperations(machine.NewOperationLoadLocalByLocalIndexImmediate(li))
	if p.localIsBoxed(li) {
		p.AppendOperations(machine.NewOperationUnbox())
	}
}

// procBoxBinders returns the binders a procedure body's own frame owns: the
// parameters, the rest parameter, and every internal define predeclared into the
// same frame.
func procBoxBinders(v validate.ValidatedBodyAndParams) []*syntax.SyntaxSymbol {
	q := procBinders(v)
	return append(q, bodyDefineNames(v.Body())...)
}

// bodyDefineNames returns the names every internal define in body introduces,
// recursing into begin the same way predeclareDefineFromValidatedRecursive does
// — a define reached only through a macro-produced begin is predeclared into the
// frame and so owns a slot like any other.
func bodyDefineNames(body []validate.ValidatedExpr) []*syntax.SyntaxSymbol {
	var q []*syntax.SyntaxSymbol
	for _, e := range body {
		d, ok := e.(*validate.ValidatedDefine)
		if ok {
			q = append(q, d.Name())
			continue
		}
		b, ok := e.(*validate.ValidatedBegin)
		if ok {
			q = append(q, bodyDefineNames(b.Body())...)
		}
	}
	return q
}

// markBoxedFreeVars sets freeVar.boxed from the binder-side decision, so a free
// vector slot and the frame slot it copies from can never disagree about
// whether the value is behind a cell.
func (p *CompileTimeContinuation) markBoxedFreeVars(fvs []freeVar) []freeVar {
	if len(fvs) == 0 || len(p.boxedSlots) == 0 || p.env == nil {
		return fvs
	}
	for i := range fvs {
		li := environment.NewLocalIndex(fvs[i].key.slot, fvs[i].key.depth)
		fvs[i].boxed = p.localIsBoxed(li)
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
		if !referencesBinder(v.Bindings[j].Init, self, refAnywhere) {
			continue
		}
		if referencesBinder(v.Bindings[i].Init, v.Bindings[j].Name, refAnywhere) {
			return tierMutual
		}
		return tierBoxed
	}
	for j := range v.Bindings {
		if referencesBinder(v.Bindings[j].Init, self, refOutsideClosure) {
			return tierBoxed
		}
	}
	_, isLambda := v.Bindings[i].Init.(*validate.ValidatedLambda)
	if isLambda && referencesBinder(v.Bindings[i].Init, self, refAnywhere) {
		return tierSelfPatch
	}
	return tierBoxed
}

// referencesBinder reports whether expr contains a reference to the binding
// name introduces, at the requested position.
//
// The closure boundary is WalkSubExprs's RoleClosureBody, which marks the body
// of every lambda, case-lambda clause and function-form define. That is the
// honest boundary: WalkBindingRefs's depth counts only ESCAPING closures, and an
// immediately-applied lambda's body is not an escaping closure but is still not
// evaluated where it is written.
//
// A set! target counts as a reference — a write reaches the same location a read
// does, so a closure holding only a set! of the binder has still captured it.
func referencesBinder(expr validate.ValidatedExpr, name *syntax.SyntaxSymbol, where refPosition) bool {
	if expr == nil || name == nil {
		return false
	}
	found := false
	var walk func(e validate.ValidatedExpr, inClosure bool)
	walk = func(e validate.ValidatedExpr, inClosure bool) {
		if found || e == nil {
			return
		}
		counts := where == refAnywhere ||
			(where == refInsideClosure && inClosure) ||
			(where == refOutsideClosure && !inClosure)
		sym, ok := e.(*validate.ValidatedSymbol)
		if ok {
			if counts && sameBinder(name, sym.Symbol) {
				found = true
			}
			return
		}
		setBang, ok := e.(*validate.ValidatedSetBang)
		if ok && counts && sameBinder(name, setBang.Name) {
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

// assignsBinder reports whether expr contains a set! whose target is the binding
// name introduces, at any closure depth.
func assignsBinder(expr validate.ValidatedExpr, name *syntax.SyntaxSymbol) bool {
	if expr == nil || name == nil {
		return false
	}
	found := false
	var walk func(e validate.ValidatedExpr)
	walk = func(e validate.ValidatedExpr) {
		if found || e == nil {
			return
		}
		setBang, ok := e.(*validate.ValidatedSetBang)
		if ok && sameBinder(name, setBang.Name) {
			found = true
			return
		}
		validate.WalkSubExprs(e, func(child validate.ValidatedExpr, _ validate.ChildRole) {
			walk(child)
		})
	}
	walk(expr)
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
