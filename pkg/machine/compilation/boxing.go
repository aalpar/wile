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
	"github.com/aalpar/wile/pkg/values"
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

// letrecRegionBinder is one binder of a letrec* region — a letrec/letrec*/named-let
// binding, or an internal define — paired with the expression that gives it its
// value.
//
// The two spellings are one region for this analysis and differ only in how the
// pairs are collected, which is why the tier logic below takes this rather than a
// *ValidatedLet. Internal defines are the spelling that actually occurs:
// benchmarks/larceny/src/compiler.scm alone carries 255 of them against zero
// explicit letrec forms.
type letrecRegionBinder struct {
	name *syntax.SyntaxSymbol
	init validate.ValidatedExpr
}

// letBindersOfRegion projects a let form's bindings into region binders.
func letBindersOfRegion(v *validate.ValidatedLet) []letrecRegionBinder {
	q := make([]letrecRegionBinder, 0, len(v.Bindings))
	for i := range v.Bindings {
		q = append(q, letrecRegionBinder{name: v.Bindings[i].Name, init: v.Bindings[i].Init})
	}
	return q
}

// bodyBindersOfRegion projects a body's internal defines into region binders, in
// definition order, recursing into begin exactly as bodyDefineNames does.
//
// A function-form define's "init" is the define node itself: its lambda is not a
// separate ValidatedExpr, so the reference scans walk the ValidatedDefine, whose
// body WalkSubExprs reports as RoleClosureBody — which is what makes a
// self-recursive internal define read as a capture inside a closure.
func bodyBindersOfRegion(body []validate.ValidatedExpr) []letrecRegionBinder {
	var q []letrecRegionBinder
	for _, e := range body {
		d, ok := e.(*validate.ValidatedDefine)
		if ok {
			q = append(q, letrecRegionBinder{name: d.Name(), init: d})
			continue
		}
		b, ok := e.(*validate.ValidatedBegin)
		if ok {
			q = append(q, bodyBindersOfRegion(b.Body())...)
		}
	}
	return q
}

// letrecRegionForcesBox reports whether binder i of a letrec* region must hold a
// cell for a reason that has nothing to do with mutation.
//
// THE HAZARD IS INITIALIZATION ORDER, AND IT ONLY EXISTS ONCE FREE VARIABLES ARE
// COPIED. A letrec* binder is in scope throughout the region's inits, so a
// closure built by one init can capture a slot whose value has not been stored
// yet. Under linked closures that was harmless — the closure held the frame and
// read the slot later. A flat closure copies, so it would copy #!void and keep
// it forever.
//
// The carve-out is the T2 tier: when the ONLY capture is the binder's own init
// lambda, OpMakeClosure's self back-patch writes the finished closure into its
// own free slot, and no cell is needed. That is the difference between
// (define (loop n) … (loop …)) costing an unbox per iteration and costing
// nothing, on the commonest recursion shape in the corpus.
//
// A capture in the region's BODY is safe — every store has run by then — so the
// scan covers the inits alone. That precision is what keeps a merely
// body-captured binder unboxed.
func letrecRegionForcesBox(bs []letrecRegionBinder, i int) bool {
	if i < 0 || i >= len(bs) {
		return false
	}
	capturedDuringInit := false
	for j := range bs {
		if referencesBinder(bs[j].init, bs[i].name, refInsideClosure) {
			capturedDuringInit = true
			break
		}
	}
	if !capturedDuringInit {
		return false
	}
	return letrecRegionTier(bs, i) != tierSelfPatch
}

// markBoxedBinders decides, for each binder in binders, whether it is boxed over
// scope; records the boxed ones in p.boxedSlots by absolute slot; and returns
// their LocalIndexes in binder order so the caller can emit OpBoxSlot.
//
// forced is an extra, independent reason to box the i-th binder — the letrec*
// initialization-order hazard — or nil when the binders are not a letrec* region.
// Boxing is the union: either reason alone is sufficient.
//
// The binder must already exist in p.env — this resolves it there, which is what
// keeps the emit site, every read, and every write agreeing on one slot: all
// three go through GetLocalIndex against the same frame.
func (p *CompileTimeContinuation) markBoxedBinders(
	binders []*syntax.SyntaxSymbol,
	scope []validate.ValidatedExpr,
	forced func(i int) bool,
) []*environment.LocalIndex {
	if p.env == nil || len(binders) == 0 {
		return nil
	}
	var q []*environment.LocalIndex
	for i, b := range binders {
		if b == nil {
			continue
		}
		if !binderIsBoxed(b, scope) && (forced == nil || !forced(i)) {
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
		p.boxedSlots.Set(k)
		q = append(q, li)
	}
	return q
}

// setBangTargetsIn returns every set!-target symbol in body, in traversal order.
//
// These are REFERENCES, not binders — their scope sets are supersets of the
// binder's, which is the direction sameBinder(binder, ref) tests.
func setBangTargetsIn(body []validate.ValidatedExpr) []*syntax.SyntaxSymbol {
	var q []*syntax.SyntaxSymbol
	var walk func(e validate.ValidatedExpr)
	walk = func(e validate.ValidatedExpr) {
		if e == nil {
			return
		}
		setBang, ok := e.(*validate.ValidatedSetBang)
		if ok && setBang.Name != nil {
			q = append(q, setBang.Name)
		}
		validate.WalkSubExprs(e, func(child validate.ValidatedExpr, _ validate.ChildRole) {
			walk(child)
		})
	}
	for _, e := range body {
		walk(e)
	}
	return q
}

// withBoxOnDeclare installs the set!-target list for a body region and returns a
// restore function, so a nested region does not leak its list outward.
func (p *CompileTimeContinuation) withBoxOnDeclare(body []validate.ValidatedExpr) func() {
	saved := p.boxOnDeclare
	p.boxOnDeclare = setBangTargetsIn(body)
	return func() {
		p.boxOnDeclare = saved
	}
}

// maybeBoxOnDeclare boxes an internal define's slot at the moment the slot is
// created, when some set! in the enclosing body targets that binder.
//
// THIS IS THE SAFETY NET, AND IT IS LOAD-BEARING RATHER THAN BELT-AND-BRACES.
// The region scans (compileBody, CompileValidatedLet) enumerate the defines they
// can SEE — direct body elements and those inside a begin, which is exactly what
// predeclareDefineFromValidatedRecursive reaches. A define that arrives any other
// way still allocates a slot here, lazily, and would then be captured by value
// with no cell: measured on (chibi test)'s `test` expansion, where the defines
// land in a frame no region scan enumerates, and the closure's set! faulted with
// "store-free: free slot 0 holds *values.Boolean, not a box".
//
// The predicate is ASSIGNED ALONE, deliberately weaker than captured ∧ assigned.
// Testing "captured" here would mean comparing two REFERENCES' scope sets against
// each other, and neither is a subset of the other in general — a false negative
// there is a wrong answer, while boxing an assigned-but-uncaptured define costs
// one indirection. The binder-vs-reference comparison this does make is the sound
// direction: binderScopes ⊆ refScopes, the same subset test resolution uses.
//
// The cell is installed BEFORE the define's value expression is compiled, so a
// closure built by that expression captures the cell rather than the void the
// slot holds at that instant.
func (p *CompileTimeContinuation) maybeBoxOnDeclare(binder *syntax.SyntaxSymbol, li *environment.LocalIndex) {
	if binder == nil || li == nil || len(p.boxOnDeclare) == 0 {
		return
	}
	if p.localIsBoxed(li) {
		return
	}
	assigned := false
	for _, t := range p.boxOnDeclare {
		if sameBinder(binder, t) {
			assigned = true
			break
		}
	}
	if !assigned {
		return
	}
	k, ok := absoluteSlot(p.env, li)
	if !ok {
		return
	}
	p.ensureBoxedSlots()
	p.boxedSlots.Set(k)
	p.AppendOperations(machine.NewOperationBoxSlot(p.runtimeIndex(li)))
}

// ensureBoxedSlots allocates the shared verdict map on first use.
//
// Allocated lazily but shared eagerly: compileBody calls this before handing the
// map to the child continuation, so one map serves a whole compile even when the
// outermost frame boxes nothing.
func (p *CompileTimeContinuation) ensureBoxedSlots() {
	if p.boxedSlots == nil {
		p.boxedSlots = values.NewMapSet[boxedSlotKey](0)
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
	return p.boxedSlots.Get(k)
}

// emitBoxSlots emits one OpBoxSlot per index, installing the cells before any
// closure in scope can observe the slots.
func (p *CompileTimeContinuation) emitBoxSlots(lis []*environment.LocalIndex) {
	for _, li := range lis {
		p.AppendOperations(machine.NewOperationBoxSlot(p.runtimeIndex(li)))
	}
}

// freeSlotOf reports the free-vector index of the variable li resolves to, when
// this body's layout carries it.
//
// The comparison is on the ABSOLUTE key, so it holds from anywhere inside the
// body — including under any number of let frames, which change li's depth but
// not the slot's owner. A linear scan rather than a map: measured free-variable
// density on the schelog workload is 1.8 slots per lambda, and a map would cost
// an allocation per compiled body to save nothing.
func (p *CompileTimeContinuation) freeSlotOf(li *environment.LocalIndex) (int, bool) {
	if len(p.freeLayout) == 0 || p.env == nil || li == nil {
		return 0, false
	}
	k, ok := absoluteSlot(p.env, li)
	if !ok {
		return 0, false
	}
	for i := range p.freeLayout {
		if p.freeLayout[i].abs == k {
			return i, true
		}
	}
	return 0, false
}

// emitLocalStore emits the write form the variable needs.
//
// Three forms, and the free one is not optional: a free variable is no longer in
// the frame at all, so a plain store would write an unrelated slot of whatever
// frame the depth happens to land on.
func (p *CompileTimeContinuation) emitLocalStore(li *environment.LocalIndex) {
	i, isFree := p.freeSlotOf(li)
	if isFree {
		// A free variable that is WRITTEN is captured and assigned, so the
		// boxing pass put a cell in this slot and OpStoreFree writes through it.
		// Storing the value into the vector instead would leave every other
		// holder — including the frame the variable is bound in — reading the
		// old one.
		p.AppendOperations(machine.NewOperationStoreFree(i))
		return
	}
	boxed := p.localIsBoxed(li)
	// Translated only now: localIsBoxed above and freeSlotOf before it key on the
	// COMPILE-TIME index, which is what the binder's verdict was recorded under.
	li = p.runtimeIndex(li)
	if boxed {
		p.AppendOperations(machine.NewOperationStoreThroughBox(li))
		return
	}
	p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
}

// emitLocalLoad emits the read form the variable needs: from the free vector
// when this body captured it, otherwise from the frame — and in either case
// followed by an unbox when the slot holds a cell.
func (p *CompileTimeContinuation) emitLocalLoad(li *environment.LocalIndex) {
	i, isFree := p.freeSlotOf(li)
	boxed := p.localIsBoxed(li)
	if isFree {
		p.AppendOperations(machine.NewOperationLoadFree(i))
	} else {
		p.AppendOperations(machine.NewOperationLoadLocalByLocalIndexImmediate(p.runtimeIndex(li)))
	}
	if boxed {
		p.AppendOperations(machine.NewOperationUnbox())
	}
}

// emitFreeVarPush pushes one free variable's value for OpMakeClosure to drain.
//
// THE PUSH FORM IS NOT ALWAYS OpPushLocal, and this is the whole reason Pass 1's
// transitive closure is load-bearing rather than bookkeeping. In
// (lambda (a) (lambda (b) (lambda (c) a))), when the MIDDLE lambda runs and
// builds the inner closure, `a` is not a local of the middle activation at all —
// it is entry 0 of the middle closure's OWN free vector. OpPushLocal there reads
// an unrelated slot.
//
// The value pushed is the slot's RAW contents: for a boxed variable that is the
// cell, which is exactly what must be shared.
func (p *CompileTimeContinuation) emitFreeVarPush(fv freeVar) {
	li := environment.NewLocalIndex(fv.key.slot, fv.key.depth)
	i, isFree := p.freeSlotOf(li)
	if isFree {
		p.AppendOperations(machine.NewOperationLoadFree(i), machine.NewOperationPush())
		return
	}
	p.AppendOperations(machine.NewOperationLoadLocalByLocalIndexImmediate(p.runtimeIndex(li)), machine.NewOperationPush())
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

// letrecRegionTier classifies binder i of a letrec* region: a letrec / letrec* /
// named-let binding group, or a body's internal defines.
//
// PRECONDITION for the let spelling: v.Kind must be recursive (letrec family,
// which includes the desugared named let). In a plain let or let* the bindings
// are not in each other's inits, so there is no forward reference to classify and
// every answer here would describe outer bindings instead. Internal defines are
// letrec* by R7RS §5.3.2 and always qualify.
//
// The three questions, in the order that makes the answers disjoint:
//
//  1. Does a SIBLING init reference binder i? Then i is read before its own init
//     has stored anything. If i's init reciprocates, that is mutual recursion
//     (T3); otherwise it is an ordinary forward reference (T1).
//  2. Is binder i referenced OUTSIDE a closure body by any init — including its
//     own? Then the reference is evaluated during initialization, when the value
//     really does not exist (T1).
//  3. Otherwise: does i's own init reference i from inside a closure? Then the
//     only reader is a closure that cannot run before it exists (T2).
//
// Identity is decided the usual way: the name Key narrows, ScopesCompatible
// decides. Shadowing inside an init is deliberately NOT modelled — a shadowed
// same-name reference counts as a reference, which can only move a binder toward
// tierBoxed, the conservative direction.
func letrecRegionTier(bs []letrecRegionBinder, i int) letrecTier {
	if i < 0 || i >= len(bs) {
		return tierBoxed
	}
	self := bs[i].name
	for j := range bs {
		if j == i {
			continue
		}
		if !referencesBinder(bs[j].init, self, refAnywhere) {
			continue
		}
		if referencesBinder(bs[i].init, bs[j].name, refAnywhere) {
			return tierMutual
		}
		return tierBoxed
	}
	for j := range bs {
		if referencesBinder(bs[j].init, self, refOutsideClosure) {
			return tierBoxed
		}
	}
	if regionInitIsLambda(bs[i].init) && referencesBinder(bs[i].init, self, refInsideClosure) {
		return tierSelfPatch
	}
	return tierBoxed
}

// regionInitIsLambda reports whether a region binder's init produces exactly one
// closure that OpMakeClosure can back-patch into its own free vector.
//
// The restriction is what keeps T2 honest, and case-lambda is why it is not
// merely "is the init a procedure". A case-lambda compiles to one closure PER
// CLAUSE, wrapped afterwards by OpMakeCaseLambdaClosure; back-patching a clause
// with itself would bind the self reference to the clause rather than to the
// dispatcher. Such a binder falls through to tierBoxed, where the cell it shares
// receives the finished dispatcher like any other T1.
func regionInitIsLambda(init validate.ValidatedExpr) bool {
	_, ok := init.(*validate.ValidatedLambda)
	if ok {
		return true
	}
	d, ok := init.(*validate.ValidatedDefine)
	if !ok {
		return false
	}
	if d.IsFunction {
		return true
	}
	_, ok = d.SubExp().(*validate.ValidatedLambda)
	return ok
}

// letrecBindingTier classifies the i-th binding of a letrec / letrec* group.
func letrecBindingTier(v *validate.ValidatedLet, i int) letrecTier {
	if v == nil {
		return tierBoxed
	}
	return letrecRegionTier(letBindersOfRegion(v), i)
}

// bodyDefineTier classifies the i-th internal define of a body — the OTHER
// spelling of a letrec* region, and the one that occurs in practice.
func bodyDefineTier(body []validate.ValidatedExpr, i int) letrecTier {
	return letrecRegionTier(bodyBindersOfRegion(body), i)
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
