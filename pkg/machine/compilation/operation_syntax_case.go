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

import (
	"errors"
	"fmt"
	"maps"
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// OperationSyntaxCaseMatch performs pattern matching for syntax-case.
//
// Expects:
//   - Value register: *SyntaxCaseClause with compiled pattern
//   - Per-context syntaxCaseState.input: input syntax object (set by OperationStoreSyntaxCaseInput)
//
// Results:
//   - If match succeeds: value register = #t, pattern bindings stored in context
//   - If match fails: value register = #f
type OperationSyntaxCaseMatch struct {
	machine.OperationBase
}

// syntaxCaseState holds the state of one syntax-case form: the input being
// matched, and the bindings and matcher the winning clause produced.
//
// It lives in TWO places, with two different lifetimes, and the split is the
// whole point:
//
//   - The MachineContext slot (MachineContext.SyntaxCaseState, an any-typed
//     back-channel on the clustered expansion sub-record) holds the PENDING
//     state, from OperationStoreSyntaxCaseInput until the form ends. It is a
//     hand-off buffer: the input has to survive from the store op, across each
//     clause's match attempt, to OperationBindPatternVars, and no environment
//     frame exists yet at that point. OperationSyntaxCaseMatch overwrites its
//     bindings and matcher once per clause tried.
//   - The pattern-variable environment frame that OperationBindPatternVars
//     pushes holds a SNAPSHOT, under syntaxCaseStateKey. This is the copy
//     (syntax ...) expands against, and it is what gives the state the same
//     LEXICAL extent the pattern variables themselves have.
//
// Reading the state off the frame rather than off the context is what makes
// this reentrant. A syntax-case nested in a clause body pushes its own frame,
// so it shadows rather than clobbers; the enclosing form's snapshot is
// untouched, and popping the inner frame restores it. It also makes the two
// (syntax ...) paths agree on extent: the non-ellipsis path compiles to a
// local load out of this same frame, so an escaping closure that captured the
// frame kept working while the ellipsis path, reading the context slot, failed
// as soon as the form's DYNAMIC extent ended. Pattern variables are one
// lexical binding (R6RS §12.4).
//
// machine/ cannot import this package (one-direction dependency), so the
// machine-side slot is any-typed; the constraint that only this concrete type
// is ever stored there is enforced by the slot's encapsulation rather than by
// the type system.
type syntaxCaseState struct {
	bindings map[string]syntax.SyntaxValue // pattern variable bindings from last match
	matcher  *match.SyntaxMatcher          // matcher from last match (needed for ellipsis expansion)
	input    syntax.SyntaxValue            // input syntax object being matched
}

// syntaxCaseStateKey names the reserved slot that carries a form's state on its
// pattern-variable frame. That frame is already an explicitly NOMINAL namespace
// — pattern variables are bound scopeless and looked up by bare name under a
// wildcard scope query (see compileSyntaxTemplateToOps) — so a reserved name is
// the local convention, not a new one. bindSyntaxCaseState refuses rather than
// silently sharing a slot if a pattern variable ever collides with it.
var syntaxCaseStateKey = values.NewSymbol("#%syntax-case-state")

// syntaxCaseState implements values.Value so it can occupy a binding slot. It
// is not a Scheme value and is unreachable from Scheme: the reserved key is not
// writable as a plain symbol, and nothing renders a pattern-variable frame's
// slots. The methods exist for the slot's type, not for a caller.
func (p *syntaxCaseState) SchemeString() string {
	return "#<syntax-case-state>"
}

func (p *syntaxCaseState) IsVoid() bool {
	return false
}

func (p *syntaxCaseState) EqualTo(other values.Value) bool {
	v, ok := other.(*syntaxCaseState)
	return ok && v == p
}

// bindSyntaxCaseState attaches sc to frame under the reserved key.
//
// The slot is APPENDED after the pattern variables, so pattern-variable slot
// indices are unchanged and the compile-time mirror frame
// (createPatternVarEnvironment) does not need a matching slot.
func bindSyntaxCaseState(mc *machine.MachineContext, frame *environment.EnvironmentFrame, sc *syntaxCaseState) error {
	li, created := frame.MaybeCreateLocalBinding(syntaxCaseStateKey, environment.BindingTypeVariable, nil, nil)
	if li == nil {
		return mc.WrapError(werr.ErrInternal, "syntax-case: pattern-variable frame has no local environment")
	}
	if !created {
		// Only reachable if a pattern variable is spelled exactly like the
		// reserved key, which needs |...| notation. Refusing beats overwriting
		// the user's pattern variable with an internal object.
		return mc.WrapError(werr.ErrInvalidSyntax, fmt.Sprintf(
			"syntax-case: %q is reserved and cannot be a pattern variable", syntaxCaseStateKey.Key))
	}
	err := frame.SetLocalValue(li, sc)
	if err != nil {
		return mc.WrapError(err, "syntax-case: failed to attach state to the pattern-variable frame")
	}
	return nil
}

// syntaxCaseStateFromEnv resolves the state by a NEAREST-MARKED-ANCESTOR walk
// from the current environment frame: GetLocalIndex under a wildcard scope query
// returns the first match walking innermost-out, and only a pattern-variable
// frame carries the reserved key.
//
// The walk is deliberate, and a compile-time depth immediate is deliberately
// NOT used. A depth is a lexical fact whose runtime validity depends on
// push/pop balance — precisely what the frame-leak defect broke. The walk
// follows the same graph the state's lifetime rests on: the frame is
// heap-allocated by BindPatternVars and can never enter envFramePool (it clears
// envPooled, OpReleaseEnvFrame releases only pooled frames, and
// RestoreAndRelease's shared/captured path returns before releasing), so its
// lifetime is GC reachability, which subsumes continuation reachability. That
// is why an escaping closure can still expand a template after its form ended.
func syntaxCaseStateFromEnv(mc *machine.MachineContext) (*syntaxCaseState, error) {
	env := mc.EnvironmentFrame()
	li := env.GetLocalIndex(syntaxCaseStateKey, syntax.AllScopes())
	if li == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax: no syntax-case pattern-variable frame in scope (no expansion in flight)")
	}
	binding := env.GetLocalBinding(li)
	if binding == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax: syntax-case state slot resolved to no binding")
	}
	sc, ok := binding.Value().(*syntaxCaseState)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf(
			"syntax: unexpected state type %T in the syntax-case state slot", binding.Value()))
	}
	return sc, nil
}

// loadSyntaxCaseState fetches the PENDING *syntaxCaseState payload from
// MachineContext.SyntaxCaseState(). Discriminates the two failure modes:
// nil (no syntax-case expansion in flight) and wrong concrete type
// (contract violation — the slot is any-typed and the encapsulation
// argument relies on no other code storing alternatives).
//
// Only the ops that run BEFORE the pattern-variable frame exists use this:
// SyntaxCaseMatch, BindPatternVars and SyntaxCaseNoMatch. Template expansion
// goes through syntaxCaseStateFromEnv instead.
func loadSyntaxCaseState(mc *machine.MachineContext) (*syntaxCaseState, error) {
	raw := mc.SyntaxCaseState()
	if raw == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: no state on MachineContext (no expansion in flight)")
	}
	sc, ok := raw.(*syntaxCaseState)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf(
			"syntax-case: unexpected state type %T from MachineContext.SyntaxCaseState()", raw))
	}
	return sc, nil
}

func NewOperationSyntaxCaseMatch() *OperationSyntaxCaseMatch {
	return &OperationSyntaxCaseMatch{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-case-match", "SyntaxCaseMatch"),
	}
}

func (p *OperationSyntaxCaseMatch) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Get the clause from value register
	clauseVal := mc.GetValue()
	clause, ok := clauseVal.(*SyntaxCaseClause)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf("syntax-case: expected clause in value register, got %T", clauseVal))
	}

	// Get input from per-context state (set by OperationStoreSyntaxCaseInput).
	// The state field is any-typed (see machine_context.go); discriminate
	// nil-vs-mismatch so the diagnostic identifies which contract was broken.
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.input == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: state has no input (OperationStoreSyntaxCaseInput not run?)")
	}
	input := sc.input

	// Create a matcher.
	//
	// LiteralSyntax and BindingChecker are what turn on match.go's R7RS §4.3.2
	// literal check; it needs BOTH, and this path used to supply neither, so a
	// pattern literal matched a use-site identifier that shadows it —
	// (let ((else 5)) (m else 1)) took the `else` clause where the identical
	// syntax-rules macro correctly took the fallback.
	//
	// The environment is the use site's, not the definition site's, for the
	// same reason OperationSyntaxRulesTransform gives: the question is whether
	// the INVOKING code has bound the literal.
	bindingEnv := mc.EnvironmentFrame()
	if mc.ExpanderContext() != nil && mc.ExpanderContext().Env() != nil {
		bindingEnv = mc.ExpanderContext().Env()
	}
	matcher := match.NewSyntaxMatcher(clause.PatternVars, clause.Bytecode, &match.SyntaxMatcherOpts{
		EllipsisVars:   clause.EllipsisVars,
		EllipsisDepths: clause.EllipsisDepths,
		LiteralSyntax:  clause.LiteralSyntax,
		LiteralDefs:    clause.LiteralDefs,
		BindingChecker: &envBindingChecker{env: bindingEnv},
	})

	// Try to match. ErrNotAMatch is normal control flow for syntax-case
	// (this clause didn't match — try the next one); any other error
	// (context cancellation, malformed input, ellipsis-depth invariant
	// violation, internal matcher bug) is a real failure and must surface.
	err = matcher.Match(mc.Context(), input)
	if errors.Is(err, match.ErrNotAMatch) {
		mc.SetValue(values.FalseValue)
		mc.IncrPC()
		return mc, nil
	}
	if err != nil {
		return nil, mc.WrapError(err, "syntax-case: matcher error")
	}

	// Match succeeded - store bindings and matcher in per-context state
	sc.bindings = matcher.GetBindings()
	sc.matcher = matcher
	mc.SetValue(values.TrueValue)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationSyntaxCaseMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseMatch)
	return machine.SameType(p, v, ok)
}

// OperationBindPatternVars binds pattern variables from the last match
// into a new local environment frame that is pushed onto the current environment.
//
// This operation creates a new environment frame with local slots for each
// pattern variable, binds the matched values, and makes this the current environment.
type OperationBindPatternVars struct {
	machine.OperationBase
	PatternVars []string // Ordered list for consistent indexing
	// MergedSlots is how many anonymous slots a `let` in the clause body took
	// out of this frame instead of pushing one of its own.
	//
	// WRITTEN AFTER THE OPERATION IS EMITTED, by compileSyntaxCaseClause, once
	// the body it brackets has been compiled — the count is not knowable before
	// then, and this operation is reached through the side table by pointer, so
	// the emitted instruction sees the final value. Compare
	// CompileValidatedLet's OpPushEnv operand patch, which solves the same
	// problem for an operand that is packed into the instruction word.
	MergedSlots int
}

func NewOperationBindPatternVars(patternVars values.StringSet) *OperationBindPatternVars {
	// Convert to a sorted list for consistent indexing
	vars := slices.Sorted(maps.Keys(patternVars))
	return &OperationBindPatternVars{
		OperationBase: machine.NewOperationBaseWithGoName("operation:bind-pattern-vars", "BindPatternVars"),
		PatternVars:   vars,
	}
}

func (p *OperationBindPatternVars) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.bindings == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: state has no bindings (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Create the pattern-variable frame EMPTY and let every slot be appended.
	//
	// NewLocalEnvironment(n) would prefix n void filler slots that nothing ever
	// names — MaybeCreateLocalBinding appends, so the pattern variables land at
	// n..2n-1 either way. The filler was harmless while the frame's layout was
	// private to this operation and its compile-time mirror
	// (createPatternVarEnvironment builds it the same way, which is why the two
	// agreed). It stops being harmless once a merged `let` takes slots out of
	// this frame: the two sides must agree on WHICH INDEX comes next, and
	// "however many appends have happened" is the only description of that both
	// can compute. Passing 0 makes the layout literally the append order:
	// pattern variables, the state slot, then the clause body's merged slots.
	localEnv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, mc.EnvironmentFrame())

	// Bind each pattern variable. MaybeCreateLocalBinding returns
	// (*LocalIndex, created bool) and the bool is discarded, because on this
	// frame it carries no information: localEnv is freshly allocated, so its
	// keys map is empty, no dedup lookup can hit, and every call creates a
	// slot. For the same reason li is never nil here (a non-nil keys map makes
	// hasLocal() true); the guard below is defensive only.
	//
	// The ellipsis signal is therefore not in the return values, it is the
	// ABSENCE of varName from sc.bindings. matcher.GetBindings() exposes only
	// the ROOT capture context; ellipsis-captured pattern variables (e.g. `x`
	// in `(_ x ...)`) live in that context's `children` field, walked at
	// template-expansion time (see internal/match/match.go's "Capture Context"
	// comment block and findMatchingEllipsisID). Indexing the missing key
	// yields a nil syntax.SyntaxValue, and writing that nil overrides the
	// slot's default `values.Void` initialization from NewLocalEnvironment.
	// The nil is the protocol signal: it tells the downstream
	// OperationSyntaxTemplateExpand to consult sc.matcher's child contexts
	// when expanding `(syntax (x ...))` templates.
	for _, varName := range p.PatternVars {
		sym := values.NewSymbol(varName)
		li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
		if li == nil {
			// No local frame to write to: nothing to bind.
			continue
		}
		// stxVal is nil when varName is absent from sc.bindings (the
		// ellipsis case). Writing nil here is the protocol signal —
		// see the comment block above.
		stxVal := sc.bindings[varName]
		err := childEnv.SetLocalValue(li, stxVal)
		if err != nil {
			return nil, mc.WrapError(err, fmt.Sprintf("syntax-case: failed to bind pattern variable %s", varName))
		}
	}

	// Attach this clause's state to the frame, as a SNAPSHOT rather than the
	// pending pointer. The pending state is reused across clause attempts —
	// a false fender pops this frame and lets the next clause's match overwrite
	// bindings and matcher — so sharing the pointer would let a later clause
	// rewrite an earlier clause's captured state.
	snapshot := &syntaxCaseState{
		bindings: sc.bindings,
		matcher:  sc.matcher,
		input:    sc.input,
	}
	err = bindSyntaxCaseState(mc, childEnv, snapshot)
	if err != nil {
		return nil, err
	}

	// The clause body's merged `let` slots, appended after the state slot so the
	// indices match the compile-time mirror exactly. They are anonymous: the
	// binder's NAME stays in the let's own compile-time frame, where scope-set
	// resolution can still shadow it, and only the slot moves here.
	lenv := childEnv.LocalEnvironment()
	if lenv != nil {
		for range p.MergedSlots {
			lenv.AppendAnonymousSlot()
		}
	}

	// Switch to the new environment. childEnv was heap-allocated (not from
	// envFramePool), so clear envPooled to prevent RestoreAndRelease from
	// recycling it. See vm_state.go envPooled write-site table. That is also
	// what makes the frame a sound home for the state: it can never be
	// recycled under a live clause body or under a closure that escaped one.
	mc.SetEnvironmentFrame(childEnv)
	mc.SetEnvPooled(false)
	mc.IncrPC()
	return mc, nil
}

// EqualTo compares the frame this operation builds: which pattern variables it
// binds AND how wide the frame ends up. Two clauses over the same pattern whose
// bodies merge different numbers of `let` slots build different frames, so
// MergedSlots is part of the identity rather than metadata.
func (p *OperationBindPatternVars) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationBindPatternVars)
	if ok && p != nil && v != nil && p.MergedSlots != v.MergedSlots {
		return false
	}
	return machine.SliceMatches(p, v, ok,
		func(op *OperationBindPatternVars) []string {
			return op.PatternVars
		})
}

// OperationSyntaxCaseNoMatch is emitted at the end of syntax-case when no clause matches.
type OperationSyntaxCaseNoMatch struct {
	machine.OperationBase
}

func NewOperationSyntaxCaseNoMatch() *OperationSyntaxCaseNoMatch {
	return &OperationSyntaxCaseNoMatch{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-case-no-match", "SyntaxCaseNoMatch"),
	}
}

func (p *OperationSyntaxCaseNoMatch) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Include the input form in the diagnostic when it's still available.
	// Macro debugging is hard precisely because the output is "syntax";
	// stripping the actual input forces users into trial-and-error.
	raw := mc.SyntaxCaseState()
	if raw != nil {
		sc, ok := raw.(*syntaxCaseState)
		if ok && sc.input != nil {
			return nil, mc.WrapError(werr.ErrInvalidSyntax, fmt.Sprintf(
				"syntax-case: no matching clause for input %s", sc.input.SchemeString()))
		}
	}
	return nil, mc.WrapError(werr.ErrInvalidSyntax, "syntax-case: no matching clause (input unavailable)")
}

func (p *OperationSyntaxCaseNoMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseNoMatch)
	return machine.SameType(p, v, ok)
}

// OperationSyntaxTemplateExpand expands a syntax template using the current
// pattern variable bindings. This is used for templates containing ellipsis,
// which require runtime expansion rather than compile-time code generation.
//
// The template is stored in the value register (loaded from literals).
// The result is the expanded syntax object, left in the value register.
type OperationSyntaxTemplateExpand struct {
	machine.OperationBase
	// FreeIds and PatternVarSyntax carry the enclosing syntax-case clause's
	// hygiene data, computed once at compile time by CompileSyntax. They mirror
	// the SyntaxRulesClause fields the syntax-rules transformer uses, so the
	// ellipsis template-expansion path is hygienic (R7RS §4.3): free template
	// identifiers resolve at the macro definition site and template-introduced
	// binders carry a fresh intro scope rather than capturing use-site identifiers.
	FreeIds          map[string]*FreeIdResolution
	PatternVarSyntax map[string]*syntax.SyntaxSymbol
}

func NewOperationSyntaxTemplateExpand(freeIds map[string]*FreeIdResolution, patternVarSyntax map[string]*syntax.SyntaxSymbol) *OperationSyntaxTemplateExpand {
	return &OperationSyntaxTemplateExpand{
		OperationBase:    machine.NewOperationBaseWithGoName("operation:syntax-template-expand", "SyntaxTemplateExpand"),
		FreeIds:          freeIds,
		PatternVarSyntax: patternVarSyntax,
	}
}

func (p *OperationSyntaxTemplateExpand) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Resolve off the environment, not off the MachineContext. This op is
	// emitted by the clause body's compiler, whose env IS the compile-time
	// mirror of the frame BindPatternVars pushes, so the handle is the one the
	// adjacent non-ellipsis branch of CompileSyntax already computes.
	sc, err := syntaxCaseStateFromEnv(mc)
	if err != nil {
		return nil, err
	}
	if sc.matcher == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax: state has no matcher (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Get the template from value register
	templateVal := mc.GetValue()
	template, ok := templateVal.(syntax.SyntaxValue)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf("syntax: expected syntax template, got %T", templateVal))
	}

	// Expand the template using the matcher (handles ellipsis). Mirror the
	// syntax-rules transformer (OperationSyntaxRulesTransform.Apply): a fresh
	// intro scope per expansion, plus the compile-time free-id and pattern-var
	// hygiene data. This makes the ellipsis path hygienic (R7RS §4.3) — the
	// non-ellipsis path achieves the same by emitting template symbols as
	// def-site-scoped literals. UseSiteCtx/Origin are intentionally nil:
	// syntax-case has no single macro-invocation use-site.
	introScope := syntax.NewScopeWithLabel("intro")
	freeIds := make(map[string]match.FreeIdResolver, len(p.FreeIds))
	for k, v := range p.FreeIds {
		freeIds[k] = v
	}
	expanded, err := sc.matcher.Expand(template, match.ExpandOptions{
		IntroScope:       introScope,
		FreeIds:          freeIds,
		PatternVarSyntax: p.PatternVarSyntax,
	})
	if err != nil {
		return nil, mc.WrapError(err, "syntax: template expansion error")
	}

	mc.SetValue(expanded)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationSyntaxTemplateExpand) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxTemplateExpand)
	return machine.SameType(p, v, ok)
}

// OperationStoreSyntaxCaseInput opens a syntax-case form by installing a FRESH
// pending syntaxCaseState holding the value register as the input, for
// OperationSyntaxCaseMatch to match against.
//
// Minting a fresh state rather than reusing whatever is installed is what keeps
// a nested syntax-case from mutating the enclosing form's snapshot: the
// enclosing frame holds its own object, not this one.
type OperationStoreSyntaxCaseInput struct {
	machine.OperationBase
}

func NewOperationStoreSyntaxCaseInput() *OperationStoreSyntaxCaseInput {
	return &OperationStoreSyntaxCaseInput{
		OperationBase: machine.NewOperationBaseWithGoName("operation:store-syntax-case-input", "StoreSyntaxCaseInput"),
	}
}

func (p *OperationStoreSyntaxCaseInput) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc := &syntaxCaseState{}
	val := mc.GetValue()
	// Convert to syntax value if needed (handles Pairs, Vectors, etc.)
	stx, ok := val.(syntax.SyntaxValue)
	if ok {
		sc.input = stx
	} else {
		converted, err := schemeutil.DatumToSyntaxValue(mc.Context(), nil, val)
		if err != nil {
			// Name the operation. Bare, the diagnostic reads as an internal Go
			// helper failing rather than as syntax-case refusing its input.
			return nil, werr.WrapForeignErrorf(err, "syntax-case: cannot use this value as input")
		}
		sc.input = converted
	}
	mc.SetSyntaxCaseState(sc)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationStoreSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationStoreSyntaxCaseInput)
	return machine.SameType(p, v, ok)
}

// OperationClearSyntaxCaseInput drops the PENDING syntax-case state at the end
// of a syntax-case form, releasing the input syntax object and the matcher.
//
// It is no longer load-bearing for correctness, and that is the point. It used
// to be the only thing that stopped a later (syntax ...) from expanding against
// a finished form's matcher, which made it a bug that a clause body ending in a
// tail CALL never reaches it — the epilogue sits after the body's tail
// position. Template expansion now resolves through the pattern-variable frame,
// so skipping this instruction costs a retention, not an answer: the state a
// tail call leaves installed is read by nothing.
type OperationClearSyntaxCaseInput struct {
	machine.OperationBase
}

func NewOperationClearSyntaxCaseInput() *OperationClearSyntaxCaseInput {
	return &OperationClearSyntaxCaseInput{
		OperationBase: machine.NewOperationBaseWithGoName("operation:clear-syntax-case-input", "ClearSyntaxCaseInput"),
	}
}

func (p *OperationClearSyntaxCaseInput) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	mc.SetSyntaxCaseState(nil)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationClearSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationClearSyntaxCaseInput)
	return machine.SameType(p, v, ok)
}
