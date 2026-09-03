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

package match

// syntax_adapter.go bridges between syntax objects (with hygiene info) and
// the pattern matching VM.
//
// Design: The macro system uses a layered architecture:
//   - Pattern Matching: Now operates directly on syntax.SyntaxValue types
//   - Syntax Adapter (this file): Manages hygiene, scope handling, and matching
//   - Template Expansion (syntax_expand.go): Recursive expansion with intro scopes
//
// The SyntaxMatcher wraps the core Matcher, handling:
//   - Literal hygiene checking (R7RS §4.3.2 auxiliary syntax like => and else)
//   - Scope-aware literal matching with rebinding scope filtering
//   - Pattern compilation and binding extraction
//
// Template expansion methods live in syntax_expand.go.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// FreeIdResolver provides free identifier resolution information for
// template expansion hygiene. Implemented by
// machine/compilation.FreeIdResolution.
//
// In ExpandOptions.FreeIds, a non-nil FreeIdResolver carries binding
// information from macro definition time. A nil map value is treated
// the same as an absent key (the identifier receives the intro scope).
// Methods return zero values when that aspect of resolution is absent.
type FreeIdResolver interface {
	GetLocalScopes() []*syntax.Scope
	GetGlobal() *environment.GlobalIndex
	GetHasLocalBinding() bool
	GetLibraryScope() *syntax.Scope
}

// LiteralPin is the DEFINITION-site resolution of one pattern literal, captured
// when the syntax-rules or syntax-case form was compiled.
//
// R7RS §4.3.2 compares the literal's binding at the macro DEFINITION site with
// the input identifier's binding at the USE site. Resolving both sides through
// the one use-site environment answers neither question for a zero-scoped global:
// a top-level (define else #f) made after the macro was defined is visible from
// the definition frame too (both reach one owner store), so the two sides resolve
// to the identical *Binding and the comparison is vacuous. Only a definition-TIME
// snapshot discriminates.
//
// The zero value is deliberately NOT a pin. A nil Binding with Ambiguous false
// means "unbound at definition time", which is R7RS's "the two identifiers are
// the same and both have no lexical binding" arm, and must leave the use-site-only
// comparison verbatim — the pin tightens, it never loosens. Ambiguous records
// that the definition-site probe found an incomparable, equal-cardinality
// scope-set tie rather than a single binding (no error is raised or involved); a
// scope-aware lookup has three answers, and for this consumer the conservative
// one is "the literal does not match".
type LiteralPin struct {
	Binding   *environment.Binding
	Ambiguous bool
}

// BindingChecker is an interface for checking if a symbol has a lexical binding.
// This is used for R7RS auxiliary syntax hygiene: literals like => and else
// should not match when the identifier has been locally bound.
// Implemented by machine/compilation (*envBindingChecker) to avoid circular
// imports.
type BindingChecker interface {
	// HasBinding checks if sym with the given scopes has a lexical binding.
	// Returns true if the symbol is bound (to a variable, macro, etc.).
	HasBinding(sym string, scopes []*syntax.Scope) bool

	// GetBinding returns the binding for sym with the given scopes.
	// Returns nil if no binding exists. Bindings can be compared for
	// pointer equality to check if two identifiers have the same
	// binding (per R7RS §4.3.2).
	GetBinding(sym string, scopes []*syntax.Scope) *environment.Binding

	// GetLiteralBinding resolves the USE-SITE side of the R7RS §4.3.2 comparison:
	// the frame's own lexical chain at its own phase, then the ambient keyword of
	// the name (else, =>, and every special-form name live at the ambient
	// coordinate, reachable from every phase), and no other phase. Searching
	// further would let one phase's binding of the name decide another phase's
	// literal. ok is false when resolution was ambiguous.
	GetLiteralBinding(sym string, scopes []*syntax.Scope) (*environment.Binding, bool)
}

// SyntaxMatcher adapts the core Matcher to work with syntax objects and hygiene.
//
// It provides:
//   - Syntax-native pattern matching with source location preservation
//   - Template expansion with hygiene (intro scope for newly created syntax)
//   - Literal hygiene checking for R7RS auxiliary syntax
//
// Key features:
//
// Pattern Variable Capture: Pattern variables are captured directly as
// syntax.SyntaxValue, preserving source context through the entire match.
// No conversion to raw values is needed - the Matcher's MatchSyntaxWithLiterals
// operates on SyntaxPair directly.
//
// Literal Hygiene: The literalSyntax map stores pattern literals with their
// scopes. During matching, if an input symbol has a literal's name but
// incompatible scopes (e.g., shadowed by let), it won't match the literal.
// This implements R7RS's requirement that auxiliary syntax like => and else
// be treated as regular expressions when locally shadowed.
//
// R7RS Binding Check: For full R7RS compliance (§4.3.2), the input identifier's
// binding is compared with the pattern literal's. WHICH binding stands for the
// literal depends on literalDefs, the definition-site pins:
//
//   - Pinned (the primary path — every syntax-rules/syntax-case form with
//     literals compiled against a non-nil env): the pattern literal is never
//     resolved at match time. pin.Binding, captured when the macro was compiled,
//     is compared against a phase-scoped use-site resolution. An Ambiguous pin
//     refuses the literal outright, before any checker is consulted.
//   - Unpinned (a literal unbound at definition time, or no env): both sides are
//     resolved through the use-site checker, which is the pre-pin behaviour and
//     is left verbatim so the pin only ever tightens.
//
// The checker can arrive two ways, and both are read-only after construction: on
// the opts struct, for a matcher built per invocation (syntax-case, whose clause
// is matched in the environment it was compiled against), or as
// MatchWithBindingChecker's argument, for a matcher compiled once at
// macro-definition time and matched against a different use-site environment each
// expansion (syntax-rules). Match() supplies no per-call checker and falls back
// to the opts one — that fallback is how syntax-case gets its checker at all.
// With a checker in neither position the binding comparison is skipped, but an
// Ambiguous pin still refuses the literal.
type SyntaxMatcher struct {
	matcher        *Matcher
	ellipsisID     string                // Custom ellipsis identifier (default "...")
	literalSyntax  syntax.LiteralSymbols // Pattern literals with their scopes for hygiene
	literalDefs    map[string]LiteralPin // Definition-site resolution of each pattern literal; absent key = no pin
	bindingChecker BindingChecker        // Construction-time R7RS binding lookup; nil when the caller supplies one per match instead
}

// SyntaxMatcherOpts holds optional parameters for NewSyntaxMatcher.
// A nil opts pointer means all defaults (no ellipsis vars, default "...", no literals).
type SyntaxMatcherOpts struct {
	EllipsisVars   map[int]values.StringSet
	EllipsisDepths map[int]int // ellipsisID -> compilation order (lower = inner)
	EllipsisID     string
	LiteralSyntax  syntax.LiteralSymbols
	// LiteralDefs pins each pattern literal to its definition-site binding. It is
	// optional and tightening-only: an absent or zero LiteralPin leaves the
	// use-site-only comparison in place.
	LiteralDefs map[string]LiteralPin
	// BindingChecker resolves an identifier to its binding for the R7RS §4.3.2
	// literal check. Pair it with LiteralSyntax: the check needs both, and a
	// nil in either position turns it off.
	BindingChecker BindingChecker
}

// NewSyntaxMatcher creates a syntax-aware matcher that wraps the core Matcher
// with hygiene support. Pass nil opts for default behavior (default ellipsis "...",
// no literal syntax).
//
// The literalSyntax in opts enables scope-aware literal matching: if an input symbol
// has a literal's name but has been shadowed (has additional scopes), it won't match
// the pattern literal. R7RS §4.3.2 requires this for auxiliary syntax like => and else.
func NewSyntaxMatcher(
	variables values.StringSet,
	codes []SyntaxCommand,
	opts *SyntaxMatcherOpts,
) *SyntaxMatcher {
	var (
		ellipsisVars   map[int]values.StringSet
		ellipsisDepths map[int]int
		ellipsisID     = DefaultEllipsis
		literalSyntax  syntax.LiteralSymbols
		literalDefs    map[string]LiteralPin
		bindingChecker BindingChecker
	)
	if opts != nil {
		ellipsisVars = opts.EllipsisVars
		ellipsisDepths = opts.EllipsisDepths
		if opts.EllipsisID != "" {
			ellipsisID = opts.EllipsisID
		}
		literalSyntax = opts.LiteralSyntax
		literalDefs = opts.LiteralDefs
		bindingChecker = opts.BindingChecker
	}
	return &SyntaxMatcher{
		matcher: NewMatcher(variables, codes,
			WithEllipsisVars(ellipsisVars),
			WithEllipsisDepths(ellipsisDepths),
			WithEllipsisID(ellipsisID),
		),
		ellipsisID:     ellipsisID,
		literalSyntax:  literalSyntax,
		literalDefs:    literalDefs,
		bindingChecker: bindingChecker,
	}
}

// CloneForMatch returns a SyntaxMatcher that shares this matcher's immutable
// compiled pattern (bytecode, pattern variables, ellipsis metadata, literal
// syntax, literal pins, binding checker) but has independent per-invocation
// matching state — the embedded Matcher's capture and syntax stacks.
//
// A SyntaxMatcher built once at macro-definition time is stored on the (shared)
// macro binding and reused for every expansion of that macro, including
// concurrent expansions from SRFI-18 threads. The embedded Matcher's stacks are
// rewritten per call and Expand reads the capture stack the match populated, so
// concurrent expansions on one shared instance corrupt each other. Each
// expansion must therefore run on its own clone. Every other field is read-only
// after construction, so a clone is one small allocation, not a deep copy — and
// the match stacks are reallocated per match regardless, so this adds no
// per-call allocation beyond the wrapper itself.
func (p *SyntaxMatcher) CloneForMatch() *SyntaxMatcher {
	return &SyntaxMatcher{
		matcher:        p.matcher.clone(),
		ellipsisID:     p.ellipsisID,
		literalSyntax:  p.literalSyntax,
		literalDefs:    p.literalDefs,
		bindingChecker: p.bindingChecker,
	}
}

// Match performs pattern matching on syntax objects, using whatever binding
// checker was supplied at construction (SyntaxMatcherOpts.BindingChecker) —
// none, for a matcher built without one. Callers that resolve the checker per
// invocation, because the matcher outlives one use site, call
// MatchWithBindingChecker instead.
func (p *SyntaxMatcher) Match(ctx context.Context, input syntax.SyntaxValue) error {
	return p.MatchWithBindingChecker(ctx, input, nil)
}

// MatchWithBindingChecker performs pattern matching on syntax objects with
// R7RS-compliant auxiliary syntax hygiene.
//
// The checker parameter enables R7RS §4.3.2 compliant literal matching:
// literals match only if both identifiers have the same lexical binding,
// or both have no lexical binding. If the input has a binding (from let,
// lambda, etc.) but the pattern literal doesn't, they won't match.
//
// Pass nil for checker to fall back to the checker supplied at construction
// (SyntaxMatcherOpts.BindingChecker), and nil in both places for scope-based
// matching only (less strict).
//
// The checker is CLOSED OVER rather than stored on the receiver. It used to be
// assigned to p.bindingChecker and cleared by a defer, which turned a field
// that reads as configuration into per-call state on a value shared across
// expansions.
func (p *SyntaxMatcher) MatchWithBindingChecker(ctx context.Context, input syntax.SyntaxValue, checker BindingChecker) error {
	if checker == nil {
		checker = p.bindingChecker
	}

	// Ensure input is a pair
	inputPair, ok := input.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAPair, "MatchWithBindingChecker: pattern matching requires a pair")
	}

	// Create literal matcher function that uses the binding checker
	var literalMatcher LiteralMatcher
	if p.literalSyntax != nil {
		literalMatcher = func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool {
			patternLit := p.literalSyntax[patternLiteralKey]
			pin := p.literalDefs[patternLiteralKey]
			return literalScopesMatchWithDef(checker, inputSym, patternLit, pin)
		}
	}

	// Use syntax-native matching to preserve source context
	return p.matcher.MatchSyntaxWithLiterals(ctx, inputPair, p.literalSyntax, literalMatcher)
}

// CompiledPattern contains the compiled bytecode and ellipsis variable mapping.
type CompiledPattern struct {
	Codes          []SyntaxCommand
	EllipsisVars   map[int]values.StringSet
	EllipsisDepths map[int]int // ellipsisID -> compilation order (lower = inner)
	EllipsisID     string      // The ellipsis identifier used during compilation
}

// CompilePatternOpts holds optional parameters for CompileSyntaxPattern.
// A nil opts pointer means all defaults (no literals, default "...").
type CompilePatternOpts struct {
	Literals         values.StringSet
	EllipsisID       string
	MatchAllElements bool // false (default): skip first element (R7RS syntax-rules macro keyword); true: match all elements (syntax-case)
}

// CompileSyntaxPattern compiles a syntax pattern into bytecode with optional
// literals and custom ellipsis. Pass nil opts for default behavior.
//
// R7RS §4.3.2: The first subform of each pattern is the keyword of the macro
// being transformed; it is not matched against the macro use being transformed.
func CompileSyntaxPattern(
	ctx context.Context,
	pattern syntax.SyntaxValue,
	variables values.StringSet,
	opts *CompilePatternOpts,
) (*CompiledPattern, error) {
	ellipsisID := DefaultEllipsis
	var literals values.StringSet
	skipKeyword := true // R7RS §4.3.2 default: first element is macro keyword
	if opts != nil {
		if opts.EllipsisID != "" {
			ellipsisID = opts.EllipsisID
		}
		literals = opts.Literals
		if opts.MatchAllElements {
			skipKeyword = false
		}
	}

	// Pattern must be a syntax pair
	pair, ok := pattern.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "CompileSyntaxPattern: pattern must be a list")
	}

	// Compile using compiler with custom ellipsis and literals
	compiler := NewSyntaxCompilerWithEllipsis(ellipsisID)
	compiler.variables = variables
	if literals != nil {
		compiler.literals = literals
	}
	// R7RS §4.3.2: For syntax-rules, the first subform is the macro keyword.
	// For syntax-case, patterns don't have a leading keyword.
	compiler.SetSkipMacroKeyword(skipKeyword)
	err := compiler.Compile(ctx, pair)
	if err != nil {
		return nil, err
	}

	return &CompiledPattern{
		Codes:          compiler.codes,
		EllipsisVars:   compiler.ellipsisVars,
		EllipsisDepths: compiler.ellipsisDepths,
		EllipsisID:     ellipsisID,
	}, nil
}

// GetBindings returns the captured pattern variable bindings from the last match.
// Bindings are now stored as syntax.SyntaxValue directly, preserving source context.
// This is used by syntax-case to bind pattern variables in the body's environment.
func (p *SyntaxMatcher) GetBindings() map[string]syntax.SyntaxValue {
	return p.matcher.GetBindings()
}

// literalScopesMatchWithDef checks if an input symbol should match a pattern literal.
//
// Per R7RS §4.3.2, a subform in the input matches a literal identifier if and
// only if it is an identifier and either:
//   - both its occurrence in the macro expression and its occurrence in the
//     macro definition have the same lexical binding, or
//   - the two identifiers are the same and both have no lexical binding.
//
// The checker is a parameter, not a field read: see MatchWithBindingChecker.
//
// 1. Binding check (R7RS compliant). With a pin (pin.Binding non-nil) the
// definition-site binding captured at macro-compile time is compared against the
// use site's phase-scoped resolution — the only pairing that actually asks the
// spec's two questions, since a definition-site ENVIRONMENT resolved at match
// time returns the same *Binding as the use-site one (see LiteralPin). Without a
// pin, both sides are resolved through the use-site checker by pointer identity,
// which still discriminates a lexical shadow.
//
// 2. Scope check (for let-syntax): We also check rebinding scopes from
// let-syntax/letrec-syntax. If input has rebinding scopes that pattern doesn't,
// the literal has been shadowed by a macro binding.
//
// Example with regular let:
//
//	(let ((=> #f)) (cond (#t => 'ok)))
//
// cond's `=>` IS pinned (to the ambient auxiliary-syntax binding), so this takes
// the PINNED branch, and the operative reason is that the use site resolves `=>`
// to the let-bound local: a non-primitive, non-imported binding, which
// literalNotShadowed refuses. It is not the unpinned arm's "one is bound and one
// isn't" — a reader instrumenting that arm for this program will see nothing run.
//
// Example with let-syntax:
//
//	(let-syntax ((=> ...)) (cond (#t => 'ok)))
//	The input => has rebinding scope {letSyntaxScope}
//	Pattern => has no rebinding scopes, so they don't match
func literalScopesMatchWithDef(checker BindingChecker, input, pattern *syntax.SyntaxSymbol, pin LiteralPin) bool {
	if input == nil || pattern == nil {
		return false
	}

	// An ambiguous definition-site resolution is refused rather than broken by
	// order: for this consumer the conservative answer is "not the literal".
	if pin.Ambiguous {
		return false
	}

	if pin.Binding != nil && checker != nil {
		useB, ok := checker.GetLiteralBinding(input.Key(), input.Scopes())
		if !ok {
			return false
		}
		if !literalNotShadowed(pin.Binding, useB) {
			return false
		}
	} else if checker != nil {
		// Unpinned: both sides through the use-site environment. Auxiliary syntax
		// like => is ambient in every owner and arrives at a use site imported from
		// (scheme base), so both input and pattern may have bindings: compare the
		// bindings, not their existence.
		inputBinding := checker.GetBinding(input.Key(), input.Scopes())
		patternBinding := checker.GetBinding(pattern.Key(), pattern.Scopes())
		if inputBinding != patternBinding {
			// Different bindings (or one bound and one not) - don't match
			return false
		}
	}

	// Also check rebinding scopes for let-syntax shadowing.
	// This handles cases where the binding checker isn't available,
	// and provides defense-in-depth for let-syntax cases.
	inputRebindingScopes := filterRebindingScopes(input.Scopes())
	patternRebindingScopes := filterRebindingScopes(pattern.Scopes())

	// For the input to match the pattern literal, the input must not have
	// any rebinding scopes that the pattern doesn't have.
	return syntax.ScopesMatch(patternRebindingScopes, inputRebindingScopes)
}

// sameLiteralBinding decides whether a definition-site and a use-site resolution
// denote the same pattern literal.
//
// Pointer identity is the primary test, and it covers both-nil. Kind equivalence
// on BindingTypePrimitive is a deliberate widening: each library environment mints
// its OWN *Binding for every special form and auxiliary-syntax name (memory
// "library envs are primitive islands"), so a bootstrap macro's pinned ambient
// `else` and the one a library-loaded use site resolves can be different objects
// for one ambient name.
//
// Reachability, measured rather than asserted: NO test in this repo reaches the
// widening. Instrumenting the arm and running `go test ./pkg/... ./integration/...
// ./extensions/...` plus `make cover-scm` (64 Scheme files) prints it zero times —
// the two-distinct-primitives shape never arises, because a library-resolved
// ambient name arrives IMPORTED and literalNotShadowed's rider takes it first. An
// earlier revision of this comment named TestChibiTestComparator and the SRFI
// library tests as observing the widening; re-measurement refutes that, and
// ablating the widening leaves every one of those suites green.
//
// It is kept, rather than deleted on that evidence, because "no test reaches it"
// is not "no program reaches it": the per-library minting above is real, and the
// arm's failure mode is a forgone tightening, never a capture. It is bounded by
// the caller, which already requires identical spelling before any binding check
// runs (match.go, ByteCodeCompareCar), and by the rebinding-scope check that
// follows. Since no end-to-end program pins it, the rule itself is pinned as a
// truth table by TestSameLiteralBinding in syntax_adapter_literal_test.go — that
// is the red test a future change to the rule has to answer to.
func sameLiteralBinding(a, b *environment.Binding) bool {
	if a == b {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	return a.BindingType() == environment.BindingTypePrimitive &&
		b.BindingType() == environment.BindingTypePrimitive
}

// literalNotShadowed decides whether the use site's resolution of a pattern
// literal is still the literal the macro was defined against.
//
// The IsImported rider covers the one legitimate case pointer identity cannot:
// an import mints a FRESH *Binding for a re-exported ordinary name, so a library
// that exports both a macro and the variable the macro uses as a literal can
// never be pointer-equal at the use site. Any imported binding of the name is
// therefore accepted — deliberately over-accepting, since the rider cannot tell
// which library the import came from. The under-accepting alternative breaks the
// legitimate re-export, and this predicate's false positive is a forgone
// discrimination, not a capture.
//
// The over-acceptance is a SURVIVING residual of R7RS §4.3.2, not a closed case,
// and it is not hypothetical: with a library exporting only a macro over its own
// private `lit`, and an unrelated library exporting a different `lit`, importing
// both makes (mg lit) match the literal where the spec wants the fallback. That
// exact program is pinned — as today's answer, labelled residual — by the
// imported-shadow row of TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding
// (pkg/wile/matcher_pattern_gaps_test.go), so a later tightening of the rider has
// a measurement to flip rather than a silent behaviour change.
func literalNotShadowed(defB, useB *environment.Binding) bool {
	if sameLiteralBinding(defB, useB) {
		return true
	}
	return useB != nil && useB.IsImported()
}

// filterRebindingScopes returns only the scopes that are marked as rebinding scopes.
// These are scopes from let-syntax/letrec-syntax that could shadow auxiliary syntax.
func filterRebindingScopes(scopes []*syntax.Scope) []*syntax.Scope {
	var result []*syntax.Scope
	for _, s := range scopes {
		if s != nil && s.IsRebinding {
			result = append(result, s)
		}
	}
	return result
}
