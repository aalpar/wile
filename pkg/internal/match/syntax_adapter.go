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
// R7RS Binding Check: For full R7RS compliance (§4.3.2), we also compare the
// input identifier's binding with the pattern literal's. The checker can arrive
// two ways, and both are read-only after construction: on the opts struct, for
// a matcher built per invocation (syntax-case, whose clause is matched in the
// environment it was compiled against), or as MatchWithBindingChecker's
// argument, for a matcher compiled once at macro-definition time and matched
// against a different use-site environment each expansion (syntax-rules).
// Match() supplies neither and so skips the binding check.
type SyntaxMatcher struct {
	matcher        *Matcher
	ellipsisID     string                          // Custom ellipsis identifier (default "...")
	literalSyntax  map[string]*syntax.SyntaxSymbol // Pattern literals with their scopes for hygiene
	bindingChecker BindingChecker                  // Construction-time R7RS binding lookup; nil when supplied per match
}

// SyntaxMatcherOpts holds optional parameters for NewSyntaxMatcher.
// A nil opts pointer means all defaults (no ellipsis vars, default "...", no literals).
type SyntaxMatcherOpts struct {
	EllipsisVars   map[int]map[string]struct{}
	EllipsisDepths map[int]int // ellipsisID -> compilation order (lower = inner)
	EllipsisID     string
	LiteralSyntax  map[string]*syntax.SyntaxSymbol
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
	variables map[string]struct{},
	codes []SyntaxCommand,
	opts *SyntaxMatcherOpts,
) *SyntaxMatcher {
	var (
		ellipsisVars   map[int]map[string]struct{}
		ellipsisDepths map[int]int
		ellipsisID     = DefaultEllipsis
		literalSyntax  map[string]*syntax.SyntaxSymbol
		bindingChecker BindingChecker
	)
	if opts != nil {
		ellipsisVars = opts.EllipsisVars
		ellipsisDepths = opts.EllipsisDepths
		if opts.EllipsisID != "" {
			ellipsisID = opts.EllipsisID
		}
		literalSyntax = opts.LiteralSyntax
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
		bindingChecker: bindingChecker,
	}
}

// CloneForMatch returns a SyntaxMatcher that shares this matcher's immutable
// compiled pattern (bytecode, pattern variables, ellipsis metadata, literal
// syntax, binding checker) but has independent per-invocation matching state —
// the embedded Matcher's capture and syntax stacks.
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
// assigned to p.bindingChecker and cleared by a defer, which made a field that
// reads as configuration into per-call state — the only reason CloneForMatch
// had to exist for a single-threaded caller.
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
			return literalScopesMatchWithChecker(checker, inputSym, patternLit)
		}
	}

	// Use syntax-native matching to preserve source context
	return p.matcher.MatchSyntaxWithLiterals(ctx, inputPair, p.literalSyntax, literalMatcher)
}

// CompiledPattern contains the compiled bytecode and ellipsis variable mapping.
type CompiledPattern struct {
	Codes          []SyntaxCommand
	EllipsisVars   map[int]map[string]struct{}
	EllipsisDepths map[int]int // ellipsisID -> compilation order (lower = inner)
	EllipsisID     string      // The ellipsis identifier used during compilation
}

// CompilePatternOpts holds optional parameters for CompileSyntaxPattern.
// A nil opts pointer means all defaults (no literals, default "...").
type CompilePatternOpts struct {
	Literals         map[string]struct{}
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
	variables map[string]struct{},
	opts *CompilePatternOpts,
) (*CompiledPattern, error) {
	ellipsisID := DefaultEllipsis
	var literals map[string]struct{}
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

// literalScopesMatchWithChecker checks if an input symbol should match a pattern literal.
//
// Per R7RS §4.3.2, a subform in the input matches a literal identifier if and
// only if it is an identifier and either:
//   - both its occurrence in the macro expression and its occurrence in the
//     macro definition have the same lexical binding, or
//   - the two identifiers are the same and both have no lexical binding.
//
// This function returns true if the input symbol refers to the same binding
// as the pattern literal. It performs two checks:
//
// The checker is a parameter, not a field read: see MatchWithBindingChecker.
//
// 1. Binding check (R7RS compliant): If a binding checker is available, compare
// the input's binding with the pattern literal's binding by pointer identity.
// They match only when both resolve to the same binding, or both are unbound.
// This is an identity comparison, not a boundness test: after library import,
// auxiliary syntax like => is bound on both sides.
//
// 2. Scope check (for let-syntax): We also check rebinding scopes from
// let-syntax/letrec-syntax. If input has rebinding scopes that pattern doesn't,
// the literal has been shadowed by a macro binding.
//
// Example with regular let:
//
//	(let ((=> #f)) (cond (#t => 'ok)))
//	The input => has a lexical binding (the let-bound variable)
//	Pattern => has no lexical binding
//	They don't match because one is bound and one isn't
//
// Example with let-syntax:
//
//	(let-syntax ((=> ...)) (cond (#t => 'ok)))
//	The input => has rebinding scope {letSyntaxScope}
//	Pattern => has no rebinding scopes, so they don't match
func literalScopesMatchWithChecker(checker BindingChecker, input, pattern *syntax.SyntaxSymbol) bool {
	if input == nil || pattern == nil {
		return false
	}

	// R7RS §4.3.2 binding check: literals match if both have the same lexical
	// binding, or both have no lexical binding. After library import, auxiliary
	// syntax like => gets exported to phase 0, so both input and pattern may
	// have bindings. We compare the actual bindings, not just whether they exist.
	if checker != nil {
		inputBinding := checker.GetBinding(input.Key(), input.Scopes())
		patternBinding := checker.GetBinding(pattern.Key(), pattern.Scopes())

		// R7RS §4.3.2: literals match if both have the same binding, or both unbound
		if inputBinding != patternBinding {
			// Different bindings (or one bound and one not) - don't match
			return false
		}
		// Same binding (including both nil) - continue to scope check below
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
