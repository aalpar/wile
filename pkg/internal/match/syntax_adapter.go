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
	GetLocalScopes() syntax.Scopes
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
//
// Startup records that Binding is the startup set's own binding object
// (GlobalEnvironmentFrame.IsStartupBinding), taken with the pin because only the
// definition environment can answer it. literalNotShadowed reads it: such a
// binding carries no provenance root, yet every library's export of the name
// may be the same base definition.
type LiteralPin struct {
	Binding   *environment.Binding
	Ambiguous bool
	Startup   bool
}

// BindingChecker is an interface for checking if a symbol has a lexical binding.
// This is used for R7RS auxiliary syntax hygiene: literals like => and else
// should not match when the identifier has been locally bound.
// Implemented by machine/compilation (*envBindingChecker) to avoid circular
// imports.
type BindingChecker interface {
	// HasBinding checks if sym with the given scopes has a lexical binding.
	// Returns true if the symbol is bound (to a variable, macro, etc.).
	HasBinding(sym string, scopes syntax.Scopes) bool

	// GetBinding returns the binding for sym with the given scopes.
	// Returns nil if no binding exists. Bindings can be compared for
	// pointer equality to check if two identifiers have the same
	// binding (per R7RS §4.3.2).
	GetBinding(sym string, scopes syntax.Scopes) *environment.Binding

	// GetLiteralBinding resolves the USE-SITE side of the R7RS §4.3.2 comparison:
	// the frame's own lexical chain at its own phase, then what the LANGUAGE
	// supplies there (else, =>, and every special-form name sit at (phase 0,
	// sealed); a higher phase reaches them through the dialect's declared bulk
	// rows, not through a phase-blind coordinate), and then — only on a clean
	// miss — the phase ONE ABOVE. ok is false when resolution was ambiguous.
	//
	// THE ONE PHASE ABOVE, and nothing beyond it. This read "and no other phase"
	// until I169, on the reasoning that one phase's binding of a name must not
	// decide another phase's literal. That reasoning is right about every phase
	// except +1, where it is not another program's binding at all: a
	// define-syntax binds at NextPhase(), so an identifier naming a USER MACRO
	// is one above the use site BY CONSTRUCTION. Refusing to look there made a
	// pattern literal that names a macro match nothing and lose its clause
	// silently. The descent is still refused, and so is +2.
	//
	// The implementation is compilation.useSiteLiteralBinding over
	// compilation.lookupLiteralBinding; their docs carry the ordering argument
	// (own-phase per-symbol, own-phase bulk, THEN the climb) and must agree with
	// this one.
	GetLiteralBinding(sym string, scopes syntax.Scopes) (*environment.Binding, bool)
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
// With a checker in neither position the binding comparison is skipped
// entirely and a non-Ambiguous literal matches ANY identifier, spelling
// included — production never does this (operation_syntax_rules_transform.go
// and operation_syntax_case.go always construct a checker), so the gap is
// real but unexercised, not a documented "matches by spelling" fallback.
type SyntaxMatcher struct {
	matcher        *Matcher
	ellipsisID     string                // Custom ellipsis identifier (default "...")
	literalSyntax  syntax.LiteralSymbols // Pattern literals with their scopes for hygiene
	literalDefs    map[string]LiteralPin // Definition-site resolution of each pattern literal; absent key = no pin
	bindingChecker BindingChecker        // Construction-time R7RS binding lookup; nil when the caller supplies one per match instead
	wrapInput      bool                  // Match (input) against a pattern CompileSyntaxPattern wrapped; see CompiledPattern.WrapInput
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
	// WrapInput must be CompiledPattern.WrapInput of the pattern the codes came
	// from.
	WrapInput bool
}

// NewSyntaxMatcher creates a syntax-aware matcher that wraps the core Matcher
// with hygiene support. Pass nil opts for default behavior (default ellipsis "...",
// no literal syntax).
//
// The literalSyntax in opts enables scope-aware literal matching: if an input symbol
// has a literal's name but has been shadowed (has additional scopes), it won't match
// the pattern literal. R7RS §4.3.2 requires this for auxiliary syntax like => and else.
func NewSyntaxMatcher(
	variables syntax.PatternVarSymbols,
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
		wrapInput      bool
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
		wrapInput = opts.WrapInput
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
		wrapInput:      wrapInput,
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
		wrapInput:      p.wrapInput,
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
// (SyntaxMatcherOpts.BindingChecker). With nil in both places the binding
// comparison is skipped entirely: a non-Ambiguous literal then matches ANY
// identifier regardless of spelling, not "by spelling" as this doc used to
// claim — match.go's literal arm no longer enforces spelling upstream, so
// there is nothing left to fall back to. Both production callers
// (operation_syntax_rules_transform.go, operation_syntax_case.go) always
// construct a real checker, so this gap is unreachable today, not merely
// undesirable; a future caller that matches literals without one would need
// to add the spelling check itself.
//
// The checker is CLOSED OVER rather than stored on the receiver. It used to be
// assigned to p.bindingChecker and cleared by a defer, which turned a field
// that reads as configuration into per-call state on a value shared across
// expansions.
func (p *SyntaxMatcher) MatchWithBindingChecker(ctx context.Context, input syntax.SyntaxValue, checker BindingChecker) error {
	if checker == nil {
		checker = p.bindingChecker
	}

	if p.wrapInput {
		input = syntax.NewSyntaxCons(input, syntax.SyntaxEmptyList, input.SourceContext())
	}
	// A non-pair input is a non-match, not a malformed call. Only an unwrapped
	// matcher (syntax-rules) reaches this: syntax-case wraps every input, so its
	// shape mismatches surface from the element-level match instead.
	inputPair, ok := input.(*syntax.SyntaxPair)
	if !ok {
		return werr.WrapForeignErrorf(ErrNotAMatch, "MatchWithBindingChecker: input is not a pair")
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
	// WrapInput: the codes match (pattern), so the matcher must match (input).
	// The two wraps live in this file because a disagreement between them is a
	// silent wrong answer, not a failure.
	WrapInput bool
}

// CompilePatternOpts holds optional parameters for CompileSyntaxPattern.
// A nil opts pointer means all defaults (no literals, default "...").
type CompilePatternOpts struct {
	Literals   syntax.LiteralSymbols
	EllipsisID string
	// MatchAllElements false (default): skip the first element, the R7RS
	// syntax-rules macro keyword. True (syntax-case): match all elements, and
	// wrap the pattern one level so it may be any shape, which sets
	// CompiledPattern.WrapInput.
	MatchAllElements bool
}

// CompileSyntaxPattern compiles a syntax pattern into bytecode with optional
// literals and custom ellipsis. Pass nil opts for default behavior.
//
// R7RS §4.3.2: The first subform of each pattern is the keyword of the macro
// being transformed; it is not matched against the macro use being transformed.
func CompileSyntaxPattern(
	ctx context.Context,
	pattern syntax.SyntaxValue,
	variables syntax.PatternVarSymbols,
	opts *CompilePatternOpts,
) (*CompiledPattern, error) {
	ellipsisID := DefaultEllipsis
	var literals syntax.LiteralSymbols
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

	// A syntax-case pattern can be any R6RS §12.4 pattern (a bare variable, _,
	// (), a vector, a constant), and its input any syntax object, including ()
	// against (a ...). The compiler walks a list, so both sides are wrapped one
	// level: every pattern, not only the non-pair ones, because () is not a
	// *SyntaxPair either and must still reach an ellipsis pattern.
	wrapInput := !skipKeyword
	if wrapInput {
		// A list led by an ellipsis compiles it as a literal compared by
		// spelling: a bare ... because the wrap makes it the head of one, a
		// vector because its elements compile as one wherever it appears.
		// Before P4.4 neither shape compiled here; Chez rejects both. Only the
		// whole-clause position is rejected here.
		if leadsWithEllipsis(pattern, ellipsisID) {
			return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "CompileSyntaxPattern: misplaced ellipsis in syntax-case pattern")
		}
		pattern = syntax.NewSyntaxCons(pattern, syntax.SyntaxEmptyList, pattern.SourceContext())
	}
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
		WrapInput:      wrapInput,
	}, nil
}

// leadsWithEllipsis reports whether a whole-clause pattern is the ellipsis
// itself or a vector whose first element is the ellipsis. The same shape nested
// inside a list pattern is not caught here.
func leadsWithEllipsis(pattern syntax.SyntaxValue, ellipsisID string) bool {
	vec, ok := pattern.(*syntax.SyntaxVector)
	if ok {
		if len(vec.Values) == 0 {
			return false
		}
		pattern = vec.Values[0]
	}
	sym, ok := pattern.(*syntax.SyntaxSymbol)
	return ok && sym.Key() == ellipsisID
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
// The decision is the binding comparison alone. With a pin (pin.Binding non-nil)
// the definition-site binding captured at macro-compile time is compared against
// the use site's phase-scoped resolution — the only pairing that actually asks the
// spec's two questions, since a definition-site ENVIRONMENT resolved at match
// time returns the same *Binding as the use-site one (see LiteralPin). Without a
// pin, both sides are resolved through the use-site checker by pointer identity,
// which still discriminates a lexical shadow.
//
// No scope-set test follows it. A let-syntax keyword that shadows the literal is a
// local binding the use site resolves, so the comparison already refuses it; the
// scope let-syntax stamps on its body says nothing about which names it bound. A
// rule keyed on that scope refused every literal in every let-syntax body:
// (let-syntax ((foo ...)) (cond (#f 1) (else 2))) read `else` as a variable.
//
// Example with regular let:
//
//	(let ((=> #f)) (cond (#t => 'ok)))
//
// cond's `=>` IS pinned (to the sealed auxiliary-syntax binding), so this takes
// the PINNED branch, and the operative reason is that the use site resolves `=>`
// to the let-bound local: a non-primitive, non-imported binding, which
// literalNotShadowed refuses. It is not the unpinned arm's "one is bound and one
// isn't" — a reader instrumenting that arm for this program will see nothing run.
//
// The let-syntax shadow takes the same branch for the same reason:
//
//	(let-syntax ((=> ...)) (cond (#t => 'ok)))
//
// resolves the use-site `=>` to the let-syntax keyword, a syntax binding, neither
// the pinned primitive nor an import.
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
		if !literalNotShadowed(pin.Binding, pin.Startup, useB, input.Key() == pattern.Key()) {
			return false
		}
	} else if checker != nil {
		// Unpinned: both sides through the use-site environment. Auxiliary syntax
		// like => is part of every owner's startup set and arrives at a use site
		// imported from (scheme base), so both input and pattern may have
		// bindings: compare the bindings, not their existence.
		inputBinding := checker.GetBinding(input.Key(), input.Scopes())
		patternBinding := checker.GetBinding(pattern.Key(), pattern.Scopes())
		if inputBinding == nil && patternBinding == nil {
			// R7RS §4.3.2's unbound arm is "the two identifiers are the SAME
			// and both have no lexical binding" — with no binding on either
			// side, spelling is the only signal left, and nil == nil compares
			// none. Before match.go's literal arm could reach a differently-
			// spelled input, this was unreachable with mismatched spelling;
			// now it is, so it must be checked explicitly rather than treating
			// "both unbound" alone as a match.
			return input.Key() == pattern.Key()
		}
		if inputBinding != patternBinding {
			// Different bindings (or one bound and one not) - don't match
			return false
		}
	}

	return true
}

// sameLiteralBinding decides whether a definition-site and a use-site resolution
// denote the same pattern literal.
//
// Pointer identity is the primary test, and it covers both-nil. Kind equivalence
// on BindingTypePrimitive, narrowed to matching DenotedForm, is a deliberate
// widening: each library environment mints its OWN *Binding for every special
// form and auxiliary-syntax name (memory "library envs are primitive islands"),
// so a bootstrap macro's pinned `else` and the one a library-loaded use site
// resolves can be different objects for one name.
//
// Reachability changed with match.go's literal arm (ByteCodeCompareCar): that
// arm used to require identical spelling before any binding check ran, so two
// distinct primitives could only reach here already same-named, making the kind
// check alone sufficient. Once a differently-spelled identifier can reach this
// widening — a renamed or prefixed import of `else` or `=>` — "both primitive"
// alone would match `else` against `=>`. DenotedForm is the identity that
// survives per-library minting: every owner's `else` binding carries a value
// denoting "else", regardless of spelling, so comparing denotations rather than
// binding kind is the correct narrowing. An empty denotation (a primitive whose
// value names no form) is not an identity and never matches, even against
// itself.
//
// Pinned as a truth table by TestSameLiteralBinding in
// syntax_adapter_literal_test.go.
func sameLiteralBinding(a, b *environment.Binding) bool {
	if a == b {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	// Both sides must be the SAME keyword, not merely both keywords: a
	// differently-spelled literal now reaches this widening (match.go's literal
	// arm), and "both primitive" would match else against =>. Every owner mints
	// its own value for a keyword, so the denoted form is the identity that
	// survives per-library minting; an empty denotation is not an identity and
	// never matches.
	if a.BindingType() != environment.BindingTypePrimitive {
		return false
	}
	if b.BindingType() != environment.BindingTypePrimitive {
		return false
	}
	denoted := environment.DenotedForm(a)
	if denoted == "" {
		return false
	}
	return denoted == environment.DenotedForm(b)
}

// literalNotShadowed decides whether the use site's resolution of a pattern
// literal is still the literal the macro was defined against. sameSpelling is
// input.Key() == pattern.Key() from the caller: whether the identifier that
// actually appeared at the use site is spelled the same as the literal as
// written in the macro's literal list.
//
// The IsImported rider covers the one legitimate case pointer identity cannot:
// an import mints a FRESH *Binding for a re-exported ordinary name, so a library
// that exports both a macro and the variable the macro uses as a literal can
// never be pointer-equal at the use site. An imported binding is therefore
// not refused outright, but WHAT is required to accept it depends on what defB
// denotes, exactly as the narrowing paragraph below states: the same
// provenance root, or same spelling where no root decides, when defB denotes
// no form; the SAME denotation, spelling-independent, when defB does. Neither
// condition is universal on its own, and same spelling is never sufficient
// when defB denotes a form. The under-accepting alternative breaks the legitimate
// re-export, and this predicate's false positive is a forgone
// discrimination, not a capture.
//
// When defB denotes a form, the rider is narrowed to useB denoting the SAME
// form — spelling-independent, like sameLiteralBinding's own widening, because
// DenotedForm survives per-library minting and renaming alike. When defB denotes
// no form (defB is an ordinary variable literal, or a user macro, such as the
// private `lit` below), DenotedForm is "" on both sides and cannot
// discriminate at all, so the rider falls back to sameSpelling. Before match.go's
// literal arm could reach a differently-spelled useB, sameSpelling was always
// true here (the caller guaranteed it), so this fallback was unreachable with a
// mismatch and returning true unconditionally was equivalent. It no longer is:
// with a renamed or prefixed literal now reaching this rider, an unconditional
// true would accept ANY imported binding — of any name — as any OTHER literal
// that happens to denote no form, which is most literals (every plain-variable
// or user-macro literal). Falling back to sameSpelling instead keeps every
// documented row above (all same-spelled) while refusing that cross-name case.
//
// THE ORIGIN COMPARISON, which is what the sameSpelling fallback now yields to.
// When both sides carry an OriginRef, §4.3.2's own question is answerable
// directly — SameBinding — and spelling stops deciding. That closes the
// BOUNDARY case this paragraph used to record as a known limitation: with
// (w16lib) exporting `mg`/`lit`, (import (prefix (w16lib) p:)) (p:mg p:lit)
// answered OTHER where §4.3.2 wants MATCHED-LITERAL, because sameSpelling is
// false ("p:lit" != "lit") and DenotedForm had nothing to widen past. A prefix
// re-export is the SAME binding under another spelling, and the origins say so.
// Pinned as "BOUNDARY: a prefixed re-export of a variable literal matches".
//
// IT ALSO NARROWS, in the opposite direction, and that is the one consequence
// no row recorded before this change. Two origins that are PRESENT and DIFFER
// refuse what sameSpelling accepted: a library exporting both `mg` and `lit`,
// imported (except (w16lib) lit) alongside an unrelated library exporting its
// own `lit`, answered MATCHED-LITERAL and now answers OTHER. That is §4.3.2's
// answer too, for the case where the macro's literal is EXPORTED. Pinned as
// "NARROWING: an exported literal no longer matches another library's same
// name". Blast radius measured at zero in the stdlib: all 70 syntax-rules forms
// under pkg/stdlib/lib carry empty literal lists, so no stdlib macro reaches
// this predicate at all.
//
// THE UNROOTED DEFINITION SIDE closes the RESIDUAL case, where the literal is
// library-PRIVATE and defined before the macro. stampLibraryExportOrigins roots
// every define a library exports at itself, at finalization, before anything
// can import it; so a defB that is the library's own define and has no root was
// never exported, and no import rooted at a library can be it. With a library exporting only a macro over its own private `lit`, and an
// unrelated library exporting a different `lit`, importing both made (mg lit)
// match where §4.3.2 wants the fallback; it now answers OTHER. Pinned as
// "RESIDUAL: an unrelated library's lit is not the private literal", with
// "INSIDE: ..." as the library's own direct use, which pointer identity decides
// before this arm runs.
//
// The private define is NOT stamped to get there. OriginRef is {RootLib,
// RootName, RootPhase} with no scope discriminator, so stamping every private
// slot would make two hygienically distinct same-named bindings SameBinding-
// equal, and free-identifier=? (prim_syntax.go), ER-compare
// (er_macro_compare.go) and importConflicts are pure functions of that stamp.
// A nil root already means "its own root, equal only to itself", which is the
// answer this arm needs.
//
// TWO EXCEPTIONS keep master's spelling answer for an unrooted defB.
//
// defStartup: the pin is the startup set's own binding object
// (LiteralPin.Startup). It carries no root, yet a library's export of the name
// may be the very definition the pin stands for, so the premise above does not
// hold for it. The pin is often not the binding the literal's author meant: a
// library that defines `car` and lists it as a literal pins the startup set's
// phase-1 `car`, because resolveLiteralDefinitions probes the transformer env's
// own phase before descending to the phase-0 define, so the private define is
// never seen. Refusing here flips "P2D: an exported base-named literal matches
// its own export" to OTHER, against master and petite. The cost is "MYCAR":
// another library's `car` has the same (startup pin, library-rooted import)
// shape and still matches.
//
// A useB rooted at environment.BaseOriginLib: an unrooted defB can be the base
// definition a #%base import denotes. Refusing there flips
// TestPatternLiteralNamingAUserMacroMatches's define-syntax-over-else row,
// whose use site resolves the base import rather than the user's macro.
// "RESIDUAL-BASE" meets both exceptions, so either alone keeps it.
//
// Both make this predicate and SameBinding-based identity (free-identifier=?,
// ER-compare) knowingly disagree: SameBinding never equates an unrooted binding
// with a rooted one, while here spelling does.
//
// The useB-without-origin fallback to sameSpelling is pinned by a unit fixture
// only, TestLiteralNotShadowed/an_unrooted_import_of_the_same_name_is_accepted_by_spelling:
// no production import site makes an imported binding without an origin
// (importOrigin always returns one).
//
// THE OTHER EDGE THE SAME THREE LINES MOVED, and unlike the boundary above it is
// a BEHAVIOUR CHANGE rather than an unchanged limitation, so it is stated here
// rather than left to be rediscovered. When defB DOES denote a form, the rider
// now requires useB to denote the SAME form, which refuses an imported useB that
// denotes none — where the unconditional rider accepted it. That refusal is the
// R7RS answer: a program importing its own `else` as an ordinary variable has
// shadowed cond's auxiliary keyword, and §4.3.2 asks whether the two identifiers
// denote the same binding, which a variable and a keyword never do. It is still
// a flip, and a visible one — with a library exporting (define else 42), imported
// alongside (only (scheme base) cond quote lambda define),
//
//	(cond (#f 1) (else => (lambda (x) x)))
//
// raised `syntactic keyword "=>" used as a variable` on the pre-task base
// (90c4f2c2), because the literal reading puts => in expression position, and
// answers 42 here, taking the (test => proc) reading. Both measured. Pinned as
// "FLIP: an imported variable no longer matches an auxiliary-keyword literal" in
// TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding, next to the
// RESIDUAL/BOUNDARY pair.
func literalNotShadowed(defB *environment.Binding, defStartup bool, useB *environment.Binding, sameSpelling bool) bool {
	if sameLiteralBinding(defB, useB) {
		return true
	}
	if useB == nil || !useB.IsImported() {
		return false
	}
	denoted := environment.DenotedForm(defB)
	if denoted == "" {
		// R7RS §4.3.2 compares BINDINGS, and a useB with an OriginRef makes the
		// comparison available: a prefixed or renamed re-export is the same
		// binding under another spelling and matches, and two libraries'
		// same-named exports are different bindings and do not.
		//
		// The defB == nil test is not defensiveness: Origin() reaches
		// (*Binding).Meta(), which is NOT nil-safe, while DenotedForm and
		// sameLiteralBinding both nil-guard. The one production call site is
		// guarded, so a nil defB is unreachable there, but the unit table
		// supplies one.
		// No root to compare on one side: spelling is the only signal left,
		// and refusing would break the legitimate re-export this rider exists for.
		if defB == nil || useB.Origin() == nil {
			return sameSpelling
		}
		if defB.Origin() != nil {
			return environment.SameBinding(defB, useB)
		}
		// An unrooted library define was never exported, so an import rooted at
		// a library is some other definition. The two exceptions (see the doc)
		// keep spelling.
		if defStartup || useB.Origin().RootLib == environment.BaseOriginLib {
			return sameSpelling
		}
		return false
	}
	return denoted == environment.DenotedForm(useB)
}
