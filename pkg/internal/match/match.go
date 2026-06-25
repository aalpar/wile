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

// match.go implements the pattern matching VM for syntax-rules.
//
// This is Layer 1 of the macro system - the unhygienic core that operates
// on raw values.wrt types. It knows nothing about syntax objects or scopes;
// those are handled by the syntax adapter layer (syntax_adapter.go).
//
// The VM uses two stacks:
//   - valueStack: Tracks position in the input tree during matching
//   - captureStack: Tracks captured bindings, with nesting for ellipsis
//
// Execution Model:
//   The matcher executes compiled bytecode (from syntax_compiler.go).
//   On success, captured bindings are available for template expansion.
//   On failure (ErrNotAMatch), the caller tries the next clause.
//
// Ellipsis Capture:
//   Each ellipsis iteration creates a child captureContext. During expansion,
//   the matcher walks these children to produce repeated template elements.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"context"
	"slices"
	"sort"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// syntaxPathEntry tracks position in the input syntax tree during matching.
type syntaxPathEntry struct {
	pr syntax.SyntaxTuple
}

// DefaultEllipsis is the standard R7RS ellipsis identifier.
const DefaultEllipsis = "..."

// Matcher is the pattern matching VM for syntax-rules.
//
// It executes compiled pattern bytecode against an input form,
// capturing pattern variable bindings that can be used for template expansion.
type Matcher struct {
	variables      map[string]struct{}         // Known pattern variables
	codes          []SyntaxCommand             // Compiled pattern bytecode
	captureStack   []*captureContext           // Binding capture stack (nesting for ellipsis)
	syntaxStack    []syntaxPathEntry           // Input traversal stack (syntax-native)
	ellipsisVars   map[int]map[string]struct{} // ellipsisID -> captured pattern variables
	ellipsisDepths map[int]int                 // ellipsisID -> compilation order (lower = inner)
	ellipsisID     string                      // Custom ellipsis identifier (default "...")

	// tailCountCache and tailCountPC cache the remaining element count for
	// ByteCodeSkipIfTailCount to avoid re-walking the list on every loop
	// iteration. Without caching, ellipsis-in-middle patterns like (a ... b c)
	// have O(n²) matching cost. With caching, the count is computed once and
	// decremented per iteration, reducing total cost to O(n).
	tailCountCache int // cached remaining count; -1 = not cached
	tailCountPC    int // bytecode index where the cache was set
}

// MatcherOption configures a Matcher at construction time. Pass options to
// NewMatcher; later options override earlier ones if they touch the same field.
type MatcherOption func(*Matcher)

// WithEllipsisVars supplies the ellipsis-id → captured-variables map. When
// absent, the matcher operates with no ellipsis-bound variables.
func WithEllipsisVars(v map[int]map[string]struct{}) MatcherOption {
	return func(m *Matcher) {
		m.ellipsisVars = v
	}
}

// WithEllipsisDepths supplies the ellipsis-id → compilation-order map
// (lower = inner, higher = outer). When absent and ellipsisVars is non-empty,
// depth is inferred from the ID values: the compiler assigns IDs sequentially
// inner-first, so the ID itself is a valid depth proxy.
func WithEllipsisDepths(d map[int]int) MatcherOption {
	return func(m *Matcher) {
		m.ellipsisDepths = d
	}
}

// WithEllipsisID overrides the ellipsis identifier (default "..." per R7RS).
// R7RS §4.3.2 allows custom ellipsis identifiers; this option supplies that.
// Empty string is treated as an explicit reset to DefaultEllipsis (rather
// than a silent no-op), so the option behaves correctly even if a future
// refactor drops NewMatcher's default-initialization.
func WithEllipsisID(id string) MatcherOption {
	return func(m *Matcher) {
		if id == "" {
			m.ellipsisID = DefaultEllipsis
			return
		}
		m.ellipsisID = id
	}
}

// NewMatcher creates a pattern matcher. Required: the pattern variables set
// and the compiled bytecode. Options supply ellipsis-related metadata (see
// WithEllipsisVars, WithEllipsisDepths, WithEllipsisID).
//
// Defaults: ellipsisID = DefaultEllipsis ("..."); ellipsisVars / ellipsisDepths
// nil. When ellipsisVars is supplied but ellipsisDepths is not, depths are
// inferred from the ID values.
func NewMatcher(variables map[string]struct{}, codes []SyntaxCommand, opts ...MatcherOption) *Matcher {
	q := &Matcher{
		variables:  variables,
		codes:      codes,
		ellipsisID: DefaultEllipsis,
	}
	for _, opt := range opts {
		opt(q)
	}
	// When explicit depths are not provided, infer from ID ordering: the
	// compiler assigns IDs sequentially, so inner ellipsis always gets a
	// lower ID than outer ellipsis. Use the ID value as a depth proxy.
	// 'len == 0' (rather than '== nil') covers both nil and empty-non-nil
	// maps so a caller passing WithEllipsisDepths(emptyMap) does not silently
	// suppress inference (per CLAUDE.md's prefer-broad-predicates rule).
	if len(q.ellipsisDepths) == 0 && len(q.ellipsisVars) > 0 {
		q.ellipsisDepths = make(map[int]int, len(q.ellipsisVars))
		for id := range q.ellipsisVars {
			q.ellipsisDepths[id] = id
		}
	}
	return q
}

// handleByteCodeDone processes the ByteCodeDone instruction, which marks
// completion of a pattern level.
//
// This method:
//   - Validates that all input elements at the current level were consumed
//   - Handles improper lists and ellipsis loop contexts correctly
//   - Pops the syntax stack when a nested pattern completes
//   - Advances to the next sibling element after popping
//
// Parameters:
//   - i: Current instruction index (for checking next instruction)
//   - lvs: Current syntax stack length
//
// Returns:
//   - Updated syntax stack length
//   - Error if pattern doesn't match (ErrNotAMatch)
func (p *Matcher) handleByteCodeDone(i int, lvs int) (int, error) {
	// Before popping, check that the cdr of current pair is empty.
	// This ensures the pattern consumed all elements at this level.
	// If the position is already at the empty list singleton, all elements
	// were consumed — skip the cdr check.
	currentPr := p.syntaxStack[lvs-1].pr
	if !syntax.IsSyntaxEmptyList(currentPr) {
		cdr := currentPr.SyntaxCdr()
		if !syntax.IsSyntaxEmptyList(cdr) && cdr != nil {
			// There are more elements in the input than in the pattern.
			// Check if we're in a loop context (ellipsis) — non-empty cdr
			// is expected there.
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			if ok && !cdrPair.IsVoid() {
				// More elements exist - this is only OK in a loop context
				if i+1 >= len(p.codes) {
					return 0, ErrNotAMatch
				}
				// Check if the next instruction continues processing
				switch p.codes[i+1].(type) {
				case ByteCodeJump, ByteCodePopContext:
					// Loop context - cdr being non-empty is expected
				default:
					// Not in loop context, extra elements means no match
					return 0, ErrNotAMatch
				}
			} else if !syntax.IsSyntaxEmptyList(cdr) {
				// Improper list or other non-pair cdr when we expected end
				return 0, ErrNotAMatch
			}
		}
	}

	// Pop current level
	lvs = len(p.syntaxStack) - 1
	p.syntaxStack = p.syntaxStack[:lvs]
	if len(p.syntaxStack) == 0 {
		return lvs, nil
	}

	// Advance parent to next sibling.
	// If parent is already at the empty list (set by a previous Done),
	// there are no siblings to advance to.
	parentPr := p.syntaxStack[lvs-1].pr
	if syntax.IsSyntaxEmptyList(parentPr) {
		lvs = len(p.syntaxStack)
		return lvs, nil
	}
	cdr := parentPr.SyntaxCdr()

	// Check if there are more elements at the parent level.
	// If the next instruction is ByteCodeDone (no more pattern elements),
	// then extra siblings means the pattern doesn't match.
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(cdrPair) {
		if i+1 < len(p.codes) {
			_, ok = p.codes[i+1].(ByteCodeDone)
			if ok {
				// Pattern expects no more elements but input has more
				return 0, ErrNotAMatch
			}
		}
	}

	pr, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		// cdr is not a pair, check if it's empty
		if syntax.IsSyntaxEmptyList(cdr) || cdr == nil {
			// No more siblings - set position to empty list
			p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
		} else {
			return 0, ErrNotAMatch
		}
	} else {
		p.syntaxStack[lvs-1] = syntaxPathEntry{pr: pr}
	}
	lvs = len(p.syntaxStack)
	return lvs, nil
}

// MatchSyntax runs the pattern matcher against the syntax target.
// This is the syntax-native entry point that operates directly on SyntaxPair.
// Captured values are stored as syntax.SyntaxValue to preserve source context.
//
// Delegates to MatchSyntaxWithLiterals with nil literal arguments, which
// skips the literal hygiene check in ByteCodeCompareCar.
func (p *Matcher) MatchSyntax(ctx context.Context, target *syntax.SyntaxPair) error {
	return p.MatchSyntaxWithLiterals(ctx, target, nil, nil)
}

// LiteralMatcher is a function that checks if an input symbol matches a pattern literal.
// Returns true if the input should match, false if it's shadowed and should not match.
type LiteralMatcher func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool

// MatchSyntaxWithLiterals runs the pattern matcher with literal hygiene checking.
// The literalSyntax map contains pattern literals that need scope/binding checking.
// The literalMatcher function is called for each literal comparison to check if the
// input symbol should match (returns true) or is shadowed (returns false).
func (p *Matcher) MatchSyntaxWithLiterals(ctx context.Context, target *syntax.SyntaxPair, literalSyntax map[string]*syntax.SyntaxSymbol, literalMatcher LiteralMatcher) error {
	p.syntaxStack = []syntaxPathEntry{
		{
			pr: target,
		},
	}
	p.captureStack = []*captureContext{
		{
			children: make(map[int][]*captureContext),
			bindings: map[string]syntax.SyntaxValue{},
		},
	}
	// Reset tail count cache so the first ByteCodeSkipIfTailCount hit
	// computes a fresh count via countRemainingSyntaxElements.
	p.tailCountCache = -1
	lvs := len(p.syntaxStack)
	i := 0
	iterations := 0
	for len(p.syntaxStack) > 0 {
		iterations++
		if iterations&0x3FF == 0 {
			select {
			case <-ctx.Done():
				return ctx.Err()
			default:
			}
		}
		code := p.codes[i]
		switch cd := code.(type) {
		case ByteCodeCompareCar:
			vsv := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(vsv) {
				return ErrNotAMatch
			}
			inputCar := vsv.SyntaxCar()

			// Check for literal hygiene: if pattern is a literal symbol,
			// verify input symbol passes the literal matcher
			if literalMatcher != nil && literalSyntax != nil {
				patternSym, ok := cd.Value.(*syntax.SyntaxSymbol)
				if ok {
					symKey := patternSym.Key()
					_, isLiteral := literalSyntax[symKey]
					if isLiteral {
						inputSym, inputIsSym := inputCar.(*syntax.SyntaxSymbol)
						if !inputIsSym {
							// Input is not a symbol, can't match literal
							return ErrNotAMatch
						}
						// Check if input symbol key matches (shadowed symbols have $shadowed$ suffix)
						if inputSym.Key() != symKey {
							return ErrNotAMatch
						}
						// Check binding compatibility
						if !literalMatcher(inputSym, symKey) {
							return ErrNotAMatch
						}
					}
				}
			}

			if !syntaxValuesEqualForMatch(cd.Value, inputCar) {
				return ErrNotAMatch
			}
		case ByteCodeCompareCdr:
			// Compare the CDR with a literal value (for improper list patterns with literal tail)
			vsv := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(vsv) {
				return ErrNotAMatch
			}
			if !syntaxValuesEqualForMatch(cd.Value, vsv.SyntaxCdr()) {
				return ErrNotAMatch
			}
		case ByteCodeCaptureCar:
			lcs := len(p.captureStack)
			vsv := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(vsv) {
				return ErrNotAMatch
			}
			capturedSyntax := vsv.SyntaxCar()
			bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
			if ok && !syntaxValuesEqualForMatch(capturedSyntax, bv) {
				return ErrNotAMatch
			}
			p.captureStack[lcs-1].bindings[cd.Binding] = capturedSyntax
		case ByteCodeCaptureCdr:
			// Capture the CDR of the current pair (for improper list patterns like (_ a . rest))
			lcs := len(p.captureStack)
			vsv := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(vsv) {
				// The `. rest` tail follows a preceding ellipsis that already
				// consumed every element of a PROPER list (e.g. (a ... . rest)
				// matching (1 2 3)). The rest of an exhausted proper list is the
				// empty list, so bind `rest` to () rather than failing.
				// (CaptureCar guards the empty-input case for the leading
				// element, so reaching here with an empty position can only mean
				// an exhausted-list tail, never a missing required element.)
				captured := syntax.SyntaxEmptyList
				bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
				if ok && !syntaxValuesEqualForMatch(captured, bv) {
					return ErrNotAMatch
				}
				p.captureStack[lcs-1].bindings[cd.Binding] = captured
				break
			}
			capturedSyntax := vsv.SyntaxCdr()
			bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
			if ok && !syntaxValuesEqualForMatch(capturedSyntax, bv) {
				return ErrNotAMatch
			}
			p.captureStack[lcs-1].bindings[cd.Binding] = capturedSyntax
			// After capturing CDR, update position to indicate the entire rest is consumed.
			// Set the current pair's cdr to empty so Done doesn't think there are extra elements.
			p.syntaxStack[lvs-1].pr = syntax.NewSyntaxCons(vsv.SyntaxCar(), syntax.SyntaxEmptyList, vsv.SourceContext())
		case ByteCodeJump:
			if len(p.syntaxStack) == 0 {
				return nil
			}
			if !syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) && !p.syntaxStack[lvs-1].pr.IsVoid() {
				i += cd.Offset - 1
			}
		case ByteCodeDone:
			newLvs, err := p.handleByteCodeDone(i, lvs)
			if err != nil {
				return err
			}
			lvs = newLvs
		case ByteCodePushContext:
			lcs := len(p.captureStack)
			ellipsisID := cd.EllipsisID
			cs := &captureContext{
				children: make(map[int][]*captureContext),
				bindings: map[string]syntax.SyntaxValue{},
				parent:   p.captureStack[lcs-1],
			}
			if p.captureStack[lcs-1].children == nil {
				p.captureStack[lcs-1].children = make(map[int][]*captureContext)
			}
			p.captureStack[lcs-1].children[ellipsisID] = append(
				p.captureStack[lcs-1].children[ellipsisID], cs)
			p.captureStack = append(p.captureStack, cs)
		case ByteCodePopContext:
			lcs := len(p.captureStack)
			p.captureStack = p.captureStack[:lcs-1]
		case ByteCodeVisitCar:
			if syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) {
				return ErrNotAMatch
			}
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			pr, ok := car.(*syntax.SyntaxPair)
			if !ok {
				// The car might be the empty list singleton (e.g., () in let bindings).
				// Push it as a valid position — SkipIfEmpty will handle it in ellipsis loops.
				if syntax.IsSyntaxEmptyList(car) {
					p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: syntax.SyntaxEmptyList})
					lvs = len(p.syntaxStack)
					break
				}
				return ErrNotAMatch
			}
			p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: pr})
			lvs = len(p.syntaxStack)
		case ByteCodeVisitCdr:
			if syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) {
				return ErrNotAMatch
			}
			cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
			pr, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				// In ellipsis loops, reaching end-of-list via VisitCdr is normal —
				// SkipIfEmpty at the loop head will exit. Set position to empty list.
				if syntax.IsSyntaxEmptyList(cdr) {
					p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
				} else {
					return ErrNotAMatch
				}
			} else {
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: pr}
			}
			lvs = len(p.syntaxStack)
		case ByteCodeSkipIfEmpty:
			// Skip forward if the current position is empty or void
			// This enables while-loop semantics for ellipsis patterns
			if len(p.syntaxStack) == 0 || syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) || p.syntaxStack[lvs-1].pr.IsVoid() {
				i += cd.Offset - 1 // -1 because i++ at end of loop
			}
		case ByteCodeSkipIfTailCount:
			// Skip forward if remaining elements equals Count (for ellipsis-in-middle).
			// R7RS §4.3.2 allows patterns like (a ... b c) where ellipsis is
			// followed by more elements. The remaining count is cached to avoid
			// O(n) list walks on every iteration, reducing O(n²) to O(n) total.
			var remaining int
			if p.tailCountPC == i && p.tailCountCache >= 0 {
				// Same loop iteration — one element was consumed since last visit
				p.tailCountCache--
				remaining = p.tailCountCache
			} else {
				// First encounter or different SkipIfTailCount instruction
				remaining = countRemainingSyntaxElements(p.syntaxStack[lvs-1].pr)
				p.tailCountCache = remaining
				p.tailCountPC = i
			}
			if remaining == cd.Count {
				// Exactly enough for trailing pattern, exit loop
				i += cd.Offset - 1 // -1 because i++ at end of loop
				p.tailCountCache = -1
			} else if remaining < cd.Count {
				// Not enough elements for trailing pattern
				p.tailCountCache = -1
				return ErrNotAMatch
			}
			// remaining > Count: continue loop to match more ellipsis iterations
		case ByteCodeRequireCarEmpty:
			// Verify that the car at the current position is an empty list.
			// This is generated for patterns like () that must match empty input.
			entryPr := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(entryPr) {
				return ErrNotAMatch
			}
			car := entryPr.SyntaxCar()
			if !syntax.IsSyntaxEmptyList(car) {
				// Car is not an empty list - pattern doesn't match
				return ErrNotAMatch
			}
			// Move to next element in the list
			cdr := entryPr.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			switch {
			case ok:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: cdrPair}
			case syntax.IsSyntaxEmptyList(cdr) || cdr == nil:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
			default:
				return ErrNotAMatch
			}
		case ByteCodeRequireCarEmptyVector:
			// R7RS §4.3.2: Empty vector pattern #() — car must be an empty vector.
			entryPr := p.syntaxStack[lvs-1].pr
			if syntax.IsSyntaxEmptyList(entryPr) {
				return ErrNotAMatch
			}
			car := entryPr.SyntaxCar()
			vec, ok := car.(*syntax.SyntaxVector)
			if !ok || len(vec.Values) != 0 {
				return ErrNotAMatch
			}
			// Advance to next element
			cdr := entryPr.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			switch {
			case ok:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: cdrPair}
			case syntax.IsSyntaxEmptyList(cdr) || cdr == nil:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
			default:
				return ErrNotAMatch
			}
		case ByteCodeVisitCarAsVector:
			// R7RS §4.3.2: Vector pattern — car must be a SyntaxVector.
			// Convert its elements to a pair chain for pair-based matching.
			if syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) {
				return ErrNotAMatch
			}
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			vec, ok := car.(*syntax.SyntaxVector)
			if !ok {
				return ErrNotAMatch
			}
			chain := vectorToSyntaxPairChain(vec)
			p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: chain})
			lvs = len(p.syntaxStack)
		default:
			return ErrUnknownOpCode
		}
		i++
	}
	return nil
}

// GetBindings returns the captured pattern variable bindings from the last match.
// Bindings are stored as syntax.SyntaxValue to preserve source context.
// Returns nil if no match has been performed.
func (p *Matcher) GetBindings() map[string]syntax.SyntaxValue {
	if len(p.captureStack) == 0 {
		return nil
	}
	return p.captureStack[0].bindings
}

// findMatchingEllipsisIDs returns ALL ellipsis IDs that contribute variables to the
// template expression. In the common case where one ID covers all variables, a single-
// element slice is returned. When variables span multiple groups (cross-group zipping),
// every contributing ID is returned in sorted order.
func (p *Matcher) findMatchingEllipsisIDs(vars map[string]struct{}, excludeIDs map[int]struct{}) []int {
	if p.ellipsisVars == nil {
		return []int{0}
	}

	// Restrict selection to variables actually captured under some ellipsis ID.
	// A lower-depth ("broadcast", R7RS §4.3.2) variable belongs to no ellipsis
	// group; it is replicated into each iteration, not iterated over, so it must
	// not drive which group(s) repeat. Leaving it in would fail the single-ID
	// "all vars present" test and force a spurious cross-group zip against the
	// iterated variable's groups.
	sel := make(map[string]struct{}, len(vars))
	for v := range vars {
		for id := range p.ellipsisVars {
			_, ok := p.ellipsisVars[id][v]
			if ok {
				sel[v] = struct{}{}
				break
			}
		}
	}
	if len(sel) == 0 {
		return nil
	}
	vars = sel

	// Collect and sort IDs for deterministic order.
	ids := make([]int, 0, len(p.ellipsisVars))
	for id := range p.ellipsisVars {
		if excludeIDs != nil {
			_, excluded := excludeIDs[id]
			if excluded {
				continue
			}
		}
		ids = append(ids, id)
	}
	sort.Ints(ids)

	// Try single-ID match first (common case). When multiple IDs each contain
	// all vars, prefer the one with the highest compilation order (outermost).
	bestID := -1
	bestDepth := -1
	for _, id := range ids {
		allFound := true
		for v := range vars {
			_, ok := p.ellipsisVars[id][v]
			if !ok {
				allFound = false
				break
			}
		}
		if !allFound {
			continue
		}
		depth := p.ellipsisDepths[id]
		if depth > bestDepth {
			bestDepth = depth
			bestID = id
		}
	}
	if bestID >= 0 {
		return []int{bestID}
	}

	// Multi-group case: collect all IDs that contribute at least one variable.
	var contributing []int
	for _, id := range ids {
		for v := range vars {
			_, ok := p.ellipsisVars[id][v]
			if ok {
				contributing = append(contributing, id)
				break
			}
		}
	}
	if len(contributing) == 0 {
		return nil
	}
	return contributing
}

// countRemainingSyntaxElements counts the number of elements from the current position
// to the end of the syntax list. Used by ByteCodeSkipIfTailCount for ellipsis-in-middle.
func countRemainingSyntaxElements(pr syntax.SyntaxTuple) int {
	count := 0
	current, ok := pr.(*syntax.SyntaxPair)
	if !ok {
		return 0
	}
	for current != nil && !syntax.IsSyntaxEmptyList(current) && !current.IsVoid() {
		count++
		cdr := current.SyntaxCdr()
		next, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			// Improper list or end
			break
		}
		current = next
	}
	return count
}

// syntaxValuesEqualForMatch compares two syntax values for pattern matching purposes.
// For symbols, compares by key (value equality, not pointer equality).
// For other values, uses the underlying value comparison.
func syntaxValuesEqualForMatch(a, b syntax.SyntaxValue) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}

	// Compare symbols by key
	aSym, aIsSym := a.(*syntax.SyntaxSymbol)
	bSym, bIsSym := b.(*syntax.SyntaxSymbol)
	if aIsSym && bIsSym {
		return aSym.Key() == bSym.Key()
	}

	// Compare objects by unwrapped value
	aObj, aIsObj := a.(*syntax.SyntaxObject)
	bObj, bIsObj := b.(*syntax.SyntaxObject)
	if aIsObj && bIsObj {
		return values.EqualTo(aObj.Unwrap(), bObj.Unwrap())
	}

	// Compare empty lists
	if syntax.IsSyntaxEmptyList(a) && syntax.IsSyntaxEmptyList(b) {
		return true
	}

	// For pairs and mixed types, use deep comparison
	return values.EqualTo(a.UnwrapAll(), b.UnwrapAll())
}

// vectorToSyntaxPairChain converts a SyntaxVector's elements into a
// SyntaxPair chain for pair-based matching. Empty vectors produce
// SyntaxEmptyList.
func vectorToSyntaxPairChain(vec *syntax.SyntaxVector) syntax.SyntaxTuple {
	if len(vec.Values) == 0 {
		return syntax.SyntaxEmptyList
	}
	var chain syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := range slices.Backward(vec.Values) {
		chain = syntax.NewSyntaxCons(vec.Values[i], chain, vec.SourceContext())
	}
	return chain.(syntax.SyntaxTuple)
}
