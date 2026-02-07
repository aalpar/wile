// Copyright 2025 Aaron Alpar
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
	"sort"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// valuePathEntry tracks position in the input tree during values-based matching.
type valuePathEntry struct {
	pr *values.Pair
}

// syntaxPathEntry tracks position in the input syntax tree during matching.
type syntaxPathEntry struct {
	pr *syntax.SyntaxPair
}

// DefaultEllipsis is the standard R7RS ellipsis identifier.
const DefaultEllipsis = "..."

// Matcher is the pattern matching VM for syntax-rules.
//
// It executes compiled pattern bytecode against an input form,
// capturing pattern variable bindings that can be used for template expansion.
type Matcher struct {
	variables    map[string]struct{}         // Known pattern variables
	codes        []SyntaxCommand             // Compiled pattern bytecode
	captureStack []*captureContext           // Binding capture stack (nesting for ellipsis)
	valueStack   []valuePathEntry            // Input traversal stack (values-based)
	syntaxStack  []syntaxPathEntry           // Input traversal stack (syntax-native)
	ellipsisVars map[int]map[string]struct{} // ellipsisID -> captured pattern variables
	ellipsisID   string                      // Custom ellipsis identifier (default "...")
}

// NewMatcher creates a new pattern matcher with the default ellipsis ("...").
func NewMatcher(variables map[string]struct{}, codes []SyntaxCommand) *Matcher {
	return NewMatcherWithEllipsisVars(variables, codes, nil)
}

// NewMatcherWithEllipsisVars creates a matcher with ellipsis variable mapping.
// The ellipsisVars parameter maps each ellipsis ID to its captured pattern variables.
// Uses the default ellipsis identifier ("...").
func NewMatcherWithEllipsisVars(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}) *Matcher {
	return NewMatcherFull(variables, codes, ellipsisVars, DefaultEllipsis)
}

// NewMatcherFull creates a matcher with all parameters including custom ellipsis.
// The ellipsisID parameter specifies the identifier used for ellipsis patterns
// (default is "..." per R7RS, but can be customized per R7RS §4.3.2).
func NewMatcherFull(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}, ellipsisID string) *Matcher {
	if ellipsisID == "" {
		ellipsisID = DefaultEllipsis
	}
	q := &Matcher{
		variables:    variables,
		codes:        codes,
		ellipsisVars: ellipsisVars,
		ellipsisID:   ellipsisID,
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
	// Before popping, check that the cdr of current pair is empty
	// This ensures the pattern consumed all elements at this level
	cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
	if !syntax.IsSyntaxEmptyList(cdr) && cdr != nil {
		// There are more elements in the input than in the pattern
		// Check if we're in a loop context (ellipsis) - in that case
		// cdr being non-empty is expected
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

	lvs = len(p.syntaxStack) - 1
	p.syntaxStack = p.syntaxStack[:lvs]
	if len(p.syntaxStack) == 0 {
		return lvs, nil
	}
	cdr = p.syntaxStack[lvs-1].pr.SyntaxCdr()

	// Check if there are more elements at the parent level
	// After popping, if cdr is not empty, there are more siblings to match
	// If the next instruction is ByteCodeDone (no more pattern elements),
	// then extra siblings means the pattern doesn't match
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
func (p *Matcher) MatchSyntax(ctx context.Context, target *syntax.SyntaxPair) error {
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
			if !syntaxValuesEqualForMatch(cd.Value, vsv.SyntaxCar()) {
				return ErrNotAMatch
			}
		case ByteCodeCompareCdr:
			// Compare the CDR with a literal value (for improper list patterns with literal tail)
			vsv := p.syntaxStack[lvs-1].pr
			if !syntaxValuesEqualForMatch(cd.Value, vsv.SyntaxCdr()) {
				return ErrNotAMatch
			}
		case ByteCodeCaptureCar:
			lcs := len(p.captureStack)
			vsv := p.syntaxStack[lvs-1].pr
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
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			pr, ok := car.(*syntax.SyntaxPair)
			if !ok {
				return ErrNotAMatch
			}
			p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: pr})
			lvs = len(p.syntaxStack)
		case ByteCodeVisitCdr:
			cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
			pr, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				return ErrNotAMatch
			}
			p.syntaxStack[lvs-1] = syntaxPathEntry{pr: pr}
			lvs = len(p.syntaxStack)
		case ByteCodeSkipIfEmpty:
			// Skip forward if the current position is empty or void
			// This enables while-loop semantics for ellipsis patterns
			if len(p.syntaxStack) == 0 || syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) || p.syntaxStack[lvs-1].pr.IsVoid() {
				i += cd.Offset - 1 // -1 because i++ at end of loop
			}
		case ByteCodeSkipIfTailCount:
			// Skip forward if remaining elements equals Count (for ellipsis-in-middle)
			// R7RS §4.3.2 allows patterns like (a ... b c) where ellipsis is followed by more elements
			remaining := countRemainingSyntaxElements(p.syntaxStack[lvs-1].pr)
			if remaining == cd.Count {
				// Exactly enough for trailing pattern, exit loop
				i += cd.Offset - 1 // -1 because i++ at end of loop
			} else if remaining < cd.Count {
				// Not enough elements for trailing pattern
				return ErrNotAMatch
			}
			// remaining > Count: continue loop to match more ellipsis iterations
		case ByteCodeRequireCarEmpty:
			// Verify that the car at the current position is an empty list
			// This is generated for patterns like () that must match empty input
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			carPair, ok := car.(*syntax.SyntaxPair)
			if !ok || !syntax.IsSyntaxEmptyList(carPair) {
				// Car is not an empty list - pattern doesn't match
				return ErrNotAMatch
			}
			// Move to next element in the list
			cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			switch {
			case ok:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: cdrPair}
			case syntax.IsSyntaxEmptyList(cdr) || cdr == nil:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
			default:
				return ErrNotAMatch
			}
		default:
			return ErrUnknownOpCode
		}
		i++
	}
	return nil
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
			inputCar := vsv.SyntaxCar()

			// Check for literal hygiene: if pattern is a literal symbol,
			// verify input symbol passes the literal matcher
			if literalMatcher != nil && literalSyntax != nil {
				if patternSym, ok := cd.Value.(*syntax.SyntaxSymbol); ok {
					symKey := patternSym.Sym.Key
					if _, isLiteral := literalSyntax[symKey]; isLiteral {
						inputSym, inputIsSym := inputCar.(*syntax.SyntaxSymbol)
						if !inputIsSym {
							// Input is not a symbol, can't match literal
							return ErrNotAMatch
						}
						// Check if input symbol key matches (shadowed symbols have $shadowed$ suffix)
						if inputSym.Sym.Key != symKey {
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
			if !syntaxValuesEqualForMatch(cd.Value, vsv.SyntaxCdr()) {
				return ErrNotAMatch
			}
		case ByteCodeCaptureCar:
			lcs := len(p.captureStack)
			vsv := p.syntaxStack[lvs-1].pr
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
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			pr, ok := car.(*syntax.SyntaxPair)
			if !ok {
				return ErrNotAMatch
			}
			p.syntaxStack = append(p.syntaxStack, syntaxPathEntry{pr: pr})
			lvs = len(p.syntaxStack)
		case ByteCodeVisitCdr:
			cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
			pr, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				return ErrNotAMatch
			}
			p.syntaxStack[lvs-1] = syntaxPathEntry{pr: pr}
			lvs = len(p.syntaxStack)
		case ByteCodeSkipIfEmpty:
			// Skip forward if the current position is empty or void
			// This enables while-loop semantics for ellipsis patterns
			if len(p.syntaxStack) == 0 || syntax.IsSyntaxEmptyList(p.syntaxStack[lvs-1].pr) || p.syntaxStack[lvs-1].pr.IsVoid() {
				i += cd.Offset - 1 // -1 because i++ at end of loop
			}
		case ByteCodeSkipIfTailCount:
			// Skip forward if remaining elements equals Count (for ellipsis-in-middle)
			// R7RS §4.3.2 allows patterns like (a ... b c) where ellipsis is followed by more elements
			remaining := countRemainingSyntaxElements(p.syntaxStack[lvs-1].pr)
			if remaining == cd.Count {
				// Exactly enough for trailing pattern, exit loop
				i += cd.Offset - 1 // -1 because i++ at end of loop
			} else if remaining < cd.Count {
				// Not enough elements for trailing pattern
				return ErrNotAMatch
			}
			// remaining > Count: continue loop to match more ellipsis iterations
		case ByteCodeRequireCarEmpty:
			// Verify that the car at the current position is an empty list
			// This is generated for patterns like () that must match empty input
			car := p.syntaxStack[lvs-1].pr.SyntaxCar()
			carPair, ok := car.(*syntax.SyntaxPair)
			if !ok || !syntax.IsSyntaxEmptyList(carPair) {
				// Car is not an empty list - pattern doesn't match
				return ErrNotAMatch
			}
			// Move to next element in the list
			cdr := p.syntaxStack[lvs-1].pr.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			switch {
			case ok:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: cdrPair}
			case syntax.IsSyntaxEmptyList(cdr) || cdr == nil:
				p.syntaxStack[lvs-1] = syntaxPathEntry{pr: syntax.SyntaxEmptyList}
			default:
				return ErrNotAMatch
			}
		default:
			return ErrUnknownOpCode
		}
		i++
	}
	return nil
}

// Match runs the pattern matcher against the target using values-based matching.
// Captured values are wrapped in syntax objects for uniform bindings storage.
func (p *Matcher) Match(ctx context.Context, target *values.Pair) error {
	p.valueStack = []valuePathEntry{
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
	lvs := len(p.valueStack)
	i := 0
	iterations := 0
	for len(p.valueStack) > 0 {
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
			vsv := p.valueStack[lvs-1].pr
			// Compare using underlying values - extract from syntax wrapper if needed
			patternVal := cd.Value.UnwrapAll()
			inputVal := vsv.Car()
			if !values.EqualTo(patternVal, inputVal) {
				return ErrNotAMatch
			}
		case ByteCodeCompareCdr:
			// Compare the CDR with a literal value (for improper list patterns with literal tail)
			vsv := p.valueStack[lvs-1].pr
			patternVal := cd.Value.UnwrapAll()
			inputVal := vsv.Cdr()
			if !values.EqualTo(patternVal, inputVal) {
				return ErrNotAMatch
			}
		case ByteCodeCaptureCar:
			lcs := len(p.captureStack)
			vsv := p.valueStack[lvs-1].pr
			capturedSyntax := valueToSyntaxValue(vsv.Car())
			bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
			if ok && !syntaxValuesEqualForMatch(capturedSyntax, bv) {
				return ErrNotAMatch
			}
			p.captureStack[lcs-1].bindings[cd.Binding] = capturedSyntax
		case ByteCodeCaptureCdr:
			// Capture the CDR of the current pair (for improper list patterns like (_ a . rest))
			lcs := len(p.captureStack)
			vsv := p.valueStack[lvs-1].pr
			capturedSyntax := valueToSyntaxValue(vsv.Cdr())
			bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
			if ok && !syntaxValuesEqualForMatch(capturedSyntax, bv) {
				return ErrNotAMatch
			}
			p.captureStack[lcs-1].bindings[cd.Binding] = capturedSyntax
			// After capturing CDR, update position to indicate the entire rest is consumed.
			// Set the current pair's cdr to empty so Done doesn't see "extra" elements.
			p.valueStack[lvs-1].pr = values.NewCons(vsv.Car(), values.EmptyList)
		case ByteCodeJump:
			if len(p.valueStack) == 0 {
				return nil
			}
			if p.valueStack[lvs-1].pr != nil && !values.IsVoid(p.valueStack[lvs-1].pr) {
				i += cd.Offset - 1
			}
		case ByteCodeDone:
			// Before popping, check that the cdr of current pair is empty
			// This ensures the pattern consumed all elements at this level
			if p.valueStack[lvs-1].pr != nil {
				cdr := p.valueStack[lvs-1].pr.Cdr()
				if !values.IsEmptyList(cdr) && cdr != nil {
					// There are more elements in the input than in the pattern
					// Check if we're in a loop context (ellipsis) - in that case
					// cdr being non-empty is expected
					cdrPair, ok := cdr.(*values.Pair)
					if ok && !values.IsVoid(cdrPair) {
						// More elements exist - this is only OK in a loop context
						if i+1 >= len(p.codes) {
							return ErrNotAMatch
						}
						// Check if the next instruction continues processing
						switch p.codes[i+1].(type) {
						case ByteCodeJump, ByteCodePopContext:
							// Loop context - cdr being non-empty is expected
						default:
							// Not in loop context, extra elements means no match
							return ErrNotAMatch
						}
					} else if !values.IsEmptyList(cdr) {
						// Improper list or other non-pair cdr when we expected end
						return ErrNotAMatch
					}
				}
			}

			// Pop current level
			lvs = len(p.valueStack) - 1
			p.valueStack = p.valueStack[:lvs]
			if len(p.valueStack) == 0 {
				return nil
			}

			// Advance parent level past the element we just finished processing
			if p.valueStack[lvs-1].pr == nil {
				break
			}
			cdr := p.valueStack[lvs-1].pr.Cdr()

			// Check if there are more elements at the parent level
			// After popping, if cdr is not empty, there are more siblings to match
			// If the next instruction is ByteCodeDone (no more pattern elements),
			// then extra siblings means the pattern doesn't match
			pr, ok := cdr.(*values.Pair)
			if ok {
				if i+1 < len(p.codes) {
					if _, isDone := p.codes[i+1].(ByteCodeDone); isDone {
						// Pattern expects no more elements but input has more
						return ErrNotAMatch
					}
				}
			}
			if !ok {
				// nil cdr: malformed pair; EmptyList: proper list terminator
				// Store nil to signal end-of-list (pr field is *Pair)
				if values.IsEmptyList(cdr) || cdr == nil {
					p.valueStack[lvs-1] = valuePathEntry{pr: nil}
				} else {
					return ErrNotAMatch
				}
			} else {
				p.valueStack[lvs-1] = valuePathEntry{pr: pr}
			}
			lvs = len(p.valueStack)
		case ByteCodePushContext:
			lcs := len(p.captureStack)
			ellipsisID := cd.EllipsisID
			cs := &captureContext{
				children: make(map[int][]*captureContext),
				bindings: map[string]syntax.SyntaxValue{},
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
			car := p.valueStack[lvs-1].pr.Car()
			pr, ok := car.(*values.Pair)
			if !ok {
				return ErrNotAMatch
			}
			p.valueStack = append(p.valueStack, valuePathEntry{pr: pr})
			lvs = len(p.valueStack)
		case ByteCodeVisitCdr:
			cdr := p.valueStack[lvs-1].pr.Cdr()
			pr, ok := cdr.(*values.Pair)
			if !ok {
				if values.IsEmptyList(cdr) || cdr == nil {
					p.valueStack[lvs-1] = valuePathEntry{pr: nil}
				} else {
					return ErrNotAMatch
				}
			} else {
				p.valueStack[lvs-1] = valuePathEntry{pr: pr}
			}
			lvs = len(p.valueStack)
		case ByteCodeSkipIfEmpty:
			// Skip forward if the current position is empty or void
			// This enables while-loop semantics for ellipsis patterns
			// pr is nil when at the end of a list (EmptyList cdr was encountered)
			if len(p.valueStack) == 0 || p.valueStack[lvs-1].pr == nil || values.IsVoid(p.valueStack[lvs-1].pr) {
				i += cd.Offset - 1 // -1 because i++ at end of loop
			}
		case ByteCodeSkipIfTailCount:
			// Skip forward if remaining elements equals Count (for ellipsis-in-middle)
			// R7RS §4.3.2 allows patterns like (a ... b c) where ellipsis is followed by more elements
			remaining := countRemainingElements(p.valueStack[lvs-1].pr)
			if remaining == cd.Count {
				// Exactly enough for trailing pattern, exit loop
				i += cd.Offset - 1 // -1 because i++ at end of loop
			} else if remaining < cd.Count {
				// Not enough elements for trailing pattern
				return ErrNotAMatch
			}
			// remaining > Count: continue loop to match more ellipsis iterations
		case ByteCodeRequireCarEmpty:
			// Verify that the car at the current position is an empty list
			// This is generated for patterns like () that must match empty input
			car := p.valueStack[lvs-1].pr.Car()
			if !values.IsEmptyList(car) {
				// Car is not an empty list - pattern doesn't match
				return ErrNotAMatch
			}
			// Move to next element in the list
			cdr := p.valueStack[lvs-1].pr.Cdr()
			cdrPair, ok := cdr.(*values.Pair)
			switch {
			case ok:
				p.valueStack[lvs-1] = valuePathEntry{pr: cdrPair}
			case values.IsEmptyList(cdr) || cdr == nil: // nil cdr: malformed pair; EmptyList: proper list terminator
				// Store nil to signal end-of-list (pr field is *Pair)
				p.valueStack[lvs-1] = valuePathEntry{pr: nil}
			default:
				return ErrNotAMatch
			}
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

// Expand substitutes pattern variables in template with captured values.
// This is the legacy API that returns unwrapped values.Value.
func (p *Matcher) Expand(template values.Value) (values.Value, error) {
	if len(p.captureStack) == 0 {
		return nil, values.WrapForeignErrorf(values.ErrNoCaptureContext, "Expand: no captures available for template expansion")
	}
	result, err := p.expandValue(template, p.captureStack[0], nil)
	if err != nil {
		return nil, err
	}
	// Unwrap any syntax values in the result for backward compatibility
	return unwrapSyntaxValues(result), nil
}

// ExpandPreservingSyntax substitutes pattern variables in template with captured values,
// but preserves syntax values in the result. This allows callers to maintain syntax context
// for captured pattern variables.
func (p *Matcher) ExpandPreservingSyntax(template values.Value) (values.Value, error) {
	if len(p.captureStack) == 0 {
		return nil, values.WrapForeignErrorf(values.ErrNoCaptureContext, "ExpandPreservingSyntax: no captures available for template expansion")
	}
	return p.expandValue(template, p.captureStack[0], nil)
}

// unwrapSyntaxValues recursively unwraps syntax values in a result.
// This is used by Expand to maintain backward compatibility.
func unwrapSyntaxValues(v values.Value) values.Value {
	if v == nil {
		return nil
	}

	// Unwrap syntax values
	if sv, ok := v.(syntax.SyntaxValue); ok {
		return sv.UnwrapAll()
	}

	// Recursively unwrap pairs
	if pr, ok := v.(*values.Pair); ok {
		car := unwrapSyntaxValues(pr.Car())
		cdr := unwrapSyntaxValues(pr.Cdr())
		return values.NewCons(car, cdr)
	}

	return v
}

// expandValue recursively expands a template value with captured bindings.
func (p *Matcher) expandValue(template values.Value, ctx *captureContext, ellipsisVars map[string]struct{}) (values.Value, error) {
	switch t := template.(type) {
	case *values.Symbol:
		// Check if it's a pattern variable
		val, ok := ctx.bindings[t.Key]
		if ok {
			return val, nil
		}
		// Check if it's an ellipsis variable (from outer repetition)
		if ellipsisVars != nil {
			_, ok := ellipsisVars[t.Key]
			if ok {
				// This variable should be expanded in the context of ellipsis
				return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "ellipsis variable %s used outside of ellipsis context", t.Key)
			}
		}
		// Not a pattern variable, return as-is
		return t, nil

	case *values.Pair:
		// Check for ellipsis escape form: (<ellipsis> <template>)
		// R7RS §4.3.2: A template of the form (<ellipsis> <template>) is identical
		// to <template>, except that ellipses within the template have no special meaning.
		sym, ok := t.Car().(*values.Symbol)
		if ok && sym.Key == p.ellipsisID {
			cdr := t.Cdr()
			cdrPair, ok := cdr.(*values.Pair)
			if ok {
				// This is an escape form - return the inner template literally
				// by expanding it with a matcher that uses "" as ellipsis (no ellipsis matching)
				return p.expandEscapedTemplate(cdrPair.Car(), ctx, ellipsisVars)
			}
		}

		// Check for ellipsis pattern (something <ellipsis>)
		{
			cdr := t.Cdr()
			cdrPair, ok := cdr.(*values.Pair)
			if ok {
				sym, ok := cdrPair.Car().(*values.Symbol)
				if ok && sym.Key == p.ellipsisID {
					// Found ellipsis - need to repeat the car for each capture
					return p.expandEllipsis(t.Car(), cdrPair.Cdr(), ctx, ellipsisVars)
				}
			}
		}

		// Regular pair - expand car and cdr
		car, err := p.expandValue(t.Car(), ctx, ellipsisVars)
		if err != nil {
			return nil, err
		}
		cdr, err := p.expandValue(t.Cdr(), ctx, ellipsisVars)
		if err != nil {
			return nil, err
		}
		return values.NewCons(car, cdr), nil

	default:
		// Self-evaluating values (numbers, strings, etc.)
		return t, nil
	}
}

// expandEllipsis handles template repetition with ...
func (p *Matcher) expandEllipsis(pattern values.Value, rest values.Value, ctx *captureContext, ellipsisVars map[string]struct{}) (values.Value, error) {
	// Find which variables in the pattern are bound in child contexts
	patternVars := p.findPatternVariables(pattern)

	// Find the ellipsis ID that captured these variables
	ellipsisID := p.findMatchingEllipsisID(patternVars)
	if ellipsisID < 0 {
		// No matching ellipsis - just expand the rest
		return p.expandValue(rest, ctx, ellipsisVars)
	}

	// Get children for this specific ellipsis ID
	children := ctx.children[ellipsisID]
	if len(children) == 0 {
		// No repetitions captured, just expand the rest
		return p.expandValue(rest, ctx, ellipsisVars)
	}

	// Build result by repeating pattern for each child context
	results := []values.Value{}
	for _, childCtx := range children {
		// Create a new ellipsis variable set for this expansion
		newEllipsisVars := make(map[string]struct{})
		for k, v := range ellipsisVars {
			newEllipsisVars[k] = v
		}
		for v := range patternVars {
			newEllipsisVars[v] = struct{}{}
		}

		expanded, err := p.expandValue(pattern, childCtx, newEllipsisVars)
		if err != nil {
			return nil, err
		}
		results = append(results, expanded)
	}

	// Combine results into a list and append the rest
	result := values.EmptyList
	for i := len(results) - 1; i >= 0; i-- {
		result = values.NewCons(results[i], result)
	}

	// Expand and append the rest
	expandedRest, err := p.expandValue(rest, ctx, ellipsisVars)
	if err != nil {
		return nil, err
	}

	// Append expanded rest to result
	return p.appendToList(result, expandedRest), nil
}

// findMatchingEllipsisID finds the ellipsis ID that captured the given pattern variables.
// Returns -1 if no matching ellipsis ID is found.
// When multiple variables are requested, finds the ID that contains ALL of them.
// This is important for nested ellipsis patterns like ((var init step ...) ...)
// where step appears in both the inner (step only) and outer (var, init, step) IDs.
func (p *Matcher) findMatchingEllipsisID(vars map[string]struct{}) int {
	if p.ellipsisVars == nil {
		// Legacy mode: no ellipsis IDs, use ID 0
		return 0
	}

	// Collect and sort IDs for deterministic iteration order.
	// Go map iteration order is non-deterministic, which could cause
	// different ellipsis IDs to be returned on different runs when
	// multiple IDs match. This led to intermittent hangs in macro expansion.
	ids := make([]int, 0, len(p.ellipsisVars))
	for id := range p.ellipsisVars {
		ids = append(ids, id)
	}
	sort.Ints(ids)

	// Find the ID that contains ALL the requested variables
	for _, id := range ids {
		ellipsisVars := p.ellipsisVars[id]
		allFound := true
		for v := range vars {
			if _, ok := ellipsisVars[v]; !ok {
				allFound = false
				break
			}
		}
		if allFound {
			return id
		}
	}

	// Fallback: find any ID that contains at least one variable
	for _, id := range ids {
		ellipsisVars := p.ellipsisVars[id]
		for v := range vars {
			if _, ok := ellipsisVars[v]; ok {
				return id
			}
		}
	}
	return -1
}

// findPatternVariables finds all pattern variables in a template
func (p *Matcher) findPatternVariables(template values.Value) map[string]struct{} {
	vars := make(map[string]struct{})
	p.findVarsRecursive(template, vars)
	return vars
}

func (p *Matcher) findVarsRecursive(template values.Value, vars map[string]struct{}) {
	switch t := template.(type) {
	case *values.Symbol:
		if _, ok := p.variables[t.Key]; ok {
			vars[t.Key] = struct{}{}
		}
	case *values.Pair:
		p.findVarsRecursive(t.Car(), vars)
		p.findVarsRecursive(t.Cdr(), vars)
	}
}

// appendToList appends two values treating them as lists
func (p *Matcher) appendToList(list values.Value, rest values.Value) values.Value {
	if values.IsEmptyList(list) {
		return rest
	}
	pair := list.(*values.Pair)
	return values.NewCons(pair.Car(), p.appendToList(pair.Cdr(), rest))
}

// expandEscapedTemplate expands a template inside an ellipsis escape form.
// R7RS §4.3.2: A template of the form (<ellipsis> <template>) is identical to
// <template>, except that ellipses within the template have no special meaning.
// This function expands the template literally - substituting pattern variables
// but not treating ellipsis as a repetition marker.
func (p *Matcher) expandEscapedTemplate(template values.Value, ctx *captureContext, ellipsisVars map[string]struct{}) (values.Value, error) {
	switch t := template.(type) {
	case *values.Symbol:
		// Check if it's a pattern variable
		if val, ok := ctx.bindings[t.Key]; ok {
			return val, nil
		}
		// Check if it's an ellipsis variable (from outer repetition)
		if ellipsisVars != nil {
			if _, ok := ellipsisVars[t.Key]; ok {
				return nil, values.WrapForeignErrorf(values.ErrInvalidSyntax, "ellipsis variable %s used outside of ellipsis context", t.Key)
			}
		}
		// Not a pattern variable, return as-is (including ellipsis symbols)
		return t, nil

	case *values.Pair:
		// In escaped context, don't check for ellipsis patterns - just expand car and cdr
		car, err := p.expandEscapedTemplate(t.Car(), ctx, ellipsisVars)
		if err != nil {
			return nil, err
		}
		cdr, err := p.expandEscapedTemplate(t.Cdr(), ctx, ellipsisVars)
		if err != nil {
			return nil, err
		}
		return values.NewCons(car, cdr), nil

	default:
		// Self-evaluating values (numbers, strings, etc.)
		return t, nil
	}
}

// countRemainingElements counts the number of elements from the current position
// to the end of the list. Used by ByteCodeSkipIfTailCount for ellipsis-in-middle.
func countRemainingElements(pr *values.Pair) int {
	count := 0
	current := pr
	for current != nil && !values.IsVoid(current) {
		count++
		cdr := current.Cdr()
		next, ok := cdr.(*values.Pair)
		if !ok {
			// Improper list or end
			break
		}
		current = next
	}
	return count
}

// countRemainingSyntaxElements counts the number of elements from the current position
// to the end of the syntax list. Used by ByteCodeSkipIfTailCount for ellipsis-in-middle.
func countRemainingSyntaxElements(pr *syntax.SyntaxPair) int {
	count := 0
	current := pr
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
		return aSym.Sym.Key == bSym.Sym.Key
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

// valuePairToSyntaxPair converts a values.Pair to syntax.SyntaxPair recursively.
// This is used for legacy compatibility with code that passes raw pairs.
func valuePairToSyntaxPair(pr *values.Pair) *syntax.SyntaxPair {
	if pr == nil {
		return nil
	}
	if values.IsVoid(pr) {
		return nil
	}

	car := valueToSyntaxValue(pr.Car())
	cdr := valueToSyntaxValue(pr.Cdr())
	return syntax.NewSyntaxCons(car, cdr, nil)
}

// valueToSyntaxValue converts a values.Value to syntax.SyntaxValue.
func valueToSyntaxValue(v values.Value) syntax.SyntaxValue {
	if v == nil {
		return nil
	}

	// Already a syntax value
	if sv, ok := v.(syntax.SyntaxValue); ok {
		return sv
	}

	switch t := v.(type) {
	case *values.Pair:
		return valuePairToSyntaxPair(t)
	case *values.Symbol:
		return syntax.NewSyntaxSymbolForSymbol(t, nil)
	default:
		return syntax.NewSyntaxObject(v, nil)
	}
}
