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

// syntax_compiler.go compiles R7RS syntax-rules patterns into bytecode.
//
// The pattern matching VM uses a stack-based bytecode interpreter for
// flexibility and performance. Patterns are compiled once at macro definition
// time, then executed efficiently at each macro invocation.
//
// Bytecode Operations:
//   - CompareCar: Compare current car with a literal value
//   - CaptureCar: Capture current car as a pattern variable binding
//   - VisitCar/VisitCdr: Navigate the input tree
//   - PushContext/PopContext: Track nested bindings for ellipsis
//   - SkipIfEmpty: While-loop check for ellipsis (check BEFORE body)
//   - Jump: Loop back for ellipsis patterns
//   - Done: Complete current subtree
//
// Ellipsis Handling:
//   R7RS ellipsis (...) matches zero or more repetitions of the preceding
//   pattern element. The compiler generates a while-loop structure:
//
//     SkipIfEmpty(exit)   ; Check if list is empty BEFORE entering loop
//     PushContext         ; Start new capture context for this iteration
//     [pattern bytecode]  ; Match and capture pattern variables
//     VisitCdr            ; Advance to next element
//     PopContext          ; Close capture context
//     Jump(back)          ; Loop to SkipIfEmpty
//     exit:               ; Continue after loop
//
//   The SkipIfEmpty instruction is critical for matching zero repetitions:
//   Pattern (foo e1 e2 ...) must match input (foo x) where e2... is empty.
//
// Reference: R7RS Section 4.3.2 (syntax-rules pattern language)

import (
	"context"
	"fmt"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var (
	// ErrUnknownOpCode is returned when an unknown bytecode is encountered.
	ErrUnknownOpCode = werr.ErrUnknownOpCode
	// ErrNotAMatch is returned when the input does not match the pattern.
	ErrNotAMatch = werr.ErrNotAMatch
)

type syntaxCompilerStackEntry struct {
	lastElementStart int                // position where the last element's bytecode starts
	lastElement      syntax.SyntaxValue // the actual last element value for analysis lookup
	pr               *syntax.SyntaxPair
	variables        values.StringSet
	vararg           bool
}

type captureContext struct {
	children map[int][]*captureContext     // Key: ellipsisID
	bindings map[string]syntax.SyntaxValue // Pattern variable bindings (syntax-native)
	// parent is the context one ellipsis level up (nil at the root). A
	// per-iteration child context only holds the bindings captured under its
	// own ellipsis; lower-depth pattern variables (R7RS §4.3.2 "broadcast")
	// live in an ancestor, so binding lookup walks this chain.
	parent *captureContext
}

// SyntaxCommand represents a pattern bytecode instruction.
type SyntaxCommand interface {
	fmt.Stringer
}

// SyntaxCompiler compiles pattern syntax into bytecode.
type SyntaxCompiler struct {
	codes              []SyntaxCommand
	variables          syntax.PatternVarSymbols
	literals           syntax.LiteralSymbols
	analysis           *PatternAnalysis         // pattern analysis results
	nextEllipsisID     int                      // counter for assigning unique ellipsis IDs
	ellipsisVars       map[int]values.StringSet // ellipsisID -> captured pattern variables
	ellipsisDepth      int                      // monotonic counter: inner ellipsis < outer ellipsis
	ellipsisDepths     map[int]int              // ellipsisID -> compilation order (lower = inner)
	ellipsis           string                   // custom ellipsis identifier (default "...")
	skipMacroKeyword   bool                     // true = skip first element as macro keyword placeholder
	macroKeywordPassed bool                     // true = first root element has been processed
}

// NewSyntaxCompiler creates a new syntax compiler with the default ellipsis ("...").
func NewSyntaxCompiler() *SyntaxCompiler {
	return NewSyntaxCompilerWithEllipsis(DefaultEllipsis)
}

// NewSyntaxCompilerWithEllipsis creates a new syntax compiler with a custom ellipsis identifier.
// Per R7RS §4.3.2, syntax-rules can specify an alternative ellipsis identifier.
func NewSyntaxCompilerWithEllipsis(ellipsis string) *SyntaxCompiler {
	if ellipsis == "" {
		ellipsis = DefaultEllipsis
	}
	q := &SyntaxCompiler{
		variables:        syntax.PatternVarSymbols{},
		literals:         syntax.LiteralSymbols{},
		ellipsisVars:     map[int]values.StringSet{},
		ellipsisDepths:   map[int]int{},
		ellipsis:         ellipsis,
		skipMacroKeyword: false, // Default: match all elements. Set to true for syntax-rules.
	}
	return q
}

// SetSkipMacroKeyword enables or disables skipping the first pattern element as a macro keyword.
// R7RS §4.3.2: The first subform of each syntax-rules pattern is the keyword of the macro
// being transformed; it is not matched against the macro use.
// Call this with true when compiling syntax-rules patterns.
func (p *SyntaxCompiler) SetSkipMacroKeyword(skip bool) {
	p.skipMacroKeyword = skip
}

// Compile compiles a pattern pair into bytecode.
func (p *SyntaxCompiler) Compile(ctx context.Context, pr *syntax.SyntaxPair) error {
	// Analyze pattern first to identify which subtrees contain variables
	p.analysis = AnalyzePattern(pr, p.variables)

	compile(ctx, p, pr)
	return nil
}

func compile(ctx context.Context, vis *SyntaxCompiler, v0 *syntax.SyntaxPair) bool {
	stack := []syntaxCompilerStackEntry{
		{
			pr:        v0,
			variables: values.StringSet{},
		},
	}

	for len(stack) > 0 {
		stack = compileCurrentLevel(ctx, vis, stack)
	}
	return true
}

// compileCurrentLevel processes all elements at the current nesting level.
// Returns the updated stack (with current level popped and Done emitted).
func compileCurrentLevel(ctx context.Context, vis *SyntaxCompiler, stack []syntaxCompilerStackEntry) []syntaxCompilerStackEntry { //nolint:unparam
	l := len(stack)
	for !syntax.IsSyntaxEmptyList(stack[l-1].pr) && !stack[l-1].pr.IsVoid() {
		elementStart := len(vis.codes)
		element := stack[l-1].pr.SyntaxCar()

		// Process the current element and get updated stack
		var shouldContinue bool
		stack, shouldContinue = compileElement(vis, stack, element, elementStart)
		if shouldContinue {
			l = len(stack)
			continue
		}

		// Advance to next element in the list (CDR)
		l = len(stack)
		if !advanceToNextElement(vis, &stack[l-1], element, elementStart) {
			break
		}
	}

	// Emit Done for this level and pop stack
	vis.codes = append(vis.codes, ByteCodeDone{})
	return stack[:l-1]
}

// compileElement compiles a single element (car of current pair).
// Returns the updated stack and whether the main loop should continue (skip CDR handling).
func compileElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry, element syntax.SyntaxValue, elementStart int) ([]syntaxCompilerStackEntry, bool) {
	l := len(stack)

	// R7RS §4.3.2: The first subform of each pattern is the keyword of the macro
	// being transformed; it is not matched against the macro use being transformed.
	// Skip the first element at the root level (stack depth 1) as it's the macro keyword placeholder.
	if vis.skipMacroKeyword && !vis.macroKeywordPassed && l == 1 {
		vis.macroKeywordPassed = true
		// Don't emit any bytecode for the macro keyword placeholder
		return stack, false
	}

	// Handle pair elements (nested lists)
	pr, ok := element.(*syntax.SyntaxPair)
	if ok {
		return compilePairElement(vis, stack, pr, element, elementStart)
	}

	// Handle vector elements (R7RS §4.3.2)
	vec, ok := element.(*syntax.SyntaxVector)
	if ok {
		return compileVectorElement(vis, stack, vec, element, elementStart)
	}

	// Handle box elements (#&<pattern>)
	box, ok := element.(*syntax.SyntaxBox)
	if ok {
		return compileBoxElement(vis, stack, box, elementStart)
	}

	// Handle symbol elements
	sym, ok := element.(*syntax.SyntaxSymbol)
	if ok {
		skipCdr := compileSymbolElement(vis, &stack[l-1], sym)
		return stack, skipCdr
	}

	// Handle literal values (numbers, strings, etc.) — already syntax
	vis.codes = append(vis.codes, ByteCodeCompareCar{Value: element})
	return stack, false
}

// compilePairElement handles when the current element is a pair (nested list).
// Emits VisitCar and pushes the sub-pattern onto the stack.
func compilePairElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry, pr *syntax.SyntaxPair, element syntax.SyntaxValue, elementStart int) ([]syntaxCompilerStackEntry, bool) {
	l := len(stack)

	// C12/C13: a nested sub-list in NON-FINAL position can be followed by an
	// improper `. rest` tail, e.g. `((x) . body)`. The descent paths below skip
	// advanceToNextElement (they return shouldContinue=true), so the improper
	// tail must be captured here — and BEFORE the descent op, because at runtime
	// CaptureCdr/CompareCdr read the parent pair's cdr at the current position;
	// the descent op then operates on the car, which CaptureCdr preserves.
	emitImproperTailIfPresent(vis, &stack[l-1])

	// Nested pair - descend into it. There is no empty-list case here: the
	// caller reaches this function only after element.(*syntax.SyntaxPair)
	// succeeds, and (*SyntaxPair).IsEmptyList is a constant false, so an empty
	// list pattern arrives as the SyntaxEmptyList singleton and is compiled by
	// compileElement's literal fallthrough instead.
	vis.codes = append(vis.codes, ByteCodeVisitCar{})
	stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
	stack[l-1].lastElement = element
	stack[l-1].lastElementStart = elementStart

	// Push new stack entry for nested processing
	stack = append(stack, syntaxCompilerStackEntry{
		pr:        pr,
		variables: values.StringSet{},
	})
	return stack, true
}

// compileVectorElement handles when the current element is a vector pattern.
// R7RS §4.3.2: #(<pattern> ...) matches vectors. The vector is converted to
// a SyntaxPair chain so pair-based compilation handles the contents.
func compileVectorElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry,
	vec *syntax.SyntaxVector, element syntax.SyntaxValue, elementStart int,
) ([]syntaxCompilerStackEntry, bool) {
	l := len(stack)

	// C12/C13: a vector sub-pattern in non-final position can be followed by an
	// improper `. rest` tail; capture it before the descent op (same rationale
	// as compilePairElement).
	emitImproperTailIfPresent(vis, &stack[l-1])

	if len(vec.Values) == 0 {
		// Empty vector pattern #() — verify input car is an empty vector.
		vis.codes = append(vis.codes, ByteCodeRequireCarEmptyVector{})
		stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
		stack[l-1].lastElement = element
		stack[l-1].lastElementStart = elementStart
		return stack, true
	}

	// Non-empty vector — convert to pair chain for pair-based matching
	vis.codes = append(vis.codes, ByteCodeVisitCarAsVector{})
	chain := vectorElementsToPairChain(vec)

	// Analyze the converted chain so pointer-based analysis works for
	// ellipsis detection inside vector patterns.
	localAnalysis := AnalyzePattern(chain, vis.variables)
	vis.analysis.Merge(localAnalysis)

	stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
	stack[l-1].lastElement = chain
	stack[l-1].lastElementStart = elementStart

	// Push converted chain for nested processing
	stack = append(stack, syntaxCompilerStackEntry{
		pr:        chain,
		variables: values.StringSet{},
	})
	return stack, true
}

// compileBoxElement handles when the current element is a box pattern.
//
// A box holds exactly one datum, so `#&<pattern>` is compiled as a one-element
// sub-list `(<pattern>)`: the descent op converts the input box the same way,
// and every existing pair opcode then applies unchanged. Without this a box
// pattern fell through to the literal CompareCar below, which compares the
// box's CONTENT by value — so `((_ #&y) y)` never matched `(p #&7)`.
func compileBoxElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry,
	box *syntax.SyntaxBox, elementStart int,
) ([]syntaxCompilerStackEntry, bool) {
	l := len(stack)

	// Capture an improper `. rest` tail before the descent op, same rationale as
	// compilePairElement.
	emitImproperTailIfPresent(vis, &stack[l-1])

	vis.codes = append(vis.codes, ByteCodeVisitCarAsBox{})
	chain := boxContentToPairChain(box)

	// Analyze the converted chain so pointer-based analysis works for ellipsis
	// detection inside box patterns (as compileVectorElement does).
	localAnalysis := AnalyzePattern(chain, vis.variables)
	vis.analysis.Merge(localAnalysis)

	stack[l-1].pr, _ = stack[l-1].pr.SyntaxCdr().(*syntax.SyntaxPair)
	stack[l-1].lastElement = chain
	stack[l-1].lastElementStart = elementStart

	stack = append(stack, syntaxCompilerStackEntry{
		pr:        chain,
		variables: values.StringSet{},
	})
	return stack, true
}

// boxContentToPairChain wraps a SyntaxBox's content in a one-element pair chain
// so pair-based compilation handles it.
func boxContentToPairChain(box *syntax.SyntaxBox) *syntax.SyntaxPair {
	var content syntax.SyntaxValue = syntax.SyntaxEmptyList
	if box.Value != nil {
		content = box.Value
	}
	return syntax.NewSyntaxCons(content, syntax.SyntaxEmptyList, box.SourceContext())
}

// vectorElementsToPairChain is vectorToSyntaxPairChain narrowed to *SyntaxPair,
// for the compile- and expand-time callers that have already established the
// vector is non-empty. An empty vector yields SyntaxEmptyList, which is not a
// *SyntaxPair — hence the precondition, which both callers check.
func vectorElementsToPairChain(vec *syntax.SyntaxVector) *syntax.SyntaxPair {
	return vectorToSyntaxPairChain(vec).(*syntax.SyntaxPair)
}

// compileSymbolElement handles symbol elements: ellipsis, wildcards, variables, and literals.
// Returns true if CDR handling should be skipped (e.g., for ellipsis-in-middle patterns).
func compileSymbolElement(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, sym *syntax.SyntaxSymbol) bool {
	// Check for ellipsis (custom or default)
	if sym.Key() == vis.ellipsis {
		return compileEllipsis(vis, entry)
	}
	// Check for wildcard
	// R7RS §4.3.2: The identifier _ is a wildcard that matches any input, unless
	// it appears in the list of literals, in which case it is matched literally.
	if sym.Key() == "_" {
		isLiteral := vis.literals.ContainsOne(sym.Key())
		if !isLiteral {
			// Wildcard - matches an element without binding it. It has to emit
			// SOMETHING: an element that produces no bytecode is invisible to
			// ByteCodeDone's one-instruction lookahead, which is what made a
			// trailing `_` after a sublist both reject valid input and accept
			// input one element short.
			vis.codes = append(vis.codes, ByteCodeDiscardCar{})
			return false
		}
		// _ is in literals list, fall through to treat as literal
	}
	// Otherwise it's a variable or literal
	compileSymbolOrLiteral(vis, entry, sym)
	return false
}

// compileSymbolOrLiteral handles a symbol that's either a pattern variable or a literal.
func compileSymbolOrLiteral(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, sym *syntax.SyntaxSymbol) {
	isVar := vis.variables.ContainsOne(sym.Key())
	if isVar {
		// Pattern variable - capture it
		vis.codes = append(vis.codes, ByteCodeCaptureCar{Binding: sym.Key()})
		entry.variables.Set(sym.Key())
	} else {
		// Literal symbol - compare exactly
		vis.codes = append(vis.codes, ByteCodeCompareCar{Value: syntax.NewSyntaxSymbol(sym.Key(), nil)})
	}
}

// compileEllipsis handles the ellipsis pattern, which matches zero or more
// repetitions of the preceding pattern element.
//
// R7RS §4.3.2's "F matches P" clause list is purely structural: whether the
// repeated subpattern happens to contain a pattern variable is not one of its
// conditions, so `(_ 1 ...)` repeats the literal `1` exactly as `(_ x ...)`
// repeats `x`. The two `...`-related exemptions the standard does grant are
// implemented elsewhere and by other mechanisms: declaring `...` a literal
// (the "\x00" ellipsis swap in compile_syntax_rules.go) and the `(... ...)`
// escape, which arrives here with no preceding element.
//
// R7RS §4.3.2 allows ellipsis in the middle of a pattern, e.g., (a ... b c).
// In this case, the loop must exit when exactly N elements remain (where N is
// the count of trailing pattern elements).
//
// Returns true if CDR handling should be skipped (for ellipsis-in-middle patterns,
// where the loop exits with the position already at the tail elements).
func compileEllipsis(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) bool {
	if entry.lastElement == nil {
		// Nothing precedes the ellipsis at this level, so there is no subpattern
		// to repeat and entry.lastElementStart names no bytecode of ours (it is
		// still the zero value, which indexes the enclosing level's codes).
		// Match the ellipsis identifier literally.
		vis.codes = append(vis.codes, ByteCodeCompareCar{Value: syntax.NewSyntaxSymbol(vis.ellipsis, nil)})
		return false
	}

	entry.vararg = true
	ellipsisID := vis.nextEllipsisID
	vis.nextEllipsisID++
	// Record compilation order. The depth-first, inner-before-outer traversal
	// guarantees inner ellipsis get lower values than outer ellipsis. Siblings
	// at the same nesting level get different values, but this is harmless:
	// siblings capture different variables, so the depth comparison in
	// findMatchingEllipsisIDs never fires for them.
	vis.ellipsisDepths[ellipsisID] = vis.ellipsisDepth
	vis.ellipsisDepth++

	// Collect variables captured by this ellipsis
	vis.ellipsisVars[ellipsisID] = collectCapturedVariables(vis, entry)

	// Count trailing elements after the ellipsis (for ellipsis-in-middle support)
	// entry.pr car is "...", entry.pr cdr is what comes after
	tailCount := countPatternTailElements(entry.pr)

	// Extract and relocate pattern bytecode into loop structure
	patternCodes := extractPatternBytecode(vis, entry)
	emitEllipsisLoop(vis, ellipsisID, patternCodes, tailCount)

	// For ellipsis-in-middle patterns, skip CDR handling because:
	// 1. The loop exits via SkipIfTailCount when exactly tailCount elements remain
	// 2. The value stack is already positioned at the first tail element
	// 3. We just need to update entry.pr to point to the tail pattern elements
	if tailCount > 0 {
		// Advance entry.pr past the ellipsis to the tail elements
		entry.pr, _ = entry.pr.SyntaxCdr().(*syntax.SyntaxPair)
		return true
	}
	return false
}

// countPatternTailElements counts the number of pattern elements that follow
// the ellipsis. This is used for ellipsis-in-middle patterns like (a ... b c).
func countPatternTailElements(ellipsisPair *syntax.SyntaxPair) int {
	// ellipsisPair car is "...", ellipsisPair cdr is the rest
	cdr := ellipsisPair.SyntaxCdr()
	count := 0
	for {
		pr, ok := cdr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(pr) {
			break
		}
		count++
		cdr = pr.SyntaxCdr()
	}
	return count
}

// collectCapturedVariables gathers all pattern variables captured by an ellipsis pattern.
func collectCapturedVariables(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) values.StringSet {
	prevPair, ok := entry.lastElement.(*syntax.SyntaxPair)
	if ok {
		return values.Union(vis.analysis.GetVariables(prevPair))
	}
	capturedVars := values.NewStringSet(0)
	prevSym, ok := entry.lastElement.(*syntax.SyntaxSymbol)
	if ok {
		isVar := vis.variables.ContainsOne(prevSym.Key())
		if isVar {
			capturedVars.Set(prevSym.Key())
		}
	}
	return capturedVars
}

// extractPatternBytecode removes the bytecode for the pattern preceding ...
// and returns it for insertion into the loop structure.
func extractPatternBytecode(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) []SyntaxCommand {
	patternStart := entry.lastElementStart
	patternEnd := len(vis.codes)

	// Remove trailing VisitCdr if present (loop handles advancement)
	if patternEnd > 0 {
		_, ok := vis.codes[patternEnd-1].(ByteCodeVisitCdr)
		if ok {
			patternEnd--
		}
	}

	if patternEnd <= patternStart {
		return nil
	}

	// Copy and remove pattern bytecode
	patternCodes := make([]SyntaxCommand, patternEnd-patternStart)
	copy(patternCodes, vis.codes[patternStart:patternEnd])
	vis.codes = vis.codes[:patternStart]
	return patternCodes
}

// emitEllipsisLoop generates the loop structure for ellipsis matching:
//
//	SkipIfEmpty(exit)   ; Check if list is empty BEFORE loop (or SkipIfTailCount for ellipsis-in-middle)
//	PushContext(id)     ; Start capture context for this iteration
//	[pattern bytecode]  ; Match and capture pattern variables
//	VisitCdr            ; Advance to next element (if needed)
//	PopContext(id)      ; Close capture context
//	Jump(back)          ; Loop to SkipIfEmpty
//	exit:               ; Continue after loop
//
// For ellipsis-in-middle patterns (tailCount > 0), uses SkipIfTailCount instead of
// SkipIfEmpty. This exits the loop when exactly tailCount elements remain, leaving
// them for the trailing pattern elements to match.
func emitEllipsisLoop(vis *SyntaxCompiler, ellipsisID int, patternCodes []SyntaxCommand, tailCount int) {
	loopStart := len(vis.codes)

	// Placeholder for loop exit check - will be patched after loop
	skipIdx := len(vis.codes)
	if tailCount > 0 {
		// Ellipsis-in-middle: exit when tailCount elements remain
		vis.codes = append(vis.codes, ByteCodeSkipIfTailCount{Offset: 0, Count: tailCount})
	} else {
		// Standard ellipsis at end: exit when list is empty
		vis.codes = append(vis.codes, ByteCodeSkipIfEmpty{Offset: 0})
	}

	// Loop body
	vis.codes = append(vis.codes, ByteCodePushContext{EllipsisID: ellipsisID})
	vis.codes = append(vis.codes, patternCodes...)

	// Add VisitCdr unless pattern already advances (pair patterns end with Done)
	if !isPairPattern(patternCodes) {
		vis.codes = append(vis.codes, ByteCodeVisitCdr{})
	}

	vis.codes = append(vis.codes, ByteCodePopContext{EllipsisID: ellipsisID})

	// Jump back to loop start
	jumpOffset := loopStart - len(vis.codes)
	vis.codes = append(vis.codes, ByteCodeJump{Offset: jumpOffset})

	// Patch the skip instruction to jump past loop
	loopEnd := len(vis.codes)
	if tailCount > 0 {
		vis.codes[skipIdx] = ByteCodeSkipIfTailCount{Offset: loopEnd - skipIdx, Count: tailCount}
	} else {
		vis.codes[skipIdx] = ByteCodeSkipIfEmpty{Offset: loopEnd - skipIdx}
	}
}

// isPairPattern checks if bytecode represents a descend pattern (VisitCar...Done,
// VisitCarAsVector...Done or VisitCarAsBox...Done). These auto-advance via
// handleByteCodeDone, so they don't need an explicit VisitCdr in ellipsis loops —
// adding one skips every other element.
func isPairPattern(codes []SyntaxCommand) bool {
	if len(codes) < 2 {
		return false
	}
	_, endsWithDone := codes[len(codes)-1].(ByteCodeDone)
	if !endsWithDone {
		return false
	}
	switch codes[0].(type) {
	case ByteCodeVisitCar, ByteCodeVisitCarAsVector, ByteCodeVisitCarAsBox:
		return true
	default:
		return false
	}
}

// advanceToNextElement handles CDR advancement after processing an element.
// Returns false if at end of list (should break from loop).
func advanceToNextElement(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, element syntax.SyntaxValue, elementStart int) bool {
	cdr := entry.pr.SyntaxCdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(cdrPair) {
		// Normal case: more elements in proper list
		vis.codes = append(vis.codes, ByteCodeVisitCdr{})
		entry.lastElementStart = elementStart
		entry.lastElement = element
		entry.pr = cdrPair
		return true
	}

	// Improper list pattern: (_ a . rest) where rest is a pattern variable
	// or a literal symbol.
	emitImproperTailIfPresent(vis, entry)
	return false
}

// emitImproperTailIfPresent emits bytecode for an improper `. rest` tail at the
// current list level. The cdr of entry.pr is an improper tail when it is a bare
// symbol (not a pair and not the empty list). A pattern variable becomes
// CaptureCdr (binds the rest of the input); a literal symbol becomes CompareCdr
// (matches it exactly). Does nothing when the cdr is a pair or the empty list.
//
// Shared by advanceToNextElement (symbol/literal preceding element) and by the
// pair/vector descent paths (compilePairElement, compileVectorElement), which
// otherwise skip advanceToNextElement and would drop the tail (C12/C13). In the
// descent paths it must run BEFORE the descent op so CaptureCdr reads the parent
// pair's cdr at the current position rather than the already-advanced sibling.
func emitImproperTailIfPresent(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) {
	cdr := entry.pr.SyntaxCdr()
	sym, ok := cdr.(*syntax.SyntaxSymbol)
	if !ok {
		return
	}
	// R7RS §4.3.2: `_` is the wildcard everywhere in a pattern, dotted tail
	// included — it matches the rest of the input but must not bind, so a
	// template `_` stays a free identifier. Unless it was declared a literal, in
	// which case it is matched literally like any other literal symbol.
	isLiteral := vis.literals.ContainsOne(sym.Key())
	if sym.Key() == "_" && !isLiteral {
		vis.codes = append(vis.codes, ByteCodeDiscardCdr{})
		return
	}
	isVar := vis.variables.ContainsOne(sym.Key())
	if isVar {
		// The CDR is a pattern variable - emit CaptureCdr to capture the rest
		vis.codes = append(vis.codes, ByteCodeCaptureCdr{Binding: sym.Key()})
		entry.variables.Set(sym.Key())
		return
	}
	// The CDR is a literal symbol - compare it
	vis.codes = append(vis.codes, ByteCodeCompareCdr{Value: syntax.NewSyntaxSymbol(sym.Key(), nil)})
}
