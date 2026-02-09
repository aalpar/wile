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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

var (
	// ErrUnknownOpCode is returned when an unknown bytecode is encountered.
	ErrUnknownOpCode = values.ErrUnknownOpCode
	// ErrNotAMatch is returned when the input does not match the pattern.
	ErrNotAMatch = values.ErrNotAMatch
)

type syntaxCompilerStackEntry struct {
	mark             int          // position in instructions in which loops will return to
	lastElementStart int          // position where the last element's bytecode starts
	lastElement      values.Value // the actual last element value for analysis lookup
	pr               *values.Pair
	variables        map[string]struct{}
	vararg           bool
}

type captureContext struct {
	children map[int][]*captureContext     // Key: ellipsisID
	bindings map[string]syntax.SyntaxValue // Pattern variable bindings (syntax-native)
}

// SyntaxCommand represents a pattern bytecode instruction.
type SyntaxCommand interface {
	fmt.Stringer
}

// SyntaxCompiler compiles pattern syntax into bytecode.
type SyntaxCompiler struct {
	codes              []SyntaxCommand
	variables          map[string]struct{}
	literals           map[string]struct{}         // literals to match exactly
	analysis           *PatternAnalysis            // pattern analysis results
	nextEllipsisID     int                         // counter for assigning unique ellipsis IDs
	ellipsisVars       map[int]map[string]struct{} // ellipsisID -> captured pattern variables
	ellipsis           string                      // custom ellipsis identifier (default "...")
	skipMacroKeyword   bool                        // true = skip first element as macro keyword placeholder
	macroKeywordPassed bool                        // true = first root element has been processed
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
		variables:        map[string]struct{}{},
		literals:         map[string]struct{}{},
		ellipsisVars:     map[int]map[string]struct{}{},
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
func (p *SyntaxCompiler) Compile(ctx context.Context, pr *values.Pair) error {
	// Analyze pattern first to identify which subtrees contain variables
	// Use the pre-set variables for now (from test setup)
	p.analysis = AnalyzePattern(pr, p.variables)

	compile(ctx, p, pr)
	return nil
}

func compile(ctx context.Context, vis *SyntaxCompiler, v0 *values.Pair) bool {
	stack := []syntaxCompilerStackEntry{
		{pr: v0, variables: map[string]struct{}{}},
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
	for !values.IsEmptyList(stack[l-1].pr) && !values.IsVoid(stack[l-1].pr) {
		elementStart := len(vis.codes)
		element := stack[l-1].pr.Car()

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
func compileElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry, element values.Value, elementStart int) ([]syntaxCompilerStackEntry, bool) {
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
	if pr, ok := element.(*values.Pair); ok { //nolint:gocritic
		return compilePairElement(vis, stack, pr, element, elementStart)
	}

	// Handle symbol elements
	if sym, ok := element.(*values.Symbol); ok { //nolint:gocritic
		skipCdr := compileSymbolElement(vis, &stack[l-1], sym)
		return stack, skipCdr
	}

	// Handle literal values (numbers, strings, etc.)
	vis.codes = append(vis.codes, ByteCodeCompareCar{Value: valueToSyntaxValue(element)})
	return stack, false
}

// compilePairElement handles when the current element is a pair (nested list).
// For empty pairs, emits RequireCarEmpty. For non-empty, emits VisitCar and pushes to stack.
func compilePairElement(vis *SyntaxCompiler, stack []syntaxCompilerStackEntry, pr *values.Pair, element values.Value, elementStart int) ([]syntaxCompilerStackEntry, bool) {
	l := len(stack)

	if values.IsEmptyList(pr) {
		// Empty pair pattern () - verify input car is also empty
		vis.codes = append(vis.codes, ByteCodeRequireCarEmpty{})
		stack[l-1].pr, _ = stack[l-1].pr.Cdr().(*values.Pair)
		stack[l-1].lastElement = element
		stack[l-1].lastElementStart = elementStart
		return stack, true
	}

	// Non-empty nested pair - descend into it
	vis.codes = append(vis.codes, ByteCodeVisitCar{})
	stack[l-1].pr, _ = stack[l-1].pr.Cdr().(*values.Pair)
	stack[l-1].lastElement = element
	stack[l-1].lastElementStart = elementStart

	// Push new stack entry for nested processing
	stack = append(stack, syntaxCompilerStackEntry{
		pr:        pr,
		variables: map[string]struct{}{},
	})
	return stack, true
}

// compileSymbolElement handles symbol elements: ellipsis, wildcards, variables, and literals.
// Returns true if CDR handling should be skipped (e.g., for ellipsis-in-middle patterns).
func compileSymbolElement(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, sym *values.Symbol) bool {
	// Check for ellipsis (custom or default)
	if sym.Key == vis.ellipsis {
		return compileEllipsis(vis, entry)
	}
	// Check for wildcard
	// R7RS §4.3.2: The identifier _ is a wildcard that matches any input, unless
	// it appears in the list of literals, in which case it is matched literally.
	if sym.Key == "_" {
		if _, isLiteral := vis.literals[sym.Key]; !isLiteral { //nolint:gocritic
			// Wildcard - matches anything but doesn't bind (no bytecode emitted)
			return false
		}
		// _ is in literals list, fall through to treat as literal
	}
	// Otherwise it's a variable or literal
	compileSymbolOrLiteral(vis, entry, sym)
	return false
}

// compileSymbolOrLiteral handles a symbol that's either a pattern variable or a literal.
func compileSymbolOrLiteral(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, sym *values.Symbol) {
	if _, isVar := vis.variables[sym.Key]; isVar {
		// Pattern variable - capture it
		vis.codes = append(vis.codes, ByteCodeCaptureCar{Binding: sym.Key})
		entry.variables[sym.Key] = struct{}{}
	} else {
		// Literal symbol - compare exactly
		vis.codes = append(vis.codes, ByteCodeCompareCar{Value: syntax.NewSyntaxSymbolForSymbol(sym, nil)})
	}
}

// compileEllipsis handles the ellipsis pattern, which matches zero or more repetitions.
// If the previous element contains pattern variables, generates a loop structure.
// Otherwise, treats the ellipsis as a literal symbol.
//
// R7RS §4.3.2 allows ellipsis in the middle of a pattern, e.g., (a ... b c).
// In this case, the loop must exit when exactly N elements remain (where N is
// the count of trailing pattern elements).
//
// Returns true if CDR handling should be skipped (for ellipsis-in-middle patterns,
// where the loop exits with the position already at the tail elements).
func compileEllipsis(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) bool {
	if !previousElementHasVariables(vis, entry) {
		// No pattern variables - treat ellipsis as literal
		vis.codes = append(vis.codes, ByteCodeCompareCar{Value: syntax.NewSyntaxSymbol(vis.ellipsis, nil)})
		return false
	}

	entry.vararg = true
	ellipsisID := vis.nextEllipsisID
	vis.nextEllipsisID++

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
		entry.pr, _ = entry.pr.Cdr().(*values.Pair)
		return true
	}
	return false
}

// countPatternTailElements counts the number of pattern elements that follow
// the ellipsis. This is used for ellipsis-in-middle patterns like (a ... b c).
func countPatternTailElements(ellipsisPair *values.Pair) int {
	// ellipsisPair car is "...", ellipsisPair cdr is the rest
	cdr := ellipsisPair.Cdr()
	count := 0
	for {
		pr, ok := cdr.(*values.Pair)
		if !ok || values.IsEmptyList(pr) {
			break
		}
		count++
		cdr = pr.Cdr()
	}
	return count
}

// previousElementHasVariables checks if the element before ... contains pattern variables.
func previousElementHasVariables(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) bool {
	if entry.lastElement == nil {
		return false
	}

	if prevPair, ok := entry.lastElement.(*values.Pair); ok { //nolint:gocritic
		return vis.analysis.ContainsVariables(prevPair)
	}
	if prevSym, ok := entry.lastElement.(*values.Symbol); ok { //nolint:gocritic
		_, isVar := vis.variables[prevSym.Key]
		return isVar
	}
	return false
}

// collectCapturedVariables gathers all pattern variables captured by an ellipsis pattern.
func collectCapturedVariables(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry) map[string]struct{} {
	capturedVars := make(map[string]struct{})

	if prevPair, ok := entry.lastElement.(*values.Pair); ok {
		vars := vis.analysis.GetVariables(prevPair)
		for v := range vars {
			capturedVars[v] = struct{}{}
		}
	} else if prevSym, ok := entry.lastElement.(*values.Symbol); ok { //nolint:gocritic
		_, isVar := vis.variables[prevSym.Key]
		if isVar {
			capturedVars[prevSym.Key] = struct{}{}
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
		if _, ok := vis.codes[patternEnd-1].(ByteCodeVisitCdr); ok { //nolint:gocritic
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

// isPairPattern checks if bytecode represents a pair pattern (VisitCar...Done).
// Pair patterns auto-advance, so they don't need an explicit VisitCdr in loops.
func isPairPattern(codes []SyntaxCommand) bool {
	if len(codes) < 2 {
		return false
	}
	_, startsWithVisitCar := codes[0].(ByteCodeVisitCar)
	_, endsWithDone := codes[len(codes)-1].(ByteCodeDone)
	return startsWithVisitCar && endsWithDone
}

// advanceToNextElement handles CDR advancement after processing an element.
// Returns false if at end of list (should break from loop).
func advanceToNextElement(vis *SyntaxCompiler, entry *syntaxCompilerStackEntry, element values.Value, elementStart int) bool {
	cdr := entry.pr.Cdr()
	cdrPair, ok := cdr.(*values.Pair)
	if ok && !values.IsEmptyList(cdrPair) {
		// Normal case: more elements in proper list
		vis.codes = append(vis.codes, ByteCodeVisitCdr{})
		entry.lastElementStart = elementStart
		entry.lastElement = element
		entry.mark = len(vis.codes)
		entry.pr = cdrPair
		return true
	}

	// Check for improper list pattern: (_ a . rest) where rest is a pattern variable
	if sym, ok := cdr.(*values.Symbol); ok { //nolint:gocritic
		if _, isVar := vis.variables[sym.Key]; isVar {
			// The CDR is a pattern variable - emit CaptureCdr to capture the rest
			vis.codes = append(vis.codes, ByteCodeCaptureCdr{Binding: sym.Key})
			entry.variables[sym.Key] = struct{}{}
		} else {
			// The CDR is a literal symbol - compare it
			vis.codes = append(vis.codes, ByteCodeCompareCdr{Value: syntax.NewSyntaxSymbolForSymbol(sym, nil)})
		}
	}

	return false
}

// insert inserts codes into target at index i, adjusting jump offsets as needed.
func insert(i int, target, codes []SyntaxCommand) []SyntaxCommand {
	q := append([]SyntaxCommand{}, target[:i]...)
	q = append(q, codes...)
	q = append(q, target[i:]...)
	for j := range q {
		bc, ok := q[j].(ByteCodeJump)
		if !ok {
			continue
		}
		if j < i && j+bc.Offset >= i {
			bc.Offset += len(codes)
		} else if j > i+len(codes)-1 && j+bc.Offset <= i {
			bc.Offset -= len(codes)
		}
		q[j] = bc
	}
	return q
}
