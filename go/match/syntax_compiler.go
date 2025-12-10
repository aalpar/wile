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
	"errors"
	"fmt"
	"skeme/values"
)

var (
	ErrUnknownOpCode = errors.New("unknown op code")
	ErrNotAMatch     = errors.New("not a match")
)

type syntaxCompilerStackEntry struct {
	mark              int // position in instructions in which loops will return to
	lastElementStart  int // position where the last element's bytecode starts
	lastElement       values.Value // the actual last element value for analysis lookup
	pr                *values.Pair
	variables         map[string]struct{}
	vararg            bool
}

type captureContext struct {
	children map[int][]*captureContext // Key: ellipsisID
	bindings map[string]values.Value
}

type SyntaxCommand interface {
	fmt.Stringer
}

type ByteCodeCompareCar struct {
	Value values.Value
}

func (p ByteCodeCompareCar) String() string {
	return fmt.Sprintf("CompareCar(%s)", p.Value.SchemeString())
}

type ByteCodeCaptureCar struct {
	Binding string
}

func (p ByteCodeCaptureCar) String() string {
	return fmt.Sprintf("CaptureCar(%s)", p.Binding)
}

type ByteCodeJump struct {
	Offset int
}

func (p ByteCodeJump) String() string {
	return fmt.Sprintf("Jump(%d)", p.Offset)
}

type ByteCodeDone struct{}

func (ByteCodeDone) String() string {
	return "Done"
}

type ByteCodeVisitCar struct{}

func (ByteCodeVisitCar) String() string {
	return "VisitCar"
}

type ByteCodeVisitCdr struct{}

func (ByteCodeVisitCdr) String() string {
	return "VisitCdr"
}

// ByteCodePushContext starts a new capture context for an ellipsis iteration.
// EllipsisID identifies which ellipsis pattern this context belongs to,
// enabling multiple independent ellipsis patterns in the same clause.
type ByteCodePushContext struct {
	EllipsisID int
}

func (p ByteCodePushContext) String() string {
	return fmt.Sprintf("PushContext(%d)", p.EllipsisID)
}

// ByteCodePopContext ends the current capture context.
type ByteCodePopContext struct {
	EllipsisID int
}

func (p ByteCodePopContext) String() string {
	return fmt.Sprintf("PopContext(%d)", p.EllipsisID)
}

// ByteCodeSkipIfEmpty implements while-loop semantics for ellipsis patterns.
//
// Problem: Without this, ellipsis patterns use do-while semantics, executing
// the loop body at least once. This breaks patterns like (foo e1 e2 ...)
// when matching (foo x) - the e2... part should match zero elements.
//
// Solution: Check if the list is empty BEFORE entering the loop body.
// If empty, skip forward by Offset instructions to exit the loop.
//
// This is the key fix for zero-iteration ellipsis matching in R7RS.
type ByteCodeSkipIfEmpty struct {
	Offset int
}

// ByteCodeRequireCarEmpty verifies that the car at the current position is an empty list.
//
// Problem: Pattern () should only match input (). Without this check,
// VisitCar + Done would match any list, because Done only checks that CDR is empty.
//
// Solution: Generate this instruction instead of VisitCar when the pattern
// element is (). It verifies the input car is also empty before proceeding.
type ByteCodeRequireCarEmpty struct{}

func (p ByteCodeSkipIfEmpty) String() string {
	return fmt.Sprintf("SkipIfEmpty(%d)", p.Offset)
}

func (p ByteCodeRequireCarEmpty) String() string {
	return "RequireCarEmpty"
}

type SyntaxCompiler struct {
	val            *values.Pair
	codes          []SyntaxCommand
	variables      map[string]struct{}
	literals       map[string]struct{}            // literals to match exactly
	analysis       *PatternAnalysis               // pattern analysis results
	nextEllipsisID int                            // counter for assigning unique ellipsis IDs
	ellipsisVars   map[int]map[string]struct{}   // ellipsisID -> captured pattern variables
}

func NewSyntaxCompiler() *SyntaxCompiler {
	q := &SyntaxCompiler{
		variables:    map[string]struct{}{},
		literals:     map[string]struct{}{},
		ellipsisVars: map[int]map[string]struct{}{},
	}
	return q
}

// GetEllipsisVariables returns the mapping of ellipsis IDs to their captured pattern variables.
func (q *SyntaxCompiler) GetEllipsisVariables() map[int]map[string]struct{} {
	return q.ellipsisVars
}

func (q *SyntaxCompiler) Compile(pr *values.Pair) error {
	// Analyze pattern first to identify which subtrees contain variables
	// Use the pre-set variables for now (from test setup)
	q.analysis = AnalyzePattern(pr, q.variables)

	compile(q, pr)
	return nil
}

func compile(vis *SyntaxCompiler, v0 *values.Pair) bool {
	// start with original pair as stack
	stack := []syntaxCompilerStackEntry{
		{
			pr:        v0,
			variables: map[string]struct{}{},
		},
	}
	// why there are still elements in the stack
	for len(stack) > 0 {
		l := len(stack)
		for !values.IsEmptyList(stack[l-1].pr) && !values.IsVoid(stack[l-1].pr) {
			// Track where this element's bytecode starts
			elementStart := len(vis.codes)

			v1 := stack[l-1].pr[0]
			pr, ok := v1.(*values.Pair)
			if ok {
				// Check if the nested pair is empty - pattern ()
				if values.IsEmptyList(pr) {
					// Generate RequireCarEmpty instead of VisitCar for empty patterns
					// This verifies the input car is also an empty list
					vis.codes = append(vis.codes, ByteCodeRequireCarEmpty{})
					// Advance to next element
					stack[l-1].pr, _ = stack[l-1].pr[1].(*values.Pair)
					stack[l-1].lastElement = v1
					stack[l-1].lastElementStart = elementStart
					// Don't push onto stack - nothing to process inside ()
					// Continue to next iteration (skip the VisitCdr handling below)
					continue
				} else {
					vis.codes = append(vis.codes, ByteCodeVisitCar{})
					// Advance to next element and track the pair as lastElement
					stack[l-1].pr, _ = stack[l-1].pr[1].(*values.Pair)
					stack[l-1].lastElement = v1
					stack[l-1].lastElementStart = elementStart
					// Push new stack for nested processing
					stack = append(stack, syntaxCompilerStackEntry{
						pr:        pr,
						variables: map[string]struct{}{},
					})
					l = len(stack)
					continue
				}
			} else {
				sym, ok := v1.(*values.Symbol)
				if ok {
					// it's a symbol
					if sym.Key == "..." {
						l = len(stack)
						// Check if the previous element contains pattern variables
						// If it does, treat ... as ellipsis; otherwise as literal
						prevElementHasVars := false
						if stack[l-1].lastElement != nil {
							if prevPair, ok := stack[l-1].lastElement.(*values.Pair); ok {
								prevElementHasVars = vis.analysis.ContainsVariables(prevPair)
							} else if prevSym, ok := stack[l-1].lastElement.(*values.Symbol); ok {
								// Check if this symbol is a pattern variable
								_, isVar := vis.variables[prevSym.Key]
								prevElementHasVars = isVar
							}
						}

						if prevElementHasVars {
							stack[l-1].vararg = true

							// Assign unique ellipsis ID and collect captured variables
							ellipsisID := vis.nextEllipsisID
							vis.nextEllipsisID++

							// Collect variables captured by this ellipsis pattern
							capturedVars := make(map[string]struct{})
							if prevPair, ok := stack[l-1].lastElement.(*values.Pair); ok {
								if vars := vis.analysis.GetVariables(prevPair); vars != nil {
									for v := range vars {
										capturedVars[v] = struct{}{}
									}
								}
							} else if prevSym, ok := stack[l-1].lastElement.(*values.Symbol); ok {
								if _, isVar := vis.variables[prevSym.Key]; isVar {
									capturedVars[prevSym.Key] = struct{}{}
								}
							}
							vis.ellipsisVars[ellipsisID] = capturedVars

							// Need to reorganize bytecode: move the last pattern element inside the loop
							// The mark shows where the last VisitCdr was added
							// We need to wrap everything from lastElementStart to before the VisitCdr

							patternStart := stack[l-1].lastElementStart

							// Find where the last VisitCdr was added (should be at the end)
							patternEnd := len(vis.codes)
							// Check if the last instruction is VisitCdr and remove it
							if patternEnd > 0 {
								if _, ok := vis.codes[patternEnd-1].(ByteCodeVisitCdr); ok {
									patternEnd--
								}
							}

							// Extract the pattern bytecode that needs to repeat
							patternCodes := []SyntaxCommand{}
							if patternEnd > patternStart {
								patternCodes = make([]SyntaxCommand, patternEnd-patternStart)
								copy(patternCodes, vis.codes[patternStart:patternEnd])
								// Remove the pattern from its current position
								vis.codes = vis.codes[:patternStart]
							}

							// Build the loop structure with while-loop semantics:
							// SkipIfEmpty (forward to after loop - skip if nothing to match)
							// PushContext(ellipsisID)
							// [pattern bytecode] - this contains the capture/compare operations
							// VisitCdr (to advance to next element) - only if pattern doesn't auto-advance
							// PopContext(ellipsisID)
							// Jump back to SkipIfEmpty

							loopStart := len(vis.codes)
							// Placeholder for SkipIfEmpty - will be patched after we know the loop end
							skipIfEmptyIdx := len(vis.codes)
							vis.codes = append(vis.codes, ByteCodeSkipIfEmpty{Offset: 0}) // placeholder

							vis.codes = append(vis.codes, ByteCodePushContext{EllipsisID: ellipsisID})
							vis.codes = append(vis.codes, patternCodes...)

							// Check if pattern is a pair pattern (starts with VisitCar and ends with Done)
							// If so, Done already advances to the next element, so we don't need VisitCdr
							isPairPattern := false
							if len(patternCodes) > 1 {
								_, startsWithVisitCar := patternCodes[0].(ByteCodeVisitCar)
								_, endsWithDone := patternCodes[len(patternCodes)-1].(ByteCodeDone)
								isPairPattern = startsWithVisitCar && endsWithDone
							}

							if !isPairPattern {
								vis.codes = append(vis.codes, ByteCodeVisitCdr{})
							}

							vis.codes = append(vis.codes, ByteCodePopContext{EllipsisID: ellipsisID})
							jumpOffset := loopStart - len(vis.codes)
							vis.codes = append(vis.codes, ByteCodeJump{Offset: jumpOffset})

							// Patch the SkipIfEmpty to skip to after the loop
							loopEnd := len(vis.codes)
							vis.codes[skipIfEmptyIdx] = ByteCodeSkipIfEmpty{Offset: loopEnd - skipIfEmptyIdx}
						} else {
							// No pattern variables declared, treat ... as literal
							vis.codes = append(vis.codes, ByteCodeCompareCar{Value: sym})
						}
					} else {
						// its not "..."
						_, ok = vis.variables[sym.Key]
						if ok {
							// it's a variable
							vis.codes = append(vis.codes, ByteCodeCaptureCar{Binding: sym.Key})
							stack[l-1].variables[sym.Key] = struct{}{}
						} else {
							vis.codes = append(vis.codes, ByteCodeCompareCar{Value: sym})
						}
					}
				} else {
					// not a symbol
					vis.codes = append(vis.codes, ByteCodeCompareCar{Value: v1})
				}
			}
			v2 := stack[l-1].pr[1]
			v2pr, ok := v2.(*values.Pair)
			if ok {
				if !values.IsEmptyList(v2pr) {
					vis.codes = append(vis.codes, ByteCodeVisitCdr{})
					stack[l-1].lastElementStart = elementStart
					stack[l-1].lastElement = v1 // Track the element we just processed
					stack[l-1].mark = len(vis.codes)
					stack[l-1].pr = v2pr
				} else {
					break
				}
			}
		}
		vis.codes = append(vis.codes, ByteCodeDone{})
		stack = stack[:l-1]
		l = len(stack)
	}
	return true
}

func insert(i int, target, codes []SyntaxCommand) []SyntaxCommand {
	q := append([]SyntaxCommand{}, target[:i]...)
	q = append(q, codes...)
	q = append(q, target[i:]...)
	for j := range q {
		bc, ok := q[j].(ByteCodeJump)
		if ok {
			if j < i && j+bc.Offset >= i {
				bc.Offset = bc.Offset + len(codes)
			} else if j > i+len(codes)-1 && j+bc.Offset <= i {
				bc.Offset = bc.Offset - len(codes)
			}
			q[j] = bc
		}
	}
	return q
}
