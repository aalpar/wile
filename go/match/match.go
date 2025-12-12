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
// on raw values.Value types. It knows nothing about syntax objects or scopes;
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
	"errors"
	"fmt"
	"wile/values"
)

// valuePathEntry tracks position in the input tree during matching.
type valuePathEntry struct {
	pr *values.Pair
}

// Matcher is the pattern matching VM for syntax-rules.
//
// It executes compiled pattern bytecode against an input form,
// capturing pattern variable bindings that can be used for template expansion.
type Matcher struct {
	variables    map[string]struct{}          // Known pattern variables
	codes        []SyntaxCommand              // Compiled pattern bytecode
	captureStack []*captureContext            // Binding capture stack (nesting for ellipsis)
	valueStack   []valuePathEntry             // Input traversal stack
	ellipsisVars map[int]map[string]struct{}  // ellipsisID -> captured pattern variables
}

func NewMatcher(variables map[string]struct{}, codes []SyntaxCommand) *Matcher {
	return NewMatcherWithEllipsisVars(variables, codes, nil)
}

// NewMatcherWithEllipsisVars creates a matcher with ellipsis variable mapping.
// The ellipsisVars parameter maps each ellipsis ID to its captured pattern variables.
func NewMatcherWithEllipsisVars(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}) *Matcher {
	q := &Matcher{
		variables:    variables,
		codes:        codes,
		ellipsisVars: ellipsisVars,
	}
	return q
}

func (p *Matcher) Match(target *values.Pair) error {
	p.valueStack = []valuePathEntry{
		{
			pr: target,
		},
	}
	p.captureStack = []*captureContext{
		{
			children: make(map[int][]*captureContext),
			bindings: map[string]values.Value{},
		},
	}
	lvs := len(p.valueStack)
	i := 0
	for len(p.valueStack) > 0 {
		code := p.codes[i]
		switch cd := code.(type) {
		case ByteCodeCompareCar:
			if !values.EqualTo(cd.Value, p.valueStack[lvs-1].pr[0]) {
				return ErrNotAMatch
			}
		case ByteCodeCaptureCar:
			lcs := len(p.captureStack)
			sv := p.valueStack[lvs-1].pr[0]
			bv, ok := p.captureStack[lcs-1].bindings[cd.Binding]
			if ok && !values.EqualTo(sv, bv) {
				return ErrNotAMatch
			}
			p.captureStack[lcs-1].bindings[cd.Binding] = p.valueStack[lvs-1].pr[0]
		case ByteCodeJump:
			if len(p.valueStack) == 0 {
				return nil
			}
			if !values.IsEmptyList(p.valueStack[lvs-1].pr) && !values.IsVoid(p.valueStack[lvs-1].pr) {
				i += cd.Offset - 1
			}
		case ByteCodeDone:
			// Before popping, check that the cdr of current pair is empty
			// This ensures the pattern consumed all elements at this level
			cdr := p.valueStack[lvs-1].pr[1]
			if !values.IsEmptyList(cdr) && cdr != nil {
				// There are more elements in the input than in the pattern
				// Check if we're in a loop context (ellipsis) - in that case
				// cdr being non-empty is expected
				if cdrPair, ok := cdr.(*values.Pair); ok && !values.IsVoid(cdrPair) {
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
				} else if cdr != nil && !values.IsEmptyList(cdr) {
					// Improper list or other non-pair cdr when we expected end
					return ErrNotAMatch
				}
			}

			lvs = len(p.valueStack) - 1
			p.valueStack = p.valueStack[:lvs]
			if len(p.valueStack) == 0 {
				return nil
			}
			cdr = p.valueStack[lvs-1].pr[1]

			// Check if there are more elements at the parent level
			// After popping, if cdr is not empty, there are more siblings to match
			// If the next instruction is ByteCodeDone (no more pattern elements),
			// then extra siblings means the pattern doesn't match
			if cdrPair, ok := cdr.(*values.Pair); ok && !values.IsEmptyList(cdrPair) {
				if i+1 < len(p.codes) {
					switch p.codes[i+1].(type) {
					case ByteCodeDone:
						// Pattern expects no more elements but input has more
						return ErrNotAMatch
					}
				}
			}

			pr, ok := cdr.(*values.Pair)
			if !ok {
				// cdr is not a pair, check if it's empty
				if values.IsEmptyList(cdr) || cdr == nil {
					// No more siblings - set position to empty list
					p.valueStack[lvs-1] = valuePathEntry{pr: values.EmptyList}
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
				bindings: map[string]values.Value{},
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
			car := p.valueStack[lvs-1].pr[0]
			pr, ok := car.(*values.Pair)
			if !ok {
				return ErrNotAMatch
			}
			p.valueStack = append(p.valueStack, valuePathEntry{pr: pr})
			lvs = len(p.valueStack)
		case ByteCodeVisitCdr:
			cdr := p.valueStack[lvs-1].pr[1]
			pr, ok := cdr.(*values.Pair)
			if !ok {
				return ErrNotAMatch
			}
			p.valueStack[lvs-1] = valuePathEntry{pr: pr}
			lvs = len(p.valueStack)
		case ByteCodeSkipIfEmpty:
			// Skip forward if the current position is empty or void
			// This enables while-loop semantics for ellipsis patterns
			if len(p.valueStack) == 0 || values.IsEmptyList(p.valueStack[lvs-1].pr) || values.IsVoid(p.valueStack[lvs-1].pr) {
				i += cd.Offset - 1 // -1 because i++ at end of loop
			}
		case ByteCodeRequireCarEmpty:
			// Verify that the car at the current position is an empty list
			// This is generated for patterns like () that must match empty input
			car := p.valueStack[lvs-1].pr[0]
			carPair, ok := car.(*values.Pair)
			if !ok || !values.IsEmptyList(carPair) {
				// Car is not an empty list - pattern doesn't match
				return ErrNotAMatch
			}
			// Move to next element in the list
			cdr := p.valueStack[lvs-1].pr[1]
			if cdrPair, ok := cdr.(*values.Pair); ok {
				p.valueStack[lvs-1] = valuePathEntry{pr: cdrPair}
			} else if values.IsEmptyList(cdr) || cdr == nil {
				p.valueStack[lvs-1] = valuePathEntry{pr: values.EmptyList}
			} else {
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
// Returns nil if no match has been performed.
func (p *Matcher) GetBindings() map[string]values.Value {
	if len(p.captureStack) == 0 {
		return nil
	}
	return p.captureStack[0].bindings
}

// Expand substitutes pattern variables in template with captured values
func (p *Matcher) Expand(template values.Value) (values.Value, error) {
	if len(p.captureStack) == 0 {
		return nil, errors.New("no capture context for expansion")
	}
	return p.expandValue(template, p.captureStack[0], nil)
}

// expandValue recursively expands a template value with captured bindings
func (p *Matcher) expandValue(template values.Value, ctx *captureContext, ellipsisVars map[string]struct{}) (values.Value, error) {
	switch t := template.(type) {
	case *values.Symbol:
		// Check if it's a pattern variable
		if val, ok := ctx.bindings[t.Key]; ok {
			return val, nil
		}
		// Check if it's an ellipsis variable (from outer repetition)
		if ellipsisVars != nil {
			if _, ok := ellipsisVars[t.Key]; ok {
				// This variable should be expanded in the context of ellipsis
				return nil, fmt.Errorf("ellipsis variable %s used outside of ellipsis context", t.Key)
			}
		}
		// Not a pattern variable, return as-is
		return t, nil

	case *values.Pair:
		// Check for ellipsis pattern (something ...)
		if !values.IsEmptyList(t) {
			cdr := t[1]
			if cdrPair, ok := cdr.(*values.Pair); ok && !values.IsEmptyList(cdrPair) {
				if sym, ok := cdrPair[0].(*values.Symbol); ok && sym.Key == "..." {
					// Found ellipsis - need to repeat the car for each capture
					return p.expandEllipsis(t[0], cdrPair[1], ctx, ellipsisVars)
				}
			}
		}

		// Regular pair - expand car and cdr
		car, err := p.expandValue(t[0], ctx, ellipsisVars)
		if err != nil {
			return nil, err
		}
		cdr, err := p.expandValue(t[1], ctx, ellipsisVars)
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
		if ellipsisVars != nil {
			for k, v := range ellipsisVars {
				newEllipsisVars[k] = v
			}
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

	// Find the ID that contains ALL the requested variables
	for id, ellipsisVars := range p.ellipsisVars {
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
	for id, ellipsisVars := range p.ellipsisVars {
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
		if !values.IsEmptyList(t) {
			p.findVarsRecursive(t[0], vars)
			p.findVarsRecursive(t[1], vars)
		}
	}
}

// appendToList appends two values treating them as lists
func (p *Matcher) appendToList(list values.Value, rest values.Value) values.Value {
	if values.IsEmptyList(list) {
		return rest
	}
	pair := list.(*values.Pair)
	return values.NewCons(pair[0], p.appendToList(pair[1], rest))
}
