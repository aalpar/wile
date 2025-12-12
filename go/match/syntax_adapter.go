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

// syntax_adapter.go bridges between syntax objects (with hygiene info) and
// the unhygienic pattern matching VM.
//
// Design: The macro system uses a layered architecture (see go/DESIGN.md):
//   Layer 1: Pattern Matching VM - operates on raw values.Value types
//   Layer 2: Syntax Adapter (this file) - converts syntax ↔ values
//   Layer 3: Hygienic Layer - adds/checks scopes during expansion
//
// This file implements Layer 2. The SyntaxMatcher wraps the core Matcher,
// converting syntax objects to raw values for matching, and converting
// expanded results back to syntax objects (preserving source context).
//
// Key functions:
//   - syntaxToValue: Unwraps syntax objects to raw values for pattern matching
//   - valueToSyntax: Wraps raw values back into syntax objects for the result
//
// The hygiene layer (Layer 3) operates after expansion by adding intro scopes
// to the syntax objects returned by valueToSyntax.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"errors"

	"wile/syntax"
	"wile/values"
)

// SyntaxMatcher adapts the unhygienic Matcher to work with syntax objects.
//
// It provides the bridge between:
//   - Syntax objects (SyntaxPair, SyntaxSymbol) with source locations and scopes
//   - Raw values (Pair, Symbol) that the pattern matching VM operates on
//
// The separation allows the pattern matcher to be simple and efficient,
// while syntax/scope handling is done in the adapter layer.
//
// Key feature: The syntaxMap preserves original syntax objects for captured
// pattern variables. This is essential for hygiene - content captured from
// the input must retain its original scopes, not receive new ones.
type SyntaxMatcher struct {
	matcher   *Matcher
	syntaxMap map[values.Value]syntax.SyntaxValue // Maps raw values to their original syntax
}

// NewSyntaxMatcher creates a new syntax-aware matcher
func NewSyntaxMatcher(variables map[string]struct{}, codes []SyntaxCommand) *SyntaxMatcher {
	return NewSyntaxMatcherWithEllipsisVars(variables, codes, nil)
}

// NewSyntaxMatcherWithEllipsisVars creates a syntax-aware matcher with ellipsis variable mapping.
// The ellipsisVars parameter maps each ellipsis ID to its captured pattern variables.
func NewSyntaxMatcherWithEllipsisVars(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}) *SyntaxMatcher {
	return &SyntaxMatcher{
		matcher:   NewMatcherWithEllipsisVars(variables, codes, ellipsisVars),
		syntaxMap: make(map[values.Value]syntax.SyntaxValue),
	}
}

// Match performs pattern matching on syntax objects
func (sm *SyntaxMatcher) Match(input syntax.SyntaxValue) error {
	// Clear the syntax map for this match
	sm.syntaxMap = make(map[values.Value]syntax.SyntaxValue)

	// Convert syntax to raw values for matching, building the syntax map
	rawInput := sm.syntaxToValueWithMap(input)

	// Ensure it's a pair as the matcher expects
	pair, ok := rawInput.(*values.Pair)
	if !ok {
		return errors.New("pattern matching requires a pair")
	}

	return sm.matcher.Match(pair)
}

// Expand performs template expansion, preserving syntax wrappers
func (sm *SyntaxMatcher) Expand(template syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return sm.ExpandWithIntroScope(template, nil, nil)
}

// ExpandWithIntroScope performs template expansion with hygiene support.
// The introScope is added to newly created syntax objects (from the template),
// but NOT to syntax objects preserved from pattern variable substitution.
// The freeIds set contains identifiers that should not receive the intro scope.
func (sm *SyntaxMatcher) ExpandWithIntroScope(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}) (syntax.SyntaxValue, error) {
	return sm.ExpandWithUseSite(template, introScope, freeIds, nil)
}

// ExpandWithUseSite performs template expansion with hygiene support and use-site tracking.
// The introScope is added to newly created syntax objects (from the template),
// but NOT to syntax objects preserved from pattern variable substitution.
// The freeIds set contains identifiers that should not receive the intro scope.
// The useSiteCtx, if provided, is used for the source context of newly created syntax
// objects instead of the template's context. This allows error messages to point to
// where the macro was invoked rather than where it was defined.
func (sm *SyntaxMatcher) ExpandWithUseSite(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}, useSiteCtx *syntax.SourceContext) (syntax.SyntaxValue, error) {
	return sm.ExpandWithOrigin(template, introScope, freeIds, useSiteCtx, nil)
}

// ExpandWithOrigin performs template expansion with full hygiene and origin tracking.
// Parameters:
//   - template: The template to expand
//   - introScope: Hygiene scope added to newly created syntax (not pattern variables)
//   - freeIds: Identifiers that should not receive the intro scope
//   - useSiteCtx: Source context for newly created syntax (use-site vs template-site)
//   - origin: Origin info for tracking macro expansion chains
func (sm *SyntaxMatcher) ExpandWithOrigin(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}, useSiteCtx *syntax.SourceContext, origin *syntax.OriginInfo) (syntax.SyntaxValue, error) {
	// Convert template to raw values
	rawTemplate := syntaxToValue(template)

	// Perform unhygienic expansion
	expanded, err := sm.matcher.Expand(rawTemplate)
	if err != nil {
		return nil, err
	}

	// Wrap result back in syntax, using the syntax map to preserve
	// original syntax objects from captured pattern variables.
	// The intro scope is only added to newly created syntax objects.
	// Use use-site context if provided, otherwise fall back to template context.
	return sm.valueToSyntaxWithOrigin(expanded, template, introScope, freeIds, useSiteCtx, origin), nil
}

// syntaxToValueWithMap recursively unwraps syntax objects to raw values,
// building a map from raw values back to their original syntax objects.
// This preserves the connection needed for hygiene during expansion.
func (sm *SyntaxMatcher) syntaxToValueWithMap(stx syntax.SyntaxValue) values.Value {
	if stx == nil {
		return nil
	}

	var result values.Value

	switch s := stx.(type) {
	case *syntax.SyntaxPair:
		if s == nil || s.IsEmptyList() {
			result = values.EmptyList
		} else {
			// Recursively unwrap car and cdr, building the map
			var car values.Value
			if carVal := s.Car(); carVal != nil {
				if carSyntax, ok := carVal.(syntax.SyntaxValue); ok {
					car = sm.syntaxToValueWithMap(carSyntax)
				} else {
					car = carVal.(values.Value)
				}
			}

			var cdr values.Value
			if cdrVal := s.Cdr(); cdrVal != nil {
				if cdrSyntax, ok := cdrVal.(syntax.SyntaxValue); ok {
					cdr = sm.syntaxToValueWithMap(cdrSyntax)
				} else {
					cdr = cdrVal.(values.Value)
				}
			}

			// Build the result pair
			if cdr == nil || values.IsEmptyList(cdr) {
				result = values.NewCons(car, values.EmptyList)
			} else if cdrPair, ok := cdr.(*values.Pair); ok {
				result = values.NewCons(car, cdrPair)
			} else {
				result = values.NewCons(car, cdr)
			}
		}

	case *syntax.SyntaxSymbol:
		result = s.Unwrap()

	case *syntax.SyntaxObject:
		result = s.Unwrap()

	default:
		if unwrapper, ok := stx.(interface{ Unwrap() values.Value }); ok {
			result = unwrapper.Unwrap()
		} else if val, ok := stx.(values.Value); ok {
			result = val
		}
	}

	// Store the mapping from raw value to original syntax
	// This allows us to recover the original scopes during expansion
	if result != nil {
		sm.syntaxMap[result] = stx
	}

	return result
}

// valueToSyntaxWithMap wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. This is essential for hygiene.
func (sm *SyntaxMatcher) valueToSyntaxWithMap(val values.Value, templateStx syntax.SyntaxValue) syntax.SyntaxValue {
	return sm.valueToSyntaxWithIntroScope(val, templateStx, nil, nil)
}

// valueToSyntaxWithIntroScope wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
func (sm *SyntaxMatcher) valueToSyntaxWithIntroScope(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}) syntax.SyntaxValue {
	return sm.valueToSyntaxWithUseSite(val, templateStx, introScope, freeIds, nil)
}

// valueToSyntaxWithUseSite wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
// When useSiteCtx is provided, it's used as the source context for newly
// created syntax objects instead of the template's context.
func (sm *SyntaxMatcher) valueToSyntaxWithUseSite(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}, useSiteCtx *syntax.SourceContext) syntax.SyntaxValue {
	return sm.valueToSyntaxWithOrigin(val, templateStx, introScope, freeIds, useSiteCtx, nil)
}

// valueToSyntaxWithOrigin wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
// When useSiteCtx is provided, it's used as the source context for newly
// created syntax objects instead of the template's context.
// When origin is provided, it's attached to the source context of newly
// created syntax objects to track macro expansion chains.
func (sm *SyntaxMatcher) valueToSyntaxWithOrigin(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]struct{}, useSiteCtx *syntax.SourceContext, origin *syntax.OriginInfo) syntax.SyntaxValue {
	if val == nil {
		return nil
	}

	// Check if this value has a corresponding original syntax object
	// If so, return the original to preserve scopes (no intro scope added!)
	if origStx, ok := sm.syntaxMap[val]; ok {
		return origStx
	}

	// Determine source context: use-site context takes precedence if provided,
	// otherwise fall back to template context
	var srcCtx *syntax.SourceContext
	if useSiteCtx != nil {
		srcCtx = useSiteCtx
	} else if templateStx != nil {
		srcCtx = templateStx.SourceContext()
	}

	// If we have origin info, attach it to the source context
	if origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(origin)
	} else if origin != nil {
		srcCtx = &syntax.SourceContext{Origin: origin}
	}

	switch v := val.(type) {
	case *values.Pair:
		if values.IsEmptyList(v) {
			return syntax.NewSyntaxEmptyList(srcCtx)
		}

		// Recursively wrap car and cdr, checking the map for each
		car := sm.valueToSyntaxWithOrigin(v[0], templateStx, introScope, freeIds, useSiteCtx, origin)

		var cdr syntax.SyntaxValue
		if v[1] == nil || values.IsEmptyList(v[1]) {
			cdr = syntax.NewSyntaxEmptyList(srcCtx)
		} else {
			cdr = sm.valueToSyntaxWithOrigin(v[1], templateStx, introScope, freeIds, useSiteCtx, origin)
		}

		return syntax.NewSyntaxCons(car, cdr, srcCtx)

	case *values.Symbol:
		// Create the symbol
		sym := syntax.NewSyntaxSymbol(v.Key, srcCtx)

		// Add intro scope if:
		// 1. An intro scope was provided
		// 2. This is NOT a free identifier
		if introScope != nil {
			if freeIds != nil {
				if _, isFree := freeIds[v.Key]; !isFree {
					sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
				}
			} else {
				sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
			}
		}
		return sym

	case *values.Integer:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Float:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.String:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Boolean:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Character:
		return syntax.NewSyntaxObject(v, srcCtx)

	default:
		// For any other value type, wrap in generic syntax object
		return syntax.NewSyntaxObject(val, srcCtx)
	}
}

// syntaxToValue recursively unwraps syntax objects to raw values.Value types.
//
// This is the "datum" extraction from R7RS syntax->datum. It strips away:
//   - Source location information (file, line, column)
//   - Scope sets (used for hygiene)
//
// The pattern matching VM operates on these raw values because:
//   1. Pattern matching is structural - it doesn't care about source locations
//   2. The unhygienic core doesn't need scope information
//   3. Raw values are simpler and faster to traverse
//
// After expansion, valueToSyntax re-wraps the result, and the hygiene layer
// adds intro scopes to the new syntax objects.
func syntaxToValue(stx syntax.SyntaxValue) values.Value {
	if stx == nil {
		return nil
	}

	switch s := stx.(type) {
	case *syntax.SyntaxPair:
		if s == nil || s.IsEmptyList() {
			return values.EmptyList
		}
		// Recursively unwrap car and cdr
		var car values.Value
		if carVal := s.Car(); carVal != nil {
			if carSyntax, ok := carVal.(syntax.SyntaxValue); ok {
				car = syntaxToValue(carSyntax)
			} else {
				// If it's already a value, use it directly
				car = carVal.(values.Value)
			}
		}

		var cdr values.Value
		if cdrVal := s.Cdr(); cdrVal != nil {
			if cdrSyntax, ok := cdrVal.(syntax.SyntaxValue); ok {
				cdr = syntaxToValue(cdrSyntax)
			} else {
				// If it's already a value, use it directly
				cdr = cdrVal.(values.Value)
			}
		}

		// Handle proper lists and improper lists
		if cdr == nil || values.IsEmptyList(cdr) {
			return values.NewCons(car, values.EmptyList)
		}
		cdrPair, ok := cdr.(*values.Pair)
		if ok {
			return values.NewCons(car, cdrPair)
		}
		// Improper list
		return values.NewCons(car, cdr)

	case *syntax.SyntaxSymbol:
		return s.Unwrap()

	case *syntax.SyntaxObject:
		return s.Unwrap()

	default:
		// For other syntax types, try to unwrap
		if unwrapper, ok := stx.(interface{ Unwrap() values.Value }); ok {
			return unwrapper.Unwrap()
		}
		// If it's already a value, return as-is
		if val, ok := stx.(values.Value); ok {
			return val
		}
		return nil
	}
}

// valueToSyntax wraps raw values back into syntax objects.
//
// This is the inverse of syntaxToValue, similar to R7RS datum->syntax.
// It reconstructs syntax objects from the expanded template, preserving:
//   - Source context from the original template (for error reporting)
//   - Structure of the expanded form
//
// Note: The scopes are NOT preserved during this conversion. Instead,
// the hygiene layer (in operation_syntax_rules_transform.go) adds a fresh
// "intro scope" to all syntax objects after expansion. This is the key to
// Flatt's "sets of scopes" hygiene model.
//
// The templateStx parameter provides the source context (file, line, etc.)
// that will be attached to the new syntax objects.
func valueToSyntax(val values.Value, templateStx syntax.SyntaxValue) syntax.SyntaxValue {
	if val == nil {
		return nil
	}

	// Get source context from template if available
	var srcCtx *syntax.SourceContext
	if templateStx != nil {
		srcCtx = templateStx.SourceContext()
	}

	switch v := val.(type) {
	case *values.Pair:
		if values.IsEmptyList(v) {
			// Return syntax empty list for empty list
			return syntax.NewSyntaxEmptyList(srcCtx)
		}

		// Recursively wrap car and cdr
		car := valueToSyntax(v[0], templateStx)

		// Handle cdr - could be another pair or an atom (improper list)
		var cdr syntax.SyntaxValue
		if v[1] == nil || values.IsEmptyList(v[1]) {
			cdr = syntax.NewSyntaxEmptyList(srcCtx)
		} else {
			cdr = valueToSyntax(v[1], templateStx)
		}

		return syntax.NewSyntaxCons(car, cdr, srcCtx)

	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, srcCtx)

	case *values.Integer:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Float:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.String:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Boolean:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Character:
		return syntax.NewSyntaxObject(v, srcCtx)

	default:
		// For any other value type, wrap in generic syntax object
		return syntax.NewSyntaxObject(val, srcCtx)
	}
}

// CompiledPattern contains the compiled bytecode and ellipsis variable mapping.
type CompiledPattern struct {
	Codes        []SyntaxCommand
	EllipsisVars map[int]map[string]struct{}
}

// CompileSyntaxPattern compiles a syntax pattern into bytecode
// This is a convenience function that unwraps syntax before compilation
func CompileSyntaxPattern(pattern syntax.SyntaxValue, variables map[string]struct{}) ([]SyntaxCommand, error) {
	result, err := CompileSyntaxPatternFull(pattern, variables)
	if err != nil {
		return nil, err
	}
	return result.Codes, nil
}

// CompileSyntaxPatternFull compiles a syntax pattern into bytecode with ellipsis variable mapping.
// Returns a CompiledPattern containing both the bytecode and the ellipsis variable mapping.
func CompileSyntaxPatternFull(pattern syntax.SyntaxValue, variables map[string]struct{}) (*CompiledPattern, error) {
	// Convert syntax pattern to raw values
	rawPattern := syntaxToValue(pattern)

	// Ensure it's a pair
	pair, ok := rawPattern.(*values.Pair)
	if !ok {
		return nil, errors.New("pattern must be a list")
	}

	// Compile using existing compiler
	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(pair)
	if err != nil {
		return nil, err
	}

	return &CompiledPattern{
		Codes:        compiler.codes,
		EllipsisVars: compiler.ellipsisVars,
	}, nil
}

// GetBindings returns the captured pattern variable bindings from the last match,
// converted back to syntax values. This is used by syntax-case to bind pattern
// variables in the body's environment.
func (sm *SyntaxMatcher) GetBindings() map[string]syntax.SyntaxValue {
	rawBindings := sm.matcher.GetBindings()
	if rawBindings == nil {
		return nil
	}

	result := make(map[string]syntax.SyntaxValue)
	for name, rawValue := range rawBindings {
		// Convert raw value back to syntax using the syntax map
		if stx, ok := sm.syntaxMap[rawValue]; ok {
			result[name] = stx
		} else {
			// Wrap the raw value in a SyntaxObject if not in map
			result[name] = syntax.NewSyntaxObject(rawValue, nil)
		}
	}
	return result
}
