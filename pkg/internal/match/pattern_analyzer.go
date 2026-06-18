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

import (
	"maps"

	"github.com/aalpar/wile/pkg/syntax"
)

// PatternAnalysis holds analysis results for a pattern
type PatternAnalysis struct {
	// Maps each subtree (SyntaxPair) to whether it contains pattern variables
	containsVariables map[*syntax.SyntaxPair]bool
	// Maps each subtree to the set of variables it contains
	variablesInSubtree map[*syntax.SyntaxPair]map[string]struct{}
}

// NewPatternAnalysis creates a new pattern analysis
func NewPatternAnalysis() *PatternAnalysis {
	return &PatternAnalysis{
		containsVariables:  make(map[*syntax.SyntaxPair]bool),
		variablesInSubtree: make(map[*syntax.SyntaxPair]map[string]struct{}),
	}
}

// AnalyzePattern analyzes a pattern and returns analysis results
func AnalyzePattern(pattern *syntax.SyntaxPair, variables map[string]struct{}) *PatternAnalysis {
	analysis := NewPatternAnalysis()
	analyzeRecursive(pattern, variables, analysis)
	return analysis
}

// analyzeRecursive analyzes which subtrees contain pattern variables
func analyzeRecursive(v syntax.SyntaxValue, variables map[string]struct{}, analysis *PatternAnalysis) bool {
	switch t := v.(type) {
	case *syntax.SyntaxSymbol:
		_, isVar := variables[t.Key()]
		return isVar
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return false
		}

		// Initialize variable set for this subtree
		varsInSubtree := make(map[string]struct{})

		// Check car (first element)
		carHasVars := analyzeRecursive(t.SyntaxCar(), variables, analysis)
		if carHasVars {
			// If car is a symbol variable, add it
			sym, ok := t.SyntaxCar().(*syntax.SyntaxSymbol)
			if ok {
				_, isVar := variables[sym.Key()]
				if isVar {
					varsInSubtree[sym.Key()] = struct{}{}
				}
			}
			// If car is a pair, merge its variables
			carPair, ok := t.SyntaxCar().(*syntax.SyntaxPair)
			if ok {
				carVars, exists := analysis.variablesInSubtree[carPair]
				if exists {
					for v := range carVars {
						varsInSubtree[v] = struct{}{}
					}
				}
			}
		}

		// Check cdr (rest)
		cdrHasVars := analyzeRecursive(t.SyntaxCdr(), variables, analysis)
		if cdrHasVars {
			// If cdr is a pair, merge its variables
			cdrPair, ok := t.SyntaxCdr().(*syntax.SyntaxPair)
			if ok {
				cdrVars, exists := analysis.variablesInSubtree[cdrPair]
				if exists {
					for v := range cdrVars {
						varsInSubtree[v] = struct{}{}
					}
				}
			}
		}

		hasVars := carHasVars || cdrHasVars
		analysis.containsVariables[t] = hasVars
		if len(varsInSubtree) > 0 {
			analysis.variablesInSubtree[t] = varsInSubtree
		}
		return hasVars
	case *syntax.SyntaxVector:
		// R7RS §4.3.2: #(<pattern> ...) — recurse into vector elements.
		// Returns whether any element contains variables, but does NOT store
		// entries in the analysis maps (those use *SyntaxPair keys). The
		// converted pair chain entries are added via Merge in the compiler.
		hasVars := false
		for _, elem := range t.Values {
			if analyzeRecursive(elem, variables, analysis) {
				hasVars = true
			}
		}
		return hasVars
	default:
		return false
	}
}

// Merge incorporates analysis results from another PatternAnalysis.
// Used when vector patterns are converted to pair chains at compile time,
// creating fresh SyntaxPair nodes that need analysis entries.
func (p *PatternAnalysis) Merge(other *PatternAnalysis) {
	maps.Copy(p.containsVariables, other.containsVariables)
	maps.Copy(p.variablesInSubtree, other.variablesInSubtree)
}

// ContainsVariables returns whether a subtree contains pattern variables
func (p *PatternAnalysis) ContainsVariables(pair *syntax.SyntaxPair) bool {
	if pair == nil {
		return false
	}
	return p.containsVariables[pair]
}

// GetVariables returns the set of variables in a subtree
func (p *PatternAnalysis) GetVariables(pair *syntax.SyntaxPair) map[string]struct{} {
	if pair == nil {
		return nil
	}
	return p.variablesInSubtree[pair]
}
