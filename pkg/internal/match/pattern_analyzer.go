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

// analyzeRecursive returns the set of pattern variables occurring in v (nil when
// there are none), recording the result for every *SyntaxPair subtree it visits.
//
// It returns the set rather than a bool because that is what keeps a vector
// sub-pattern's variables attached to the enclosing subtree: the analysis maps
// are keyed by *SyntaxPair, so a *SyntaxVector has nowhere of its own to record
// them and the names must travel up to the enclosing pair. compileEllipsis reads
// that enclosing pair's set to decide which variables an ellipsis group
// captures, and R7RS §4.3.2 requires a vector sub-pattern to repeat under `...`
// exactly as a list sub-pattern does — a dropped name leaves the group unable to
// claim it.
func analyzeRecursive(
	v syntax.SyntaxValue,
	variables map[string]struct{},
	analysis *PatternAnalysis,
) map[string]struct{} {
	switch t := v.(type) {
	case *syntax.SyntaxSymbol:
		_, isVar := variables[t.Key()]
		if !isVar {
			return nil
		}
		return map[string]struct{}{t.Key(): {}}

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return nil
		}

		varsInSubtree := make(map[string]struct{})
		maps.Copy(varsInSubtree, analyzeRecursive(t.SyntaxCar(), variables, analysis))
		maps.Copy(varsInSubtree, analyzeRecursive(t.SyntaxCdr(), variables, analysis))

		hasVars := len(varsInSubtree) > 0
		analysis.containsVariables[t] = hasVars
		if !hasVars {
			return nil
		}
		analysis.variablesInSubtree[t] = varsInSubtree
		return varsInSubtree

	case *syntax.SyntaxVector:
		// R7RS §4.3.2: #(<pattern> ...) — recurse into vector elements. The
		// compiler converts the vector to a pair chain and Merges that chain's
		// analysis, which is what supplies map entries for the chain's own pairs;
		// here we only propagate the names upward.
		varsInVector := make(map[string]struct{})
		for _, elem := range t.Values {
			maps.Copy(varsInVector, analyzeRecursive(elem, variables, analysis))
		}
		if len(varsInVector) == 0 {
			return nil
		}
		return varsInVector

	case *syntax.SyntaxBox:
		// `#&<pattern>` — same treatment as a vector: the compiler converts the
		// box to a one-element pair chain and Merges that chain's analysis, so
		// here we only propagate the names upward.
		if t.Value == nil {
			return nil
		}
		return analyzeRecursive(t.Value, variables, analysis)

	default:
		return nil
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
