package match

import (
	"skeme/values"
)

// PatternAnalysis holds analysis results for a pattern
type PatternAnalysis struct {
	// Maps each subtree (Pair) to whether it contains pattern variables
	containsVariables map[*values.Pair]bool
	// Maps each subtree to the set of variables it contains
	variablesInSubtree map[*values.Pair]map[string]struct{}
}

// NewPatternAnalysis creates a new pattern analysis
func NewPatternAnalysis() *PatternAnalysis {
	return &PatternAnalysis{
		containsVariables:  make(map[*values.Pair]bool),
		variablesInSubtree: make(map[*values.Pair]map[string]struct{}),
	}
}

// AnalyzePattern analyzes a pattern and returns analysis results
func AnalyzePattern(pattern *values.Pair, variables map[string]struct{}) *PatternAnalysis {
	analysis := NewPatternAnalysis()
	// Use the provided variables directly
	// In a full implementation, these would be determined from syntax-rules literals

	// Analyze which subtrees contain variables
	analyzeRecursive(pattern, variables, analysis)
	return analysis
}

// AnalyzePatternWithLiterals analyzes a pattern determining variables from literals
func AnalyzePatternWithLiterals(pattern *values.Pair, literals map[string]struct{}, isKeyword bool) *PatternAnalysis {
	analysis := NewPatternAnalysis()
	// Determine pattern variables first
	variables := make(map[string]struct{})
	collectPatternVariables(pattern, literals, isKeyword, variables)

	// Then analyze which subtrees contain variables
	analyzeRecursive(pattern, variables, analysis)
	return analysis
}

// collectPatternVariables walks the pattern and identifies all pattern variables
func collectPatternVariables(v values.Value, literals map[string]struct{}, isFirst bool, variables map[string]struct{}) {
	switch t := v.(type) {
	case *values.Symbol:
		// Skip if it's a keyword (first element), literal, or ellipsis
		if !isFirst && t.Key != "..." {
			if _, isLiteral := literals[t.Key]; !isLiteral {
				variables[t.Key] = struct{}{}
			}
		}
	case *values.Pair:
		if !values.IsEmptyList(t) {
			// First element in a pattern is the keyword
			collectPatternVariables(t[0], literals, isFirst, variables)
			// Rest of the pattern
			collectPatternVariables(t[1], literals, false, variables)
		}
	}
}

// analyzeRecursive analyzes which subtrees contain pattern variables
func analyzeRecursive(v values.Value, variables map[string]struct{}, analysis *PatternAnalysis) bool {
	switch t := v.(type) {
	case *values.Symbol:
		_, isVar := variables[t.Key]
		return isVar
	case *values.Pair:
		if values.IsEmptyList(t) {
			return false
		}

		// Initialize variable set for this subtree
		varsInSubtree := make(map[string]struct{})

		// Check car (first element)
		carHasVars := analyzeRecursive(t[0], variables, analysis)
		if carHasVars {
			// If car is a symbol variable, add it
			if sym, ok := t[0].(*values.Symbol); ok {
				if _, isVar := variables[sym.Key]; isVar {
					varsInSubtree[sym.Key] = struct{}{}
				}
			}
			// If car is a pair, merge its variables
			if carPair, ok := t[0].(*values.Pair); ok {
				if carVars, exists := analysis.variablesInSubtree[carPair]; exists {
					for v := range carVars {
						varsInSubtree[v] = struct{}{}
					}
				}
			}
		}

		// Check cdr (rest)
		cdrHasVars := analyzeRecursive(t[1], variables, analysis)
		if cdrHasVars {
			// If cdr is a pair, merge its variables
			if cdrPair, ok := t[1].(*values.Pair); ok {
				if cdrVars, exists := analysis.variablesInSubtree[cdrPair]; exists {
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
	default:
		return false
	}
}

// ContainsVariables returns whether a subtree contains pattern variables
func (a *PatternAnalysis) ContainsVariables(pair *values.Pair) bool {
	if pair == nil {
		return false
	}
	return a.containsVariables[pair]
}

// GetVariables returns the set of variables in a subtree
func (a *PatternAnalysis) GetVariables(pair *values.Pair) map[string]struct{} {
	if pair == nil {
		return nil
	}
	return a.variablesInSubtree[pair]
}