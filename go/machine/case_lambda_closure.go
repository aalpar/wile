package machine

import (
	"wile/values"
)

// CaseLambdaClause represents a single clause in a case-lambda.
// Each clause has its own template and environment for parameter bindings.
type CaseLambdaClause struct {
	closure *MachineClosure
}

type CaseLambdaClosure struct {
	clauses []*CaseLambdaClause
}

func NewCaseLambdaClosure(closures []*MachineClosure) *CaseLambdaClosure {
	clauses := make([]*CaseLambdaClause, len(closures))
	for i, cls := range closures {
		clauses[i] = &CaseLambdaClause{closure: cls}
	}
	return &CaseLambdaClosure{
		clauses: clauses,
	}
}

func (p *CaseLambdaClosure) Clauses() []*CaseLambdaClause {
	return p.clauses
}

// FindMatchingClause finds the first clause that matches the given argument count.
// Returns the matching closure and a boolean indicating success.
func (p *CaseLambdaClosure) FindMatchingClause(argCount int) (*MachineClosure, bool) {
	for _, clause := range p.clauses {
		tpl := clause.closure.Template()
		if tpl.IsVariadic() {
			// Variadic: needs at least (parameterCount - 1) args
			if argCount >= tpl.ParameterCount()-1 {
				return clause.closure, true
			}
		} else {
			// Fixed arity: needs exact match
			if argCount == tpl.ParameterCount() {
				return clause.closure, true
			}
		}
	}
	return nil, false
}

func (p *CaseLambdaClosure) IsVoid() bool {
	return p == nil
}

func (p *CaseLambdaClosure) SchemeString() string {
	return "#<case-lambda-closure>"
}

func (p *CaseLambdaClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*CaseLambdaClosure)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	if len(p.clauses) != len(v.clauses) {
		return false
	}
	for i, clause := range p.clauses {
		if !clause.closure.EqualTo(v.clauses[i].closure) {
			return false
		}
	}
	return true
}
