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

package machine

import (
	"github.com/aalpar/wile/values"
)

// CaseLambdaClosure dispatches to the first clause whose arity matches
// the argument count. Each clause is a MachineClosure with its own
// template and captured environment.
type CaseLambdaClosure struct {
	clauses []*MachineClosure
}

func NewCaseLambdaClosure(closures []*MachineClosure) *CaseLambdaClosure {
	clauses := make([]*MachineClosure, len(closures))
	copy(clauses, closures)
	return &CaseLambdaClosure{
		clauses: clauses,
	}
}

func (p *CaseLambdaClosure) Clauses() []*MachineClosure {
	return p.clauses
}

// FindMatchingClause finds the first clause that matches the given argument count.
// Returns the matching closure and a boolean indicating success.
func (p *CaseLambdaClosure) FindMatchingClause(argCount int) (*MachineClosure, bool) {
	// If p is nil, there are no clauses to match.
	if p == nil {
		return nil, false
	}
	for _, clause := range p.clauses {
		tpl := clause.Template()
		if tpl.IsVariadic() {
			// Variadic: needs at least (parameterCount - 1) args
			if argCount >= tpl.ParameterCount()-1 {
				return clause, true
			}
		} else {
			// Fixed arity: needs exact match
			if argCount == tpl.ParameterCount() {
				return clause, true
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
	if p.IsVoid() {
		return v.IsVoid()
	}
	if v.IsVoid() {
		return false
	}
	if len(p.clauses) != len(v.clauses) {
		return false
	}
	for i, clause := range p.clauses {
		if !clause.EqualTo(v.clauses[i]) {
			return false
		}
	}
	return true
}
