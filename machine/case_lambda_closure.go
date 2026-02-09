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
// It returns the matching closure and true when a clause can accept exactly
// argCount arguments (for fixed-arity clauses) or at least argCount arguments
// (for variadic clauses). If the receiver is nil or no clause matches, it
// returns nil, false.
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

// IsVoid reports whether this value represents the absence of a case-lambda
// closure. A nil receiver is treated as a distinguished "void" closure value,
// used as a sentinel to mean "no closure" rather than an error.
func (p *CaseLambdaClosure) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme-readable representation of a case-lambda
// closure. Note that the void value (nil receiver) still prints as a
// case-lambda closure; callers must use IsVoid to distinguish the sentinel.
func (p *CaseLambdaClosure) SchemeString() string {
	return "#<case-lambda-closure>"
}

// EqualTo implements Scheme equality for case-lambda closures. Two void
// closures (nil receivers) are considered equal to each other. Non-void
// closures are equal only if they have the same number of clauses and each
// corresponding clause is EqualTo its counterpart.
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
