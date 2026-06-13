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

package validate

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// makeTestEnvAndBindings creates an EnvironmentFrame with local bindings for
// the given names and returns the bindings slice with corresponding entries.
func makeTestEnvAndBindings(names ...string) (
	*environment.EnvironmentFrame,
	[]ValidatedLetBinding,
) {
	parent := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, parent)
	var bindings []ValidatedLetBinding
	for _, name := range names {
		ssym := syntax.NewSyntaxSymbol(name, nil)
		env.MaybeCreateLocalBinding(
			ssym.Sym,
			environment.BindingTypeVariable,
			nil,
			nil,
		)
		bindings = append(bindings, ValidatedLetBinding{
			Name: ssym,
			Init: lit(),
		})
	}
	return env, bindings
}

// symRef creates a ValidatedSymbol referencing the given name.
func symRef(name string) *ValidatedSymbol {
	return &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        syntax.NewSyntaxSymbol(name, nil),
	}
}

// lit creates a ValidatedLiteral (no sub-expressions).
func lit() *ValidatedLiteral {
	return &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
	}
}

// lam creates a ValidatedLambda with given body expressions and no params.
func lam(body ...ValidatedExpr) *ValidatedLambda {
	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{},
			body:   body,
		},
	}
}

// call creates a ValidatedCall.
func call(proc ValidatedExpr, args ...ValidatedExpr) *ValidatedCall {
	return &ValidatedCall{
		validatedBase: validatedBase{formName: "@call"},
		proc:          proc,
		args:          args,
	}
}

// caseLam creates a ValidatedCaseLambda with a single clause.
func caseLam(body ...ValidatedExpr) *ValidatedCaseLambda {
	return &ValidatedCaseLambda{
		validatedBase: validatedBase{formName: "case-lambda"},
		clauses: []*ValidatedCaseLambdaClause{{
			validatedBase: validatedBase{formName: "@clause"},
			validatedProcBase: validatedProcBase{
				params: &ValidatedParams{},
				body:   body,
			},
		}},
	}
}

// defineFn creates a ValidatedDefine in function form (body at depth+1).
func defineFn(name string, body ...ValidatedExpr) *ValidatedDefine {
	return &ValidatedDefine{
		validatedBase: validatedBase{formName: "define"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{},
			body:   body,
		},
		name:       syntax.NewSyntaxSymbol(name, nil),
		IsFunction: true,
	}
}

// defineVal creates a ValidatedDefine in value form (expr at current depth).
func defineVal(name string, expr ValidatedExpr) *ValidatedDefine {
	return &ValidatedDefine{
		validatedBase: validatedBase{formName: "define"},
		name:          syntax.NewSyntaxSymbol(name, nil),
		subExp:        expr,
		IsFunction:    false,
	}
}

// applyExpr creates a ValidatedApply.
func applyExpr(
	proc ValidatedExpr,
	prefixArgs []ValidatedExpr,
	finalList ValidatedExpr,
) *ValidatedApply {
	return &ValidatedApply{
		validatedBase: validatedBase{formName: "apply"},
		Proc:          proc,
		PrefixArgs:    prefixArgs,
		FinalList:     finalList,
	}
}

// nestedLet creates a ValidatedLet for nesting inside another let's body.
func nestedLet(bindings []ValidatedLetBinding, body ...ValidatedExpr) *ValidatedLet {
	return &ValidatedLet{
		validatedBase: validatedBase{formName: "let"},
		Kind:          LetKindLet,
		Bindings:      bindings,
		body:          body,
	}
}

// setBang creates a ValidatedSetBang targeting the given name.
func setBang(name string, expr ValidatedExpr) *ValidatedSetBang {
	return &ValidatedSetBang{
		validatedBase: validatedBase{formName: "set!"},
		Name:          syntax.NewSyntaxSymbol(name, nil),
		subExp:        expr,
	}
}
