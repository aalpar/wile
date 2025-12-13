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
	"wile/environment"
	"wile/syntax"
	"wile/values"
)

// RegisterPrimitiveExpanders binds all primitive expanders in the expand-time
// environment (env.Expand()). These are looked up by ExpandPrimitiveForm()
// when the expander encounters a special form.
//
// Each primitive has different expansion behavior:
//   - quote, define-syntax, quasiquote: return unchanged (no expansion)
//   - if: expand test, consequent, alternative separately
//   - begin: expand all subexpressions
//   - set!: expand only the value expression
//   - define: expand value if simple define
//   - lambda, case-lambda: expand body expressions
//   - syntax-case, cond-expand: return unchanged (compile-time forms)
func RegisterPrimitiveExpanders(env *environment.EnvironmentFrame) error {
	expandEnv := env.Expand()

	// All primitive expanders.
	// Each entry maps a keyword to its expand function.
	primitives := []struct {
		name string
		fn   PrimitiveExpanderFunc
	}{
		// Forms that return unchanged (no expansion)
		{"quote", (*ExpanderTimeContinuation).expandQuote},
		{"define-syntax", (*ExpanderTimeContinuation).expandDefineSyntax},
		{"quasiquote", (*ExpanderTimeContinuation).expandQuasiquote},
		{"unquote", (*ExpanderTimeContinuation).expandUnquote},
		{"unquote-splicing", (*ExpanderTimeContinuation).expandUnquoteSplicing},
		{"include", (*ExpanderTimeContinuation).expandInclude},
		{"include-ci", (*ExpanderTimeContinuation).expandIncludeCi},
		{"cond-expand", (*ExpanderTimeContinuation).expandCondExpand},
		{"syntax", (*ExpanderTimeContinuation).expandSyntaxForm},
		{"syntax-case", (*ExpanderTimeContinuation).expandSyntaxCase},
		{"quasisyntax", (*ExpanderTimeContinuation).expandQuasisyntax},
		{"unsyntax", (*ExpanderTimeContinuation).expandUnsyntax},
		{"unsyntax-splicing", (*ExpanderTimeContinuation).expandUnsyntaxSplicing},
		{"with-syntax", (*ExpanderTimeContinuation).expandWithSyntax},

		// Forms that expand their subexpressions
		{"if", (*ExpanderTimeContinuation).expandIfForm},
		{"begin", (*ExpanderTimeContinuation).expandBeginForm},
		{"set!", (*ExpanderTimeContinuation).expandSetForm},
		{"define", (*ExpanderTimeContinuation).expandDefineForm},
		{"lambda", (*ExpanderTimeContinuation).expandLambdaForm},
		{"case-lambda", (*ExpanderTimeContinuation).expandCaseLambdaForm},
	}

	for _, prim := range primitives {
		sym := env.InternSymbol(values.NewSymbol(prim.name))
		expander := NewPrimitiveExpander(prim.name, prim.fn)

		// Create binding in expand environment
		idx, _ := expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
		expandEnv.SetOwnGlobalValue(idx, expander) //nolint:errcheck
	}

	return nil
}

// LookupPrimitiveExpander looks up a primitive expander by symbol in the expand
// environment. Returns the PrimitiveExpander if found, or nil if the symbol does
// not name a primitive expander.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupPrimitiveExpander(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *PrimitiveExpander {
	expandEnv := env.Expand()

	// Look up with scopes for hygiene
	bnd := expandEnv.GetBindingWithScopes(sym, scopes)
	if bnd == nil {
		return nil
	}

	// Check if it's a primitive expander binding
	if bnd.BindingType() != environment.BindingTypePrimitive {
		return nil
	}

	// Get the value and check if it's a PrimitiveExpander
	val := bnd.Value()
	if pe, ok := val.(*PrimitiveExpander); ok {
		return pe
	}

	return nil
}
