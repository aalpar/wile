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

package validate

import (
	"wile/syntax"
)

// ValidatedExpr is the interface for all validated expressions
type ValidatedExpr interface {
	SetFormName(name string)
	FormName() string
	validatedExpr()
	Source() *syntax.SourceContext // Original source for error messages
}

// ValidatedIf represents (if test conseq [alt])
type ValidatedIf struct {
	formName string
	source   *syntax.SourceContext
	Test     ValidatedExpr
	Conseq   ValidatedExpr
	Alt      ValidatedExpr // nil if no alternative (will produce void)
}

func (v *ValidatedIf) FormName() string      { return v.formName }
func (v *ValidatedIf) SetFormName(nm string) { v.formName = nm }
func (*ValidatedIf) validatedExpr()          {}
func (v *ValidatedIf) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedDefine represents both forms:
// (define name expr) and (define (name params...) body...)
type ValidatedDefine struct {
	formName   string
	source     *syntax.SourceContext
	Name       *syntax.SyntaxSymbol
	Value      ValidatedExpr    // For (define name expr), nil for function form
	IsFunction bool             // True for (define (name ...) ...)
	Params     *ValidatedParams // For function form, nil for value form
	Body       []ValidatedExpr  // For function form, nil for value form
}

func (v *ValidatedDefine) FormName() string      { return v.formName }
func (v *ValidatedDefine) SetFormName(nm string) { v.formName = nm }
func (*ValidatedDefine) validatedExpr()          {}
func (v *ValidatedDefine) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedLambda represents (lambda (params...) body...)
type ValidatedLambda struct {
	formName string
	source   *syntax.SourceContext
	Params   *ValidatedParams
	Body     []ValidatedExpr // At least one expression required
}

func (v *ValidatedLambda) FormName() string      { return v.formName }
func (v *ValidatedLambda) SetFormName(nm string) { v.formName = nm }
func (*ValidatedLambda) validatedExpr()          {}
func (v *ValidatedLambda) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedParams represents a parameter list
// Handles: (a b c), (a b . rest), and just rest
type ValidatedParams struct {
	formName string // for error reporting, does not have a form name
	Required []*syntax.SyntaxSymbol
	Rest     *syntax.SyntaxSymbol // nil if no rest parameter
}

// ValidatedSetBang represents (set! name expr)
type ValidatedSetBang struct {
	formName string
	source   *syntax.SourceContext
	Name     *syntax.SyntaxSymbol
	Value    ValidatedExpr
}

func (v *ValidatedSetBang) FormName() string      { return v.formName }
func (v *ValidatedSetBang) SetFormName(nm string) { v.formName = nm }
func (*ValidatedSetBang) validatedExpr()          {}
func (v *ValidatedSetBang) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedQuote represents (quote datum)
type ValidatedQuote struct {
	formName string
	source   *syntax.SourceContext
	Datum    syntax.SyntaxValue
}

func (v *ValidatedQuote) FormName() string      { return v.formName }
func (v *ValidatedQuote) SetFormName(nm string) { v.formName = nm }
func (*ValidatedQuote) validatedExpr()          {}
func (v *ValidatedQuote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedBegin represents (begin expr...)
type ValidatedBegin struct {
	formName string
	source   *syntax.SourceContext
	Exprs    []ValidatedExpr
}

func (v *ValidatedBegin) FormName() string      { return v.formName }
func (v *ValidatedBegin) SetFormName(nm string) { v.formName = nm }
func (*ValidatedBegin) validatedExpr()          {}
func (v *ValidatedBegin) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedCall represents (proc arg...)
type ValidatedCall struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	Proc     ValidatedExpr
	Args     []ValidatedExpr
}

func (v *ValidatedCall) FormName() string      { return v.formName }
func (v *ValidatedCall) SetFormName(nm string) { v.formName = nm }
func (*ValidatedCall) validatedExpr()          {}
func (v *ValidatedCall) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedSymbol represents a variable reference
type ValidatedSymbol struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	Symbol   *syntax.SyntaxSymbol
}

func (v *ValidatedSymbol) FormName() string      { return v.formName }
func (v *ValidatedSymbol) SetFormName(nm string) { v.formName = nm }
func (*ValidatedSymbol) validatedExpr()          {}
func (v *ValidatedSymbol) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedLiteral represents self-evaluating data (numbers, strings, booleans, etc.)
// It's also used for passthrough forms like define-syntax, syntax-case, etc.
type ValidatedLiteral struct {
	formName string
	source   *syntax.SourceContext
	Value    syntax.SyntaxValue
}

func (v *ValidatedLiteral) FormName() string      { return v.formName }
func (v *ValidatedLiteral) SetFormName(nm string) { v.formName = nm }
func (*ValidatedLiteral) validatedExpr()          {}
func (v *ValidatedLiteral) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedQuasiquote represents (quasiquote template)
type ValidatedQuasiquote struct {
	formName string
	source   *syntax.SourceContext
	Template syntax.SyntaxValue // The raw template - quasiquote has complex runtime semantics
}

func (v *ValidatedQuasiquote) FormName() string      { return v.formName }
func (v *ValidatedQuasiquote) SetFormName(nm string) { v.formName = nm }
func (*ValidatedQuasiquote) validatedExpr()          {}
func (v *ValidatedQuasiquote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedCaseLambdaClause represents a single clause in case-lambda
type ValidatedCaseLambdaClause struct {
	formName string
	Params   *ValidatedParams
	Body     []ValidatedExpr
}

// ValidatedCaseLambda represents (case-lambda [clause] ...)
type ValidatedCaseLambda struct {
	formName string
	source   *syntax.SourceContext
	Clauses  []*ValidatedCaseLambdaClause
}

func (v *ValidatedCaseLambda) FormName() string      { return v.formName }
func (v *ValidatedCaseLambda) SetFormName(nm string) { v.formName = nm }
func (*ValidatedCaseLambda) validatedExpr()          {}
func (v *ValidatedCaseLambda) Source() *syntax.SourceContext {
	return v.source
}
