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
	"github.com/aalpar/wile/internal/syntax"
)

// ValidatedExpr is the interface for all validated expressions
type ValidatedExpr interface {
	SetFormName(name string)
	FormName() string
	Source() *syntax.SourceContext // Original source for error messages
}

// ValidatedProcedure represents a validated procedure form with parameters and body.
type ValidatedProcedure interface {
	ValidatedExpr
	ValidatedBodyAndParams
}

// ValidatedBodyAndParams provides access to parameters and body for procedure forms.
type ValidatedBodyAndParams interface {
	Params() *ValidatedParams
	Body() []ValidatedExpr
}

// ValidatedIf represents (if test conseq [alt])
type ValidatedIf struct {
	formName string
	source   *syntax.SourceContext
	Test     ValidatedExpr
	Conseq   ValidatedExpr
	Alt      ValidatedExpr // nil if no alternative (will produce void)
}

// FormName returns the name of the form for error messages.
func (p *ValidatedIf) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedIf) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedIf) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedDefine represents both forms:
// (define name expr) and (define (name params...) body...)
type ValidatedDefine struct {
	formName   string
	source     *syntax.SourceContext
	params     *ValidatedParams // For function form, nil for value form
	body       []ValidatedExpr  // For function form, nil for value form
	name       *syntax.SyntaxSymbol
	subExp     ValidatedExpr // For (define name expr), nil for function form
	IsFunction bool          // True for (define (name ...) ...)
}

// Name returns the name being defined.
func (p *ValidatedDefine) Name() *syntax.SyntaxSymbol {
	return p.name
}

// FormName returns the name of the form for error messages.
func (p *ValidatedDefine) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedDefine) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedDefine) Source() *syntax.SourceContext {
	return p.source
}

// Params returns the parameter list for function definitions.
func (p *ValidatedDefine) Params() *ValidatedParams {
	return p.params
}

// Body returns the body expressions for function definitions.
func (p *ValidatedDefine) Body() []ValidatedExpr {
	return p.body
}

// SubExp returns the value expression for simple definitions.
func (p *ValidatedDefine) SubExp() ValidatedExpr {
	return p.subExp
}

// ValidatedLambda represents (lambda (params...) body...)
type ValidatedLambda struct {
	formName string
	source   *syntax.SourceContext
	params   *ValidatedParams
	body     []ValidatedExpr // At least one expression required
}

// FormName returns the name of the form for error messages.
func (p *ValidatedLambda) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedLambda) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedLambda) Source() *syntax.SourceContext {
	return p.source
}

// Params returns the parameter list for this lambda.
func (p *ValidatedLambda) Params() *ValidatedParams {
	return p.params
}

// Body returns the body expressions for this lambda.
func (p *ValidatedLambda) Body() []ValidatedExpr {
	return p.body
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
	subExp   ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (p *ValidatedSetBang) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedSetBang) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedSetBang) Source() *syntax.SourceContext {
	return p.source
}

// SubExp returns the value expression to be assigned.
func (p *ValidatedSetBang) SubExp() ValidatedExpr {
	return p.subExp
}

// ValidatedQuote represents (quote datum)
type ValidatedQuote struct {
	formName string
	source   *syntax.SourceContext
	Datum    syntax.SyntaxValue
}

// FormName returns the name of the form for error messages.
func (p *ValidatedQuote) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedQuote) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedQuote) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedBegin represents (begin expr...)
type ValidatedBegin struct {
	formName string
	source   *syntax.SourceContext
	body     []ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (p *ValidatedBegin) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedBegin) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedBegin) Source() *syntax.SourceContext {
	return p.source
}

// Body returns the sequence of expressions in this begin form.
func (p *ValidatedBegin) Body() []ValidatedExpr {
	return p.body
}

// ValidatedCall represents (proc arg...)
type ValidatedCall struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	proc     ValidatedExpr
	args     []ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (p *ValidatedCall) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedCall) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedCall) Source() *syntax.SourceContext {
	return p.source
}

// Proc returns the procedure expression being called.
func (p *ValidatedCall) Proc() ValidatedExpr {
	return p.proc
}

// Body returns the argument expressions for this call.
func (p *ValidatedCall) Body() []ValidatedExpr {
	return p.args
}

// ValidatedSymbol represents a variable reference
type ValidatedSymbol struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	Symbol   *syntax.SyntaxSymbol
}

// FormName returns the name of the form for error messages.
func (p *ValidatedSymbol) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedSymbol) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedSymbol) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedLiteral represents self-evaluating data (numbers, strings, booleans, etc.)
// It's also used for passthrough forms like define-syntax, syntax-case, etc.
type ValidatedLiteral struct {
	formName string
	source   *syntax.SourceContext
	Value    syntax.SyntaxValue
}

// FormName returns the name of the form for error messages.
func (p *ValidatedLiteral) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedLiteral) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedLiteral) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedQuasiquote represents (quasiquote template)
type ValidatedQuasiquote struct {
	formName string
	source   *syntax.SourceContext
	Template syntax.SyntaxValue // The raw template - quasiquote has complex runtime semantics
}

// FormName returns the name of the form for error messages.
func (p *ValidatedQuasiquote) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedQuasiquote) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedQuasiquote) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedCaseLambdaClause represents a single clause in case-lambda
type ValidatedCaseLambdaClause struct {
	formName string
	params   *ValidatedParams
	body     []ValidatedExpr
}

// Params returns the parameter list for this clause.
func (p *ValidatedCaseLambdaClause) Params() *ValidatedParams {
	return p.params
}

// FormName returns the name of the form for error messages.
func (p *ValidatedCaseLambdaClause) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedCaseLambdaClause) SetFormName(nm string) {
	p.formName = nm
}

// Body returns the body expressions for this clause.
func (p *ValidatedCaseLambdaClause) Body() []ValidatedExpr {
	return p.body
}

// ValidatedCaseLambda represents (case-lambda [clause] ...)
type ValidatedCaseLambda struct {
	formName string
	source   *syntax.SourceContext
	clauses  []*ValidatedCaseLambdaClause
}

// FormName returns the name of the form for error messages.
func (p *ValidatedCaseLambda) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedCaseLambda) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedCaseLambda) Source() *syntax.SourceContext {
	return p.source
}

// Clauses returns the list of case-lambda clauses.
func (p *ValidatedCaseLambda) Clauses() []*ValidatedCaseLambdaClause {
	return p.clauses
}

// ValidatedDynamicWind represents (dynamic-wind before thunk after)
//
// R7RS §6.10: dynamic-wind calls thunk without arguments, returning the result(s).
// Before is called whenever execution enters the dynamic extent of the call to thunk,
// and after is called whenever it exits.
type ValidatedDynamicWind struct {
	formName string
	source   *syntax.SourceContext
	Before   ValidatedExpr
	Thunk    ValidatedExpr
	After    ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (p *ValidatedDynamicWind) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *ValidatedDynamicWind) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *ValidatedDynamicWind) Source() *syntax.SourceContext {
	return p.source
}
