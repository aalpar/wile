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
func (v *ValidatedIf) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedIf) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedIf) Source() *syntax.SourceContext {
	return v.source
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
func (v *ValidatedDefine) Name() *syntax.SyntaxSymbol {
	return v.name
}

// FormName returns the name of the form for error messages.
func (v *ValidatedDefine) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedDefine) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedDefine) Source() *syntax.SourceContext {
	return v.source
}

// Params returns the parameter list for function definitions.
func (v *ValidatedDefine) Params() *ValidatedParams {
	return v.params
}

// Body returns the body expressions for function definitions.
func (v *ValidatedDefine) Body() []ValidatedExpr {
	return v.body
}

// SubExp returns the value expression for simple definitions.
func (v *ValidatedDefine) SubExp() ValidatedExpr {
	return v.subExp
}

// ValidatedLambda represents (lambda (params...) body...)
type ValidatedLambda struct {
	formName string
	source   *syntax.SourceContext
	params   *ValidatedParams
	body     []ValidatedExpr // At least one expression required
}

// FormName returns the name of the form for error messages.
func (v *ValidatedLambda) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedLambda) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedLambda) Source() *syntax.SourceContext {
	return v.source
}

// Params returns the parameter list for this lambda.
func (v *ValidatedLambda) Params() *ValidatedParams {
	return v.params
}

// Body returns the body expressions for this lambda.
func (v *ValidatedLambda) Body() []ValidatedExpr {
	return v.body
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
func (v *ValidatedSetBang) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedSetBang) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedSetBang) Source() *syntax.SourceContext {
	return v.source
}

// SubExp returns the value expression to be assigned.
func (v *ValidatedSetBang) SubExp() ValidatedExpr {
	return v.subExp
}

// ValidatedQuote represents (quote datum)
type ValidatedQuote struct {
	formName string
	source   *syntax.SourceContext
	Datum    syntax.SyntaxValue
}

// FormName returns the name of the form for error messages.
func (v *ValidatedQuote) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedQuote) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedQuote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedBegin represents (begin expr...)
type ValidatedBegin struct {
	formName string
	source   *syntax.SourceContext
	body     []ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (v *ValidatedBegin) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedBegin) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedBegin) Source() *syntax.SourceContext {
	return v.source
}

// Body returns the sequence of expressions in this begin form.
func (v *ValidatedBegin) Body() []ValidatedExpr {
	return v.body
}

// ValidatedCall represents (proc arg...)
type ValidatedCall struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	proc     ValidatedExpr
	args     []ValidatedExpr
}

// FormName returns the name of the form for error messages.
func (v *ValidatedCall) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedCall) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedCall) Source() *syntax.SourceContext {
	return v.source
}

// Proc returns the procedure expression being called.
func (v *ValidatedCall) Proc() ValidatedExpr {
	return v.proc
}

// Body returns the argument expressions for this call.
func (v *ValidatedCall) Body() []ValidatedExpr {
	return v.args
}

// ValidatedSymbol represents a variable reference
type ValidatedSymbol struct {
	formName string // does not have a specific form name
	source   *syntax.SourceContext
	Symbol   *syntax.SyntaxSymbol
}

// FormName returns the name of the form for error messages.
func (v *ValidatedSymbol) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedSymbol) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
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

// FormName returns the name of the form for error messages.
func (v *ValidatedLiteral) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedLiteral) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedLiteral) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedQuasiquote represents (quasiquote template)
type ValidatedQuasiquote struct {
	formName string
	source   *syntax.SourceContext
	Template syntax.SyntaxValue // The raw template - quasiquote has complex runtime semantics
}

// FormName returns the name of the form for error messages.
func (v *ValidatedQuasiquote) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedQuasiquote) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedQuasiquote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedCaseLambdaClause represents a single clause in case-lambda
type ValidatedCaseLambdaClause struct {
	formName string
	params   *ValidatedParams
	body     []ValidatedExpr
}

// Params returns the parameter list for this clause.
func (v *ValidatedCaseLambdaClause) Params() *ValidatedParams {
	return v.params
}

// FormName returns the name of the form for error messages.
func (v *ValidatedCaseLambdaClause) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedCaseLambdaClause) SetFormName(nm string) {
	v.formName = nm
}

// Body returns the body expressions for this clause.
func (v *ValidatedCaseLambdaClause) Body() []ValidatedExpr {
	return v.body
}

// ValidatedCaseLambda represents (case-lambda [clause] ...)
type ValidatedCaseLambda struct {
	formName string
	source   *syntax.SourceContext
	clauses  []*ValidatedCaseLambdaClause
}

// FormName returns the name of the form for error messages.
func (v *ValidatedCaseLambda) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedCaseLambda) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedCaseLambda) Source() *syntax.SourceContext {
	return v.source
}

// Clauses returns the list of case-lambda clauses.
func (v *ValidatedCaseLambda) Clauses() []*ValidatedCaseLambdaClause {
	return v.clauses
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
func (v *ValidatedDynamicWind) FormName() string {
	return v.formName
}

// SetFormName sets the form name for error messages.
func (v *ValidatedDynamicWind) SetFormName(nm string) {
	v.formName = nm
}

// Source returns the source context for error reporting.
func (v *ValidatedDynamicWind) Source() *syntax.SourceContext {
	return v.source
}
