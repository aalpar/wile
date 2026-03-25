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
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
)

// ValidatedExpr is the interface for all validated expressions.
// The canonical definition lives in [forms.ValidatedExpr] to break
// the validate → forms ← machine import cycle.
type ValidatedExpr = forms.ValidatedExpr

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

// validatedProcBase provides the shared params/body fields and accessors
// for procedure-like validated forms (lambda, define-function, case-lambda clause).
type validatedProcBase struct {
	params *ValidatedParams
	body   []ValidatedExpr
}

// Params returns the parameter list.
func (p *validatedProcBase) Params() *ValidatedParams {
	return p.params
}

// Body returns the body expressions.
func (p *validatedProcBase) Body() []ValidatedExpr {
	return p.body
}

// validatedBase provides the common FormName/SetFormName/Source implementation
// embedded by all ValidatedExpr types. This eliminates ~36 identical method
// definitions across the 13 validated form structs.
type validatedBase struct {
	formName string
	source   *syntax.SourceContext
}

// FormName returns the name of the form for error messages.
func (p *validatedBase) FormName() string {
	return p.formName
}

// SetFormName sets the form name for error messages.
func (p *validatedBase) SetFormName(nm string) {
	p.formName = nm
}

// Source returns the source context for error reporting.
func (p *validatedBase) Source() *syntax.SourceContext {
	return p.source
}

// ValidatedIf represents (if test conseq [alt])
type ValidatedIf struct {
	validatedBase
	Test   ValidatedExpr
	Conseq ValidatedExpr
	Alt    ValidatedExpr // nil if no alternative (will produce void)
}

// ValidatedDefine represents both forms:
// (define name expr) and (define (name params...) body...)
type ValidatedDefine struct {
	validatedBase
	validatedProcBase // params/body for function form, zero values for value form
	name              *syntax.SyntaxSymbol
	subExp            ValidatedExpr // For (define name expr), nil for function form
	IsFunction        bool          // True for (define (name ...) ...)
}

// Name returns the name being defined.
func (p *ValidatedDefine) Name() *syntax.SyntaxSymbol {
	return p.name
}

// SubExp returns the value expression for simple definitions.
func (p *ValidatedDefine) SubExp() ValidatedExpr {
	return p.subExp
}

// ValidatedLambda represents (lambda (params...) body...)
type ValidatedLambda struct {
	validatedBase
	validatedProcBase
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
	validatedBase
	Name   *syntax.SyntaxSymbol
	subExp ValidatedExpr
}

// SubExp returns the value expression to be assigned.
func (p *ValidatedSetBang) SubExp() ValidatedExpr {
	return p.subExp
}

// ValidatedQuote represents (quote datum)
type ValidatedQuote struct {
	validatedBase
	Datum syntax.SyntaxValue
}

// ValidatedBegin represents (begin expr...)
type ValidatedBegin struct {
	validatedBase
	body []ValidatedExpr
}

// Body returns the sequence of expressions in this begin form.
func (p *ValidatedBegin) Body() []ValidatedExpr {
	return p.body
}

// ValidatedCall represents (proc arg...)
type ValidatedCall struct {
	validatedBase
	proc ValidatedExpr
	args []ValidatedExpr
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
	validatedBase
	Symbol *syntax.SyntaxSymbol
}

// ValidatedLiteral represents self-evaluating data (numbers, strings, booleans, etc.)
// It's also used for passthrough forms like define-syntax, syntax-case, etc.
type ValidatedLiteral struct {
	validatedBase
	Value syntax.SyntaxValue
}

// newLiteralExpr creates a ValidatedLiteral wrapping a syntax value as a
// passthrough form. Used by structural validators and the passthrough registry.
func newLiteralExpr(source *syntax.SourceContext, value syntax.SyntaxValue) *ValidatedLiteral {
	return &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal", source: source},
		Value:         value,
	}
}

// ValidatedQuasiquote represents (quasiquote template)
type ValidatedQuasiquote struct {
	validatedBase
	Template syntax.SyntaxValue // The raw template - quasiquote has complex runtime semantics
}

// ValidatedCaseLambdaClause represents a single clause in case-lambda
type ValidatedCaseLambdaClause struct {
	validatedBase
	validatedProcBase
}

// ValidatedCaseLambda represents (case-lambda [clause] ...)
type ValidatedCaseLambda struct {
	validatedBase
	clauses []*ValidatedCaseLambdaClause
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
	validatedBase
	Before ValidatedExpr
	Thunk  ValidatedExpr
	After  ValidatedExpr
}

// ValidatedWithContinuationMark represents (with-continuation-mark key val body)
//
// Sets a continuation mark on the current frame during body evaluation.
// In tail position, the mark replaces any existing mark with the same key
// on the current frame. In non-tail position, the mark is removed after
// body completes.
type ValidatedWithContinuationMark struct {
	validatedBase
	Key  ValidatedExpr
	Val  ValidatedExpr
	Body ValidatedExpr
}

// ValidatedApply represents (apply proc arg1 ... args)
//
// R7RS §6.10: apply calls proc with the elements of the list
// (append (list arg1 ...) args) as arguments.
type ValidatedApply struct {
	validatedBase
	Proc       ValidatedExpr
	PrefixArgs []ValidatedExpr
	FinalList  ValidatedExpr
}

// --- Binding forms ---

// ValidatedLetBinding represents a single (name init-expr) binding pair.
// Mutable is true if the binding is targeted by set! in the body.
type ValidatedLetBinding struct {
	Name    *syntax.SyntaxSymbol
	Init    ValidatedExpr
	Mutable bool
}

// ValidatedLet represents (let ((name val) ...) body ...).
type ValidatedLet struct {
	validatedBase
	Bindings []ValidatedLetBinding
	body     []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLet) Body() []ValidatedExpr {
	return p.body
}

// ValidatedLetStar represents (let* ((name val) ...) body ...).
type ValidatedLetStar struct {
	validatedBase
	Bindings []ValidatedLetBinding
	body     []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLetStar) Body() []ValidatedExpr {
	return p.body
}

// ValidatedLetrec represents (letrec ((name val) ...) body ...)
// and (letrec* ((name val) ...) body ...).
// LetrecStar distinguishes the two: false = letrec, true = letrec*.
// Tag is non-nil for named let (compiled as letrec).
type ValidatedLetrec struct {
	validatedBase
	Bindings   []ValidatedLetBinding
	LetrecStar bool
	Tag        *syntax.SyntaxSymbol
	body       []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLetrec) Body() []ValidatedExpr {
	return p.body
}
