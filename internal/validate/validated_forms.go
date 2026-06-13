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
	Docstring() string
}

// validatedProcBase provides the shared params/body fields and accessors
// for procedure-like validated forms (lambda, define-function, case-lambda clause).
type validatedProcBase struct {
	params    *ValidatedParams
	body      []ValidatedExpr
	docstring string
}

// Params returns the parameter list.
func (p *validatedProcBase) Params() *ValidatedParams {
	return p.params
}

// Body returns the body expressions.
func (p *validatedProcBase) Body() []ValidatedExpr {
	return p.body
}

// Docstring returns the Guile-style docstring extracted from the body,
// or "" if no docstring was present.
func (p *validatedProcBase) Docstring() string {
	return p.docstring
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

	// StableInUnit reports that this define's name is defined exactly once and
	// never set! within the compilation unit the validator saw, computed
	// syntactically by symbol Key (conservative over-approximation: a same-Key
	// local set! or shadowing define marks it non-stable — a false match costs
	// optimization, never soundness). It is the in-unit evidence the compiler
	// consumes (only for top-level/global defines) to populate
	// BindingMeta.Stable when opt-in top-level immutability is enabled. Stamped
	// by finalizeStability after the whole unit is validated.
	StableInUnit bool
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

// LetKind encodes the two orthogonal dimensions of binding form semantics:
// init-visibility (do inits see the bindings?) and evaluation order
// (all-then-store vs sequential store-as-you-go).
//
//	| Kind       | Inits see bindings? | Eval order      |
//	|------------|---------------------|-----------------|
//	| Let        | No (outer scope)    | All-then-store  |
//	| LetStar    | Preceding only      | Sequential      |
//	| Letrec     | All (full scope)    | All-then-store  |
//	| LetrecStar | All (full scope)    | Sequential      |
type LetKind int

const (
	LetKindLet        LetKind = iota // R7RS §4.2.2: inits in outer scope, all-then-store
	LetKindLetStar                   // R7RS §4.2.2: inits see preceding, sequential
	LetKindLetrec                    // R7RS §4.2.2: inits in full scope, all-then-store
	LetKindLetrecStar                // R7RS §4.2.2: inits in full scope, sequential
)

// String returns the Scheme keyword for this kind.
func (p LetKind) String() string {
	switch p {
	case LetKindLet:
		return "let"
	case LetKindLetStar:
		return "let*"
	case LetKindLetrec:
		return "letrec"
	case LetKindLetrecStar:
		return "letrec*"
	default:
		return "let?"
	}
}

// InitsInScope reports whether init expressions see the let bindings.
func (p LetKind) InitsInScope() bool {
	return p == LetKindLetrec || p == LetKindLetrecStar
}

// Sequential reports whether init expressions are stored immediately
// (left-to-right) rather than all-then-store.
func (p LetKind) Sequential() bool {
	return p == LetKindLetStar || p == LetKindLetrecStar
}

// ValidatedLetBinding represents a single (name init-expr) binding pair.
// Mutable is true if the binding is targeted by set! in the body.
// Escapes is true if the binding is referenced in a non-call position.
type ValidatedLetBinding struct {
	Name    *syntax.SyntaxSymbol
	Init    ValidatedExpr
	Mutable bool
	Escapes bool
}

// ValidatedLet represents all four R7RS binding forms: let, let*, letrec,
// letrec*. The Kind field encodes which form this is — the type is shared
// because the four forms differ only in two orthogonal dimensions (init
// visibility and evaluation order), not in structure.
// Tag is non-nil for named let (compiled with letrec semantics).
type ValidatedLet struct {
	validatedBase
	Kind     LetKind
	Bindings []ValidatedLetBinding
	Tag      *syntax.SyntaxSymbol
	body     []ValidatedExpr
}

// NewValidatedLet constructs a ValidatedLet from pre-validated components.
// Used by the compiler for synthetic let forms (e.g., procedure inlining).
// Callers must ensure all bindings and body expressions are individually
// validated — this constructor does not re-validate its inputs.
//
// This is the only exported constructor in the validate package. Other
// validated types are constructed exclusively within the package. Add
// exported constructors for other types only when an external consumer
// requires them.
func NewValidatedLet(
	formName string,
	source *syntax.SourceContext,
	kind LetKind,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
) *ValidatedLet {
	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          kind,
		Bindings:      bindings,
		body:          body,
	}
}

// Body returns the body expressions.
func (p *ValidatedLet) Body() []ValidatedExpr {
	return p.body
}
