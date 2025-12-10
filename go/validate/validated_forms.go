package validate

import (
	"wile/syntax"
)

// ValidatedExpr is the interface for all validated expressions
type ValidatedExpr interface {
	validatedExpr()
	Source() *syntax.SourceContext // Original source for error messages
}

// ValidatedIf represents (if test conseq [alt])
type ValidatedIf struct {
	source *syntax.SourceContext
	Test   ValidatedExpr
	Conseq ValidatedExpr
	Alt    ValidatedExpr // nil if no alternative (will produce void)
}

func (*ValidatedIf) validatedExpr() {}
func (v *ValidatedIf) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedDefine represents both forms:
// (define name expr) and (define (name params...) body...)
type ValidatedDefine struct {
	source     *syntax.SourceContext
	Name       *syntax.SyntaxSymbol
	Value      ValidatedExpr     // For (define name expr), nil for function form
	IsFunction bool              // True for (define (name ...) ...)
	Params     *ValidatedParams  // For function form, nil for value form
	Body       []ValidatedExpr   // For function form, nil for value form
}

func (*ValidatedDefine) validatedExpr() {}
func (v *ValidatedDefine) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedLambda represents (lambda (params...) body...)
type ValidatedLambda struct {
	source *syntax.SourceContext
	Params *ValidatedParams
	Body   []ValidatedExpr // At least one expression required
}

func (*ValidatedLambda) validatedExpr() {}
func (v *ValidatedLambda) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedParams represents a parameter list
// Handles: (a b c), (a b . rest), and just rest
type ValidatedParams struct {
	Required []*syntax.SyntaxSymbol
	Rest     *syntax.SyntaxSymbol // nil if no rest parameter
}

// ValidatedSetBang represents (set! name expr)
type ValidatedSetBang struct {
	source *syntax.SourceContext
	Name   *syntax.SyntaxSymbol
	Value  ValidatedExpr
}

func (*ValidatedSetBang) validatedExpr() {}
func (v *ValidatedSetBang) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedQuote represents (quote datum)
type ValidatedQuote struct {
	source *syntax.SourceContext
	Datum  syntax.SyntaxValue
}

func (*ValidatedQuote) validatedExpr() {}
func (v *ValidatedQuote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedBegin represents (begin expr...)
type ValidatedBegin struct {
	source *syntax.SourceContext
	Exprs  []ValidatedExpr
}

func (*ValidatedBegin) validatedExpr() {}
func (v *ValidatedBegin) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedCall represents (proc arg...)
type ValidatedCall struct {
	source *syntax.SourceContext
	Proc   ValidatedExpr
	Args   []ValidatedExpr
}

func (*ValidatedCall) validatedExpr() {}
func (v *ValidatedCall) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedSymbol represents a variable reference
type ValidatedSymbol struct {
	source *syntax.SourceContext
	Symbol *syntax.SyntaxSymbol
}

func (*ValidatedSymbol) validatedExpr() {}
func (v *ValidatedSymbol) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedLiteral represents self-evaluating data (numbers, strings, booleans, etc.)
type ValidatedLiteral struct {
	source *syntax.SourceContext
	Value  syntax.SyntaxValue
}

func (*ValidatedLiteral) validatedExpr() {}
func (v *ValidatedLiteral) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedQuasiquote represents (quasiquote template)
type ValidatedQuasiquote struct {
	source   *syntax.SourceContext
	Template syntax.SyntaxValue // The raw template - quasiquote has complex runtime semantics
}

func (*ValidatedQuasiquote) validatedExpr() {}
func (v *ValidatedQuasiquote) Source() *syntax.SourceContext {
	return v.source
}

// ValidatedCaseLambdaClause represents a single clause in case-lambda
type ValidatedCaseLambdaClause struct {
	Params *ValidatedParams
	Body   []ValidatedExpr
}

// ValidatedCaseLambda represents (case-lambda [clause] ...)
type ValidatedCaseLambda struct {
	source  *syntax.SourceContext
	Clauses []*ValidatedCaseLambdaClause
}

func (*ValidatedCaseLambda) validatedExpr() {}
func (v *ValidatedCaseLambda) Source() *syntax.SourceContext {
	return v.source
}
