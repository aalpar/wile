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

package compilation

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// resolveCondExpandClause finds the first matching clause in a cond-expand form.
// Handles validation, registry lookup, feature requirement parsing, and clause
// matching. Returns the matched clause pair whose cdr contains the body forms.
func (p *CompileTimeContinuation) resolveCondExpandClause(ctx context.Context, args syntax.SyntaxValue) (*syntax.SyntaxPair, error) {
	if syntax.IsSyntaxEmptyList(args) {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNoMatchingClause, "cond-expand: no clauses"))
	}

	argsPair, ok := args.(*syntax.SyntaxPair)
	if !ok {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "cond-expand: expected list of clauses"))
	}

	var registry *LibraryRegistry
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		registry, _ = regAny.(*LibraryRegistry)
	}
	resolver := p.env.FileResolver()

	// A (library X) requirement is satisfied only if X actually imports, not
	// merely if its .sld resolves on disk (5F/P6). Probe via a real LoadLibrary
	// when we have the env + evaluator to do so; LoadLibrary caches on success,
	// so the subsequent real import is free. nil falls back to file existence.
	var load LibraryLoadProbe
	if p.env != nil && p.evaluator != nil {
		load = func(name LibraryName) bool {
			_, loadErr := LoadLibrary(ctx, name, p.env, p.evaluator)
			return loadErr == nil
		}
	}

	var matchedClause syntax.SyntaxValue
	_, err := syntax.SyntaxForEach(ctx, argsPair, func(_ context.Context, _ int, hasNext bool, clause syntax.SyntaxValue) error {
		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "cond-expand: clause must be a list"))
		}

		reqExpr := clausePair.SyntaxCar()

		// R7RS §4.2.1: else, if present, must be the last clause. This is a
		// structural constraint on the whole clause list, so it is checked on
		// every clause — before the matched-clause short-circuit below, which
		// would otherwise stop scanning once an earlier clause matches.
		if isElseClause(reqExpr) && hasNext {
			return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax,
				"cond-expand: else must be the last clause"))
		}

		if matchedClause != nil {
			return nil // Already found a match; keep scanning to validate else position.
		}

		req, err := parseFeatureRequirement(ctx, reqExpr)
		if err != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(err, "cond-expand: invalid feature requirement"))
		}

		if req.IsSatisfied(ctx, registry, resolver, load) {
			matchedClause = clausePair
		}

		return nil
	})
	if err != nil {
		return nil, err
	}

	if matchedClause == nil {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNoMatchingClause, "cond-expand: no matching clause"))
	}

	return matchedClause.(*syntax.SyntaxPair), nil
}

// CompileCondExpand compiles a cond-expand expression.
// cond-expand is evaluated at compile-time and expands to the body of the first
// clause whose feature requirement is satisfied.
//
// Syntax: (cond-expand <clause> ...)
// where <clause> is (<feature-requirement> <expression> ...)
//
// Example:
//
//	(cond-expand
//	  (r7rs (display "R7RS"))
//	  (else (display "other")))
func (p *CompileTimeContinuation) CompileCondExpand(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	matchedPair, err := p.resolveCondExpandClause(ctctx.ctx, expr)
	if err != nil {
		return err
	}

	// Compile the expressions in the matched clause
	bodyExpr := matchedPair.SyntaxCdr()
	if syntax.IsSyntaxEmptyList(bodyExpr) {
		// Empty body - emit void
		voidIdx := p.template.MaybeAppendLiteral(values.Void)
		p.AppendOperations(
			machine.NewOperationLoadLiteralByLiteralIndexImmediate(voidIdx),
		)
		return nil
	}

	bodyPair, ok := bodyExpr.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "cond-expand: expected list of expressions"))
	}

	// Expand and compile each body expression
	// (since cond-expand is not expanded, we must expand the body here)
	_, err = syntax.SyntaxForEach(ctctx.ctx, bodyPair, func(_ context.Context, _ int, hasNext bool, expr syntax.SyntaxValue) error {
		// Expand the expression
		expanded, expandErr := NewExpanderTimeContinuation(ctctx.ctx, p.env, p.evaluator).ExpandExpression(expr)
		if expandErr != nil {
			return p.wrapCompilationError(werr.WrapForeignErrorf(expandErr, "cond-expand: error expanding body expression"))
		}

		// Compile the expanded expression
		// Use the appropriate context for tail position (only last expression is in tail position)
		bodyCtx := ctctx
		if hasNext {
			bodyCtx = ctctx.NotInTail()
		}
		return p.CompileExpression(bodyCtx, expanded)
	})
	return err
}

// processCondExpand handles (cond-expand <clause> ...) within a library.
// Each clause is (<feature-requirement> <library-declaration> ...)
// The first clause whose feature requirement is satisfied has its declarations processed.
func (p *CompileTimeContinuation) processCondExpand(ctctx CompileTimeCallContext, lib *CompiledLibrary, args syntax.SyntaxValue) error {
	matchedPair, err := p.resolveCondExpandClause(ctctx.ctx, args)
	if err != nil {
		return err
	}

	// Process the declarations in the matched clause
	declsExpr := matchedPair.SyntaxCdr()
	if syntax.IsSyntaxEmptyList(declsExpr) {
		return nil // Empty clause body is valid
	}

	declsPair, ok := declsExpr.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAPair, "cond-expand: expected list of declarations"))
	}

	// Process each declaration
	_, err = syntax.SyntaxForEach(ctctx.ctx, declsPair, func(_ context.Context, _ int, _ bool, decl syntax.SyntaxValue) error {
		return p.processLibraryDeclaration(ctctx, lib, decl)
	})
	return err
}

// isElseClause reports whether a clause's feature-requirement expression is the
// literal symbol else. Used to enforce R7RS §4.2.1's else-must-be-last constraint.
func isElseClause(reqExpr syntax.SyntaxValue) bool {
	sym, ok := reqExpr.(*syntax.SyntaxSymbol)
	if !ok {
		return false
	}
	return sym.Key() == "else"
}

// parseFeatureRequirement parses a feature requirement expression.
// Feature requirements can be:
//   - <identifier> - simple feature check
//   - (library <library-name>) - library availability check
//   - (and <req> ...) - all requirements must be satisfied
//   - (or <req> ...) - at least one must be satisfied
//   - (not <req>) - must NOT be satisfied
//   - else - always satisfied (only valid as the last clause)
func parseFeatureRequirement(ctx context.Context, expr syntax.SyntaxValue) (FeatureRequirement, error) {
	switch v := expr.(type) {
	case *syntax.SyntaxSymbol:
		name := v.Key()
		if name == "else" {
			return NewElseRequirement(), nil
		}
		return NewFeatureIdentifier(name), nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "empty feature requirement"))
		}

		carExpr := v.SyntaxCar()
		carSym, ok := carExpr.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "feature requirement must start with symbol"))
		}

		keyword := carSym.Key()
		argsExpr := v.SyntaxCdr()
		// check keyword against known feature predicates
		switch keyword {
		case "library":
			// (library <library-name>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "library: expected library name"))
			}
			libNameExpr := argsPair.SyntaxCar()
			libName, err := ParseLibraryNameFromDatum(ctx, libNameExpr.UnwrapAll())
			if err != nil {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "library: invalid library name"))
			}
			return NewLibraryRequirement(libName), nil

		case "and":
			// (and <req> ...)
			reqs, err := parseFeatureRequirementList(ctx, argsExpr)
			if err != nil {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "and: invalid requirements"))
			}
			return NewAndRequirement(reqs...), nil

		case "or":
			// (or <req> ...)
			reqs, err := parseFeatureRequirementList(ctx, argsExpr)
			if err != nil {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "or: invalid requirements"))
			}
			return NewOrRequirement(reqs...), nil

		case "not":
			// (not <req>)
			argsPair, ok := argsExpr.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(argsPair) {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "not: expected one requirement"))
			}
			reqExpr := argsPair.SyntaxCar()
			req, err := parseFeatureRequirement(ctx, reqExpr)
			if err != nil {
				return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(err, "not: invalid requirement"))
			}
			return NewNotRequirement(req), nil

		default:
			return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unknown feature requirement keyword: %s", keyword))
		}

	default:
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrInvalidArgument, "invalid feature requirement type: %T", expr))
	}
}

// parseFeatureRequirementList parses a list of feature requirements.
func parseFeatureRequirementList(ctx context.Context, expr syntax.SyntaxValue) ([]FeatureRequirement, error) {
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}

	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(expr.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAPair, "expected list of requirements"))
	}

	var reqs []FeatureRequirement
	_, err := syntax.SyntaxForEach(ctx, pair, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		req, err := parseFeatureRequirement(ctx, v)
		if err != nil {
			return err
		}
		reqs = append(reqs, req)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return reqs, nil
}
