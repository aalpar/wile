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

// Package validate validates Scheme syntax and produces typed expressions.
//
// The package converts syntax.SyntaxValue into typed ValidatedExpr values
// for the compiler, accumulating multiple errors without short-circuiting:
//
//	result := validate.ValidateExpression(ctx, env, expr)
//	if !result.Ok() {
//	    for _, err := range result.Errors {
//	        fmt.Println(err)
//	    }
//	}
//	validated := result.Expr
//
// # Validated Types
//
//   - [ValidatedIf]: (if test conseq [alt])
//   - [ValidatedDefine]: (define name expr) or (define (name params...) body...)
//   - [ValidatedLambda]: (lambda (params...) body...)
//   - [ValidatedCaseLambda]: (case-lambda [clause] ...)
//   - [ValidatedCaseLambdaClause]: one clause of a case-lambda
//   - [ValidatedSetBang]: (set! name expr)
//   - [ValidatedQuote]: (quote datum)
//   - [ValidatedQuasiquote]: (quasiquote template)
//   - [ValidatedBegin]: (begin expr...)
//   - [ValidatedLet]: let, let*, letrec, letrec*, and named let (Kind selects)
//   - [ValidatedDynamicWind]: (dynamic-wind before thunk after)
//   - [ValidatedWithContinuationMark]: (with-continuation-mark key val body)
//   - [ValidatedApply]: (apply proc arg... args)
//   - [ValidatedCall]: (proc arg...)
//   - [ValidatedSymbol]: variable reference
//   - [ValidatedLiteral]: self-evaluating or passthrough
//
// # Hygiene
//
// The validator supports local variable shadowing of special forms (R7RS 4.2.2)
// using Flatt's scope-set model for identifier resolution.
package validate
