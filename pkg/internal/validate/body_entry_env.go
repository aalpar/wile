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
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// body_entry_env.go remembers the environment a body STARTED in, so that
// headDenotesSpecialForm can tell a binding the body's own define introduces
// from one an enclosing binding form already held.
//
// THE QUESTION IT ANSWERS. headDenotesSpecialForm exempts a definition's own
// head — (define define 3) is a definition, not a call, per R7RS §5.3.1 p.26,
// which contemplates defining a variable whose name is a syntactic keyword. The
// exemption is asked by resolving the definiendum and comparing bindings, and
// inside (let ((define f)) (define define 2)) that comparison SUCCEEDS for the
// wrong reason: the definiendum resolves onto the LET's binding, not onto one
// this define establishes, so a call gets re-read as a definition. Measured
// 2026-08-11: Petite Chez 10.4.1 and Racket 9.2 both answer `called`, and so did
// Wile master.
//
// WHY THE ENTRY ENV IS THE DISCRIMINATOR. "Is b local?" is not enough, because
// bindBodyDefineNames fabricates a frame for the body's OWN defines as the loop
// runs — after (define define 1), a following (define define 2) resolves its
// head to a local binding too, and that one IS the body's, so it keeps the
// exemption and stays a redefinition. The entry env is the chain BEFORE any of
// that, so a local hit there names a binder the body inherited: a let binder, a
// lambda parameter.
//
// THE LOCAL CHAIN ONLY. GetLocalIndex walks the lexical parents, which is wanted
// — an enclosing let three frames out still owns its binder. A GLOBAL hit is
// deliberately not consulted: the expander's top-level letrec* pre-scan puts a
// define's binder in the global environment before validation starts, so a
// global hit says nothing about whether the binding predates the body. That
// direction is P1's job (BindingMeta.Predeclared), not this file's.
//
// THE CARRIER IS ctx, not ValidationResult. The record is LEXICAL: every head
// validated inside the body must see it at any depth, and it must revert when
// the body ends. context.WithValue has exactly that shape — a child body derives
// a new ctx and the parent keeps its own, so nothing is restored on the way out.
// ValidationResult is threaded just as widely but is a single accumulator for
// the whole ValidateExpression call, so it would need a push/pop per body.
//
// Only the INNERMOST record is consulted. An outer body's entry env would answer
// about a scope this form is no longer in.
type bodyEntryKey struct{}

// withBodyEntryEnv records env as the environment the body about to be validated
// began in.
//
// A nil env leaves ctx alone: this package's callers routinely validate with no
// environment at all, and an entry record that cannot resolve anything would
// only shadow a usable outer one.
func withBodyEntryEnv(ctx context.Context, env *environment.EnvironmentFrame) context.Context {
	if env == nil {
		return ctx
	}
	return context.WithValue(ctx, bodyEntryKey{}, env)
}

// bodyBindingPredatesBody reports whether b was already reachable as a LOCAL
// binding when the innermost body in scope began — i.e. it belongs to an
// enclosing binding form rather than to a define of this body.
//
// The resolution runs here rather than at the caller because a local *Binding is
// a pointer into a frame's []Binding and EnsureLocalBinding can reallocate it:
// a *Binding is an identity only inside a window that creates no bindings, and
// this call is inside headDenotesSpecialForm's window.
func bodyBindingPredatesBody(
	ctx context.Context,
	symVal *values.Symbol,
	scopes []*syntax.Scope,
	b *environment.Binding,
) bool {
	entry, _ := ctx.Value(bodyEntryKey{}).(*environment.EnvironmentFrame)
	if entry == nil {
		return false
	}
	li := entry.GetLocalIndex(symVal, syntax.ScopesOf(scopes))
	if li == nil {
		return false
	}
	return entry.GetLocalBinding(li) == b
}
