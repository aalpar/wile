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

package machine

// compile_let.go compiles all four binding forms (let, let*, letrec, letrec*)
// through a single entry point. The ValidatedLet.Kind field determines which
// bytecode pattern is emitted.
//
// Two orthogonal dimensions:
//   - Init compilation env: before OpPushEnv (let) vs after (let*, letrec, letrec*)
//   - Store order: all-then-store (let, letrec) vs sequential (let*, letrec*)

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/validate"
)

// CompileValidatedLet compiles all binding forms based on Kind.
//
// let:     <inits> Push... | OpPushEnv | StoreLocal(reverse) | body | OpPopEnv
// let*:    OpPushEnv | (init Push StoreLocal)... | body | OpPopEnv
// letrec:  OpPushEnv | <inits> Push... | StoreLocal(reverse) | body | OpPopEnv
// letrec*: OpPushEnv | (init Push StoreLocal)... | body | OpPopEnv
func (p *CompileTimeContinuation) CompileValidatedLet(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLet,
) error {
	n := len(v.Bindings)

	if n == 0 {
		return p.compileLetBody(ctctx, v.Body())
	}

	// For plain let: compile inits BEFORE creating the env frame
	// (inits don't see bindings, so they're compiled in the parent env).
	if v.Kind == validate.LetKindLet {
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			p.AppendOperations(NewOperationPush())
		}
	}

	// Build compile-time env with all bindings.
	childEnv := p.createLetCompileEnv(v)

	savedEnv := p.env
	p.env = childEnv

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())
	totalSlots := len(childEnv.LocalEnvironment().Bindings())

	p.AppendOperations(NewOperationPushEnv(totalSlots))

	// Emit init compilation + stores based on Kind.
	var err error
	switch v.Kind {
	case validate.LetKindLet:
		// Inits already on stack — store in reverse (LIFO).
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetStar, validate.LetKindLetrecStar:
		// Sequential: compile init, push, store — one at a time.
		for _, b := range v.Bindings {
			err = p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				p.env = savedEnv
				return err
			}
			li := childEnv.GetLocalIndex(b.Name.Sym)
			p.AppendOperations(NewOperationPush())
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetrec:
		// Delayed assignment: compile all inits, push all, then store all.
		for _, b := range v.Bindings {
			err = p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				p.env = savedEnv
				return err
			}
			p.AppendOperations(NewOperationPush())
		}
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}
	}

	err = p.compileValidatedSequence(ctctx, v.Body())
	p.env = savedEnv

	if err != nil {
		return err
	}

	if !ctctx.inTail {
		p.AppendOperations(NewOperationPopEnv())
	}

	return nil
}

// createLetCompileEnv creates a compile-time child environment with all
// let bindings as local variables.
func (p *CompileTimeContinuation) createLetCompileEnv(
	v *validate.ValidatedLet,
) *environment.EnvironmentFrame {
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

	// For let*: add ALL binding names upfront. The validator already
	// enforced sequential visibility; the compiler only needs the slots.
	for _, b := range v.Bindings {
		childEnv.MaybeCreateLocalBindingWithScopes(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
	}
	return childEnv
}

// predeclareBodyDefines scans the body for define forms and pre-creates
// their bindings in the current compile-time env.
func (p *CompileTimeContinuation) predeclareBodyDefines(
	body []validate.ValidatedExpr,
) {
	for _, expr := range body {
		p.predeclareDefineBindingFromValidated(expr)
	}
}

// compileLetBody compiles a sequence of body expressions with
// letrec* pre-declaration and tail position semantics.
// Used only for empty-binding cases where no OpPushEnv is needed.
func (p *CompileTimeContinuation) compileLetBody(
	ctctx CompileTimeCallContext,
	body []validate.ValidatedExpr,
) error {
	for _, expr := range body {
		p.predeclareDefineBindingFromValidated(expr)
	}
	return p.compileValidatedSequence(ctctx, body)
}
