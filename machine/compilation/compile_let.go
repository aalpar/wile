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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/werr"
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

	// For plain let: compile inits BEFORE creating the env frame
	// (inits don't see bindings, so they're compiled in the parent env).
	if n > 0 && v.Kind == validate.LetKindLet {
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			p.AppendOperations(machine.NewOperationPush())
		}
	}

	// Build compile-time env with all bindings (even for empty-binding
	// case, so that internal defines get their own scope boundary).
	childEnv := p.createLetCompileEnv(v)

	savedEnv := p.env
	p.env = childEnv
	defer func() {
		p.env = savedEnv
	}()

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())
	totalSlots := len(childEnv.LocalEnvironment().Bindings())

	p.AppendOperations(machine.NewOperationPushEnv(totalSlots))

	// Emit init compilation + stores based on Kind.
	switch v.Kind {
	case validate.LetKindLet:
		// Inits already on stack — store in reverse (LIFO).
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile let: binding %q not found in local environment",
					v.Bindings[i].Name.Sym)
			}
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetStar, validate.LetKindLetrecStar:
		// Sequential: compile init, push, store — one at a time.
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			li := childEnv.GetLocalIndex(b.Name.Sym)
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile %s: binding %q not found in local environment",
					v.Kind, b.Name.Sym)
			}
			p.AppendOperations(machine.NewOperationPush())
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetrec:
		// Delayed assignment: compile all inits, push all, then store all.
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			p.AppendOperations(machine.NewOperationPush())
		}
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile letrec: binding %q not found in local environment",
					v.Bindings[i].Name.Sym)
			}
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}
	}

	err := p.compileValidatedSequence(ctctx, v.Body())
	if err != nil {
		return err
	}

	if !ctctx.inTail {
		p.AppendOperations(machine.NewOperationPopEnv())
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
// their bindings in the current compile-time env. Unwraps begin blocks
// to find defines from macro-expanded forms like define-values.
func (p *CompileTimeContinuation) predeclareBodyDefines(
	body []validate.ValidatedExpr,
) {
	for _, expr := range body {
		p.predeclareDefineFromValidatedRecursive(expr)
	}
}

// predeclareDefineFromValidatedRecursive pre-creates bindings for defines,
// recursing into begin blocks to find defines from macro expansions
// (e.g., define-values expands to (begin (define ...) ...)).
func (p *CompileTimeContinuation) predeclareDefineFromValidatedRecursive(
	expr validate.ValidatedExpr,
) {
	switch v := expr.(type) {
	case *validate.ValidatedDefine:
		predeclareBinding(p.env, v.Name().Sym, v.Name().Scopes(), v.Name().SourceContext())
	case *validate.ValidatedBegin:
		for _, sub := range v.Body() {
			p.predeclareDefineFromValidatedRecursive(sub)
		}
	}
}
