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

// compile_let.go compiles let, let*, letrec, and letrec* binding forms.
//
// Each form uses OpPushEnv/StoreLocal/OpPopEnv to create local binding slots
// directly, bypassing the closure+apply overhead of the old macro expansion.
//
// The slot count for OpPushEnv must account for BOTH the let bindings AND
// any internal defines in the body (R7RS §5.3.2 letrec* semantics). We
// pre-scan the body for defines and include them in the total before emitting
// OpPushEnv. This mirrors how lambda's Apply copies the full compile-time env
// structure (including predeclared define slots) via InitApplyFrame.

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/validate"
)

// CompileValidatedLet compiles (let ((name val) ...) body ...).
//
// Bytecode:
//
//	<compile init-1> Push    ; init exprs in parent env
//	<compile init-2> Push
//	OpPushEnv(T)             ; new env frame with T slots (bindings + defines)
//	StoreLocal name-N        ; pop from stack into slots (LIFO)
//	...
//	StoreLocal name-1
//	<compile body>           ; last expr inherits tail position
//	OpPopEnv                 ; only if let is NOT in tail position
func (p *CompileTimeContinuation) CompileValidatedLet(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLet,
) error {
	n := len(v.Bindings)

	if n == 0 {
		return p.compileLetBody(ctctx, v.Body())
	}

	// Phase 1: Compile all init expressions in the CURRENT env and push to stack.
	for _, b := range v.Bindings {
		err := p.compileValidated(ctctx.NotInTail(), b.Init)
		if err != nil {
			return err
		}
		p.AppendOperations(NewOperationPush())
	}

	// Phase 2: Build compile-time env with bindings + body defines.
	childEnv := p.createLetCompileEnv(v.Bindings)
	savedEnv := p.env
	p.env = childEnv

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())
	totalSlots := len(childEnv.LocalEnvironment().Bindings())

	p.AppendOperations(NewOperationPushEnv(totalSlots))

	// Store values from stack into local slots (reverse order — LIFO).
	for i := n - 1; i >= 0; i-- {
		li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
		p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	}

	err := p.compileValidatedSequence(ctctx, v.Body())
	p.env = savedEnv

	if err != nil {
		return err
	}

	if !ctctx.inTail {
		p.AppendOperations(NewOperationPopEnv())
	}

	return nil
}

// CompileValidatedLetStar compiles (let* ((name val) ...) body ...).
//
// Bytecode:
//
//	OpPushEnv(T)             ; all slots upfront (bindings + defines)
//	<compile init-1>
//	StoreLocal name-1        ; name-1 now visible
//	<compile init-2>         ; can reference name-1
//	StoreLocal name-2
//	<compile body>
//	OpPopEnv                 ; only if not tail
func (p *CompileTimeContinuation) CompileValidatedLetStar(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLetStar,
) error {
	n := len(v.Bindings)

	if n == 0 {
		return p.compileLetBody(ctctx, v.Body())
	}

	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

	savedEnv := p.env
	p.env = childEnv

	// Add all binding names first (needed to count total slots with defines).
	for _, b := range v.Bindings {
		childEnv.MaybeCreateLocalBindingWithScopes(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
	}

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())
	totalSlots := len(childEnv.LocalEnvironment().Bindings())

	p.AppendOperations(NewOperationPushEnv(totalSlots))

	// Compile and store each init sequentially. The compile-time env
	// already has all bindings, so sequential visibility is correct
	// (the validator ensures each init only references preceding bindings).
	for _, b := range v.Bindings {
		err := p.compileValidated(ctctx.NotInTail(), b.Init)
		if err != nil {
			p.env = savedEnv
			return err
		}

		li := childEnv.GetLocalIndex(b.Name.Sym)
		p.AppendOperations(NewOperationPush())
		p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
	}

	err := p.compileValidatedSequence(ctctx, v.Body())
	p.env = savedEnv

	if err != nil {
		return err
	}

	if !ctctx.inTail {
		p.AppendOperations(NewOperationPopEnv())
	}

	return nil
}

// CompileValidatedLetrec compiles (letrec ...) and (letrec* ...).
//
// letrec (delayed assignment):
//
//	OpPushEnv(T)             ; all bindings in scope (T = bindings + defines)
//	<compile init-1> Push    ; all inits evaluated first
//	<compile init-2> Push
//	StoreLocal name-N        ; then assigned (LIFO)
//	StoreLocal name-1
//	<compile body>
//	OpPopEnv
//
// letrec* (sequential assignment):
//
//	OpPushEnv(T)             ; all bindings in scope
//	<compile init-1>
//	StoreLocal name-1        ; assigned immediately
//	<compile init-2>         ; sees name-1's value
//	StoreLocal name-2
//	<compile body>
//	OpPopEnv
func (p *CompileTimeContinuation) CompileValidatedLetrec(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLetrec,
) error {
	n := len(v.Bindings)

	if n == 0 {
		return p.compileLetBody(ctctx, v.Body())
	}

	// Create child env with ALL bindings visible before compiling any init.
	childEnv := p.createLetCompileEnv(v.Bindings)
	savedEnv := p.env
	p.env = childEnv

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())
	totalSlots := len(childEnv.LocalEnvironment().Bindings())

	p.AppendOperations(NewOperationPushEnv(totalSlots))

	var err error
	if v.LetrecStar {
		// letrec*: compile and store each init sequentially
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
	} else {
		// letrec: compile all inits first, then store all (delayed assignment)
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

// createLetCompileEnv creates a compile-time child environment with the
// given let bindings as local variables.
func (p *CompileTimeContinuation) createLetCompileEnv(
	bindings []validate.ValidatedLetBinding,
) *environment.EnvironmentFrame {
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)
	for _, b := range bindings {
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
// their bindings in the current compile-time env. This must be called
// before emitting OpPushEnv so the slot count includes define slots.
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
