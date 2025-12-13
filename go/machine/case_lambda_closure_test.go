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

package machine

import (
	"wile/environment"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestCaseLambdaClosure_FindMatchingClause(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Create closures with different arities
	// Closure 1: fixed arity of 1 parameter
	tpl1 := NewNativeTemplate(1, 0, false, NewOperationLoadVoid())
	cls1 := NewClosureWithTemplate(tpl1, env)

	// Closure 2: fixed arity of 2 parameters
	tpl2 := NewNativeTemplate(2, 0, false, NewOperationLoadVoid())
	cls2 := NewClosureWithTemplate(tpl2, env)

	// Closure 3: variadic with at least 1 parameter (. rest)
	tpl3 := NewNativeTemplate(1, 0, true, NewOperationLoadVoid())
	cls3 := NewClosureWithTemplate(tpl3, env)

	// Create case-lambda with these closures
	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2, cls3})

	// Test finding matching clause for 1 argument
	match1, ok1 := caseLambda.FindMatchingClause(1)
	qt.Assert(t, ok1, qt.IsTrue)
	qt.Assert(t, match1, qt.Equals, cls1)

	// Test finding matching clause for 2 arguments
	match2, ok2 := caseLambda.FindMatchingClause(2)
	qt.Assert(t, ok2, qt.IsTrue)
	qt.Assert(t, match2, qt.Equals, cls2)

	// Test finding matching clause for 3 arguments (should match variadic)
	match3, ok3 := caseLambda.FindMatchingClause(3)
	qt.Assert(t, ok3, qt.IsTrue)
	qt.Assert(t, match3, qt.Equals, cls3)

	// Test matching clause for 0 arguments (should match variadic cls3)
	match0, ok0 := caseLambda.FindMatchingClause(0)
	qt.Assert(t, ok0, qt.IsTrue)
	qt.Assert(t, match0, qt.Equals, cls3)
}

func TestCaseLambdaClosure_FindMatchingClause_Variadic(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Create variadic closure with 2 required params + rest (a b . rest)
	tpl := NewNativeTemplate(3, 0, true, NewOperationLoadVoid())
	cls := NewClosureWithTemplate(tpl, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})

	// Should NOT match with fewer than required args
	_, ok1 := caseLambda.FindMatchingClause(1)
	qt.Assert(t, ok1, qt.IsFalse)

	// Should match with exactly required args (2)
	match2, ok2 := caseLambda.FindMatchingClause(2)
	qt.Assert(t, ok2, qt.IsTrue)
	qt.Assert(t, match2, qt.Equals, cls)

	// Should match with more than required args
	match3, ok3 := caseLambda.FindMatchingClause(3)
	qt.Assert(t, ok3, qt.IsTrue)
	qt.Assert(t, match3, qt.Equals, cls)

	match5, ok5 := caseLambda.FindMatchingClause(5)
	qt.Assert(t, ok5, qt.IsTrue)
	qt.Assert(t, match5, qt.Equals, cls)
}

func TestCaseLambdaClosure_Clauses(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env)

	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})
	clauses := caseLambda.Clauses()

	qt.Assert(t, clauses, qt.HasLen, 2)
	qt.Assert(t, clauses[0].closure, qt.Equals, cls1)
	qt.Assert(t, clauses[1].closure, qt.Equals, cls2)
}

func TestCaseLambdaClosure_IsVoid(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	tpl := NewNativeTemplate(1, 0, false)
	cls := NewClosureWithTemplate(tpl, env)
	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})

	qt.Assert(t, caseLambda.IsVoid(), qt.IsFalse)

	var nilCaseLambda *CaseLambdaClosure
	qt.Assert(t, nilCaseLambda.IsVoid(), qt.IsTrue)
}

func TestCaseLambdaClosure_SchemeString(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	tpl := NewNativeTemplate(1, 0, false)
	cls := NewClosureWithTemplate(tpl, env)
	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})

	qt.Assert(t, caseLambda.SchemeString(), qt.Equals, "#<case-lambda-closure>")
}

func TestCaseLambdaClosure_EqualTo(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env)

	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env)

	caseLambda1 := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})
	caseLambda2 := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})
	caseLambda3 := NewCaseLambdaClosure([]*MachineClosure{cls1})

	// Same structure should be equal
	qt.Assert(t, caseLambda1.EqualTo(caseLambda2), qt.IsTrue)

	// Different number of clauses
	qt.Assert(t, caseLambda1.EqualTo(caseLambda3), qt.IsFalse)

	// Different type
	qt.Assert(t, caseLambda1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil cases
	var nilCaseLambda1 *CaseLambdaClosure
	var nilCaseLambda2 *CaseLambdaClosure
	qt.Assert(t, nilCaseLambda1.EqualTo(nilCaseLambda2), qt.IsTrue)
	qt.Assert(t, caseLambda1.EqualTo(nilCaseLambda1), qt.IsFalse)
}

func TestOperationMakeCaseLambdaClosure(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Create closures
	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env)

	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env)

	// Create machine context with closures on stack
	mc := &MachineContext{
		evals: NewStack(cls1, cls2),
		env:   env,
	}

	op := NewOperationMakeCaseLambdaClosure(2)
	newMc, err := op.Apply(nil, mc)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, newMc.pc, qt.Equals, 1)
	qt.Assert(t, *newMc.evals, qt.HasLen, 0)
	qt.Assert(t, newMc.value, qt.HasLen, 1)

	caseLambda, ok := newMc.value[0].(*CaseLambdaClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, caseLambda.Clauses(), qt.HasLen, 2)
}

func TestOperationMakeCaseLambdaClosure_Error(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	// Create machine context with non-closure on stack
	mc := &MachineContext{
		evals: NewStack(values.NewInteger(42), values.NewInteger(99)),
		env:   env,
	}

	op := NewOperationMakeCaseLambdaClosure(2)
	_, err := op.Apply(nil, mc)

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected closure in case-lambda")
}

func TestOperationMakeCaseLambdaClosure_SchemeString(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.SchemeString(), qt.Equals, "#<machine-operation-make-case-lambda-closure>")
}

func TestOperationMakeCaseLambdaClosure_IsVoid(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	var nilOp *OperationMakeCaseLambdaClosure
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationMakeCaseLambdaClosure_EqualTo(t *testing.T) {
	op1 := NewOperationMakeCaseLambdaClosure(2)
	op2 := NewOperationMakeCaseLambdaClosure(2)
	op3 := NewOperationMakeCaseLambdaClosure(3)

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	var nilOp1 *OperationMakeCaseLambdaClosure
	var nilOp2 *OperationMakeCaseLambdaClosure
	qt.Assert(t, nilOp1.EqualTo(nilOp2), qt.IsTrue)
}

// Tests moved from coverage_additional_test.go
// TestClausesWrapperAdditional tests the clausesWrapper type
func TestClausesWrapperAdditional(t *testing.T) {
	cw := &clausesWrapper{
		clauses: nil,
	}
	qt.Assert(t, cw.IsVoid(), qt.IsFalse)
	qt.Assert(t, cw.SchemeString(), qt.Contains, "syntax-rules-clauses")

	// EqualTo always returns false because clauses are not comparable
	qt.Assert(t, cw.EqualTo(cw), qt.IsFalse)

	var nilCw *clausesWrapper
	qt.Assert(t, cw.EqualTo(nilCw), qt.IsFalse)
}

// TestClausesWrapperMethods tests clausesWrapper methods
func TestClausesWrapperMethods(t *testing.T) {
	cw := &clausesWrapper{
		clauses: nil,
	}

	qt.Assert(t, cw.SchemeString(), qt.Contains, "syntax-rules")
	qt.Assert(t, cw.IsVoid(), qt.IsFalse)

	// Test EqualTo - clausesWrapper always returns false (not comparable)
	cw2 := &clausesWrapper{clauses: nil}
	qt.Assert(t, cw.EqualTo(cw2), qt.IsFalse) // clauses are not comparable per implementation
	qt.Assert(t, cw.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestCaseLambdaClosureFindMatching tests CaseLambdaClosure.FindMatchingClause
func TestCaseLambdaClosureFindMatching(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Create clauses with different arities
	tpl1 := NewNativeTemplate(1, 1, false)
	tpl1.AppendOperations(NewOperationLoadLiteralInteger(1), NewOperationRestoreContinuation())
	cls1 := NewClosureWithTemplate(tpl1, env)

	tpl2 := NewNativeTemplate(2, 2, false)
	tpl2.AppendOperations(NewOperationLoadLiteralInteger(2), NewOperationRestoreContinuation())
	cls2 := NewClosureWithTemplate(tpl2, env)

	caseCls := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})

	qt.Assert(t, caseCls.SchemeString(), qt.Contains, "case-lambda")
	qt.Assert(t, caseCls.IsVoid(), qt.IsFalse)

	// Test clause finding
	clauses := caseCls.Clauses()
	qt.Assert(t, len(clauses), qt.Equals, 2)

	// Find matching clause for 1 arg
	match, found := caseCls.FindMatchingClause(1)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, match, qt.Equals, cls1)

	// Find matching clause for 2 args
	match, found = caseCls.FindMatchingClause(2)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, match, qt.Equals, cls2)

	// No match for 3 args
	match, found = caseCls.FindMatchingClause(3)
	qt.Assert(t, found, qt.IsFalse)
	qt.Assert(t, match, qt.IsNil)
}
