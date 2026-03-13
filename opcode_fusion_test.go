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

package wile

import (
	"context"
	"fmt"
	"testing"

	"github.com/aalpar/wile/machine"
)

// templateTreeHasOpcode recursively searches a NativeTemplate and its
// sub-templates (stored in the literals pool) for the given opcode.
func templateTreeHasOpcode(tpl *machine.NativeTemplate, op machine.OpCode) bool {
	for _, instr := range tpl.Code() {
		if instr.Op == op {
			return true
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		if templateTreeHasOpcode(sub, op) {
			return true
		}
	}
	return false
}

// validateBranchOffsets checks that every branch-family instruction in the
// template tree has an offset targeting a valid position [0, len(code)].
// This catches fusion bugs where instruction removal shifts offsets out of
// bounds.
func validateBranchOffsets(t *testing.T, tpl *machine.NativeTemplate, path string) {
	t.Helper()
	code := tpl.Code()
	for i, instr := range code {
		if !isFusionBranchOp(instr.Op) {
			continue
		}
		target := i + int(instr.Arg)
		if target < 0 || target > len(code) {
			t.Errorf("%s: instruction %d (%s): offset %d targets pc=%d, valid range [0, %d]",
				path, i, instr.Op, instr.Arg, target, len(code))
		}
	}
	for idx, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		validateBranchOffsets(t, sub, fmt.Sprintf("%s/lit[%d]", path, idx))
	}
}

// isFusionBranchOp returns true for opcodes whose Arg is a relative PC offset
// that the peephole optimizer must adjust when fusing instructions.
func isFusionBranchOp(op machine.OpCode) bool {
	return op == machine.OpBranch ||
		op == machine.OpBranchOnFalseValue ||
		op == machine.OpSaveContinuation
}

// TestOpcodeFusion verifies that the peephole optimizer correctly fuses
// opcodes on real compiler output and that execution produces correct results.
//
// Each test case:
//  1. Compiles Scheme source through the full pipeline
//  2. Validates all branch offsets are in bounds after fusion
//  3. Checks that expected fused opcodes appear in the template tree
//  4. Executes the compiled code and verifies the result
//
// This is a regression test for the SaveContinuation offset off-by-one bug
// where passes 2/3 used pullIdx = i + off (pointing past PullApply) instead
// of pullIdx = i + off - 1 (pointing at PullApply). The bug caused all
// non-tail call fusions (Wave 7/8/9) to silently fail on real compiler output
// while passing hand-crafted unit tests.
func TestOpcodeFusion(t *testing.T) {
	tests := []struct {
		name       string
		setup      string
		code       string
		wantOps    []machine.OpCode
		wantResult string
	}{
		// --- Wave 7: CallForeignCached / CallForeignCachedTail ---
		{
			name:       "non-tail foreign call fused",
			code:       "(+ 1 2)",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "3",
		},
		{
			name:       "tail foreign call fused",
			code:       "((lambda () (+ 1 2)))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVarTail},
			wantResult: "3",
		},
		{
			name:       "tail foreign call with args",
			code:       "((lambda (x y) (+ x y)) 3 4)",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVarTail},
			wantResult: "7",
		},

		// --- Wave 8: CallLocal / CallCachedBinding ---
		{
			name:       "tail CallLocal via let-bound lambda",
			code:       "(let ((f (lambda (x) (+ x 1)))) (f 42))",
			wantOps:    []machine.OpCode{machine.OpCallLocal},
			wantResult: "43",
		},

		// --- Wave 9: Promoted primitives ---
		{
			name:       "promoted eq? non-tail",
			code:       "(eq? 'a 'a)",
			wantOps:    []machine.OpCode{machine.OpEqQ},
			wantResult: "#t",
		},
		{
			name:       "promoted eq? tail",
			code:       "((lambda () (eq? 'a 'a)))",
			wantOps:    []machine.OpCode{machine.OpEqQTail},
			wantResult: "#t",
		},
		{
			name:       "promoted vector? non-tail",
			code:       "(let ((v (vector 1 2 3))) (if (vector? v) 1 2))",
			wantOps:    []machine.OpCode{machine.OpVectorQ},
			wantResult: "1",
		},
		{
			name:       "promoted vector-ref non-tail",
			code:       "(let ((v (vector 10 20 30))) (+ 0 (vector-ref v 1)))",
			wantOps:    []machine.OpCode{machine.OpVectorRef},
			wantResult: "20",
		},

		// --- Branch offset correctness after fusion ---
		//
		// These tests verify that fixSurvivingBranches correctly adjusts
		// all branch offsets when fusion removes instructions. Wrong offsets
		// would cause execution to jump to the wrong branch, producing
		// incorrect results or crashing.
		{
			name:       "branch over fused consequent (if-then)",
			code:       "(if #t (+ 2 3) (+ 4 5))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "5",
		},
		{
			name:       "branch over fused consequent (if-else)",
			code:       "(if #f (+ 2 3) (+ 4 5))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "9",
		},
		{
			name:       "fused eq? test + fused consequent call",
			code:       "(if (eq? 'a 'a) (+ 2 3) (+ 4 5))",
			wantOps:    []machine.OpCode{machine.OpEqQ, machine.OpCallForeignCachedVar},
			wantResult: "5",
		},
		{
			name:       "fused eq? false + fused alternative call",
			code:       "(if (eq? 'a 'b) (+ 2 3) (+ 4 5))",
			wantOps:    []machine.OpCode{machine.OpEqQ, machine.OpCallForeignCachedVar},
			wantResult: "9",
		},
		{
			name:       "nested fused calls",
			code:       "(+ (+ 1 2) (+ 3 4))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "10",
		},
		{
			name:       "let with multiple fused bindings",
			code:       "(let ((a (+ 1 2)) (b (+ 3 4))) (+ a b))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "10",
		},
		{
			name:       "cond: multiple eq? branches with fusion",
			code:       `(cond ((eq? 'x 'y) 100) ((eq? 'x 'x) 200) (else 300))`,
			wantOps:    []machine.OpCode{machine.OpEqQ},
			wantResult: "200",
		},
		{
			name:       "named let: recursive calls with multiple fused sub-calls",
			code:       "(let loop ((n 10) (acc 0)) (if (<= n 0) acc (loop (- n 1) (+ acc n))))",
			wantOps:    []machine.OpCode{machine.OpCallForeignCachedVar},
			wantResult: "55",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			if tt.setup != "" {
				_, err = engine.Eval(ctx, tt.setup)
				if err != nil {
					t.Fatalf("setup: %v", err)
				}
			}

			compiled, err := engine.Compile(ctx, tt.code)
			if err != nil {
				t.Fatalf("compile: %v", err)
			}

			// Validate all branch offsets are in bounds after fusion.
			validateBranchOffsets(t, compiled.template, "root")

			// Verify expected fused opcodes appear in the template tree.
			for _, op := range tt.wantOps {
				if !templateTreeHasOpcode(compiled.template, op) {
					t.Errorf("expected opcode %s not found in compiled template tree", op)
					dumpTemplateOpcodes(t, compiled.template, "root")
				}
			}

			// Execute and verify correct result.
			result, err := engine.Run(ctx, compiled)
			if err != nil {
				t.Fatalf("run: %v", err)
			}
			got := result.SchemeString()
			if got != tt.wantResult {
				t.Errorf("result = %s, want %s", got, tt.wantResult)
			}
		})
	}
}

// TestPromotedPrimitiveFallback verifies that promoted primitives (eq?,
// vector?, vector-ref) fall back correctly when their bindings are replaced
// via set! with a different ForeignClosure. Without the name guard, the
// inline implementation would run incorrectly for the replacement closure.
func TestPromotedPrimitiveFallback(t *testing.T) {
	tests := []struct {
		name       string
		code       string
		wantResult string
	}{
		{
			// eq? replaced with car — same type (*ForeignClosure) but different
			// name/arity. Without the name guard, inlineEq would run on car's args.
			name:       "set! eq? to car: fallback runs car correctly",
			code:       `(let ((original-eq? eq?)) (set! eq? car) (let ((result (eq? '(hello world)))) (set! eq? original-eq?) result))`,
			wantResult: "hello",
		},
		{
			// eq? replaced with a Scheme closure (non-ForeignClosure).
			name:       "set! eq? to lambda: fallback runs lambda",
			code:       `(let ((original-eq? eq?)) (set! eq? (lambda (a b) (+ a b))) (let ((result (eq? 3 4))) (set! eq? original-eq?) result))`,
			wantResult: "7",
		},
		{
			// vector? replaced with not — different ForeignClosure, same arity (1).
			name:       "set! vector? to not: fallback runs not correctly",
			code:       `(let ((original-vector? vector?)) (set! vector? not) (let ((result (vector? #f))) (set! vector? original-vector?) result))`,
			wantResult: "#t",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}

			result, err := engine.Eval(ctx, tt.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			got := result.SchemeString()
			if got != tt.wantResult {
				t.Errorf("result = %s, want %s", got, tt.wantResult)
			}
		})
	}
}

// dumpTemplateOpcodes logs all opcodes in a template tree for debugging
// failed assertions.
func dumpTemplateOpcodes(t *testing.T, tpl *machine.NativeTemplate, path string) {
	t.Helper()
	for i, instr := range tpl.Code() {
		t.Logf("  %s[%d] %s (Arg=%d)", path, i, instr.Op, instr.Arg)
	}
	for idx, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		dumpTemplateOpcodes(t, sub, fmt.Sprintf("%s/lit[%d]", path, idx))
	}
}
