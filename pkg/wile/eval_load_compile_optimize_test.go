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
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
)

// TestEvalLoadCompileTemplatesOptimized pins that procedures produced by the
// eval, load, and compile primitives carry the same peephole fusions as code
// compiled by Engine.Eval. It runs on a real Engine: the RunSchemeCode helper
// skips the optimizer and has a mutable top level, so none of these opcodes
// appear there.
//
// Only the compile arm discriminates. A compile thunk is the one procedure whose
// template is a TOP-LEVEL template, which the primitive left unoptimized, so its
// `<` and `+` stayed generic applies on every call. The eval and load arms define
// procedures, whose closure bodies are optimized as they compile on every path;
// they pin parity with a top-level define, not the fix.
func TestEvalLoadCompileTemplatesOptimized(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	path := filepath.Join(t.TempDir(), "defs.scm")
	err := os.WriteFile(path, []byte(
		"(define (fib-l n) (if (<= n 1) n (+ (fib-l (- n 1)) (fib-l (- n 2)))))\n"+
			"(define (loop-l n acc) (if (= n 0) acc (loop-l (- n 1) (+ acc 1))))\n"), 0o600)
	c.Assert(err, qt.IsNil)

	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS), WithSourceOS())
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `
		(eval '(define (fib-e n) (if (<= n 1) n (+ (fib-e (- n 1)) (fib-e (- n 2))))))
		(eval '(define (loop-e n acc) (if (= n 0) acc (loop-e (- n 1) (+ acc 1)))))
		(load `+strconv.Quote(path)+`)
		(define thunk (compile '(let ((x 3)) (if (< x 10) (+ x 1) x))))`)
	c.Assert(err, qt.IsNil)

	tests := []struct {
		proc  string
		call  string
		value string
		ops   []string
	}{
		{"thunk", "(thunk)", "4", []string{"NumLt", "Add"}},
		{"fib-e", "(fib-e 20)", "6765", []string{"NumLe", "Sub", "ReleaseEnvFrame", "AddTail"}},
		{"fib-l", "(fib-l 20)", "6765", []string{"NumLe", "Sub", "ReleaseEnvFrame", "AddTail"}},
		{"loop-e", "(loop-e 1000 0)", "1000", []string{"NumEq", "Sub", "Add", "SelfTailCall"}},
		{"loop-l", "(loop-l 1000 0)", "1000", []string{"NumEq", "Sub", "Add", "SelfTailCall"}},
	}
	for _, tc := range tests {
		res, err := eng.EvalMultiple(ctx, tc.call)
		c.Assert(err, qt.IsNil, qt.Commentf("%s", tc.call))
		c.Check(res.SchemeString(), qt.Equals, tc.value, qt.Commentf("%s", tc.call))

		proc, err := eng.EvalMultiple(ctx, tc.proc)
		c.Assert(err, qt.IsNil)
		dis, err := eng.DisassembleValue(proc)
		c.Assert(err, qt.IsNil)
		for _, op := range tc.ops {
			c.Check(strings.Contains(dis, " "+op+" "), qt.IsTrue,
				qt.Commentf("%s: expected opcode %s; disassembly:\n%s", tc.proc, op, dis))
		}
	}
}
