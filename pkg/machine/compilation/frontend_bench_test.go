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
	"testing"

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
)

// BenchmarkValidatePhase times validation alone (expanded syntax ->
// ValidatedExpr) over compileBenchCorpus.
//
// It exists because BenchmarkCompilePhase cannot see a change to
// validateForm's head dispatch: that benchmark runs parse, expand AND validate
// once in setup and times only compileValidated, so a per-form-head probe added
// in pkg/internal/validate is entirely outside its timer. This benchmark is the
// tightest gate on that dispatch — the denominator here is validation work
// only, so a delta shows at its largest possible magnitude.
func BenchmarkValidatePhase(b *testing.B) {
	env := newCompileBenchEnv()
	eval := machine.NewVMMacroEvaluator()
	ctx := context.Background()

	expanded := make([]syntax.SyntaxValue, 0, len(compileBenchCorpus))
	for _, code := range compileBenchCorpus {
		prog := parseForBench(b, env, code)
		econt := NewExpanderTimeContinuation(ctx, env, eval)
		ex, err := econt.ExpandExpression(prog)
		if err != nil {
			b.Fatalf("expand %q: %v", code, err)
		}
		expanded = append(expanded, ex)
	}

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, ex := range expanded {
			result := validate.ValidateExpression(ctx, env, ex)
			if !result.Ok() {
				b.Fatalf("validate: %v", result.Error())
			}
		}
	}
}

// BenchmarkFrontEndPhase times the whole front end — parse, expand, validate,
// compile — with nothing excluded, so a validate-side change is measured at the
// dilution an embedder actually pays. Read it together with
// BenchmarkValidatePhase: that one bounds the effect from above, this one
// reports it in context.
func BenchmarkFrontEndPhase(b *testing.B) {
	env := newCompileBenchEnv()
	eval := machine.NewVMMacroEvaluator()
	ctx := context.Background()

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		for _, code := range compileBenchCorpus {
			prog := parseForBench(b, env, code)
			econt := NewExpanderTimeContinuation(ctx, env, eval)
			ex, expandErr := econt.ExpandExpression(prog)
			if expandErr != nil {
				b.Fatalf("expand %q: %v", code, expandErr)
			}
			result := validate.ValidateExpression(ctx, env, ex)
			if !result.Ok() {
				b.Fatalf("validate %q: %v", code, result.Error())
			}
			tpl := machine.NewNativeTemplate(0, 0, false)
			cctx := NewCompileTimeContinuation(tpl, env, eval)
			cnt := NewCompileTimeCallContext(ctx, false)
			compileErr := cctx.compileValidated(cnt, result.Expr)
			if compileErr != nil {
				b.Fatalf("compile %q: %v", code, compileErr)
			}
		}
	}
}
