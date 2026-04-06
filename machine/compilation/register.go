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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/werr"
)

// RegisterAllPhaseHandlers registers both syntax compilers (compile phase)
// and primitive expanders (expand phase) in the correct order. Use this
// instead of calling RegisterSyntaxCompilers and RegisterPrimitiveExpanders
// separately at engine/bootstrap/test init sites.
func RegisterAllPhaseHandlers(env *environment.EnvironmentFrame) error {
	err := RegisterSyntaxCompilers(env)
	if err != nil {
		return err
	}
	return RegisterPrimitiveExpanders(env)
}

// VerifyAllPhaseHandlers cross-checks all three phase registries:
// form validators (internal/forms), compilers (Tier 1 + Tier 2), and
// primitive expanders. Returns the first inconsistency found, or nil.
//
// Call from tests only — not on the production init path.
func VerifyAllPhaseHandlers() error {
	err := forms.Verify()
	if err != nil {
		return err
	}
	err = VerifyCompilers()
	if err != nil {
		return err
	}
	return VerifyExpanders()
}

func init() {
	// Tier 2: syntax passthrough compilers — syntaxCompiler unwraps
	// ValidatedLiteral → SyntaxPair → CDR before calling the method.
	//
	// Tier 1 forms (if, define, lambda, etc.) are dispatched by type switch
	// in compileValidated (compile_validated.go) — no registry entry needed.
	//
	// Both this init() and RegisterSyntaxCompilers derive from
	// syntaxCompilerEntries (syntax_compilers_registry.go) to stay in sync.
	for _, entry := range syntaxCompilerEntries {
		registerCompiler(entry.Name, syntaxCompiler(entry.Fn))
	}
}

// syntaxCompiler adapts a SyntaxCompilerFunc into a CompilerFunc by unwrapping
// ValidatedLiteral → SyntaxPair → CDR.
func syntaxCompiler(fn SyntaxCompilerFunc) CompilerFunc {
	return func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr *validate.ValidatedLiteral) error {
		pair, ok := expr.Value.(*syntax.SyntaxPair)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: expected SyntaxPair, got %T", expr.Value)
		}
		args, ok := pair.Cdr().(syntax.SyntaxValue)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: CDR is not SyntaxValue: %T", pair.Cdr())
		}
		return fn(ctc, ctctx, args)
	}
}
