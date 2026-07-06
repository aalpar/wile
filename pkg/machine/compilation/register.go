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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
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
	// Tier 1: native-spine compilers, attached by name onto their FormSpec so
	// compileValidated dispatches them through the per-engine forms registry.
	for _, entry := range tier1CompilerEntries {
		forms.RegisterCompiler(entry.Name, entry.Fn)
	}

	// Tier 2: syntax passthrough compilers — syntaxCompiler unwraps
	// ValidatedLiteral → SyntaxPair → CDR before calling the method.
	// Both this init() and RegisterSyntaxCompilers derive from
	// syntaxCompilerEntries (syntax_compilers_registry.go) to stay in sync.
	for _, entry := range syntaxCompilerEntries {
		registerCompiler(entry.Name, syntaxCompiler(entry.Fn))
	}
}

// syntaxCompiler adapts a SyntaxCompilerFunc into a CompilerFunc by asserting the
// ValidatedLiteral it receives, then unwrapping ValidatedLiteral -> SyntaxPair -> CDR.
func syntaxCompiler(fn SyntaxCompilerFunc) CompilerFunc {
	return func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
		lit, ok := expr.(*validate.ValidatedLiteral)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: expected *validate.ValidatedLiteral, got %T", expr)
		}
		pair, ok := lit.Value.(*syntax.SyntaxPair)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: expected SyntaxPair, got %T", lit.Value)
		}
		args, ok := pair.Cdr().(syntax.SyntaxValue)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: expected SyntaxValue CDR, got %T", pair.Cdr())
		}
		return fn(ctc, ctctx, args)
	}
}
