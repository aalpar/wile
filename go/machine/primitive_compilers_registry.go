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
	"wile/syntax"
	"wile/values"
)

// RegisterPrimitiveCompilers binds all primitive compilers in the compile-time
// environment (env.Compile()). These are looked up during compilation when the
// compiler encounters a symbol that names a special form like `define`, `lambda`,
// `if`, etc.
//
// The primitive compilers are bound with BindingTypePrimitive to distinguish them
// from syntax transformers (BindingTypeSyntax) and regular variables.
func RegisterPrimitiveCompilers(env *environment.EnvironmentFrame) error {
	compileEnv := env.Compile()

	// All primitive compilers. Each entry maps a keyword to its compile function.
	// The compile function is wrapped in a method reference that will be called
	// with the appropriate CompileTimeContinuation instance.
	// Primitive compilers for forms that pass through validation as ValidatedLiteral.
	// Core forms (define, lambda, quote, quasiquote, if, set!, begin) are handled
	// by compileValidated* methods and are NOT registered here.
	primitives := []struct {
		name string
		fn   PrimitiveCompilerFunc
	}{
		{"syntax", (*CompileTimeContinuation).CompileSyntax},
		{"syntax-case", (*CompileTimeContinuation).CompileSyntaxCase},
		{"meta", (*CompileTimeContinuation).CompileMeta},
		{"include", (*CompileTimeContinuation).CompileInclude},
		{"include-ci", (*CompileTimeContinuation).CompileIncludeCi},
		{"define-syntax", (*CompileTimeContinuation).CompileDefineSyntax},
		{"define-library", (*CompileTimeContinuation).CompileDefineLibrary},
		{"library", (*CompileTimeContinuation).CompileDefineLibrary}, // R6RS alias
		{"import", (*CompileTimeContinuation).CompileImport},
		{"export", (*CompileTimeContinuation).CompileExport},
		{"unquote", (*CompileTimeContinuation).CompileUnquote},
		{"unquote-splicing", (*CompileTimeContinuation).CompileUnquoteSplicing},
		{"quasisyntax", (*CompileTimeContinuation).CompileQuasisyntax},
		{"unsyntax", (*CompileTimeContinuation).CompileUnsyntax},
		{"unsyntax-splicing", (*CompileTimeContinuation).CompileUnsyntaxSplicing},
		{"with-syntax", (*CompileTimeContinuation).CompileWithSyntax},
		{"cond-expand", (*CompileTimeContinuation).CompileCondExpand},
		{"define-for-syntax", (*CompileTimeContinuation).CompileDefineForSyntax},
		{"begin-for-syntax", (*CompileTimeContinuation).CompileBeginForSyntax},
		{"eval-when", (*CompileTimeContinuation).CompileEvalWhen},
	}

	for _, prim := range primitives {
		sym := env.InternSymbol(values.NewSymbol(prim.name))
		compiler := NewPrimitiveCompiler(prim.name, prim.fn)

		// Always set the value - the binding may already exist from runtime primitives
		// registration, but we still need to set the PrimitiveCompiler value
		idx, _ := compileEnv.MaybeCreateGlobalBinding(sym, environment.BindingTypePrimitive)
		compileEnv.SetGlobalValue(idx, compiler) //nolint:errcheck
	}

	return nil
}

// LookupPrimitiveCompiler looks up a primitive compiler by symbol in the compile
// environment. Returns the PrimitiveCompiler if found, or nil if the symbol does
// not name a primitive compiler.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupPrimitiveCompiler(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *PrimitiveCompiler {
	compileEnv := env.Compile()

	// Look up with scopes for hygiene
	bnd := compileEnv.GetBindingWithScopes(sym, scopes)
	if bnd == nil {
		return nil
	}

	// Check if it's a primitive compiler binding
	if bnd.BindingType() != environment.BindingTypePrimitive {
		return nil
	}

	// Get the value and check if it's a PrimitiveCompiler
	val := bnd.Value()
	if pc, ok := val.(*PrimitiveCompiler); ok {
		return pc
	}

	return nil
}
