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

// tier1CompilerEntries is the single source of truth for the native-spine
// (Tier-1) form compilers. register.go's init() attaches each onto its FormSpec
// via forms.RegisterCompiler, so compileValidated dispatches them by FormName
// through the per-engine registry — exactly like Tier-2 syntax compilers.
//
// The let-family maps four names onto one CompileValidatedLet: named-let
// validates to a *ValidatedLet with FormName "letrec", so all four must resolve.
//
// ADDING A NEW TIER-1 FORM: register its validator in validate/register.go, add
// its dedicated Validated* concrete type, and add one entry here.
var tier1CompilerEntries = []PhaseEntry[CompilerFunc]{
	{"if", CompileValidatedIf},
	{"define", CompileValidatedDefine},
	{"lambda", CompileValidatedLambda},
	{"case-lambda", CompileValidatedCaseLambda},
	{"set!", CompileValidatedSetBang},
	{"quote", CompileValidatedQuote},
	{"begin", CompileValidatedBegin},
	{"quasiquote", CompileValidatedQuasiquote},
	{"dynamic-wind", CompileValidatedDynamicWind},
	{"apply", CompileValidatedApply},
	{"with-continuation-mark", CompileValidatedWithContinuationMark},
	{"let", CompileValidatedLet},
	{"let*", CompileValidatedLet},
	{"letrec", CompileValidatedLet},
	{"letrec*", CompileValidatedLet},
}
