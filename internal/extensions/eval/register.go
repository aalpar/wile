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

// Package eval provides eval and environment primitives.
package eval

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the eval extension.
var Extension = registry.NewExtension("eval", AddToRegistry)

// Builder aggregates all eval registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all eval primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "eval", ParamCount: 1, IsVariadic: true, Impl: PrimEval,
			Doc: "Evaluates expression. 1-arg uses current namespace; 2-arg uses given environment.", ParamNames: []string{"expr"}, Category: "eval"},
		{Name: "load", ParamCount: 1, Impl: PrimLoad,
			Doc: "Loads and evaluates a Scheme source file.", ParamNames: []string{"filename"}, Category: "eval"},
		{Name: "current-load-path", Impl: PrimCurrentLoadPath,
			Doc: "Returns the path of the file currently being loaded.", Category: "eval"},
		{Name: "current-load-directory", Impl: PrimCurrentLoadDirectory,
			Doc: "Returns the directory of the file currently being loaded.", Category: "eval"},
		{Name: "current-load-depth", Impl: PrimCurrentLoadDepth,
			Doc: "Returns the current load nesting depth.", Category: "eval"},
		{Name: "scheme-report-environment", ParamCount: 1, Impl: PrimSchemeReportEnvironment,
			Doc: "Returns the environment for the given Scheme version.", ParamNames: []string{"version"}, Category: "eval"},
		{Name: "null-environment", ParamCount: 1, Impl: PrimNullEnvironment,
			Doc: "Returns an empty environment for the given Scheme version.", ParamNames: []string{"version"}, Category: "eval"},
		{Name: "environment", ParamCount: 1, IsVariadic: true, Impl: PrimEnvironment,
			Doc: "Creates an environment from import specs.", ParamNames: []string{"import-spec"}, Category: "eval"},
		{Name: "expand", ParamCount: 1, Impl: PrimExpand,
			Doc: "Fully expands a syntax object.", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "expand-once", ParamCount: 1, Impl: PrimExpandOnce,
			Doc: "Expands a syntax object by one level.", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "compile", ParamCount: 1, Impl: PrimCompile,
			Doc: "Compiles an expression to a thunk.", ParamNames: []string{"expr"}, Category: "eval"},
		{Name: "syntax-local-value", ParamCount: 1, Impl: PrimSyntaxLocalValue,
			Doc: "Returns the compile-time value of an identifier.", ParamNames: []string{"id"}, Category: "eval"},
		{Name: "syntax-local-value/immediate", ParamCount: 1, Impl: PrimSyntaxLocalValueImmediate,
			Doc: "Returns the compile-time value without chasing rename-transformers.", ParamNames: []string{"id"}, Category: "eval"},
		{Name: "make-compile-time-value", ParamCount: 1, Impl: PrimMakeCompileTimeValue,
			Doc: "Wraps a value for compile-time storage.", ParamNames: []string{"value"}, Category: "eval"},
		{Name: "syntax-local-introduce", ParamCount: 1, Impl: PrimSyntaxLocalIntroduce,
			Doc: "Flips the introduction scope on a syntax object.", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "syntax-local-identifier-as-binding", ParamCount: 1, Impl: PrimSyntaxLocalIdentifierAsBinding,
			Doc: "Adds use-site scope to an identifier.", ParamNames: []string{"id"}, Category: "eval"},
	}, registry.PhaseRuntime)
	return nil
}
