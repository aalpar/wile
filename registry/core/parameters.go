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

package core

import (
	"github.com/aalpar/wile/registry"
)

func addParameters(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-parameter", ParamCount: 2, IsVariadic: true, Impl: PrimMakeParameter,
			Doc: "Creates a new parameter with an initial value and optional converter.", ParamNames: []string{"init", "converter"}, Category: "parameters"},
		{Name: "parameter?", ParamCount: 1, Impl: PrimParameterQ,
			Doc: "Returns #t if obj is a parameter.", ParamNames: []string{"obj"}, Category: "parameters"},
		// Internal primitive: bypasses converter when restoring a parameter in parameterize.
		// Not part of the public R7RS API.
		{Name: "%parameter-raw-set!", ParamCount: 2, Impl: PrimParameterRawSet,
			Doc: "Sets a parameter's internal value directly, bypassing the converter.", ParamNames: []string{"param", "val"}, Category: "parameters"},
		// Internal primitive: applies converter without setting the parameter value.
		// Used by parameterize to pre-convert the value before storing as a continuation mark.
		{Name: "%parameter-convert", ParamCount: 2, Impl: PrimParameterConvert,
			Doc: "Applies the parameter's converter to val, returning the result.", ParamNames: []string{"param", "val"}, Category: "parameters"},
	}, registry.PhaseRuntime)

	return nil
}
