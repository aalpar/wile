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

package charsets

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "char-set?",
			ParamCount: 1,
			Impl:       primCharSetQ,
			Doc:        "Returns #t if obj is a char-set, otherwise #f. (SRFI-14)",
			ParamNames: []string{"obj"},
			Category:   "char-sets",
			Keywords:   []string{"char-set", "predicate", "type", "srfi-14"},
		},
	}, registry.PhaseRuntime|registry.PhaseExpand)
	return nil
}

func primCharSetQ(mc machine.CallContext) error {
	_, ok := mc.Arg(0).(*values.CharSet)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}
