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

package goastlint

import (
	"sort"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// PrimGoAnalyze stub — filled in by Task 3.
func PrimGoAnalyze(mc *machine.MachineContext) error {
	mc.SetValue(values.EmptyList)
	return nil
}

// PrimGoAnalyzeList returns a sorted list of available analyzer name strings.
func PrimGoAnalyzeList(mc *machine.MachineContext) error {
	names := make([]string, 0, len(analyzerRegistry))
	for name := range analyzerRegistry {
		names = append(names, name)
	}
	sort.Strings(names)
	result := make([]values.Value, len(names))
	for i, name := range names {
		result[i] = goast.Str(name)
	}
	mc.SetValue(goast.ValueList(result))
	return nil
}
