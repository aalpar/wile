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
	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/assign"
	"golang.org/x/tools/go/analysis/passes/bools"
	"golang.org/x/tools/go/analysis/passes/composite"
	"golang.org/x/tools/go/analysis/passes/copylock"
	"golang.org/x/tools/go/analysis/passes/defers"
	"golang.org/x/tools/go/analysis/passes/directive"
	"golang.org/x/tools/go/analysis/passes/errorsas"
	"golang.org/x/tools/go/analysis/passes/httpresponse"
	"golang.org/x/tools/go/analysis/passes/ifaceassert"
	"golang.org/x/tools/go/analysis/passes/loopclosure"
	"golang.org/x/tools/go/analysis/passes/lostcancel"
	"golang.org/x/tools/go/analysis/passes/nilfunc"
	"golang.org/x/tools/go/analysis/passes/nilness"
	"golang.org/x/tools/go/analysis/passes/printf"
	"golang.org/x/tools/go/analysis/passes/shadow"
	"golang.org/x/tools/go/analysis/passes/shift"
	"golang.org/x/tools/go/analysis/passes/sortslice"
	"golang.org/x/tools/go/analysis/passes/stdmethods"
	"golang.org/x/tools/go/analysis/passes/stringintconv"
	"golang.org/x/tools/go/analysis/passes/structtag"
	"golang.org/x/tools/go/analysis/passes/testinggoroutine"
	"golang.org/x/tools/go/analysis/passes/tests"
	"golang.org/x/tools/go/analysis/passes/timeformat"
	"golang.org/x/tools/go/analysis/passes/unmarshal"
	"golang.org/x/tools/go/analysis/passes/unreachable"
)

// analyzerRegistry maps analyzer names to their *analysis.Analyzer.
// Prerequisites vary: most use inspect; nilness needs buildssa→ctrlflow,
// lostcancel needs ctrlflow, errorsas needs typeindexanalyzer.
// The checker.Analyze driver resolves all prerequisite chains and
// cross-package fact propagation automatically.
var analyzerRegistry = map[string]*analysis.Analyzer{
	"assign":           assign.Analyzer,
	"bools":            bools.Analyzer,
	"composite":        composite.Analyzer,
	"copylocks":        copylock.Analyzer,
	"defers":           defers.Analyzer,
	"directive":        directive.Analyzer,
	"errorsas":         errorsas.Analyzer,
	"httpresponse":     httpresponse.Analyzer,
	"ifaceassert":      ifaceassert.Analyzer,
	"loopclosure":      loopclosure.Analyzer,
	"lostcancel":       lostcancel.Analyzer,
	"nilfunc":          nilfunc.Analyzer,
	"nilness":          nilness.Analyzer,
	"printf":           printf.Analyzer,
	"shadow":           shadow.Analyzer,
	"shift":            shift.Analyzer,
	"sortslice":        sortslice.Analyzer,
	"stdmethods":       stdmethods.Analyzer,
	"stringintconv":    stringintconv.Analyzer,
	"structtag":        structtag.Analyzer,
	"testinggoroutine": testinggoroutine.Analyzer,
	"tests":            tests.Analyzer,
	"timeformat":       timeformat.Analyzer,
	"unmarshal":        unmarshal.Analyzer,
	"unreachable":      unreachable.Analyzer,
}
