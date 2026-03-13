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
	"go/token"
	"sort"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/checker"
	"golang.org/x/tools/go/packages"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// parseAnalyzerNames collects and validates the variadic analyzer-name arguments.
// Returns an error on an improper list, a non-string element, or an unknown name.
func parseAnalyzerNames(rest values.Value) ([]*analysis.Analyzer, error) {
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return nil, nil
	}
	var analyzers []*analysis.Analyzer
	for !values.IsEmptyList(tuple) {
		pair, pok := tuple.(*values.Pair)
		if !pok {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
				"go-analyze: malformed analyzer list")
		}
		nameVal, sok := pair.Car().(*values.String)
		if !sok {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAString,
				"go-analyze: analyzer names must be strings")
		}
		a, found := analyzerRegistry[nameVal.Value]
		if !found {
			return nil, werr.WrapForeignErrorf(errLintUnknownName,
				"go-analyze: unknown analyzer %q; use go-analyze-list for available names",
				nameVal.Value)
		}
		analyzers = append(analyzers, a)
		cdr, cok := pair.Cdr().(values.Tuple)
		if !cok {
			return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
				"go-analyze: malformed analyzer list")
		}
		tuple = cdr
	}
	return analyzers, nil
}

var (
	errLintBuildError  = werr.NewStaticError("analyze build error")
	errLintUnknownName = werr.NewStaticError("unknown analyzer name")
	errLintRunError    = werr.NewStaticError("analyzer run error")
)

// PrimGoAnalyze implements (go-analyze pattern analyzer-name ...).
// Loads the package, runs the named analyzers, and returns diagnostics as
// a list of (diagnostic (analyzer . "...") (pos . "...") (message . "...") (category . "...")).
func PrimGoAnalyze(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-analyze")
	if err != nil {
		return err
	}

	analyzers, err := parseAnalyzerNames(mc.Arg(1))
	if err != nil {
		return err
	}

	if len(analyzers) == 0 {
		mc.SetValue(values.EmptyList)
		return nil
	}

	err = security.Check(mc.Context(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionLoad,
		Target:   "go",
	})
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	cfg := &packages.Config{
		Mode:    packages.LoadAllSyntax,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errLintBuildError,
			"go-analyze: %s: %s", pattern.Value, loadErr)
	}
	if len(pkgs) == 0 {
		return werr.WrapForeignErrorf(errLintBuildError,
			"go-analyze: %s: no packages found", pattern.Value)
	}

	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errLintBuildError,
			"go-analyze: %s: %s", pattern.Value, strings.Join(errs, "; "))
	}

	graph, analyzeErr := checker.Analyze(analyzers, pkgs, nil)
	if analyzeErr != nil {
		return werr.WrapForeignErrorf(errLintRunError,
			"go-analyze: %s", analyzeErr)
	}

	// Collect diagnostics from root actions only. The driver runs
	// analyzers on dependency packages for fact propagation, but
	// only root diagnostics are relevant to the user's query.
	var result []values.Value
	for _, act := range graph.Roots {
		if act.Err != nil {
			return werr.WrapForeignErrorf(errLintRunError,
				"go-analyze: analyzer %q on %s: %s",
				act.Analyzer.Name, act.Package.PkgPath, act.Err)
		}
		for _, d := range act.Diagnostics {
			pos := fset.Position(d.Pos)
			fields := []values.Value{
				goast.Field("analyzer", goast.Str(act.Analyzer.Name)),
				goast.Field("pos", goast.Str(pos.String())),
				goast.Field("message", goast.Str(d.Message)),
				goast.Field("category", goast.Str(d.Category)),
			}
			result = append(result, goast.Node("diagnostic", fields...))
		}
	}
	mc.SetValue(goast.ValueList(result))
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
