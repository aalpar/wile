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
	"golang.org/x/tools/go/packages"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	errAnalyzeBuildError  = werr.NewStaticError("analyze build error")
	errAnalyzeUnknownName = werr.NewStaticError("unknown analyzer name")
)

// PrimGoAnalyze implements (go-analyze pattern analyzer-name ...).
// Loads the package, runs the named analyzers, and returns diagnostics as
// a list of (diagnostic (analyzer . "...") (pos . "...") (message . "...") (category . "...")).
func PrimGoAnalyze(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-analyze")
	if err != nil {
		return err
	}

	// Collect and validate analyzer names from variadic args.
	var analyzers []*analysis.Analyzer
	rest := mc.Arg(1)
	tuple, ok := rest.(values.Tuple)
	if ok {
		for !values.IsEmptyList(tuple) {
			pair, pok := tuple.(*values.Pair)
			if !pok {
				break
			}
			nameVal, sok := pair.Car().(*values.String)
			if !sok {
				return werr.WrapForeignErrorf(werr.ErrNotAString,
					"go-analyze: analyzer names must be strings")
			}
			a, found := analyzerRegistry[nameVal.Value]
			if !found {
				return werr.WrapForeignErrorf(errAnalyzeUnknownName,
					"go-analyze: unknown analyzer %q; use go-analyze-list for available names",
					nameVal.Value)
			}
			analyzers = append(analyzers, a)
			cdr, cok := pair.Cdr().(values.Tuple)
			if !cok {
				break
			}
			tuple = cdr
		}
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
		Mode: packages.NeedName |
			packages.NeedFiles |
			packages.NeedSyntax |
			packages.NeedTypes |
			packages.NeedTypesInfo |
			packages.NeedTypesSizes |
			packages.NeedImports |
			packages.NeedDeps,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errAnalyzeBuildError,
			"go-analyze: %s: %s", pattern.Value, loadErr)
	}

	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errAnalyzeBuildError,
			"go-analyze: %s: %s", pattern.Value, strings.Join(errs, "; "))
	}

	// Run analyzers on each loaded package; collect all diagnostics.
	var allDiags []diagnostic
	for _, pkg := range pkgs {
		allDiags = append(allDiags, runAnalyzers(pkg, fset, analyzers)...)
	}

	// Map diagnostics to s-expressions.
	result := make([]values.Value, len(allDiags))
	for i, d := range allDiags {
		pos := fset.Position(d.diag.Pos)
		fields := []values.Value{
			goast.Field("analyzer", goast.Str(d.analyzerName)),
			goast.Field("pos", goast.Str(pos.String())),
			goast.Field("message", goast.Str(d.diag.Message)),
			goast.Field("category", goast.Str(d.diag.Category)),
		}
		result[i] = goast.Node("diagnostic", fields...)
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
