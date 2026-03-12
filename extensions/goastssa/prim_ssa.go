package goastssa

import (
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var errSSABuildError = werr.NewStaticError("ssa build error")

// parseSSAOpts extracts mapper options from a variadic rest-arg list.
func parseSSAOpts(rest values.Value, fset *token.FileSet) *ssaMapper {
	opts := &ssaMapper{fset: fset}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return opts
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		s, ok := pair.Car().(*values.Symbol)
		if ok && s.Key == "positions" {
			opts.positions = true
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return opts
}

// PrimGoSSABuild implements (go-ssa-build pattern . options).
func PrimGoSSABuild(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-ssa-build")
	if err != nil {
		return err
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
	mapper := parseSSAOpts(mc.Arg(1), fset)

	cfg := &packages.Config{
		Mode: packages.NeedName |
			packages.NeedFiles |
			packages.NeedSyntax |
			packages.NeedTypes |
			packages.NeedTypesInfo |
			packages.NeedImports |
			packages.NeedDeps,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errSSABuildError,
			"go-ssa-build: %s: %s", pattern.Value, loadErr)
	}

	// Check for package load errors.
	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errSSABuildError,
			"go-ssa-build: %s: %s", pattern.Value,
			strings.Join(errs, "; "))
	}

	// Build SSA.
	prog, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg != nil {
			ssaPkg.Build()
		}
	}

	// Collect source-level functions from the requested packages.
	var funcs []values.Value
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg == nil {
			continue
		}
		for _, mem := range ssaPkg.Members {
			fn, ok := mem.(*ssa.Function)
			if !ok {
				continue
			}
			if fn.Synthetic != "" {
				continue // skip compiler-generated functions
			}
			funcs = append(funcs, mapper.mapFunction(fn))
		}
		// Collect methods on named types.
		for _, mem := range ssaPkg.Members {
			typ, ok := mem.(*ssa.Type)
			if !ok {
				continue
			}
			mset := prog.MethodSets.MethodSet(types.NewPointer(typ.Type()))
			for sel := range mset.Methods() {
				fn := prog.MethodValue(sel)
				if fn == nil || fn.Synthetic != "" {
					continue
				}
				if fn.Pkg == ssaPkg {
					funcs = append(funcs, mapper.mapFunction(fn))
				}
			}
			// Value receiver methods.
			mset = prog.MethodSets.MethodSet(typ.Type())
			for sel := range mset.Methods() {
				fn := prog.MethodValue(sel)
				if fn == nil || fn.Synthetic != "" {
					continue
				}
				if fn.Pkg == ssaPkg {
					funcs = append(funcs, mapper.mapFunction(fn))
				}
			}
		}
	}

	mc.SetValue(goast.ValueList(funcs))
	return nil
}
