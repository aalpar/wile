package goastcfg

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

var (
	errCFGBuildError   = werr.NewStaticError("cfg build error")
	errCFGFuncNotFound = werr.NewStaticError("function not found in package")
)

// cfgBlockInfo holds the parsed fields of a single cfg-block s-expression.
type cfgBlockInfo struct {
	index int64
	idom  int64 // -1 means no idom (entry block)
	succs []int64
}

// parseCFGBlocks extracts index, idom, and succs from a cfg-block list.
// Blocks whose tag or required fields are missing are silently skipped.
func parseCFGBlocks(cfg values.Value) []cfgBlockInfo {
	tuple, ok := cfg.(values.Tuple)
	if !ok {
		return nil
	}
	var blocks []cfgBlockInfo
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		info, ok := parseCFGBlock(pair.Car())
		if ok {
			blocks = append(blocks, info)
		}
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return blocks
}

func parseCFGBlock(node values.Value) (cfgBlockInfo, bool) {
	np, ok := node.(*values.Pair)
	if !ok {
		return cfgBlockInfo{}, false
	}
	indexVal, found := goast.GetField(np.Cdr(), "index")
	if !found {
		return cfgBlockInfo{}, false
	}
	idx := indexVal.(*values.Integer).Value

	idomVal, found := goast.GetField(np.Cdr(), "idom")
	idom := int64(-1)
	if found && idomVal != values.FalseValue {
		idom = idomVal.(*values.Integer).Value
	}

	succsField, found := goast.GetField(np.Cdr(), "succs")
	var succs []int64
	if found {
		st, ok := succsField.(values.Tuple)
		for ok && !values.IsEmptyList(st) {
			sp, ok2 := st.(*values.Pair)
			if !ok2 {
				break
			}
			sv, ok2 := sp.Car().(*values.Integer)
			if ok2 {
				succs = append(succs, sv.Value)
			}
			st, ok = sp.Cdr().(values.Tuple)
		}
	}

	return cfgBlockInfo{index: idx, idom: idom, succs: succs}, true
}

// parseCFGOpts extracts mapper options from the variadic rest-arg list.
func parseCFGOpts(rest values.Value, fset *token.FileSet) *cfgMapper {
	opts := &cfgMapper{fset: fset}
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

// findFunction looks up a function by name across all members and methods
// in an SSA package. Returns nil if not found.
func findFunction(prog *ssa.Program, ssaPkg *ssa.Package, name string) *ssa.Function {
	fn := ssaPkg.Func(name)
	if fn != nil {
		return fn
	}
	// Search methods on named types.
	for _, mem := range ssaPkg.Members {
		typ, ok := mem.(*ssa.Type)
		if !ok {
			continue
		}
		for _, recvType := range []types.Type{types.NewPointer(typ.Type()), typ.Type()} {
			mset := prog.MethodSets.MethodSet(recvType)
			for sel := range mset.Methods() {
				fn := prog.MethodValue(sel)
				if fn != nil && fn.Name() == name && fn.Pkg == ssaPkg {
					return fn
				}
			}
		}
	}
	return nil
}

// PrimGoCFG implements (go-cfg pattern func-name . options).
func PrimGoCFG(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-cfg")
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
	mapper := parseCFGOpts(mc.Arg(2), fset)

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
		return werr.WrapForeignErrorf(errCFGBuildError,
			"go-cfg: %s: %s", pattern.Value, loadErr)
	}

	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errCFGBuildError,
			"go-cfg: %s: %s", pattern.Value, strings.Join(errs, "; "))
	}

	prog, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg != nil {
			ssaPkg.Build()
		}
	}

	for _, ssaPkg := range ssaPkgs {
		if ssaPkg == nil {
			continue
		}
		fn := findFunction(prog, ssaPkg, funcName.Value)
		if fn == nil {
			continue
		}
		mc.SetValue(mapper.mapFunction(fn))
		return nil
	}

	return werr.WrapForeignErrorf(errCFGFuncNotFound,
		"go-cfg: function %q not found in %s", funcName.Value, pattern.Value)
}

// PrimGoCFGDominators implements (go-cfg-dominators cfg).
// Takes the cfg-block list from go-cfg and returns a list of dom-node
// s-expressions (the dominator tree, rooted at the entry block).
func PrimGoCFGDominators(mc *machine.MachineContext) error {
	blocks := parseCFGBlocks(mc.Arg(0))
	if len(blocks) == 0 {
		mc.SetValue(values.EmptyList)
		return nil
	}

	// Build children map: idom index -> list of child indices.
	children := make(map[int64][]int64)
	for _, b := range blocks {
		if b.idom >= 0 {
			children[b.idom] = append(children[b.idom], b.index)
		}
	}

	// Emit dom-node for each block.
	nodes := make([]values.Value, len(blocks))
	for i, b := range blocks {
		childVals := make([]values.Value, len(children[b.index]))
		for j, c := range children[b.index] {
			childVals[j] = values.NewInteger(c)
		}
		var idomVal values.Value
		if b.idom >= 0 {
			idomVal = values.NewInteger(b.idom)
		} else {
			idomVal = values.FalseValue
		}
		nodes[i] = goast.Node("dom-node",
			goast.Field("block", values.NewInteger(b.index)),
			goast.Field("idom", idomVal),
			goast.Field("children", goast.ValueList(childVals)),
		)
	}
	mc.SetValue(goast.ValueList(nodes))
	return nil
}
