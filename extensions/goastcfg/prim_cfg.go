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
	indexInt, ok := indexVal.(*values.Integer)
	if !ok {
		return cfgBlockInfo{}, false
	}
	idx := indexInt.Value

	idomVal, found := goast.GetField(np.Cdr(), "idom")
	idom := int64(-1)
	if found && idomVal != values.FalseValue {
		idomInt, ok := idomVal.(*values.Integer)
		if !ok {
			return cfgBlockInfo{}, false
		}
		idom = idomInt.Value
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
// Returns an error for non-symbol values or unrecognized option names.
func parseCFGOpts(rest values.Value, fset *token.FileSet) (*cfgMapper, error) {
	opts := &cfgMapper{fset: fset}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return opts, nil
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		s, ok := pair.Car().(*values.Symbol)
		if !ok {
			return nil, werr.WrapForeignErrorf(errCFGBuildError,
				"go-cfg: options must be symbols, got %T", pair.Car())
		}
		switch s.Key {
		case "positions":
			opts.positions = true
		default:
			return nil, werr.WrapForeignErrorf(errCFGBuildError,
				"go-cfg: unknown option '%s'; valid options: positions", s.Key)
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return opts, nil
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
	mapper, err := parseCFGOpts(mc.Arg(2), fset)
	if err != nil {
		return err
	}

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

// PrimGoCFGDominates implements (go-cfg-dominates? dom-tree a b).
// Returns #t if block a dominates block b (a is an ancestor of b in dom-tree).
func PrimGoCFGDominates(mc *machine.MachineContext) error {
	aVal, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotANumber, "go-cfg-dominates?")
	if err != nil {
		return err
	}
	bVal, err := helpers.RequireArg[*values.Integer](mc, 2, werr.ErrNotANumber, "go-cfg-dominates?")
	if err != nil {
		return err
	}

	// Build a parent map from the dom-tree: block index -> idom index (-1 for entry).
	parent := make(map[int64]int64)
	tuple, ok := mc.Arg(0).(values.Tuple)
	for ok && !values.IsEmptyList(tuple) {
		pair, ok2 := tuple.(*values.Pair)
		if !ok2 {
			break
		}
		np, ok2 := pair.Car().(*values.Pair)
		if ok2 {
			blockVal, found := goast.GetField(np.Cdr(), "block")
			if found {
				blockInt, blockOK := blockVal.(*values.Integer)
				if !blockOK {
					tuple, ok = pair.Cdr().(values.Tuple)
					continue
				}
				idx := blockInt.Value
				idomVal, found := goast.GetField(np.Cdr(), "idom")
				if found && idomVal != values.FalseValue {
					idomInt, idomOK := idomVal.(*values.Integer)
					if !idomOK {
						tuple, ok = pair.Cdr().(values.Tuple)
						continue
					}
					parent[idx] = idomInt.Value
				} else {
					parent[idx] = -1
				}
			}
		}
		tuple, ok = pair.Cdr().(values.Tuple)
	}

	// Walk from b toward the root; a dominates b iff a appears on the path.
	current := bVal.Value
	for {
		if current == aVal.Value {
			mc.SetValue(values.TrueValue)
			return nil
		}
		p, found := parent[current]
		if !found || p < 0 {
			break
		}
		current = p
	}
	mc.SetValue(values.BoolToBoolean(current == aVal.Value))
	return nil
}

const maxCFGPaths = 1024

// PrimGoCFGPaths implements (go-cfg-paths cfg from to).
// Returns a list of simple paths (lists of block indices) from block `from`
// to block `to`. Capped at maxCFGPaths to bound cost.
func PrimGoCFGPaths(mc *machine.MachineContext) error {
	fromVal, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotANumber, "go-cfg-paths")
	if err != nil {
		return err
	}
	toVal, err := helpers.RequireArg[*values.Integer](mc, 2, werr.ErrNotANumber, "go-cfg-paths")
	if err != nil {
		return err
	}

	// Build adjacency map from cfg-block list.
	blocks := parseCFGBlocks(mc.Arg(0))
	succs := make(map[int64][]int64, len(blocks))
	for _, b := range blocks {
		succs[b.index] = b.succs
	}

	// DFS to enumerate simple paths.
	var paths [][]int64
	visited := make(map[int64]bool)

	var dfs func(current int64, path []int64)
	dfs = func(current int64, path []int64) {
		if len(paths) >= maxCFGPaths {
			return
		}
		path = append(path, current)
		if current == toVal.Value {
			cp := make([]int64, len(path))
			copy(cp, path)
			paths = append(paths, cp)
			return
		}
		visited[current] = true
		for _, next := range succs[current] {
			if !visited[next] {
				dfs(next, path)
			}
		}
		visited[current] = false
	}

	dfs(fromVal.Value, nil)

	// Convert paths to s-expression: list of lists of integers.
	pathVals := make([]values.Value, len(paths))
	for i, p := range paths {
		blockVals := make([]values.Value, len(p))
		for j, idx := range p {
			blockVals[j] = values.NewInteger(idx)
		}
		pathVals[i] = goast.ValueList(blockVals)
	}
	mc.SetValue(goast.ValueList(pathVals))
	return nil
}
