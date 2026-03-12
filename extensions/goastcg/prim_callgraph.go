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

package goastcg

import (
	"go/token"
	"sort"
	"strings"

	"golang.org/x/tools/go/callgraph"
	"golang.org/x/tools/go/callgraph/cha"
	"golang.org/x/tools/go/callgraph/rta"
	"golang.org/x/tools/go/callgraph/static"
	"golang.org/x/tools/go/callgraph/vta"
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
	errCGBuildError       = werr.NewStaticError("callgraph build error")
	errCGInvalidAlgorithm = werr.NewStaticError("invalid callgraph algorithm")
	errCGNoMainFunction   = werr.NewStaticError("no main function for rta")
)

// validAlgorithms lists the accepted algorithm symbols.
var validAlgorithms = map[string]bool{
	"static": true,
	"cha":    true,
	"rta":    true,
	"vta":    true,
}

// PrimGoCallgraph implements (go-callgraph pattern algorithm).
func PrimGoCallgraph(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-callgraph")
	if err != nil {
		return err
	}

	algo, err := helpers.RequireArg[*values.Symbol](mc, 1, werr.ErrNotASymbol, "go-callgraph")
	if err != nil {
		return err
	}

	if !validAlgorithms[algo.Key] {
		return werr.WrapForeignErrorf(errCGInvalidAlgorithm,
			"go-callgraph: algorithm must be static, cha, rta, or vta; got %s", algo.Key)
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
			packages.NeedImports |
			packages.NeedDeps,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errCGBuildError,
			"go-callgraph: %s: %s", pattern.Value, loadErr)
	}

	// Check for package load errors.
	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errCGBuildError,
			"go-callgraph: %s: %s", pattern.Value,
			strings.Join(errs, "; "))
	}

	// Build SSA for the entire program (call graph needs cross-package edges).
	prog, _ := ssautil.Packages(pkgs, ssa.SanityCheckFunctions|ssa.InstantiateGenerics)
	for _, pkg := range prog.AllPackages() {
		pkg.Build()
	}

	// Dispatch to selected algorithm.
	cg, cgErr := dispatchCallgraph(prog, algo.Key)
	if cgErr != nil {
		return cgErr
	}

	mapper := &cgMapper{fset: fset}
	mc.SetValue(mapper.mapGraph(cg))
	return nil
}

// dispatchCallgraph dispatches to the selected call graph algorithm.
func dispatchCallgraph(prog *ssa.Program, algorithm string) (*callgraph.Graph, error) {
	switch algorithm {
	case "static":
		return static.CallGraph(prog), nil

	case "cha":
		return cha.CallGraph(prog), nil

	case "rta":
		mains := ssautil.MainPackages(prog.AllPackages())
		var roots []*ssa.Function
		for _, m := range mains {
			f := m.Func("main")
			if f != nil {
				roots = append(roots, f)
			}
		}
		if len(roots) == 0 {
			return nil, werr.WrapForeignErrorf(errCGNoMainFunction,
				"go-callgraph: rta requires a main function; use cha or vta for libraries")
		}
		result := rta.Analyze(roots, true)
		return result.CallGraph, nil

	case "vta":
		initial := cha.CallGraph(prog)
		allFuncs := ssautil.AllFunctions(prog)
		return vta.CallGraph(allFuncs, initial), nil

	default:
		return nil, werr.WrapForeignErrorf(errCGInvalidAlgorithm,
			"go-callgraph: unknown algorithm %s", algorithm)
	}
}

// findCGNode walks a list of cg-node s-expressions and returns the
// node whose "name" field exactly matches the given function name.
// Returns nil if not found.
func findCGNode(graph values.Value, name string) values.Value {
	tuple, ok := graph.(values.Tuple)
	if !ok {
		return nil
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if !ok {
			goto next
		}
		{
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				s, ok := nameVal.(*values.String)
				if ok && s.Value == name {
					return node
				}
			}
		}
	next:
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return nil
}

// PrimGoCallgraphCallers implements (go-callgraph-callers graph func-name).
func PrimGoCallgraphCallers(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-callers")
	if err != nil {
		return err
	}

	node := findCGNode(graph, funcName.Value)
	if node == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	np, ok := node.(*values.Pair)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	edgesIn, ok := goast.GetField(np.Cdr(), "edges-in")
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	mc.SetValue(edgesIn)
	return nil
}

// PrimGoCallgraphCallees implements (go-callgraph-callees graph func-name).
func PrimGoCallgraphCallees(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-callees")
	if err != nil {
		return err
	}

	node := findCGNode(graph, funcName.Value)
	if node == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	np, ok := node.(*values.Pair)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	edgesOut, ok := goast.GetField(np.Cdr(), "edges-out")
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	mc.SetValue(edgesOut)
	return nil
}

// buildNodeMap parses a list of cg-node s-expressions into a Go map
// keyed by function name for efficient BFS lookup.
func buildNodeMap(graph values.Value) map[string]values.Value {
	nodeMap := make(map[string]values.Value)
	tuple, ok := graph.(values.Tuple)
	if !ok {
		return nodeMap
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if ok {
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				s, ok := nameVal.(*values.String)
				if ok {
					nodeMap[s.Value] = node
				}
			}
		}
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return nodeMap
}

// calleesOf extracts the callee function names from the edges-out of a cg-node.
func calleesOf(node values.Value) []string {
	np, ok := node.(*values.Pair)
	if !ok {
		return nil
	}
	edgesOut, ok := goast.GetField(np.Cdr(), "edges-out")
	if !ok {
		return nil
	}
	tuple, ok := edgesOut.(values.Tuple)
	if !ok {
		return nil
	}
	var names []string
	for !values.IsEmptyList(tuple) {
		ep, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		edge := ep.Car()
		edgePair, ok := edge.(*values.Pair)
		if ok {
			callee, found := goast.GetField(edgePair.Cdr(), "callee")
			if found {
				s, ok := callee.(*values.String)
				if ok {
					names = append(names, s.Value)
				}
			}
		}
		tuple, ok = ep.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return names
}

// computeReachable does BFS from rootName, returning the set of reachable function names.
// Returns an empty set if rootName is not in nodeMap.
func computeReachable(nodeMap map[string]values.Value, rootName string) map[string]bool {
	_, ok := nodeMap[rootName]
	if !ok {
		return map[string]bool{}
	}

	visited := make(map[string]bool)
	queue := []string{rootName}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		if visited[current] {
			continue
		}
		visited[current] = true

		node, ok := nodeMap[current]
		if !ok {
			continue
		}

		for _, callee := range calleesOf(node) {
			if !visited[callee] {
				queue = append(queue, callee)
			}
		}
	}
	return visited
}

// PrimGoCallgraphReachable implements (go-callgraph-reachable graph root-name).
func PrimGoCallgraphReachable(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	rootName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-reachable")
	if err != nil {
		return err
	}

	nodeMap := buildNodeMap(graph)
	reachable := computeReachable(nodeMap, rootName.Value)

	names := make([]string, 0, len(reachable))
	for name := range reachable {
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
