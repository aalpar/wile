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

	"golang.org/x/tools/go/callgraph"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type cgMapper struct {
	fset *token.FileSet
}

// mapGraph converts a callgraph.Graph to a list of cg-node s-expressions.
func (p *cgMapper) mapGraph(cg *callgraph.Graph) values.Value {
	// Collect non-nil nodes sorted by ID for deterministic output.
	sorted := make([]*callgraph.Node, 0, len(cg.Nodes))
	for _, node := range cg.Nodes {
		if node.Func == nil {
			continue
		}
		sorted = append(sorted, node)
	}
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].ID < sorted[j].ID
	})

	nodes := make([]values.Value, len(sorted))
	for i, node := range sorted {
		nodes[i] = p.mapNode(node)
	}
	return goast.ValueList(nodes)
}

// mapNode converts a callgraph.Node to a cg-node s-expression.
func (p *cgMapper) mapNode(n *callgraph.Node) values.Value {
	edgesIn := make([]values.Value, len(n.In))
	for i, e := range n.In {
		edgesIn[i] = p.mapEdge(e)
	}

	edgesOut := make([]values.Value, len(n.Out))
	for i, e := range n.Out {
		edgesOut[i] = p.mapEdge(e)
	}

	fields := []values.Value{
		goast.Field("name", goast.Str(n.Func.String())),
		goast.Field("id", values.NewInteger(int64(n.ID))),
		goast.Field("edges-in", goast.ValueList(edgesIn)),
		goast.Field("edges-out", goast.ValueList(edgesOut)),
	}
	if n.Func.Pkg != nil {
		fields = append(fields, goast.Field("pkg", goast.Str(n.Func.Pkg.Pkg.Path())))
	}
	return goast.Node("cg-node", fields...)
}

// mapEdge converts a callgraph.Edge to a cg-edge s-expression.
func (p *cgMapper) mapEdge(e *callgraph.Edge) values.Value {
	fields := make([]values.Value, 0, 4)

	if e.Caller != nil && e.Caller.Func != nil {
		fields = append(fields, goast.Field("caller", goast.Str(e.Caller.Func.String())))
	}
	if e.Callee != nil && e.Callee.Func != nil {
		fields = append(fields, goast.Field("callee", goast.Str(e.Callee.Func.String())))
	}

	pos := e.Pos()
	if pos.IsValid() && p.fset != nil {
		fields = append(fields, goast.Field("pos", goast.Str(p.fset.Position(pos).String())))
	}

	fields = append(fields, goast.Field("description", goast.Str(e.Description())))

	return goast.Node("cg-edge", fields...)
}
