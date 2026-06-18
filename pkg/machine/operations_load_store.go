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

package machine

import (
	"fmt"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// --- LoadVoid ---

var _ Operation = (*OperationLoadVoid)(nil)

type OperationLoadVoid struct {
	OperationBase
}

func NewOperationLoadVoid() *OperationLoadVoid {
	return &OperationLoadVoid{
		OperationBase: NewOperationBase("machine-operation-load-void"),
	}
}

func (p *OperationLoadVoid) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadVoid)
	return SameType(p, v, ok)
}

// --- LoadLiteral ---

// OperationLoadLiteralByLiteralIndexImmediate loads a literal value from the literals pool.
type OperationLoadLiteralByLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationLoadLiteralByLiteralIndexImmediate creates a new literal load operation.
func NewOperationLoadLiteralByLiteralIndexImmediate(li LiteralIndex) *OperationLoadLiteralByLiteralIndexImmediate {
	return &OperationLoadLiteralByLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-load-literal-by-literal-index-immediate"),
		LiteralIndex:  li,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadLiteralByLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-literal-by-literal-index-immediate %d>", p.LiteralIndex)
}

// EqualTo returns true if both operations have the same literal index.
func (p *OperationLoadLiteralByLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLiteralByLiteralIndexImmediate)
	return FieldMatches(p, v, ok, func(op *OperationLoadLiteralByLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}

// --- LoadGlobal ---

// OperationLoadGlobalByGlobalIndexLiteralIndexImmediate loads a global variable using an index from the literals pool.
type OperationLoadGlobalByGlobalIndexLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate creates a new global load operation.
func NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(li LiteralIndex) *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationLoadGlobalByGlobalIndexLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-load-global-by-global-index-literal-index-immediate"),
		LiteralIndex:  li,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-global-by-global-index-literal-index-immediate %d>", p.LiteralIndex)
}

// EqualTo returns true if both operations have the same literal index.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate)
	return FieldMatches(p, v, ok, func(op *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}

// --- LoadLocal ---

type OperationLoadLocalByLocalIndexImmediate struct {
	OperationBase
	LocalIndex *environment.LocalIndex
}

func NewOperationLoadLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationLoadLocalByLocalIndexImmediate {
	return &OperationLoadLocalByLocalIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-load-local-by-local-index-immediate"),
		LocalIndex:    li,
	}
}

func (p *OperationLoadLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationLoadLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLocalByLocalIndexImmediate)
	return fieldMethodMatches(p, v, ok,
		func(op *OperationLoadLocalByLocalIndexImmediate) *environment.LocalIndex {
			return op.LocalIndex
		},
		func(a, b *environment.LocalIndex) bool {
			return a.EqualTo(b)
		})
}

// --- LoadCachedBinding ---

// OperationLoadCachedBinding loads a global variable from a compile-time
// resolved *Binding pointer, bypassing the runtime environment lookup
// path used by OpLoadGlobal.
type OperationLoadCachedBinding struct {
	OperationBase
	BindingIndex int32
}

// NewOperationLoadCachedBinding creates a new cached binding load operation.
func NewOperationLoadCachedBinding(idx int32) *OperationLoadCachedBinding {
	return &OperationLoadCachedBinding{
		OperationBase: NewOperationBase("machine-operation-load-cached-binding"),
		BindingIndex:  idx,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadCachedBinding) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-cached-binding %d>", p.BindingIndex)
}

// EqualTo returns true if both operations have the same binding index.
func (p *OperationLoadCachedBinding) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadCachedBinding)
	return FieldMatches(p, v, ok, func(op *OperationLoadCachedBinding) int32 {
		return op.BindingIndex
	})
}

// --- StoreLocal ---

type OperationStoreLocalByLocalIndexImmediate struct {
	OperationBase
	LocalIndex *environment.LocalIndex
}

func NewOperationStoreLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationStoreLocalByLocalIndexImmediate {
	return &OperationStoreLocalByLocalIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-store-local-by-local-index-immediate"),
		LocalIndex:    li,
	}
}

func (p *OperationStoreLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationStoreLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreLocalByLocalIndexImmediate)
	return fieldMethodMatches(p, v, ok,
		func(op *OperationStoreLocalByLocalIndexImmediate) *environment.LocalIndex {
			return op.LocalIndex
		},
		func(a, b *environment.LocalIndex) bool {
			return a.EqualTo(b)
		})
}

// --- StoreGlobal ---

// OperationStoreGlobalByGlobalIndexLiteralIndexImmediate stores a value to a global variable using an index from the literals pool.
type OperationStoreGlobalByGlobalIndexLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate creates a new global store operation.
func NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti LiteralIndex) *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationStoreGlobalByGlobalIndexLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-store-global-by-global-index-literal-immediate"),
		LiteralIndex:  liti,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-global-by-global-index-literal-immediate %d>", p.LiteralIndex)
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreGlobalByGlobalIndexLiteralIndexImmediate)
	return FieldMatches(p, v, ok, func(op *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}
