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

	"github.com/aalpar/wile/values"
)

// --- Branch ---

type OperationBranchOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationBranchOffsetImmediate(offset int) *OperationBranchOffsetImmediate {
	return &OperationBranchOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-branch-offset-immediate"),
		Offset:        offset,
	}
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationBranchOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOffsetImmediate)
	return FieldMatches(p, v, ok, func(op *OperationBranchOffsetImmediate) int { return op.Offset })
}

// --- BranchOnFalseValue ---

// OperationBranchOnFalseValueOffsetImmediate branches if the value register
// is #f. This reads directly from the value register instead of popping from
// the eval stack, eliminating the Push instruction that would otherwise be
// needed.
//
// Peephole optimization (Aho et al., Compilers §8.9): examines a small
// window of generated instructions and replaces inefficient patterns. Here,
// the Push+BranchOnFalse+Pop sequence is replaced with a single
// BranchOnFalseValue that reads the value register directly.
// See BIBLIOGRAPHY.md "Peephole Optimization".
type OperationBranchOnFalseValueOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationBranchOnFalseValueOffsetImmediate(offset int) *OperationBranchOnFalseValueOffsetImmediate {
	return &OperationBranchOnFalseValueOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-branch-on-false-value-offset-immediate"),
		Offset:        offset,
	}
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationBranchOnFalseValueOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-false-value-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnFalseValueOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnFalseValueOffsetImmediate)
	return FieldMatches(p, v, ok, func(op *OperationBranchOnFalseValueOffsetImmediate) int {
		return op.Offset
	})
}

// --- SaveContinuation ---

type OperationSaveContinuationOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationSaveContinuationOffsetImmediate(off int) *OperationSaveContinuationOffsetImmediate {
	return &OperationSaveContinuationOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-save-continuation-offset-immediate"),
		Offset:        off,
	}
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationSaveContinuationOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-save-continuation-offset-immediate %d>", p.Offset)
}

func (p *OperationSaveContinuationOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSaveContinuationOffsetImmediate)
	return FieldMatches(p, v, ok, func(op *OperationSaveContinuationOffsetImmediate) int { return op.Offset })
}

// --- RestoreContinuation ---

type OperationRestoreContinuation struct {
	OperationBase
}

func NewOperationRestoreContinuation() *OperationRestoreContinuation {
	return &OperationRestoreContinuation{
		OperationBase: NewOperationBase("machine-operation-restore-continuation"),
	}
}

func (p *OperationRestoreContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationRestoreContinuation)
	return SameType(p, v, ok)
}
