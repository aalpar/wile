// Copyright 2025 Aaron Alpar
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
	"context"

	"wile/values"
)

// OperationPopEnv unconditionally restores the parent environment.
// It pops one level from the environment chain (restoring the parent environment).
//
// This is used in syntax-case fender evaluation to restore the environment
// when the fender returns false, before branching to the next clause.
type OperationPopEnv struct{}

func NewOperationPopEnv() *OperationPopEnv {
	return &OperationPopEnv{}
}

func (p *OperationPopEnv) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	parent := mctx.env.Parent()
	if parent == nil {
		return nil, values.NewForeignError("PopEnv: cannot pop top-level environment")
	}
	mctx.env = parent
	mctx.pc++
	return mctx, nil
}

func (p *OperationPopEnv) String() string {
	return "PopEnv"
}

func (p *OperationPopEnv) SchemeString() string {
	return "#<operation:pop-env>"
}

func (p *OperationPopEnv) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationPopEnv)
	return ok
}

func (p *OperationPopEnv) IsVoid() bool {
	return false
}
