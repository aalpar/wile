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

package compilation

import "github.com/aalpar/wile/machine"

// Compilation-time operations are all side-table dispatched via OpComplex.
// See machine/op_kind.go for the package-level Op type to OpCode map.
//
// OpComplex dispatch routes through sideTable[i].Apply(mc), so every type
// below must also implement machine.InlinedOperation. The var declarations
// enforce this at compile time -- any new compilation-time op that forgets
// Apply will fail to build rather than panic at runtime.

var (
	_ machine.InlinedOperation = (*OperationBuildSyntaxList)(nil)
	_ machine.InlinedOperation = (*OperationSyntaxRulesTransform)(nil)
	_ machine.InlinedOperation = (*OperationSyntaxCaseMatch)(nil)
	_ machine.InlinedOperation = (*OperationBindPatternVars)(nil)
	_ machine.InlinedOperation = (*OperationSyntaxCaseNoMatch)(nil)
	_ machine.InlinedOperation = (*OperationSyntaxTemplateExpand)(nil)
	_ machine.InlinedOperation = (*OperationStoreSyntaxCaseInput)(nil)
	_ machine.InlinedOperation = (*OperationClearSyntaxCaseInput)(nil)
)

func (*OperationBuildSyntaxList) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationSyntaxRulesTransform) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationSyntaxCaseMatch) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationBindPatternVars) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationSyntaxCaseNoMatch) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationSyntaxTemplateExpand) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationStoreSyntaxCaseInput) OpKind() machine.OpCode {
	return machine.OpComplex
}

func (*OperationClearSyntaxCaseInput) OpKind() machine.OpCode {
	return machine.OpComplex
}
