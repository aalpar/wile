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

// namedHandlerBase provides shared Name, IsVoid, and SchemeString
// implementations for named compile-time handlers stored as values.Value
// in the environment (SyntaxCompiler, PrimitiveExpander).
type namedHandlerBase struct {
	name   string
	prefix string
}

// Name returns the handler's name.
func (p *namedHandlerBase) Name() string {
	return p.name
}

// IsVoid returns false — named handlers are never void.
func (p *namedHandlerBase) IsVoid() bool {
	return false
}

// SchemeString returns the Scheme representation: #<prefix:name>.
func (p *namedHandlerBase) SchemeString() string {
	return "#<" + p.prefix + ":" + p.name + ">"
}
