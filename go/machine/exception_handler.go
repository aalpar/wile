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

import "wile/values"

// ExceptionHandler represents an installed exception handler.
// Handlers form a linked list (stack) for dynamic exception handling.
// When an exception is raised, handlers are invoked in reverse order
// of installation (most recent first).
type ExceptionHandler struct {
	handler values.Value     // The handler procedure (closure)
	parent  *ExceptionHandler // Previous handler in chain
}

// NewExceptionHandler creates a new exception handler with the given
// handler procedure and parent handler.
func NewExceptionHandler(handler values.Value, parent *ExceptionHandler) *ExceptionHandler {
	return &ExceptionHandler{
		handler: handler,
		parent:  parent,
	}
}

// Handler returns the handler procedure.
func (p *ExceptionHandler) Handler() values.Value {
	return p.handler
}

// Parent returns the previous handler in the chain.
func (p *ExceptionHandler) Parent() *ExceptionHandler {
	return p.parent
}
