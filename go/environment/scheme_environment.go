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

package environment

import (
	"fmt"

	"wile/values"
)

var _ values.Value = (*SchemeEnvironment)(nil)

// SchemeEnvironment represents a first-class environment for use with eval.
// It wraps an environment frame for use as a Scheme value.
type SchemeEnvironment struct {
	// Name is an optional descriptive name (e.g., "interaction-environment")
	Name string
	// Frame is the actual environment frame - stored as interface{} to avoid
	// circular dependency with environment package
	Frame *EnvironmentFrame
}

// NewSchemeEnvironment creates a new scheme environment.
func NewSchemeEnvironment(name string, frame *EnvironmentFrame) *SchemeEnvironment {
	return &SchemeEnvironment{
		Name:  name,
		Frame: frame,
	}
}

// IsVoid returns true if the environment is nil.
func (p *SchemeEnvironment) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the environments are the same object.
func (p *SchemeEnvironment) EqualTo(v values.Value) bool {
	other, ok := v.(*SchemeEnvironment)
	if !ok {
		return false
	}
	return p == other // Environments are compared by identity
}

// SchemeString returns the Scheme representation of the environment.
func (p *SchemeEnvironment) SchemeString() string {
	if p.Name != "" {
		return fmt.Sprintf("#<environment %s>", p.Name)
	}
	return "#<environment>"
}
