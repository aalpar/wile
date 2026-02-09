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

package environment

// Environment is the interface for environment frames that support
// parent traversal and binding storage.
type Environment interface {
	Parent() Environment
	Values() []*Binding
	SetValues(v []*Binding)
	Keys() map[string]int
}

// EnvironmentNavigation is the interface for navigating between
// environment frames, including meta phases and local/global scopes.
type EnvironmentNavigation interface {
	// TODO: remove LocalEnvironment and GlobalEnvironment methods once
	// meta environments are fully integrated.
	Meta()
	Parent() Environment
	LocalEnvironment() Environment
	GlobalEnvironment() Environment
}
