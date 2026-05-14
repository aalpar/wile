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

package values

// init validates the numeric registry at package initialization. Go runs
// init() functions across a package's files in lexical filename order. This
// file's 'v' prefix puts it after every per-type file (which sort under
// 'b'..'r') so by the time this init runs, every kind's registerNumericSpec
// call has already executed.
//
// Failing here makes registry incompleteness fatal at process startup
// rather than lazily on first arithmetic dispatch.
func init() {
	validateNumericSpecs(numericRegistry, registryFilled)
}
