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

package io

// ExportCacheSizes returns a State's (parsers, tokenizers) cache entry counts,
// for the io_test package to assert that EOF/close eviction leaves no lingering
// entries. The caches and their mutex are unexported; this read-locked accessor
// exposes only the sizes, without widening the production API.
func ExportCacheSizes(st *State) (int, int) {
	st.mu.RLock()
	defer st.mu.RUnlock()
	return len(st.parsers), len(st.tokenizers)
}
