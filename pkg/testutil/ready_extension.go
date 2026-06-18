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

package testutil

import (
	"sync"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// ReadyExtension creates an extension that registers a `test-ready!`
// primitive. When Scheme code calls `(test-ready!)`, it closes the
// returned channel exactly once, providing a deterministic "code is
// running" signal to Go test code.
//
// Each call returns a fresh extension and channel. Do not reuse across
// subtests -- create a new ReadyExtension per subtest to get a fresh channel.
func ReadyExtension() (registry.Extension, <-chan struct{}) {
	ready := make(chan struct{})
	var once sync.Once
	ext := registry.NewExtension("test-ready", func(r *registry.Registry) error {
		r.AddPrimitive(registry.PrimitiveSpec{
			Name:       "test-ready!",
			ParamCount: 0,
			Impl: func(mc machine.CallContext) error {
				once.Do(func() {
					close(ready)
				})
				mc.SetValue(values.Void)
				return nil
			},
		}, registry.PhaseSetRuntime)
		return nil
	})
	return ext, ready
}
