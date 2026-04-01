package testutil

import (
	"sync"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
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
			Impl: func(mc *machine.MachineContext) error {
				once.Do(func() {
					close(ready)
				})
				mc.SetValue(values.Void)
				return nil
			},
		}, registry.PhaseRuntime)
		return nil
	})
	return ext, ready
}
