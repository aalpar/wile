package environment

import (
	"sync"
	"testing"

	"github.com/aalpar/wile/pkg/values"
)

// TestBindingConcurrentGlobalReadWrite_D2 reproduces the D2 data race: a global
// binding shared across SRFI-18 threads is read lock-free (exactly as the VM's
// cachedBindings cache does — it holds the *Binding and calls Value() without
// taking the frame mutex) while another thread set!s it via SetOwnGlobalValue.
//
// Binding.value is a values.Value interface (two words: type + data). The
// unsynchronized reader/writer pair tears that interface, which the race
// detector reports as a DATA RACE and which can, under the wrong interleaving,
// yield a corrupt interface (nil-deref / bad type). The writer alternates
// between two DIFFERENT concrete types (*Integer, *String) so a torn read mixes
// one value's type word with another's data word.
//
// Pre-fix this fails the `-race` job. Post-fix (value published through an
// atomicCell) the read is a lock-free atomic load and the test is clean.
func TestBindingConcurrentGlobalReadWrite_D2(t *testing.T) {
	env := NewNamespaceFrame().GlobalEnvironment()
	sym := values.NewSymbol("x")
	gi, created := env.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(PhaseRuntime), false)
	if !created {
		t.Fatal("expected a fresh global binding")
	}

	// The stable *Binding pointer the compile-time cache would capture and then
	// read lock-free at runtime from any thread.
	bd := env.GetOwnGlobalBinding(gi)
	if bd == nil {
		t.Fatal("global binding not found")
	}

	valA := values.NewInteger(1)
	valB := values.NewString("two")

	const iterations = 20000
	var wg sync.WaitGroup
	stop := make(chan struct{})

	// Reader: lock-free Value(), mirroring cachedBindings[i].Value().
	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			select {
			case <-stop:
				return
			default:
			}
			// Every published value (initial Void, valA, valB) is non-nil; a
			// torn interface read could observe nil. The race detector is the
			// primary signal — this use just prevents the read being elided.
			if bd.Value() == nil {
				t.Errorf("read a nil (torn) value")
				return
			}
		}
	}()

	// Writer: set! under the frame lock.
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer close(stop)
		for i := range iterations {
			v := values.Value(valA)
			if i%2 == 1 {
				v = valB
			}
			err := env.SetOwnGlobalValue(gi, v)
			if err != nil {
				t.Errorf("SetOwnGlobalValue: %v", err)
				return
			}
		}
	}()

	wg.Wait()
}
