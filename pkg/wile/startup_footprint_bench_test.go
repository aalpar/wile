package wile_test

import (
	"context"
	"runtime"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
)

// benchProfiles enumerates the engine configurations an embedder actually
// instantiates. "default" is NewEngine(ctx) with no profile option; the rest
// pin a named profile. These are the baseline configs for the cross-engine
// sealed-base-sharing measurement gate
// (plans/2026-06-30-cross-engine-sealed-base-sharing.local.md): startup cost
// is the latency a shared base would amortize, retained footprint is the
// memory a shared base would deduplicate.
var benchProfiles = []struct {
	name string
	opts []wile.EngineOption
}{
	{"default", nil},
	{"tiny", []wile.EngineOption{wile.WithProfile(wile.Tiny)}},
	{"small", []wile.EngineOption{wile.WithProfile(wile.Small)}},
	{"kitchen-sink", []wile.EngineOption{wile.WithProfile(wile.KitchenSink)}},
}

// BenchmarkEngineStartup measures wall-clock construction time and allocation
// volume per engine. The allocs/op figure is the phase-0 build cost (primitive
// registration, bootstrap closures, BindingMeta, inline-HOF templates) that a
// content-keyed cache would let engines 2..N skip.
func BenchmarkEngineStartup(b *testing.B) {
	ctx := context.Background()
	for _, p := range benchProfiles {
		b.Run(p.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				eng, err := wile.NewEngine(ctx, p.opts...)
				if err != nil {
					b.Fatalf("NewEngine(%s): %v", p.name, err)
				}
				runtime.KeepAlive(eng)
			}
		})
	}
}

// engineFootprint creates n live engines, holds them, and reports the retained
// heap delta per engine after a settling GC. This is the top-line "cost to keep
// one engine alive" — the shareable sealed base is a subset of it. Isolating
// that subset precisely needs a type-attributed heap profile (follow-up); this
// number bounds the best-case sharing win from above.
func engineFootprint(tb testing.TB, opts []wile.EngineOption, n int) uint64 {
	tb.Helper()
	ctx := context.Background()

	var m0 runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&m0)

	engines := make([]*wile.Engine, n)
	for i := range engines {
		eng, err := wile.NewEngine(ctx, opts...)
		if err != nil {
			tb.Fatalf("NewEngine: %v", err)
		}
		engines[i] = eng
	}

	var m1 runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&m1)

	delta := m1.HeapAlloc - m0.HeapAlloc
	runtime.KeepAlive(engines)
	return delta / uint64(n)
}

// TestEngineFootprintBaseline records retained bytes per live engine for each
// profile. Run with: go test -run TestEngineFootprintBaseline -v ./pkg/wile/
// It asserts nothing — it is a measurement probe for the sealed-base-sharing
// worth decision (D4). n=64 amortizes per-process fixed overhead out of the
// per-engine figure.
func TestEngineFootprintBaseline(t *testing.T) {
	const n = 64
	for _, p := range benchProfiles {
		bytesPerEngine := engineFootprint(t, p.opts, n)
		t.Logf("footprint %-13s = %7d bytes/engine (n=%d)", p.name, bytesPerEngine, n)
	}
}
