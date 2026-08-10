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

package threads

import (
	"context"
	"errors"
	"sync"
	"time"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// threadCloseGrace bounds how long Engine.Close waits for the terminated
// threads' goroutines to actually exit. A thread parked in a Go-level operation
// that does not observe context cancellation (sync.Cond.Wait, a child's
// wait4 under (process-wait)) can outlive its Terminate, and Close must not
// block the host forever on it. The bound is for the whole set, not per thread
// — see Close.
const threadCloseGrace = 5 * time.Second

// errThreadCloseTimeout reports a thread whose goroutine was still running when
// the grace period expired.
var errThreadCloseTimeout = werr.NewStaticError("threads: thread did not exit before Close deadline")

// liveThreadsKey is this extension's private key into a Namespace's
// extension-state bag (Namespace.ExtensionState / SetExtensionState). Its
// unexported type guarantees it never collides with another extension's key.
type liveThreadsKey struct{}

// trackerMu serialises the get-or-create on the extension-state bag. Namespace
// exposes Load and Store but no LoadOrStore, so two SRFI-18 threads calling
// thread-start! at the same instant could otherwise mint two trackers and drop
// whichever lost the Store.
var trackerMu sync.Mutex

// liveThreads tracks the threads started under one engine, so Engine.Close can
// terminate them.
//
// It hangs off that engine's root Namespace rather than off a closure minted in
// addThreads, and that is load-bearing rather than stylistic: with a registry
// shared across engines (WithRegistry) Apply binds the FIRST engine's
// thread-start! in every engine, so a closure-captured tracker would collect
// every engine's threads into the first engine's tracker while the later
// engines' Close found nothing. Keyed on the Namespace, the thread lands in the
// tracker of the engine that ran the call, whichever engine's closure that was.
type liveThreads struct {
	mu      sync.Mutex
	threads []*values.Thread
}

func newLiveThreads() *liveThreads {
	return &liveThreads{}
}

// liveThreadsFor returns ns's tracker, creating it on first use. ns must already
// be the root Namespace — see trackStartedThread.
func liveThreadsFor(ns *environment.Namespace) *liveThreads {
	trackerMu.Lock()
	defer trackerMu.Unlock()
	raw, ok := ns.ExtensionState(liveThreadsKey{})
	if ok {
		live, isTracker := raw.(*liveThreads)
		if isTracker {
			return live
		}
	}
	q := newLiveThreads()
	ns.SetExtensionState(liveThreadsKey{}, q)
	return q
}

// trackStartedThread records th against the engine whose namespace mc is
// running in. A context with no environment frame or no namespace (a bare
// sub-context) cannot name an engine, so nothing is tracked and the thread is
// simply not reaped — the pre-fix behaviour, not a new failure.
//
// Root() and not the call's own namespace: a thread started through
// (environment …) belongs to the engine that owns that child namespace, and
// Engine.Close only ever looks at the engine's own frame.
func trackStartedThread(mc machine.CallContext, th *values.Thread) {
	env := mc.EnvironmentFrame()
	if env == nil {
		return
	}
	ns := env.Namespace()
	if ns == nil {
		return
	}
	liveThreadsFor(ns.Root()).track(th)
}

// closeLiveThreads is the registry.CloseFunc addThreads registers: it terminates
// the threads started under env's engine. An engine that never started one has
// no tracker and nothing to do.
func closeLiveThreads(env *environment.EnvironmentFrame) error {
	if env == nil {
		return nil
	}
	ns := env.Namespace()
	if ns == nil {
		return nil
	}
	root := ns.Root()
	trackerMu.Lock()
	raw, ok := root.ExtensionState(liveThreadsKey{})
	root.DeleteExtensionState(liveThreadsKey{})
	trackerMu.Unlock()
	if !ok {
		return nil
	}
	live, isTracker := raw.(*liveThreads)
	if !isTracker {
		return nil
	}
	return live.Close()
}

// track records a started thread, dropping already-terminated entries first so
// a long-lived engine running many short threads does not grow this slice
// without bound.
func (p *liveThreads) track(th *values.Thread) {
	p.mu.Lock()
	defer p.mu.Unlock()
	live := p.threads[:0]
	for _, t := range p.threads {
		if t.State() == values.ThreadTerminated {
			continue
		}
		live = append(live, t)
	}
	p.threads = append(live, th)
}

// Close terminates every thread started under this engine and waits, bounded,
// for their goroutines to exit. Terminate records the terminal outcome
// synchronously; the wait on Done() is what makes "Close returned" mean "the
// goroutines are gone" rather than "cancellation was requested".
//
// The grace period is ONE deadline for the whole set, not one per thread:
// terminate everything first, then wait against a single expiry. A per-thread
// timer inside the wait loop bounds Close at K x threadCloseGrace, so K threads
// parked in a Go call that ignores cancellation — (process-wait p) is one
// reachable from plain Scheme — stall the host for K x 5s, and `wile script.scm`
// hangs that long at exit before printing the warning.
func (p *liveThreads) Close() error {
	p.mu.Lock()
	threads := p.threads
	p.threads = nil
	p.mu.Unlock()

	live := make([]*values.Thread, 0, len(threads))
	for _, th := range threads {
		if th.State() == values.ThreadTerminated {
			continue
		}
		th.Terminate()
		live = append(live, th)
	}

	// A context rather than a timer: its Done channel stays closed after expiry,
	// so every straggler still gets its own diagnostic instead of the second wait
	// blocking on an already-drained timer channel.
	ctx, cancel := context.WithTimeout(context.Background(), threadCloseGrace)
	defer cancel()

	var errs []error
	for _, th := range live {
		// A finished thread wins over an expired deadline: select picks at random
		// among ready cases, which would otherwise report a timeout for a thread
		// that did exit.
		select {
		case <-th.Done():
			continue
		default:
		}
		select {
		case <-th.Done():
		case <-ctx.Done():
			errs = append(errs, werr.WrapForeignErrorf(errThreadCloseTimeout,
				"threads: Close: thread %q did not exit within %s", th.Name(), threadCloseGrace))
		}
	}
	return errors.Join(errs...)
}
