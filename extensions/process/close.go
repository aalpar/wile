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

package process

import (
	"sync"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// liveProcsKey is this extension's private key into a Namespace's
// extension-state bag (Namespace.ExtensionState / SetExtensionState). Its
// unexported type guarantees it never collides with another extension's key.
type liveProcsKey struct{}

// trackerMu serialises the get-or-create on the extension-state bag. Namespace
// exposes Load and Store but no LoadOrStore, so two SRFI-18 threads spawning at
// the same instant could otherwise mint two trackers and drop whichever lost
// the Store.
var trackerMu sync.Mutex

// liveProcs tracks the children spawned under one engine, so Engine.Close can
// kill and reap them instead of orphaning them onto the host.
//
// It hangs off that engine's root Namespace rather than off a closure minted in
// addPrimitives, and that is load-bearing rather than stylistic: with a registry
// shared across engines (WithRegistry) Apply binds the FIRST engine's
// process-spawn in every engine, so a closure-captured tracker would collect
// every engine's children into the first engine's tracker — closing the engine
// that spawned a child would leave it running, and closing an unrelated engine
// would SIGKILL it. Keyed on the Namespace, the child lands in the tracker of
// the engine that ran the call.
type liveProcs struct {
	mu    sync.Mutex
	procs []*values.Process
}

func newLiveProcs() *liveProcs {
	return &liveProcs{}
}

// liveProcsFor returns ns's tracker, creating it on first use. ns must already
// be the root Namespace — see trackSpawnedProcess.
func liveProcsFor(ns *environment.Namespace) *liveProcs {
	trackerMu.Lock()
	defer trackerMu.Unlock()
	raw, ok := ns.ExtensionState(liveProcsKey{})
	if ok {
		live, isTracker := raw.(*liveProcs)
		if isTracker {
			return live
		}
	}
	q := newLiveProcs()
	ns.SetExtensionState(liveProcsKey{}, q)
	return q
}

// trackSpawnedProcess records proc against the engine whose namespace mc is
// running in. A context with no environment frame or no namespace (a bare
// sub-context) cannot name an engine, so nothing is tracked and the child is
// simply not reaped — the pre-fix behaviour, not a new failure.
//
// Root() and not the call's own namespace: a child spawned through
// (environment …) belongs to the engine that owns that child namespace, and
// Engine.Close only ever looks at the engine's own frame.
func trackSpawnedProcess(mc machine.CallContext, proc *values.Process) {
	env := mc.EnvironmentFrame()
	if env == nil {
		return
	}
	ns := env.Namespace()
	if ns == nil {
		return
	}
	liveProcsFor(ns.Root()).track(proc)
}

// closeLiveProcs is the registry.CloseFunc addPrimitives registers: it kills and
// reaps the children spawned under env's engine. An engine that never spawned
// one has no tracker and nothing to do.
func closeLiveProcs(env *environment.EnvironmentFrame) error {
	if env == nil {
		return nil
	}
	ns := env.Namespace()
	if ns == nil {
		return nil
	}
	root := ns.Root()
	trackerMu.Lock()
	raw, ok := root.ExtensionState(liveProcsKey{})
	root.DeleteExtensionState(liveProcsKey{})
	trackerMu.Unlock()
	if !ok {
		return nil
	}
	live, isTracker := raw.(*liveProcs)
	if !isTracker {
		return nil
	}
	return live.Close()
}

// track records a spawned child, dropping already-reaped entries first so a
// long-lived engine spawning many short children does not grow this slice
// without bound.
func (p *liveProcs) track(proc *values.Process) {
	p.mu.Lock()
	defer p.mu.Unlock()
	live := p.procs[:0]
	for _, pr := range p.procs {
		if pr.Reaped() {
			continue
		}
		live = append(live, pr)
	}
	p.procs = append(live, proc)
}

// Close kills and reaps every child spawned under this engine.
//
// Kill precedes Wait and is what bounds the wait: a Scheme (process-wait p)
// running on an SRFI-18 thread may already hold the process's wait, and Wait
// here blocks behind it — but the child has just been signalled, so that wait is
// about to return. Both errors are dropped: an already-reaped child fails Kill
// on a finished process, and Wait then replays the memoised result. Neither is
// an engine-close failure.
func (p *liveProcs) Close() error {
	p.mu.Lock()
	procs := p.procs
	p.procs = nil
	p.mu.Unlock()

	for _, proc := range procs {
		cmd := proc.Cmd()
		if cmd == nil || cmd.Process == nil || proc.Reaped() {
			continue
		}
		_ = cmd.Process.Kill()
		_ = proc.Wait()
	}
	return nil
}
