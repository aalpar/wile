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

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// liveProcs tracks the children one engine spawned, so Engine.Close can kill
// and reap them instead of orphaning them onto the host. One instance per
// engine: addPrimitives mints it and closes the process-spawn Impl over it, and
// addPrimitives runs once per engine.
type liveProcs struct {
	mu    sync.Mutex
	procs []*values.Process
}

func newLiveProcs() *liveProcs {
	return &liveProcs{}
}

// track records a spawned child, dropping already-reaped entries first so a
// long-lived engine spawning many short children does not grow this slice
// without bound. A non-nil Cmd.ProcessState means some Wait already completed.
func (p *liveProcs) track(proc *values.Process) {
	p.mu.Lock()
	defer p.mu.Unlock()
	live := p.procs[:0]
	for _, pr := range p.procs {
		cmd := pr.Cmd()
		if cmd == nil || cmd.ProcessState != nil {
			continue
		}
		live = append(live, pr)
	}
	p.procs = append(live, proc)
}

// primProcessSpawn is the engine-scoped process-spawn: spawnProcess followed by
// tracking the child it started. A spawn that failed never reaches track.
func (p *liveProcs) primProcessSpawn(mc machine.CallContext) error {
	proc, err := spawnProcess(mc)
	if err != nil {
		return err
	}
	p.track(proc)
	mc.SetValue(proc)
	return nil
}

// Close kills and reaps every child this engine spawned.
//
// Both the Kill and the Wait errors are dropped: a Scheme (process-wait p) may
// already have reaped the child, in which case Kill fails on a finished process
// and Wait reports "already called" — neither is an engine-close failure, and
// the Engine is documented not safe for concurrent use, so no other goroutine
// is racing this one to the same child.
func (p *liveProcs) Close() error {
	p.mu.Lock()
	procs := p.procs
	p.procs = nil
	p.mu.Unlock()

	for _, proc := range procs {
		cmd := proc.Cmd()
		if cmd == nil || cmd.Process == nil || cmd.ProcessState != nil {
			continue
		}
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
	}
	return nil
}
