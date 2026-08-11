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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the process execution extension.
var Extension = registry.NewDescribedExtension("process",
	"OS processes: shell command execution, subprocess spawn/wait/kill with piped stdio.",
	AddToRegistry)

// Builder aggregates all process registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all process primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.PrimitiveRegistry) error {
	// Engine.Close kills and reaps the children the closing engine spawned. The
	// hook takes no state from here: it finds the tracker on the namespace of the
	// engine being closed (close.go), which is what keeps it per-engine even when
	// this registry is shared and every engine binds the first engine's
	// process-spawn. A registry.Closeable hook on the Extension var above could
	// not do this at all — that var is package-level and gets no engine handle.
	r.AddCloser(closeLiveProcs)
	r.AddPrimitives([]registry.PrimitiveSpec{
		// TODO(contracts): *values.Process has no ValueType enum entry, so
		// process-* primitives use TypeAny for the process argument; the
		// impl's RequireArg still rejects non-processes.
		{Name: "system", ParamCount: 1, Impl: PrimSystem,
			Doc: "Runs COMMAND as a shell command via /bin/sh -c. Returns the exit code as an exact integer.", ParamNames: []string{"command"}, Category: "process",
			Keywords:   []string{"shell", "exec", "run command", "subprocess"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeInteger},
		{Name: "process-spawn", ParamCount: 2, IsVariadic: true, Impl: PrimProcessSpawn,
			Doc: "Starts a child process with piped stdin/stdout/stderr. Returns a process object.", ParamNames: []string{"command", "args"}, Category: "process",
			Keywords:   []string{"fork", "exec", "launch", "subprocess", "popen"},
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeString}, ReturnType: values.TypeAny},
		{Name: "process-stdout", ParamCount: 1, Impl: PrimProcessStdout,
			Doc: "Returns the stdout of PROCESS as a textual input port.", ParamNames: []string{"process"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeTextualInputPort},
		{Name: "process-stderr", ParamCount: 1, Impl: PrimProcessStderr,
			Doc: "Returns the stderr of PROCESS as a textual input port.", ParamNames: []string{"process"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeTextualInputPort},
		{Name: "process-stdin", ParamCount: 1, Impl: PrimProcessStdin,
			Doc: "Returns the stdin of PROCESS as a textual output port.", ParamNames: []string{"process"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeTextualOutputPort},
		{Name: "process-wait", ParamCount: 1, Impl: PrimProcessWait,
			Doc: "Waits for PROCESS to exit and returns its exit code as an exact integer.", ParamNames: []string{"process"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeInteger},
		{Name: "process-kill", ParamCount: 2, Impl: PrimProcessKill,
			Doc: "Sends SIGNAL to PROCESS. SIGNAL is a symbol: term, kill, int, or hup.", ParamNames: []string{"process", "signal"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeSymbol},
			ReturnType: values.TypeVoid},
		{Name: "process?", ParamCount: 1, Impl: PrimProcessQ,
			Doc: "Returns #t if OBJ is a spawned process object.", ParamNames: []string{"obj"}, Category: "process",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)
	return nil
}
