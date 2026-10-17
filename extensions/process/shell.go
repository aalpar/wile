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
	"runtime"
	"strings"
	"syscall"
)

// shellSpec is the host shell and the flag that makes it run one command string.
type shellSpec struct {
	path string
	flag string
}

// signalSpec binds a Scheme signal symbol to the OS signal it delivers.
type signalSpec struct {
	name string
	sig  syscall.Signal
}

// defaultShell and defaultSignals serve every GOOS absent from the tables
// below. Both are keyed by GOOS as data rather than build tags so each row is
// testable on any host.
//
// The shell is a fixed table, never SHELL or COMSPEC: gateExecutable gates the
// resolved path as file:exec, and an env-derived path would let the program
// choose what gets gated.
//
// The windows rows are asserted, not measured: no workflow or release target
// runs on Windows. os.Process.Signal on Windows delivers only Kill.
var (
	defaultShell = shellSpec{path: "/bin/sh", flag: "-c"}

	shellByGOOS = map[string]shellSpec{
		"windows": {path: "cmd.exe", flag: "/c"},
	}

	defaultSignals = []signalSpec{
		{"term", syscall.SIGTERM},
		{"kill", syscall.SIGKILL},
		{"int", syscall.SIGINT},
		{"hup", syscall.SIGHUP},
	}

	signalByGOOS = map[string][]signalSpec{
		"windows": {
			{"kill", syscall.SIGKILL},
		},
	}
)

// shellFor returns the shell for goos.
func shellFor(goos string) shellSpec {
	spec, ok := shellByGOOS[goos]
	if !ok {
		return defaultShell
	}
	return spec
}

// signalsFor returns the signals process-kill offers on goos, in the order the
// error message lists them.
func signalsFor(goos string) []signalSpec {
	specs, ok := signalByGOOS[goos]
	if !ok {
		return defaultSignals
	}
	return specs
}

// hostShell returns the shell for the running GOOS.
func hostShell() shellSpec {
	return shellFor(runtime.GOOS)
}

// hostSignals returns the signals for the running GOOS.
func hostSignals() []signalSpec {
	return signalsFor(runtime.GOOS)
}

// lookupSignal finds name among specs.
func lookupSignal(specs []signalSpec, name string) (syscall.Signal, bool) {
	for _, s := range specs {
		if s.name == name {
			return s.sig, true
		}
	}
	return 0, false
}

// signalNames renders specs as "a, b, or c" for the unknown-signal message.
func signalNames(specs []signalSpec) string {
	names := make([]string, len(specs))
	for i, s := range specs {
		names[i] = s.name
	}
	if len(names) < 2 {
		return strings.Join(names, "")
	}
	last := len(names) - 1
	return strings.Join(names[:last], ", ") + ", or " + names[last]
}
