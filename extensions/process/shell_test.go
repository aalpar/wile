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
	"syscall"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestShellFor(t *testing.T) {
	tcs := []struct {
		goos string
		want shellSpec
	}{
		{"windows", shellSpec{path: "cmd.exe", flag: "/c"}},
		{"linux", shellSpec{path: "/bin/sh", flag: "-c"}},
		{"darwin", shellSpec{path: "/bin/sh", flag: "-c"}},
		{"freebsd", shellSpec{path: "/bin/sh", flag: "-c"}},
		{"", shellSpec{path: "/bin/sh", flag: "-c"}},
	}
	for _, tc := range tcs {
		t.Run(tc.goos, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(shellFor(tc.goos), qt.Equals, tc.want)
		})
	}
}

var posixOffers = map[string]syscall.Signal{
	"term": syscall.SIGTERM,
	"kill": syscall.SIGKILL,
	"int":  syscall.SIGINT,
	"hup":  syscall.SIGHUP,
}

func TestSignalsFor(t *testing.T) {
	tcs := []struct {
		goos      string
		wantNames string
		offers    map[string]syscall.Signal
		absent    []string
	}{
		{"linux", "term, kill, int, or hup", posixOffers, nil},
		{"darwin", "term, kill, int, or hup", posixOffers, nil},
		{"windows", "kill", map[string]syscall.Signal{"kill": syscall.SIGKILL}, []string{"term", "int", "hup"}},
	}
	for _, tc := range tcs {
		t.Run(tc.goos, func(t *testing.T) {
			c := qt.New(t)
			specs := signalsFor(tc.goos)
			c.Assert(signalNames(specs), qt.Equals, tc.wantNames)
			for name, want := range tc.offers {
				got, ok := lookupSignal(specs, name)
				c.Assert(ok, qt.IsTrue, qt.Commentf("signal %q", name))
				c.Assert(got, qt.Equals, want, qt.Commentf("signal %q", name))
			}
			for _, name := range tc.absent {
				_, ok := lookupSignal(specs, name)
				c.Assert(ok, qt.IsFalse, qt.Commentf("signal %q", name))
			}
		})
	}
}

func TestLookupSignalDelivers(t *testing.T) {
	c := qt.New(t)
	sig, ok := lookupSignal(signalsFor("windows"), "kill")
	c.Assert(ok, qt.IsTrue)
	c.Assert(sig, qt.Equals, syscall.SIGKILL)
}

func TestSignalNames(t *testing.T) {
	tcs := []struct {
		name  string
		specs []signalSpec
		want  string
	}{
		{"none", nil, ""},
		{"one", []signalSpec{{"kill", syscall.SIGKILL}}, "kill"},
		{"two", []signalSpec{{"a", 1}, {"b", 2}}, "a or b"},
		{"three", []signalSpec{{"a", 1}, {"b", 2}, {"c", 3}}, "a, b, or c"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(signalNames(tc.specs), qt.Equals, tc.want)
		})
	}
}
