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

package wile

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestProfile_String(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		profile Profile
		want    string
	}{
		{Tiny, "tiny"},
		{Console, "console"},
		{ConsoleWithLoad, "console-with-load"},
		{Small, "small"},
		{KitchenSink, "kitchen-sink"},
	}
	for _, tt := range tests {
		c.Run(tt.want, func(c *qt.C) {
			c.Assert(tt.profile.String(), qt.Equals, tt.want)
		})
	}
}

func TestProfile_Extensions_Tiny(t *testing.T) {
	c := qt.New(t)
	exts := Tiny.extensions()
	c.Assert(exts, qt.HasLen, 0)
}

func TestProfile_Extensions_ConsoleWithLoad_HasEval(t *testing.T) {
	c := qt.New(t)
	consoleExts := Console.extensions()
	cwlExts := ConsoleWithLoad.extensions()
	// ConsoleWithLoad is Console + eval -- exactly one more extension
	c.Assert(len(cwlExts), qt.Equals, len(consoleExts)+1)
}

func TestProfile_Extensions_KitchenSink(t *testing.T) {
	c := qt.New(t)
	exts := KitchenSink.extensions()
	c.Assert(len(exts) > 0, qt.IsTrue)
}
