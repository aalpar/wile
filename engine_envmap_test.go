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
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWithEnv_SingleVar(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnv("APP_MODE", "test"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx,
		`(get-environment-variable "APP_MODE")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `"test"`)
}

func TestWithEnv_NotFound(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnv("APP_MODE", "test"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx,
		`(get-environment-variable "NOPE")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}

func TestWithEnvMap(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{
			"DB_HOST": "localhost",
			"DB_PORT": "5432",
		}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx,
		`(get-environment-variable "DB_HOST")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `"localhost"`)
}

func TestWithEnv_ConsoleNoOSFallthrough(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	t.Setenv("HOME", "/home/test")

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx,
		`(get-environment-variable "HOME")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}

func TestWithEnv_GetEnvironmentVariables(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{"K": "V"}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalMultiple(ctx,
		`(get-environment-variables)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Contains, "K")
}
