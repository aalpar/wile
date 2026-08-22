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

package envvars

import (
	"maps"
	"os"
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// namespaceEnvMap returns the virtual env map associated with the caller's
// Namespace, or nil if none is configured. When non-nil, lookups read
// from this map instead of the process environment, bypassing the authorizer
// gate (capability granted by providing the map).
func namespaceEnvMap(mc machine.CallContext) map[string]string {
	env := mc.EnvironmentFrame()
	if env == nil {
		return nil
	}
	ns := env.Namespace()
	if ns == nil {
		return nil
	}
	return ns.EnvMap()
}

// PrimGetEnvironmentVariable implements the (get-environment-variable) primitive.
// When the Namespace has a non-nil EnvMap, reads from the virtual map;
// otherwise falls through to os.LookupEnv gated by the authorizer.
func PrimGetEnvironmentVariable(mc machine.CallContext) error {
	name, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "get-environment-variable")
	if err != nil {
		return err
	}

	envMap := namespaceEnvMap(mc)
	if envMap != nil {
		val, ok := envMap[name.Value]
		if ok {
			mc.SetValue(values.NewString(val))
			return nil
		}
		mc.SetValue(values.FalseValue)
		return nil
	}

	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceEnv,
		Action:   security.ActionRead,
		Target:   name.Value,
	})
	if err != nil {
		return err
	}
	val, exists := os.LookupEnv(name.Value)
	if exists {
		mc.SetValue(values.NewString(val))
		return nil
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// PrimGetEnvironmentVariables implements the (get-environment-variables) primitive.
// When the Namespace has a non-nil EnvMap, returns an alist built from
// the virtual map; otherwise falls through to os.Environ gated by the
// authorizer.
func PrimGetEnvironmentVariables(mc machine.CallContext) error {
	envMap := namespaceEnvMap(mc)
	if envMap != nil {
		// Sorted, because ranging a Go map is randomized per iteration: this
		// branch returned a different order on every call within one engine
		// (8 keys, 4 distinct orders in 6 calls), while the os.Environ branch
		// below walks a slice and is stable by construction. One primitive must
		// not have two stability stories. The sorted keys are consed in order, so
		// the result is ascending, matching that branch's shape.
		keys := slices.Sorted(maps.Keys(envMap))
		pairs := make([]values.Value, len(keys))
		for i, key := range keys {
			pairs[i] = values.NewCons(values.NewString(key), values.NewString(envMap[key]))
		}
		mc.SetValue(values.List(pairs...))
		return nil
	}

	err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceEnv,
		Action:   security.ActionRead,
		Target:   "*",
	})
	if err != nil {
		return err
	}
	env := os.Environ()
	pairs := make([]values.Value, 0, len(env))
	for _, entry := range env {
		parts := strings.SplitN(entry, "=", 2)
		if len(parts) == 2 {
			pairs = append(pairs, values.NewCons(values.NewString(parts[0]), values.NewString(parts[1])))
		}
	}
	mc.SetValue(values.List(pairs...))
	return nil
}
