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

package helpers

import (
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// MakeUnaryAccessor returns a 1-argument primitive that requires its argument to
// have concrete type T (raising sentinel/name on mismatch via RequireArg), then
// sets the value register to project(arg). It factors the
// extract → project → SetValue shape shared by the typed unary accessors
// (unbox, hashtable-keys, mutex-name, channel-length, ...). The projection
// performs any result wrapping (NewInteger, StringOrFalse, BoolToBoolean, ...)
// so a single helper covers every accessor return shape.
func MakeUnaryAccessor[T any](sentinel error, name string, project func(arg T) values.Value) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		arg, err := RequireArg[T](mc, 0, sentinel, name)
		if err != nil {
			return err
		}
		mc.SetValue(project(arg))
		return nil
	}
}

// MakeUnarySideEffect returns a 1-argument primitive that requires its argument
// to have concrete type T, runs act(arg) for its side effect, and returns the
// unspecified value. It factors the extract → mutate → void shape
// (mutex-lock!, wait-group-done!, thread-terminate!, hashtable-clear!, ...).
func MakeUnarySideEffect[T any](sentinel error, name string, act func(arg T)) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		arg, err := RequireArg[T](mc, 0, sentinel, name)
		if err != nil {
			return err
		}
		act(arg)
		mc.SetValue(values.Void)
		return nil
	}
}

// MakeBinarySetter returns a 2-argument primitive that requires its first
// argument to have concrete type T, applies set(arg, mc.Arg(1)) for its side
// effect, and returns the unspecified value. It factors the
// extract → set field → void shape (set-car!, atomic-store!,
// mutex-specific-set!, ...). The second argument is passed through untyped; the
// setter performs any further coercion.
func MakeBinarySetter[T any](sentinel error, name string, set func(arg T, val values.Value)) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		arg, err := RequireArg[T](mc, 0, sentinel, name)
		if err != nil {
			return err
		}
		set(arg, mc.Arg(1))
		mc.SetValue(values.Void)
		return nil
	}
}
