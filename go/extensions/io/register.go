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

// Package io provides I/O primitives for reading and writing.
//
//nolint:govet,revive // Using unkeyed struct fields, package name conflicts with stdlib
package io

import (
	"wile/environment"
	"wile/registry"
	"wile/runtime/primitives"
	"wile/values"
)

// Extension is the I/O extension.
var Extension = registry.NewExtension("io", AddToRegistry)

// Builder aggregates all I/O registration functions.
var Builder = registry.NewRegistryBuilder(
	addReadWrite,
	addPorts,
	addPortState,
)

// AddToRegistry registers all I/O primitives.
var AddToRegistry = Builder.AddToRegistry

func addReadWrite(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"read", 1, true, primitives.PrimRead},
		{"read-token", 1, true, primitives.PrimReadToken},
		{"read-syntax", 1, true, primitives.PrimReadSyntax},
		{"write", 2, true, primitives.PrimWrite},
		{"write-char", 2, true, primitives.PrimWriteChar},
		{"display", 2, true, primitives.PrimDisplay},
		{"newline", 1, true, primitives.PrimNewline},
		{"write-simple", 2, true, primitives.PrimWriteSimple},
		{"write-shared", 2, true, primitives.PrimWriteShared},
	}, registry.PhaseRuntime)
	return nil
}

func addPorts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"port?", 1, false, primitives.PrimPortQ},
		{"input-port?", 1, false, primitives.PrimInputPortQ},
		{"output-port?", 1, false, primitives.PrimOutputPortQ},
		{"input-port-open?", 1, false, primitives.PrimInputPortOpenQ},
		{"output-port-open?", 1, false, primitives.PrimOutputPortOpenQ},
		{"close-port", 1, false, primitives.PrimClosePort},
		{"close-input-port", 1, false, primitives.PrimClosePort},
		{"close-output-port", 1, false, primitives.PrimClosePort},
		{"eof-object", 0, false, primitives.PrimEofObject},
		{"eof-object?", 1, false, primitives.PrimEofObjectQ},
	}, registry.PhaseRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-string", 1, false, primitives.PrimOpenInputString},
		{"open-output-string", 0, false, primitives.PrimOpenOutputString},
		{"get-output-string", 1, false, primitives.PrimGetOutputString},
	}, registry.PhaseRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-bytevector", 1, false, primitives.PrimOpenInputBytevector},
		{"open-output-bytevector", 0, false, primitives.PrimOpenOutputBytevector},
		{"get-output-bytevector", 1, false, primitives.PrimGetOutputBytevector},
	}, registry.PhaseRuntime)

	return nil
}

func addPortState(r *registry.Registry) error {
	r.AddInitFunc(func(ctx registry.ApplyContext) error {
		primitives.InitState()
		registerPortParameters(ctx)
		return nil
	})
	return nil
}

func registerPortParameters(ctx registry.ApplyContext) {
	// Import the necessary types
	env := ctx.Environment()

	portParams := []struct {
		name  string
		param interface{}
	}{
		{"current-input-port", primitives.GetCurrentInputPortParam()},
		{"current-output-port", primitives.GetCurrentOutputPortParam()},
		{"current-error-port", primitives.GetCurrentErrorPortParam()},
	}

	for _, pp := range portParams {
		sym := env.InternSymbol(values.NewSymbol(pp.name))
		idx, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		env.SetOwnGlobalValue(idx, pp.param.(values.Value)) //nolint:errcheck
	}
}
