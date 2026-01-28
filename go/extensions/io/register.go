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
		{"read", 1, true, PrimRead},
		{"read-token", 1, true, PrimReadToken},
		{"read-syntax", 1, true, PrimReadSyntax},
		{"read-char", 1, true, PrimReadChar},
		{"peek-char", 1, true, PrimPeekChar},
		{"read-line", 1, true, PrimReadLine},
		{"read-string", 2, true, PrimReadString},
		{"char-ready?", 1, true, PrimCharReadyQ},
		{"write", 2, true, PrimWrite},
		{"write-char", 2, true, PrimWriteChar},
		{"write-string", 2, true, PrimWriteString},
		{"display", 2, true, PrimDisplay},
		{"newline", 1, true, PrimNewline},
		{"write-simple", 2, true, PrimWriteSimple},
		{"write-shared", 2, true, PrimWriteShared},
		{"flush-output-port", 1, true, PrimFlushOutputPort},
		// Binary I/O (R7RS §6.13.3)
		{"read-u8", 1, true, PrimReadU8},
		{"peek-u8", 1, true, PrimPeekU8},
		{"u8-ready?", 1, true, PrimU8ReadyQ},
		{"write-u8", 2, true, PrimWriteU8},
		{"read-bytevector", 2, true, PrimReadBytevector},
		{"read-bytevector!", 2, true, PrimReadBytevectorBang},
		{"write-bytevector", 2, true, PrimWriteBytevector},
	}, registry.PhaseRuntime)
	return nil
}

func addPorts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"port?", 1, false, PrimPortQ},
		{"input-port?", 1, false, PrimInputPortQ},
		{"output-port?", 1, false, PrimOutputPortQ},
		{"textual-port?", 1, false, PrimTextualPortQ},
		{"binary-port?", 1, false, PrimBinaryPortQ},
		{"input-port-open?", 1, false, PrimInputPortOpenQ},
		{"output-port-open?", 1, false, PrimOutputPortOpenQ},
		{"close-port", 1, false, PrimClosePort},
		{"close-input-port", 1, false, PrimClosePort},
		{"close-output-port", 1, false, PrimClosePort},
		{"eof-object", 0, false, PrimEofObject},
		{"eof-object?", 1, false, PrimEofObjectQ},
		{"call-with-port", 2, false, PrimCallWithPort},
	}, registry.PhaseRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-string", 1, false, PrimOpenInputString},
		{"open-output-string", 0, false, PrimOpenOutputString},
		{"get-output-string", 1, false, PrimGetOutputString},
	}, registry.PhaseRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-bytevector", 1, false, PrimOpenInputBytevector},
		{"open-output-bytevector", 0, false, PrimOpenOutputBytevector},
		{"get-output-bytevector", 1, false, PrimGetOutputBytevector},
	}, registry.PhaseRuntime)

	return nil
}

func addPortState(r *registry.Registry) error {
	r.AddInitFunc(func(ctx registry.ApplyContext) error {
		InitState()
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
		{"current-input-port", GetCurrentInputPortParam()},
		{"current-output-port", GetCurrentOutputPortParam()},
		{"current-error-port", GetCurrentErrorPortParam()},
	}

	for _, pp := range portParams {
		sym := env.InternSymbol(values.NewSymbol(pp.name))
		idx, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		env.SetOwnGlobalValue(idx, pp.param.(values.Value)) //nolint:errcheck
	}
}
