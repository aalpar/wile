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
//nolint:revive // package name conflicts with stdlib
package io

import (
	_ "embed"

	"github.com/aalpar/wile/registry"
)

// portProcSource contains call-with-port, defined in Scheme so that
// proc's continuation frames are capturable by call/cc.
//
//go:embed port_procs.scm
var portProcSource string

// Extension is the I/O extension.
var Extension = registry.NewExtension("io", AddToRegistry)

// Builder aggregates all I/O registration functions.
var Builder = registry.NewRegistryBuilder(
	addReadWrite,
	addPorts,
	addPortState,
	addPortProcs,
)

// AddToRegistry registers all I/O primitives.
var AddToRegistry = Builder.AddToRegistry

func addReadWrite(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "read", ParamCount: 1, IsVariadic: true, Impl: PrimRead,
			Doc: "Reads an S-expression from a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-token", ParamCount: 1, IsVariadic: true, Impl: PrimReadToken,
			Doc: "Reads a single token from a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-syntax", ParamCount: 1, IsVariadic: true, Impl: PrimReadSyntax,
			Doc: "Reads a syntax object from a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-char", ParamCount: 1, IsVariadic: true, Impl: PrimReadChar,
			Doc: "Reads a single character from a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-char", ParamCount: 1, IsVariadic: true, Impl: PrimPeekChar,
			Doc: "Peeks at the next character without consuming it.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-line", ParamCount: 1, IsVariadic: true, Impl: PrimReadLine,
			Doc: "Reads a line of text from a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-string", ParamCount: 2, IsVariadic: true, Impl: PrimReadString,
			Doc: "Reads up to k characters from a port.", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "char-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimCharReadyQ,
			Doc: "Returns #t if a character is available on the port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write", ParamCount: 2, IsVariadic: true, Impl: PrimWrite,
			Doc: "Writes a machine-readable representation to a port.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-char", ParamCount: 2, IsVariadic: true, Impl: PrimWriteChar,
			Doc: "Writes a character to a port.", ParamNames: []string{"char", "port"}, Category: "io"},
		{Name: "write-string", ParamCount: 2, IsVariadic: true, Impl: PrimWriteString,
			Doc: "Writes a string to a port, optionally from start to end.", ParamNames: []string{"string", "port"}, Category: "io"},
		{Name: "display", ParamCount: 2, IsVariadic: true, Impl: PrimDisplay,
			Doc: "Writes a human-readable representation to a port.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "newline", ParamCount: 1, IsVariadic: true, Impl: PrimNewline,
			Doc: "Writes a newline to a port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-simple", ParamCount: 2, IsVariadic: true, Impl: PrimWriteSimple,
			Doc: "Writes without shared structure labels.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-shared", ParamCount: 2, IsVariadic: true, Impl: PrimWriteShared,
			Doc: "Writes with shared structure markers.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "flush-output-port", ParamCount: 1, IsVariadic: true, Impl: PrimFlushOutputPort,
			Doc: "Flushes buffered output on a port.", ParamNames: []string{"port"}, Category: "io"},
		// Binary I/O (R7RS §6.13.3)
		{Name: "read-u8", ParamCount: 1, IsVariadic: true, Impl: PrimReadU8,
			Doc: "Reads a single byte from a binary port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-u8", ParamCount: 1, IsVariadic: true, Impl: PrimPeekU8,
			Doc: "Peeks at the next byte without consuming it.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "u8-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimU8ReadyQ,
			Doc: "Returns #t if a byte is available on the port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-u8", ParamCount: 2, IsVariadic: true, Impl: PrimWriteU8,
			Doc: "Writes a byte to a binary port.", ParamNames: []string{"byte", "port"}, Category: "io"},
		{Name: "read-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevector,
			Doc: "Reads up to k bytes from a binary port.", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "read-bytevector!", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevectorBang,
			Doc: "Reads bytes into an existing bytevector.", ParamNames: []string{"bytevector", "port"}, Category: "io"},
		{Name: "write-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimWriteBytevector,
			Doc: "Writes bytevector bytes to a binary port.", ParamNames: []string{"bytevector", "port"}, Category: "io"},
	}, registry.PhaseRuntime)
	return nil
}

func addPorts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "port?", ParamCount: 1, Impl: PrimPortQ,
			Doc: "Returns #t if obj is a port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port?", ParamCount: 1, Impl: PrimInputPortQ,
			Doc: "Returns #t if obj is an input port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "output-port?", ParamCount: 1, Impl: PrimOutputPortQ,
			Doc: "Returns #t if obj is an output port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "textual-port?", ParamCount: 1, Impl: PrimTextualPortQ,
			Doc: "Returns #t if obj is a textual port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "binary-port?", ParamCount: 1, Impl: PrimBinaryPortQ,
			Doc: "Returns #t if obj is a binary port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port-open?", ParamCount: 1, Impl: PrimInputPortOpenQ,
			Doc: "Returns #t if the input port is open.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "output-port-open?", ParamCount: 1, Impl: PrimOutputPortOpenQ,
			Doc: "Returns #t if the output port is open.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-port", ParamCount: 1, Impl: PrimClosePort,
			Doc: "Closes a port.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-input-port", ParamCount: 1, Impl: PrimCloseInputPort,
			Doc: "Closes an input port; errors if given an output port.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-output-port", ParamCount: 1, Impl: PrimCloseOutputPort,
			Doc: "Closes an output port; flushes buffered data first; errors if given an input port.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "eof-object", Impl: PrimEofObject,
			Doc: "Returns the EOF object.", Category: "ports"},
		{Name: "eof-object?", ParamCount: 1, Impl: PrimEofObjectQ,
			Doc: "Returns #t if obj is the EOF object.", ParamNames: []string{"obj"}, Category: "ports"},
		// call-with-port is defined in port_procs.scm (addPortProcs)
	}, registry.PhaseRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-string", ParamCount: 1, Impl: PrimOpenInputString,
			Doc: "Creates an input port from a string.", ParamNames: []string{"string"}, Category: "ports"},
		{Name: "open-output-string", Impl: PrimOpenOutputString,
			Doc: "Creates an output string port.", Category: "ports"},
		{Name: "get-output-string", ParamCount: 1, Impl: PrimGetOutputString,
			Doc: "Returns the accumulated string from an output string port.", ParamNames: []string{"port"}, Category: "ports"},
	}, registry.PhaseRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-bytevector", ParamCount: 1, Impl: PrimOpenInputBytevector,
			Doc: "Creates an input port from a bytevector.", ParamNames: []string{"bytevector"}, Category: "ports"},
		{Name: "open-output-bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimOpenOutputBytevector,
			Doc: "Creates an output bytevector port.", ParamNames: []string{"capacity"}, Category: "ports"},
		{Name: "get-output-bytevector", ParamCount: 1, Impl: PrimGetOutputBytevector,
			Doc: "Returns the accumulated bytevector from an output port.", ParamNames: []string{"port"}, Category: "ports"},
	}, registry.PhaseRuntime)

	return nil
}

func addPortState(r *registry.Registry) error {
	InitState()
	r.AddGlobalValue("current-input-port", GetCurrentInputPortParam())
	r.AddGlobalValue("current-output-port", GetCurrentOutputPortParam())
	r.AddGlobalValue("current-error-port", GetCurrentErrorPortParam())
	return nil
}

func addPortProcs(r *registry.Registry) error {
	r.AddMacroSource(portProcSource)
	return nil
}
