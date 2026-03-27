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
			Doc: "Reads and parses one S-expression from port. Defaults to current-input-port. Returns eof-object at end of input.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-token", ParamCount: 1, IsVariadic: true, Impl: PrimReadToken,
			Doc: "Reads a single lexical token from port. Defaults to current-input-port. Returns eof-object at end of input.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-syntax", ParamCount: 1, IsVariadic: true, Impl: PrimReadSyntax,
			Doc: "Reads one S-expression as a syntax object with source location. Defaults to current-input-port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-char", ParamCount: 1, IsVariadic: true, Impl: PrimReadChar,
			Doc: "Reads and returns the next character from port. Defaults to current-input-port. Returns eof-object at end of input.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-char", ParamCount: 1, IsVariadic: true, Impl: PrimPeekChar,
			Doc: "Returns the next character from port without consuming it. Defaults to current-input-port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-line", ParamCount: 1, IsVariadic: true, Impl: PrimReadLine,
			Doc: "Reads a line of text from port up to a newline or end of input. Defaults to current-input-port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-string", ParamCount: 2, IsVariadic: true, Impl: PrimReadString,
			Doc: "Reads up to k characters from port and returns them as a string. Returns eof-object if no characters available.", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "char-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimCharReadyQ,
			Doc: "Returns #t if a character is available for reading on port without blocking. Defaults to current-input-port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write", ParamCount: 2, IsVariadic: true, Impl: PrimWrite,
			Doc: "Writes a machine-readable (write) representation of obj to port. Strings are quoted, characters use #\\ notation.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-char", ParamCount: 2, IsVariadic: true, Impl: PrimWriteChar,
			Doc: "Writes a single character to port. Defaults to current-output-port.", ParamNames: []string{"char", "port"}, Category: "io"},
		{Name: "write-string", ParamCount: 2, IsVariadic: true, Impl: PrimWriteString,
			Doc: "Writes characters of string to port, optionally limited to the range from start to end.", ParamNames: []string{"string", "port"}, Category: "io"},
		{Name: "display", ParamCount: 2, IsVariadic: true, Impl: PrimDisplay,
			Doc: "Writes a human-readable representation of obj to port. Strings are not quoted, characters are written directly.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "newline", ParamCount: 1, IsVariadic: true, Impl: PrimNewline,
			Doc: "Writes a newline character to port. Defaults to current-output-port.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-simple", ParamCount: 2, IsVariadic: true, Impl: PrimWriteSimple,
			Doc: "Like write, but does not use datum labels for shared structure.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-shared", ParamCount: 2, IsVariadic: true, Impl: PrimWriteShared,
			Doc: "Like write, but always uses datum labels to show shared and circular structure.", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "flush-output-port", ParamCount: 1, IsVariadic: true, Impl: PrimFlushOutputPort,
			Doc: "Flushes any buffered output on port. Defaults to current-output-port.", ParamNames: []string{"port"}, Category: "io"},
		// Binary I/O (R7RS §6.13.3)
		{Name: "read-u8", ParamCount: 1, IsVariadic: true, Impl: PrimReadU8,
			Doc: "Reads and returns the next byte from a binary port as an exact integer (0-255). Returns eof-object at end of input.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-u8", ParamCount: 1, IsVariadic: true, Impl: PrimPeekU8,
			Doc: "Returns the next byte from a binary port without consuming it. Returns eof-object at end of input.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "u8-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimU8ReadyQ,
			Doc: "Returns #t if a byte is available for reading on the binary port without blocking.", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-u8", ParamCount: 2, IsVariadic: true, Impl: PrimWriteU8,
			Doc: "Writes a single byte (0-255) to a binary port. Defaults to current-output-port.", ParamNames: []string{"byte", "port"}, Category: "io"},
		{Name: "read-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevector,
			Doc: "Reads up to k bytes from a binary port and returns them as a bytevector. Returns eof-object if no bytes available.", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "read-bytevector!", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevectorBang,
			Doc: "Reads bytes into an existing bytevector from a binary port. Returns the number of bytes read or eof-object.", ParamNames: []string{"bytevector", "port"}, Category: "io"},
		{Name: "write-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimWriteBytevector,
			Doc: "Writes bytes from bytevector to a binary port, optionally limited to the range from start to end.", ParamNames: []string{"bytevector", "port"}, Category: "io"},
	}, registry.PhaseRuntime)
	return nil
}

func addPorts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "port?", ParamCount: 1, Impl: PrimPortQ,
			Doc: "Returns #t if obj is any kind of port (input, output, textual, or binary).", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port?", ParamCount: 1, Impl: PrimInputPortQ,
			Doc: "Returns #t if obj is an input port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "output-port?", ParamCount: 1, Impl: PrimOutputPortQ,
			Doc: "Returns #t if obj is an output port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "textual-port?", ParamCount: 1, Impl: PrimTextualPortQ,
			Doc: "Returns #t if obj is a textual (character-based) port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "binary-port?", ParamCount: 1, Impl: PrimBinaryPortQ,
			Doc: "Returns #t if obj is a binary (byte-based) port.", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port-open?", ParamCount: 1, Impl: PrimInputPortOpenQ,
			Doc: "Returns #t if the input port is still open for reading.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "output-port-open?", ParamCount: 1, Impl: PrimOutputPortOpenQ,
			Doc: "Returns #t if the output port is still open for writing.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-port", ParamCount: 1, Impl: PrimClosePort,
			Doc: "Closes port. Subsequent reads or writes will signal an error.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-input-port", ParamCount: 1, Impl: PrimCloseInputPort,
			Doc: "Closes an input port. Raises an error if given a non-input port.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-output-port", ParamCount: 1, Impl: PrimCloseOutputPort,
			Doc: "Closes an output port, flushing buffered data first. Raises an error if given a non-output port.", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "eof-object", Impl: PrimEofObject,
			Doc: "Returns the unique end-of-file object.", Category: "ports"},
		{Name: "eof-object?", ParamCount: 1, Impl: PrimEofObjectQ,
			Doc: "Returns #t if obj is the end-of-file object.", ParamNames: []string{"obj"}, Category: "ports"},
		// call-with-port is defined in port_procs.scm (addPortProcs)
	}, registry.PhaseRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-string", ParamCount: 1, Impl: PrimOpenInputString,
			Doc: "Returns a textual input port that reads characters from string.", ParamNames: []string{"string"}, Category: "ports"},
		{Name: "open-output-string", Impl: PrimOpenOutputString,
			Doc: "Returns a textual output port that accumulates characters into a string.", Category: "ports"},
		{Name: "get-output-string", ParamCount: 1, Impl: PrimGetOutputString,
			Doc: "Returns the accumulated string from an output string port. The port remains open.", ParamNames: []string{"port"}, Category: "ports"},
	}, registry.PhaseRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-bytevector", ParamCount: 1, Impl: PrimOpenInputBytevector,
			Doc: "Returns a binary input port that reads bytes from bytevector.", ParamNames: []string{"bytevector"}, Category: "ports"},
		{Name: "open-output-bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimOpenOutputBytevector,
			Doc: "Returns a binary output port that accumulates bytes. Optional capacity hint for pre-allocation.", ParamNames: []string{"capacity"}, Category: "ports"},
		{Name: "get-output-bytevector", ParamCount: 1, Impl: PrimGetOutputBytevector,
			Doc: "Returns the accumulated bytes from an output bytevector port as a bytevector.", ParamNames: []string{"port"}, Category: "ports"},
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
