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
			Doc: "Reads and parses one S-expression from port. Defaults to current-input-port. Returns eof-object at end of input.\n\nExamples:\n  (read (open-input-string \"42\"))        => 42\n  (read (open-input-string \"(a b c)\"))   => (a b c)", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-token", ParamCount: 1, IsVariadic: true, Impl: PrimReadToken,
			Doc: "Reads a single lexical token from port. Defaults to current-input-port. Returns eof-object at end of input.\n\nExamples:\n  (read-token (open-input-string \"hello\"))  => hello", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-syntax", ParamCount: 1, IsVariadic: true, Impl: PrimReadSyntax,
			Doc: "Reads one S-expression as a syntax object with source location. Defaults to current-input-port.\n\nExamples:\n  (read-syntax (open-input-string \"(+ 1 2)\"))  => #<syntax (+ 1 2)>", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-char", ParamCount: 1, IsVariadic: true, Impl: PrimReadChar,
			Doc: "Reads and returns the next character from port. Defaults to current-input-port. Returns eof-object at end of input.\n\nExamples:\n  (read-char (open-input-string \"abc\"))  => #\\a", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-char", ParamCount: 1, IsVariadic: true, Impl: PrimPeekChar,
			Doc: "Returns the next character from port without consuming it. Defaults to current-input-port.\n\nExamples:\n  (let ((p (open-input-string \"hi\"))) (peek-char p) (read-char p))  => #\\h", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-line", ParamCount: 1, IsVariadic: true, Impl: PrimReadLine,
			Doc: "Reads a line of text from port up to a newline or end of input. Defaults to current-input-port.\n\nExamples:\n  (read-line (open-input-string \"hello\\nworld\"))  => \"hello\"", ParamNames: []string{"port"}, Category: "io"},
		{Name: "read-string", ParamCount: 2, IsVariadic: true, Impl: PrimReadString,
			Doc: "Reads up to k characters from port and returns them as a string. Returns eof-object if no characters available.\n\nExamples:\n  (read-string 3 (open-input-string \"hello\"))  => \"hel\"", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "char-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimCharReadyQ,
			Doc: "Returns #t if a character is available for reading on port without blocking. Defaults to current-input-port.\n\nExamples:\n  (char-ready? (open-input-string \"x\"))  => #t", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write", ParamCount: 2, IsVariadic: true, Impl: PrimWrite,
			Doc: "Writes a machine-readable (write) representation of obj to port. Strings are quoted, characters use #\\ notation.\n\nExamples:\n  (let ((p (open-output-string))) (write \"hi\" p) (get-output-string p))  => \"\\\"hi\\\"\"", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-char", ParamCount: 2, IsVariadic: true, Impl: PrimWriteChar,
			Doc: "Writes a single character to port. Defaults to current-output-port.\n\nExamples:\n  (let ((p (open-output-string))) (write-char #\\A p) (get-output-string p))  => \"A\"", ParamNames: []string{"char", "port"}, Category: "io"},
		{Name: "write-string", ParamCount: 2, IsVariadic: true, Impl: PrimWriteString,
			Doc: "Writes characters of string to port, optionally limited to the range from start to end.\n\nExamples:\n  (let ((p (open-output-string))) (write-string \"hello\" p) (get-output-string p))  => \"hello\"", ParamNames: []string{"string", "port"}, Category: "io"},
		{Name: "display", ParamCount: 2, IsVariadic: true, Impl: PrimDisplay,
			Doc: "Writes a human-readable representation of obj to port. Strings are not quoted, characters are written directly.\n\nExamples:\n  (let ((p (open-output-string))) (display \"hi\" p) (get-output-string p))  => \"hi\"\n  (display 42)  ; prints: 42", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "newline", ParamCount: 1, IsVariadic: true, Impl: PrimNewline,
			Doc: "Writes a newline character to port. Defaults to current-output-port.\n\nExamples:\n  (newline)  ; prints a blank line", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-simple", ParamCount: 2, IsVariadic: true, Impl: PrimWriteSimple,
			Doc: "Like write, but does not use datum labels for shared structure.\n\nExamples:\n  (let ((p (open-output-string))) (write-simple '(1 2) p) (get-output-string p))  => \"(1 2)\"", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "write-shared", ParamCount: 2, IsVariadic: true, Impl: PrimWriteShared,
			Doc: "Like write, but always uses datum labels to show shared and circular structure.\n\nExamples:\n  (let ((p (open-output-string))) (write-shared '(1 2) p) (get-output-string p))  => \"(1 2)\"", ParamNames: []string{"obj", "port"}, Category: "io"},
		{Name: "flush-output-port", ParamCount: 1, IsVariadic: true, Impl: PrimFlushOutputPort,
			Doc: "Flushes any buffered output on port. Defaults to current-output-port.\n\nExamples:\n  (flush-output-port)  ; flushes current-output-port", ParamNames: []string{"port"}, Category: "io"},
		// Binary I/O (R7RS §6.13.3)
		{Name: "read-u8", ParamCount: 1, IsVariadic: true, Impl: PrimReadU8,
			Doc: "Reads and returns the next byte from a binary port as an exact integer (0-255). Returns eof-object at end of input.\n\nExamples:\n  (read-u8 (open-input-bytevector #u8(65 66)))  => 65", ParamNames: []string{"port"}, Category: "io"},
		{Name: "peek-u8", ParamCount: 1, IsVariadic: true, Impl: PrimPeekU8,
			Doc: "Returns the next byte from a binary port without consuming it. Returns eof-object at end of input.\n\nExamples:\n  (let ((p (open-input-bytevector #u8(42)))) (peek-u8 p) (read-u8 p))  => 42", ParamNames: []string{"port"}, Category: "io"},
		{Name: "u8-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimU8ReadyQ,
			Doc: "Returns #t if a byte is available for reading on the binary port without blocking.\n\nExamples:\n  (u8-ready? (open-input-bytevector #u8(1)))  => #t", ParamNames: []string{"port"}, Category: "io"},
		{Name: "write-u8", ParamCount: 2, IsVariadic: true, Impl: PrimWriteU8,
			Doc: "Writes a single byte (0-255) to a binary port. Defaults to current-output-port.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))  => #u8(65)", ParamNames: []string{"byte", "port"}, Category: "io"},
		{Name: "read-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevector,
			Doc: "Reads up to k bytes from a binary port and returns them as a bytevector. Returns eof-object if no bytes available.\n\nExamples:\n  (read-bytevector 2 (open-input-bytevector #u8(10 20 30)))  => #u8(10 20)", ParamNames: []string{"k", "port"}, Category: "io"},
		{Name: "read-bytevector!", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevectorBang,
			Doc: "Reads bytes into an existing bytevector from a binary port. Returns the number of bytes read or eof-object.\n\nExamples:\n  (let ((bv (make-bytevector 3 0))) (read-bytevector! bv (open-input-bytevector #u8(1 2))) bv)  => #u8(1 2 0)", ParamNames: []string{"bytevector", "port"}, Category: "io"},
		{Name: "write-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimWriteBytevector,
			Doc: "Writes bytes from bytevector to a binary port, optionally limited to the range from start to end.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p) (get-output-bytevector p))  => #u8(1 2 3)", ParamNames: []string{"bytevector", "port"}, Category: "io"},
	}, registry.PhaseRuntime)
	return nil
}

func addPorts(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "port?", ParamCount: 1, Impl: PrimPortQ,
			Doc: "Returns #t if obj is any kind of port (input, output, textual, or binary).\n\nExamples:\n  (port? (current-input-port))   => #t\n  (port? 42)                     => #f", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port?", ParamCount: 1, Impl: PrimInputPortQ,
			Doc: "Returns #t if obj is an input port.\n\nExamples:\n  (input-port? (open-input-string \"x\"))  => #t\n  (input-port? (open-output-string))     => #f", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "output-port?", ParamCount: 1, Impl: PrimOutputPortQ,
			Doc: "Returns #t if obj is an output port.\n\nExamples:\n  (output-port? (open-output-string))    => #t\n  (output-port? (open-input-string \"\"))  => #f", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "textual-port?", ParamCount: 1, Impl: PrimTextualPortQ,
			Doc: "Returns #t if obj is a textual (character-based) port.\n\nExamples:\n  (textual-port? (open-input-string \"x\"))       => #t\n  (textual-port? (open-input-bytevector #u8()))  => #f", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "binary-port?", ParamCount: 1, Impl: PrimBinaryPortQ,
			Doc: "Returns #t if obj is a binary (byte-based) port.\n\nExamples:\n  (binary-port? (open-input-bytevector #u8()))  => #t\n  (binary-port? (open-input-string \"\"))          => #f", ParamNames: []string{"obj"}, Category: "ports"},
		{Name: "input-port-open?", ParamCount: 1, Impl: PrimInputPortOpenQ,
			Doc: "Returns #t if the input port is still open for reading.\n\nExamples:\n  (let ((p (open-input-string \"x\"))) (input-port-open? p))  => #t", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "output-port-open?", ParamCount: 1, Impl: PrimOutputPortOpenQ,
			Doc: "Returns #t if the output port is still open for writing.\n\nExamples:\n  (let ((p (open-output-string))) (output-port-open? p))  => #t", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-port", ParamCount: 1, Impl: PrimClosePort,
			Doc: "Closes port. Subsequent reads or writes will signal an error.\n\nExamples:\n  (let ((p (open-output-string))) (close-port p) (output-port-open? p))  => #f", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-input-port", ParamCount: 1, Impl: PrimCloseInputPort,
			Doc: "Closes an input port. Raises an error if given a non-input port.\n\nExamples:\n  (close-input-port (open-input-string \"x\"))", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "close-output-port", ParamCount: 1, Impl: PrimCloseOutputPort,
			Doc: "Closes an output port, flushing buffered data first. Raises an error if given a non-output port.\n\nExamples:\n  (close-output-port (open-output-string))", ParamNames: []string{"port"}, Category: "ports"},
		{Name: "eof-object", Impl: PrimEofObject,
			Doc: "Returns the unique end-of-file object.\n\nExamples:\n  (eof-object)  => #<eof>", Category: "ports"},
		{Name: "eof-object?", ParamCount: 1, Impl: PrimEofObjectQ,
			Doc: "Returns #t if obj is the end-of-file object.\n\nExamples:\n  (eof-object? (eof-object))  => #t\n  (eof-object? 42)            => #f", ParamNames: []string{"obj"}, Category: "ports"},
		// call-with-port is defined in port_procs.scm (addPortProcs)
	}, registry.PhaseRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-string", ParamCount: 1, Impl: PrimOpenInputString,
			Doc: "Returns a textual input port that reads characters from string.\n\nExamples:\n  (read (open-input-string \"42\"))  => 42", ParamNames: []string{"string"}, Category: "ports"},
		{Name: "open-output-string", Impl: PrimOpenOutputString,
			Doc: "Returns a textual output port that accumulates characters into a string.\n\nExamples:\n  (let ((p (open-output-string))) (display \"hi\" p) (get-output-string p))  => \"hi\"", Category: "ports"},
		{Name: "get-output-string", ParamCount: 1, Impl: PrimGetOutputString,
			Doc: "Returns the accumulated string from an output string port. The port remains open.\n\nExamples:\n  (let ((p (open-output-string))) (display 42 p) (get-output-string p))  => \"42\"", ParamNames: []string{"port"}, Category: "ports"},
	}, registry.PhaseRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-bytevector", ParamCount: 1, Impl: PrimOpenInputBytevector,
			Doc: "Returns a binary input port that reads bytes from bytevector.\n\nExamples:\n  (read-u8 (open-input-bytevector #u8(65 66)))  => 65", ParamNames: []string{"bytevector"}, Category: "ports"},
		{Name: "open-output-bytevector", ParamCount: 1, IsVariadic: true, Impl: PrimOpenOutputBytevector,
			Doc: "Returns a binary output port that accumulates bytes. Optional capacity hint for pre-allocation.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 1 p) (get-output-bytevector p))  => #u8(1)", ParamNames: []string{"capacity"}, Category: "ports"},
		{Name: "get-output-bytevector", ParamCount: 1, Impl: PrimGetOutputBytevector,
			Doc: "Returns the accumulated bytes from an output bytevector port as a bytevector.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 10 p) (write-u8 20 p) (get-output-bytevector p))  => #u8(10 20)", ParamNames: []string{"port"}, Category: "ports"},
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
