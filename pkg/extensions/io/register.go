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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// portProcSource contains call-with-port, defined in Scheme so that
// proc's continuation frames are capturable by call/cc.
//
//go:embed port_procs.scm
var portProcSource string

// Extension is the I/O extension.
var Extension = registry.NewDescribedExtension("io",
	"I/O ports: reading, writing, string/bytevector ports, display, write.",
	AddToRegistry)

// Builder aggregates all I/O registration functions.
var Builder = registry.NewRegistryBuilder(
	addReadWrite,
	addPorts,
	addPortState,
	addPortProcs,
)

// AddToRegistry registers all I/O primitives.
var AddToRegistry = Builder.AddToRegistry

func addReadWrite(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		// Textual reads return a datum, character, string, or eof-object —
		// the union can't be expressed as a single ValueType, so ReturnType
		// is TypeAny.
		{Name: "read", ParamCount: 1, IsVariadic: true, Impl: PrimRead,
			Doc: "Reads and parses one S-expression from PORT. Defaults to current-input-port. Returns eof-object at end of input.\n\nExamples:\n  (read (open-input-string \"42\"))        => 42\n  (read (open-input-string \"(a b c)\"))   => (a b c)", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"get-datum", "parse", "input", "deserialize", "s-expression"},
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "read-token", ParamCount: 1, IsVariadic: true, Impl: PrimReadToken,
			Doc: "Reads a single lexical token from PORT. Defaults to current-input-port. Returns an opaque tokenizer-SimpleToken at each call; returns eof-object at end of input.\n\nExamples:\n  ;; (read-token (open-input-string \"hello\"))  =>  #<simple-token>", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "read-syntax", ParamCount: 1, IsVariadic: true, Impl: PrimReadSyntax,
			Doc: "Reads one S-expression as a syntax object with source location from PORT. Defaults to current-input-port.\n\nExamples:\n  (read-syntax (open-input-string \"(+ 1 2)\"))  => #<syntax (+ 1 2)>", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "read-char", ParamCount: 1, IsVariadic: true, Impl: PrimReadChar,
			Doc: "Reads and returns the next character from PORT. Defaults to current-input-port. Returns eof-object at end of input.\n\nExamples:\n  (read-char (open-input-string \"abc\"))  => #\\a", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"get-char"},
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "peek-char", ParamCount: 1, IsVariadic: true, Impl: PrimPeekChar,
			Doc: "Returns the next character from PORT without consuming it. Defaults to current-input-port.\n\nExamples:\n  (let ((p (open-input-string \"hi\"))) (peek-char p) (read-char p))  => #\\h", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"lookahead-char"},
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "read-line", ParamCount: 1, IsVariadic: true, Impl: PrimReadLine,
			Doc: "Reads a line of text from PORT up to a newline or end of input. Defaults to current-input-port.\n\nExamples:\n  (read-line (open-input-string \"hello\\nworld\"))  => \"hello\"", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"getline", "readline", "input line"},
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "read-string", ParamCount: 2, IsVariadic: true, Impl: PrimReadString,
			Doc: "Reads up to K characters from PORT and returns them as a string. Returns eof-object if no characters available.\n\nExamples:\n  (read-string 3 (open-input-string \"hello\"))  => \"hel\"", ParamNames: []string{"k", "port"}, Category: "io",
			Keywords:   []string{"get-string-n"},
			ParamTypes: []values.TypeConstraint{values.TypeInteger, values.TypeTextualInputPort}, ReturnType: values.TypeAny},
		{Name: "char-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimCharReadyQ,
			Doc: "Returns #t if a character is available for reading on PORT without blocking. Defaults to current-input-port. Wile always returns #t: non-blocking readiness detection is not implemented.\n\nExamples:\n  (char-ready? (open-input-string \"x\"))  => #t", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeTextualInputPort},
			ReturnType: values.TypeBoolean},
		{Name: "write", ParamCount: 2, IsVariadic: true, Impl: makeWriteVariant("write", values.WriteValueToString),
			Doc: "Writes a machine-readable (write) representation of OBJ to PORT. Strings are quoted, characters use #\\ notation.\n\nExamples:\n  (let ((p (open-output-string))) (write \"hi\" p) (get-output-string p))  => \"\\\"hi\\\"\"", ParamNames: []string{"obj", "port"}, Category: "io",
			Keywords:   []string{"put-datum", "serialize", "output", "machine-readable"},
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "write-char", ParamCount: 2, IsVariadic: true, Impl: PrimWriteChar,
			Doc: "Writes CHAR to PORT. Defaults to current-output-port.\n\nExamples:\n  (let ((p (open-output-string))) (write-char #\\A p) (get-output-string p))  => \"A\"", ParamNames: []string{"char", "port"}, Category: "io",
			Keywords:   []string{"put-char"},
			ParamTypes: []values.TypeConstraint{values.TypeCharacter, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "write-string", ParamCount: 2, IsVariadic: true, Impl: PrimWriteString,
			Doc: "Writes characters of STRING to PORT, optionally limited to the range from start to end.\n\nExamples:\n  (let ((p (open-output-string))) (write-string \"hello\" p) (get-output-string p))  => \"hello\"", ParamNames: []string{"string", "port"}, Category: "io",
			Keywords:   []string{"put-string"},
			ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "display", ParamCount: 2, IsVariadic: true, Impl: makeWriteVariant("display", values.DisplayValueToString),
			Doc: "Writes a human-readable representation of OBJ to PORT. Strings are not quoted, characters are written directly.\n\nExamples:\n  (let ((p (open-output-string))) (display \"hi\" p) (get-output-string p))  => \"hi\"\n  (display 42)  ; prints: 42", ParamNames: []string{"obj", "port"}, Category: "io",
			Keywords:   []string{"print", "output", "show", "human-readable"},
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "newline", ParamCount: 1, IsVariadic: true, Impl: PrimNewline,
			Doc: "Writes a newline character to PORT. Defaults to current-output-port.\n\nExamples:\n  (newline)  ; prints a blank line", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "write-simple", ParamCount: 2, IsVariadic: true, Impl: makeWriteVariant("write-simple", schemeStringRender),
			Doc: "Like write, but emits no datum labels (#N=/#N#): shared structure is printed in full at each occurrence. Cycle detection is path-scoped and bounded: a circular structure renders \"...\" at the back edge instead of looping forever, while acyclic sharing (a DAG) is not mistaken for a cycle.\n\nExamples:\n  (let ((p (open-output-string))) (write-simple '(1 2) p) (get-output-string p))  => \"(1 2)\"", ParamNames: []string{"obj", "port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "write-shared", ParamCount: 2, IsVariadic: true, Impl: makeWriteVariant("write-shared", values.WriteSharedValueToString),
			Doc: "Like write, but always uses datum labels to show shared and circular structure.\n\nExamples:\n  (let ((p (open-output-string))) (write-shared '(1 2) p) (get-output-string p))  => \"(1 2)\"", ParamNames: []string{"obj", "port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeTextualOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "flush-output-port", ParamCount: 1, IsVariadic: true, Impl: PrimFlushOutputPort,
			Doc: "Flushes any buffered output on PORT. Defaults to current-output-port.\n\nExamples:\n  (flush-output-port)  ; flushes current-output-port", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeOutputPort},
			ReturnType: values.TypeVoid},
		// Binary I/O (R7RS §6.13.3)
		{Name: "read-u8", ParamCount: 1, IsVariadic: true, Impl: PrimReadU8,
			Doc: "Reads and returns the next byte from a binary port as an exact integer (0-255). Returns eof-object at end of input.\n\nExamples:\n  (read-u8 (open-input-bytevector #u8(65 66)))  => 65", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"get-u8"},
			ParamTypes: []values.TypeConstraint{values.TypeBinaryInputPort}, ReturnType: values.TypeAny},
		{Name: "peek-u8", ParamCount: 1, IsVariadic: true, Impl: PrimPeekU8,
			Doc: "Returns the next byte from a binary port without consuming it. Returns eof-object at end of input.\n\nExamples:\n  (let ((p (open-input-bytevector #u8(42)))) (peek-u8 p) (read-u8 p))  => 42", ParamNames: []string{"port"}, Category: "io",
			Keywords:   []string{"lookahead-u8"},
			ParamTypes: []values.TypeConstraint{values.TypeBinaryInputPort}, ReturnType: values.TypeAny},
		{Name: "u8-ready?", ParamCount: 1, IsVariadic: true, Impl: PrimU8ReadyQ,
			Doc: "Returns #t if a byte is available for reading on the binary port without blocking. Wile always returns #t: non-blocking readiness detection is not implemented.\n\nExamples:\n  (u8-ready? (open-input-bytevector #u8(1)))  => #t", ParamNames: []string{"port"}, Category: "io",
			ParamTypes: []values.TypeConstraint{values.TypeBinaryInputPort},
			ReturnType: values.TypeBoolean},
		{Name: "write-u8", ParamCount: 2, IsVariadic: true, Impl: PrimWriteU8,
			Doc: "Writes BYTE (0-255) to a binary port. The port argument is required: binary I/O has no current-port default.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))  => #u8(65)", ParamNames: []string{"byte", "port"}, Category: "io",
			Keywords:   []string{"put-u8"},
			ParamTypes: []values.TypeConstraint{values.TypeInteger, values.TypeBinaryOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "read-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevector,
			Doc: "Reads up to K bytes from a binary port and returns them as a bytevector. Returns eof-object if no bytes available.\n\nExamples:\n  (read-bytevector 2 (open-input-bytevector #u8(10 20 30)))  => #u8(10 20)", ParamNames: []string{"k", "port"}, Category: "io",
			Keywords:   []string{"get-bytevector-n"},
			ParamTypes: []values.TypeConstraint{values.TypeInteger, values.TypeBinaryInputPort}, ReturnType: values.TypeAny},
		{Name: "read-bytevector!", ParamCount: 2, IsVariadic: true, Impl: PrimReadBytevectorBang,
			Doc: "Reads bytes into BYTEVECTOR from a binary port. Returns the number of bytes read or eof-object.\n\nExamples:\n  (let ((bv (make-bytevector 3 0))) (read-bytevector! bv (open-input-bytevector #u8(1 2))) bv)  => #u8(1 2 0)", ParamNames: []string{"bytevector", "port"}, Category: "io",
			Keywords:   []string{"get-bytevector-n!"},
			ParamTypes: []values.TypeConstraint{values.TypeByteVector, values.TypeBinaryInputPort}, ReturnType: values.TypeAny},
		{Name: "write-bytevector", ParamCount: 2, IsVariadic: true, Impl: PrimWriteBytevector,
			Doc: "Writes bytes from BYTEVECTOR to a binary port, optionally limited to the range from start to end.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p) (get-output-bytevector p))  => #u8(1 2 3)", ParamNames: []string{"bytevector", "port"}, Category: "io",
			Keywords:   []string{"put-bytevector"},
			ParamTypes: []values.TypeConstraint{values.TypeByteVector, values.TypeBinaryOutputPort},
			ReturnType: values.TypeVoid},
	}, registry.PhaseSetRuntime)
	return nil
}

func addPorts(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "port?", ParamCount: 1, Impl: PrimPortQ,
			Doc: "Returns #t if OBJ is any kind of port (input, output, textual, or binary).\n\nExamples:\n  (port? (current-input-port))   => #t\n  (port? 42)                     => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "input-port?", ParamCount: 1, Impl: PrimInputPortQ,
			Doc: "Returns #t if OBJ is an input port.\n\nExamples:\n  (input-port? (open-input-string \"x\"))  => #t\n  (input-port? (open-output-string))     => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "output-port?", ParamCount: 1, Impl: PrimOutputPortQ,
			Doc: "Returns #t if OBJ is an output port.\n\nExamples:\n  (output-port? (open-output-string))    => #t\n  (output-port? (open-input-string \"\"))  => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "textual-port?", ParamCount: 1, Impl: PrimTextualPortQ,
			Doc: "Returns #t if OBJ is a textual (character-based) port.\n\nExamples:\n  (textual-port? (open-input-string \"x\"))       => #t\n  (textual-port? (open-input-bytevector #u8()))  => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "binary-port?", ParamCount: 1, Impl: PrimBinaryPortQ,
			Doc: "Returns #t if OBJ is a binary (byte-based) port.\n\nExamples:\n  (binary-port? (open-input-bytevector #u8()))  => #t\n  (binary-port? (open-input-string \"\"))          => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "input-port-open?", ParamCount: 1, Impl: PrimInputPortOpenQ,
			Doc: "Returns #t if PORT is still open for reading.\n\nExamples:\n  (let ((p (open-input-string \"x\"))) (input-port-open? p))  => #t", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeInputPort},
			ReturnType: values.TypeBoolean},
		{Name: "output-port-open?", ParamCount: 1, Impl: PrimOutputPortOpenQ,
			Doc: "Returns #t if PORT is still open for writing.\n\nExamples:\n  (let ((p (open-output-string))) (output-port-open? p))  => #t", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeOutputPort},
			ReturnType: values.TypeBoolean},
		{Name: "close-port", ParamCount: 1, Impl: PrimClosePort,
			Doc: "Closes PORT. Subsequent reads or writes will signal an error.\n\nExamples:\n  (let ((p (open-output-string))) (close-port p) (output-port-open? p))  => #f", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypePort},
			ReturnType: values.TypeVoid},
		{Name: "close-input-port", ParamCount: 1, Impl: PrimCloseInputPort,
			Doc: "Closes PORT for input. Raises an error if given a non-input port.\n\nExamples:\n  (close-input-port (open-input-string \"x\"))", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeInputPort},
			ReturnType: values.TypeVoid},
		{Name: "close-output-port", ParamCount: 1, Impl: PrimCloseOutputPort,
			Doc: "Closes PORT for output, flushing buffered data first. Raises an error if given a non-output port.\n\nExamples:\n  (close-output-port (open-output-string))", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeOutputPort},
			ReturnType: values.TypeVoid},
		{Name: "eof-object", Impl: PrimEofObject,
			Doc: "Returns the unique end-of-file object.\n\nExamples:\n  (eof-object)  => #<eof>", Category: "ports",
			ReturnType: values.TypeAny},
		{Name: "eof-object?", ParamCount: 1, Impl: PrimEofObjectQ,
			Doc: "Returns #t if OBJ is the end-of-file object.\n\nExamples:\n  (eof-object? (eof-object))  => #t\n  (eof-object? 42)            => #f", ParamNames: []string{"obj"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		// call-with-port is defined in port_procs.scm (addPortProcs)
	}, registry.PhaseSetRuntime)

	// String ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-string", ParamCount: 1, Impl: PrimOpenInputString,
			Doc: "Returns a textual input port that reads characters from STRING.\n\nExamples:\n  (read (open-input-string \"42\"))  => 42", ParamNames: []string{"string"}, Category: "ports",
			Keywords:   []string{"open-string-input-port", "string port", "memory input", "from string"},
			ParamTypes: []values.TypeConstraint{values.TypeString},
			ReturnType: values.TypeTextualInputPort},
		{Name: "open-output-string", Impl: PrimOpenOutputString,
			Doc: "Returns a textual output port that accumulates characters into a string.\n\nExamples:\n  (let ((p (open-output-string))) (display \"hi\" p) (get-output-string p))  => \"hi\"", Category: "ports",
			Keywords:   []string{"open-string-output-port", "string port", "memory output", "string builder", "to string"},
			ReturnType: values.TypeTextualOutputPort},
		{Name: "get-output-string", ParamCount: 1, Impl: PrimGetOutputString,
			Doc: "Returns the accumulated string from an output string port. The port remains open.\n\nExamples:\n  (let ((p (open-output-string))) (display 42 p) (get-output-string p))  => \"42\"", ParamNames: []string{"port"}, Category: "ports",
			Keywords:   []string{"extract string", "string builder result"},
			ParamTypes: []values.TypeConstraint{values.TypeTextualOutputPort},
			ReturnType: values.TypeString},
	}, registry.PhaseSetRuntime)

	// Bytevector ports
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "open-input-bytevector", ParamCount: 1, Impl: PrimOpenInputBytevector,
			Doc: "Returns a binary input port that reads bytes from BYTEVECTOR.\n\nExamples:\n  (read-u8 (open-input-bytevector #u8(65 66)))  => 65", ParamNames: []string{"bytevector"}, Category: "ports",
			Keywords:   []string{"open-bytevector-input-port"},
			ParamTypes: []values.TypeConstraint{values.TypeByteVector},
			ReturnType: values.TypeBinaryInputPort},
		{Name: "open-output-bytevector", Impl: PrimOpenOutputBytevector,
			Doc: "Returns a fresh binary output port that accumulates bytes for retrieval by get-output-bytevector.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 1 p) (get-output-bytevector p))  => #u8(1)", Category: "ports",
			Keywords:   []string{"open-bytevector-output-port"},
			ReturnType: values.TypeBinaryOutputPort},
		{Name: "get-output-bytevector", ParamCount: 1, Impl: PrimGetOutputBytevector,
			Doc: "Returns the accumulated bytes from an output bytevector port as a bytevector.\n\nExamples:\n  (let ((p (open-output-bytevector))) (write-u8 10 p) (write-u8 20 p) (get-output-bytevector p))  => #u8(10 20)", ParamNames: []string{"port"}, Category: "ports",
			ParamTypes: []values.TypeConstraint{values.TypeBinaryOutputPort},
			ReturnType: values.TypeByteVector},
	}, registry.PhaseSetRuntime)

	return nil
}

func addPortState(r *registry.PrimitiveRegistry) error {
	r.AddDocumentation("current-input-port",
		"Parameter holding the default input port.\nParameterize to redirect standard input within a dynamic extent.\n\nCategory: ports")
	r.AddDocumentation("current-output-port",
		"Parameter holding the default output port.\nParameterize to redirect standard output within a dynamic extent.\n\nCategory: ports")
	r.AddDocumentation("current-error-port",
		"Parameter holding the default error port.\nParameterize to redirect standard error within a dynamic extent.\n\nCategory: ports")
	// Per-engine: the first Apply for a Namespace mints a State and binds
	// current-{input,output,error}-port to its own port parameters. Replaces the
	// former package-global parameters shared across engines (staff-sweep #8).
	//
	// Idempotent by design: the engine re-runs applyBaseEnvironment (hence this
	// hook) for every library environment it builds, and those library envs share
	// the engine's Namespace (NewChildRuntime). Minting a fresh State on each such
	// re-run would reset the engine's current ports (e.g. discard a mid-eval
	// output redirect the moment any library is imported). So reuse the existing
	// State when the shared Namespace already has one — the State is engine-scoped
	// (root-delegated, like ImmutableLiterals), not per-Apply. Distinct engines
	// have distinct Namespaces, so each still gets its own State.
	r.AddNamespaceInit(func(env *environment.EnvironmentFrame) error {
		ns := env.Namespace()
		st, ok := ns.IOState().(*State)
		if !ok || st == nil {
			// The namespace's authorizer is installed before applyBaseEnvironment
			// runs this hook (pkg/wile/engine.go), so the stream gate in NewState
			// sees the engine's real policy on the first Apply — the only Apply
			// that mints a State.
			st = NewState(ns.Authorizer())
			ns.SetIOState(st)
		}
		binds := []struct {
			name  string
			param *machine.Parameter
		}{
			{"current-input-port", st.inPort},
			{"current-output-port", st.outPort},
			{"current-error-port", st.errPort},
		}
		for _, b := range binds {
			err := registerPortParam(env, b.name, b.param)
			if err != nil {
				return err
			}
		}
		return nil
	})
	return nil
}

// registerPortParam binds a port-parameter name into the engine's runtime frame.
// It replicates registry.registerGlobalValue's DefineOwnGlobal call (that helper
// is unexported). BindingTypeVariable (no Stable stamp) keeps the
// parameter set!-able, matching the former AddGlobalValue path.
func registerPortParam(env *environment.EnvironmentFrame, name string, param *machine.Parameter) error {
	sym := values.NewSymbol(name)
	_, err := env.DefineOwnGlobal(sym, environment.BindingTypeVariable, nil, param)
	if err != nil {
		return werr.WrapForeignErrorf(err, "io: binding port parameter %s", name)
	}
	return nil
}

func addPortProcs(r *registry.PrimitiveRegistry) error {
	r.AddMacroSource(portProcSource)
	return nil
}
