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

package io

import (
	"unicode/utf8"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// writeAndFlush writes bs to the textual output port and flushes if
// the port has a flusher slot. Used by write/display/newline/etc.
func writeAndFlush(port *values.PortObject, bs []byte, errCtx string) error {
	w, _ := port.AsWriter()
	_, err := w.Write(bs)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: error writing to output port", errCtx)
	}
	flsh, ok := port.AsFlusher()
	if !ok {
		return nil
	}
	err = flsh.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: error flushing output port", errCtx)
	}
	return nil
}

// makeWriteVariant returns a primitive that renders its first argument with the
// given renderer and writes the result to the optional textual output port.
// write, display, write-simple, and write-shared (R7RS §6.13.3) differ only in
// the renderer and the error-context name, so they share this factory.
func makeWriteVariant(name string, render func(values.Value) (string, error)) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		obj := mc.Arg(0)
		port, err := getOptionalTextualOutputPort(mc, 1)
		if err != nil {
			return err
		}
		// render reports ErrWriteDepthExceeded when obj nests deeper than the
		// writer's bound; surface it as a Scheme exception rather than emit
		// output that could not be read back.
		s, err := render(obj)
		if err != nil {
			return err
		}
		err = writeAndFlush(port, []byte(s), name)
		if err != nil {
			return err
		}
		mc.SetValue(values.Void)
		return nil
	}
}

// schemeStringRender adapts Value.SchemeString (which cannot fail) to the
// render signature makeWriteVariant expects. write-simple uses it: it bypasses
// the SchemeWriter's datum-label (#N=/#N#) machinery, so it never emits datum
// labels even for shared or circular structure.
//
// CYCLES: SchemeString uses path-scoped cycle detection threaded through nested
// Pair/Vector/Hashtable, so a circular structure (e.g. set-cdr! self-cycle, or a
// cycle through a hashtable value) is bounded — it renders "..." at the back
// edge instead of overflowing the host stack. Structural sharing (an acyclic
// DAG) is NOT mistaken for a cycle: a node reachable by two sibling paths is
// rendered in full at each occurrence, with no datum label.
//
// DEPTH: SchemeString is depth-bounded too: schemeStringChild degrades to the
// marker "#<deep>" past DefaultMaxWriteDepth, the same bound the SchemeWriter
// uses. Unlike write/display/write-shared, which return ErrWriteDepthExceeded
// and refuse to emit, SchemeString satisfies the non-erroring Value contract and
// so degrades rather than raising.
func schemeStringRender(v values.Value) (string, error) {
	return v.SchemeString(), nil
}

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(mc machine.CallContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "write-char")
	if err != nil {
		return err
	}
	port, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	buf := make([]byte, 0, utf8.UTFMax)
	err = writeAndFlush(port, utf8.AppendRune(buf, ch.Value), "write-char")
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimNewline implements the newline primitive.
// Writes a newline character to the output port.
func PrimNewline(mc machine.CallContext) error {
	port, err := getOptionalTextualOutputPort(mc, 0)
	if err != nil {
		return err
	}
	err = writeAndFlush(port, []byte("\n"), "newline")
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteString implements the write-string primitive.
// R7RS §6.13.3: (write-string string [port [start [end]]])
// Writes the characters of string (optionally between start and end) to port.
func PrimWriteString(mc machine.CallContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "write-string")
	if err != nil {
		return err
	}

	runes := str.Runes()
	length := len(runes)
	start := 0
	end := length

	port, tuple, found, err := extractPort(
		mc.Arg(1), "write-string", werr.ErrNotAnOutputPort)
	if err != nil {
		return err
	}
	if !found {
		port, err = resolveCurrentOutputPort(mc)
		if err != nil {
			return err
		}
	} else {
		_, ok := port.AsWriter()
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
				"write-string: expected an output port, got %s", port.PortKind())
		}
	}
	err = requireTextualOutput(port)
	if err != nil {
		return err
	}
	if !tuple.IsEmptyList() {
		start, end, err = helpers.ParseSubrange(tuple.Cdr(), length, "write-string")
		if err != nil {
			return err
		}
	}

	err = writeAndFlush(port, []byte(string(runes[start:end])), "write-string")
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimFlushOutputPort implements the flush-output-port primitive.
// R7RS §6.13.3: (flush-output-port [port])
// Flushes any buffered output to the underlying output device.
//
// Ports without a real flusher slot (string output, bytevector
// buffered output) have nothing to flush — but flush-output-port
// must still reject closed ports per the general R7RS port-closed
// guard. Explicit IsClosed check covers that path; ports with a
// flusher slot get the same guard via the wrapper in
// values/port_helpers.go.
func PrimFlushOutputPort(mc machine.CallContext) error {
	port, err := getOptionalOutputPort(mc, 0)
	if err != nil {
		return err
	}
	if port.IsClosed() {
		return werr.WrapForeignErrorf(werr.ErrPortClosed,
			"flush-output-port: port is closed")
	}

	flsh, ok := port.AsFlusher()
	if ok {
		err = flsh.Flush()
		if err != nil {
			return werr.WrapForeignErrorf(err, "flush-output-port: error flushing port")
		}
	}

	mc.SetValue(values.Void)
	return nil
}
