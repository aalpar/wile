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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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

// PrimWrite implements the write primitive.
// Writes a machine-readable representation of an object to the current output port or to the specified port.
// R7RS §6.13.3: write uses datum labels to handle circular and shared structures.
func PrimWrite(mc machine.CallContext) error {
	obj := mc.Arg(0)
	port, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	err = writeAndFlush(port, []byte(values.WriteValueToString(obj)), "write")
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
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

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
// R7RS §6.13.3: display uses datum labels to handle circular and shared structures.
func PrimDisplay(mc machine.CallContext) error {
	obj := mc.Arg(0)
	port, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	err = writeAndFlush(port, []byte(values.DisplayValueToString(obj)), "display")
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

// PrimWriteSimple implements the write-simple primitive (R7RS).
// Writes a machine-readable representation of an object without using datum labels
// for shared or circular structure. This is the same as write for non-circular data.
// (write-simple obj) or (write-simple obj port)
func PrimWriteSimple(mc machine.CallContext) error {
	obj := mc.Arg(0)
	port, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	err = writeAndFlush(port, []byte(obj.SchemeString()), "write-simple")
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteShared implements the write-shared primitive (R7RS).
// Writes a machine-readable representation of an object using datum labels
// (#n= and #n#) for shared and circular structure.
// R7RS §6.13.3: write-shared always uses datum labels for shared structure.
//
// (write-shared obj) or (write-shared obj port)
func PrimWriteShared(mc machine.CallContext) error {
	obj := mc.Arg(0)
	port, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	err = writeAndFlush(port, []byte(values.WriteSharedValueToString(obj)), "write-shared")
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
		port = resolveCurrentOutputPort(mc)
	} else {
		_, ok := port.AsWriter()
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
				"write-string: expected an output port, got %s", port.PortKind())
		}
	}
	_, isBinary := port.AsByteWriter()
	if isBinary {
		return werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"write-string: expected a textual output port, got binary port")
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
