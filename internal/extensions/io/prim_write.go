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

// PrimWrite implements the write primitive.
// Writes a machine-readable representation of an object to the current output port or to the specified port.
// R7RS §6.13.3: write uses datum labels to handle circular and shared structures.
func PrimWrite(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer to handle circular structures
	_, err = writer.Write([]byte(values.WriteValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "write-char")
	if err != nil {
		return err
	}
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	buf := make([]byte, 0, utf8.UTFMax)
	_, err = writer.Write(utf8.AppendRune(buf, ch.Value))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing character to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
// R7RS §6.13.3: display uses datum labels to handle circular and shared structures.
func PrimDisplay(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer to handle circular structures
	_, err = writer.Write([]byte(values.DisplayValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimNewline implements the newline primitive.
// Writes a newline character to the output port.
func PrimNewline(mc *machine.MachineContext) error {
	writer, err := getOptionalTextualOutputPort(mc, 0)
	if err != nil {
		return err
	}
	_, err = writer.Write([]byte("\n"))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing newline to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteSimple implements the write-simple primitive (R7RS).
// Writes a machine-readable representation of an object without using datum labels
// for shared or circular structure. This is the same as write for non-circular data.
// (write-simple obj) or (write-simple obj port)
func PrimWriteSimple(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	_, err = writer.Write([]byte(obj.SchemeString()))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-simple: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-simple: error flushing output port")
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
func PrimWriteShared(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer with datum labels for all shared structure
	_, err = writer.Write([]byte(values.WriteSharedValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-shared: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-shared: error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteString implements the write-string primitive.
// R7RS §6.13.3: (write-string string [port [start [end]]])
// Writes the characters of string (optionally between start and end) to port.
func PrimWriteString(mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "write-string")
	if err != nil {
		return err
	}

	runes := str.Runes()
	length := len(runes)
	start := 0
	end := length

	writer, tuple, err := extractPort[values.OutputPort](
		mc.Arg(1), "write-string", werr.ErrNotAnOutputPort, "an output port",
		func() values.OutputPort {
			return resolveCurrentOutputPort(mc)
		},
	)
	if err != nil {
		return err
	}
	if !tuple.IsEmptyList() {
		start, end, err = helpers.ParseSubrange(tuple.Cdr(), length, "write-string")
		if err != nil {
			return err
		}
	}

	_, err = writer.Write([]byte(string(runes[start:end])))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-string: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-string: error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimFlushOutputPort implements the flush-output-port primitive.
// R7RS §6.13.3: (flush-output-port [port])
// Flushes any buffered output to the underlying output device.
func PrimFlushOutputPort(mc *machine.MachineContext) error {
	port, err := getOptionalOutputPort(mc, 0)
	if err != nil {
		return err
	}

	err = port.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "flush-output-port: error flushing port")
	}

	mc.SetValue(values.Void)
	return nil
}
