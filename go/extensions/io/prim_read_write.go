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
	"context"
	"io"
	"unicode/utf8"
	"weak"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/syntax"
	"wile/tokenizer"
	"wile/values"
)

// runeReaderUnreader is the interface needed by the parser
type runeReaderUnreader interface {
	ReadRune() (rune, int, error)
	UnreadRune() error
}

// PrimRead implements the (read) primitive.
// Reads a Scheme datum from port.
func PrimRead(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			portKey = p
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			portKey = p
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "expected an input port but got %T", port)
		}
	}

	prss, ok := Parsers[portKey]
	if !ok || prss.Value() == nil {
		prss = weak.Make(parser.NewParser(mc.EnvironmentFrame(), true, runeReader))
		Parsers[portKey] = prss
	}
	syn, err := prss.Value().ReadSyntax(context.TODO())
	if err != nil {
		return values.WrapForeignErrorf(err, "error reading from input port")
	}
	// Use UnwrapAllShared to preserve object identity for datum labels (R7RS §2.4)
	// and handle circular structures
	cache := make(map[syntax.SyntaxValue]values.Value)
	q := syntax.UnwrapAllShared(syn, cache)
	mc.SetValue(q)
	return nil
}

// PrimReadToken implements the (read-token) primitive.
// Reads a single token from port.
func PrimReadToken(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			portKey = p
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			portKey = p
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "expected an input port but got %T", port)
		}
	}

	tknz, ok := Tokenizers[portKey]
	if !ok || tknz.Value() == nil {
		tknz = weak.Make(tokenizer.NewTokenizer(runeReader, false))
		Tokenizers[portKey] = tknz
	}
	q, err := tknz.Value().Next()
	if err == io.EOF {
		return values.WrapForeignErrorf(values.ErrEndOfFile, "end of file")
	}
	if err != nil {
		return values.WrapForeignErrorf(err, "error reading token")
	}
	mc.SetValue(q.(values.Value))
	return nil
}

// PrimReadSyntax implements the (read-syntax) primitive.
// Reads datum with source information.
func PrimReadSyntax(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			portKey = p
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			portKey = p
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "expected an input port but got %T", port)
		}
	}

	prss, ok := Parsers[portKey]
	if !ok || prss.Value() == nil {
		prss = weak.Make(parser.NewParser(mc.EnvironmentFrame(), true, runeReader))
		Parsers[portKey] = prss
	}
	q, err := prss.Value().ReadSyntax(context.TODO())
	if err != nil {
		return values.WrapForeignErrorf(err, "error reading syntax from input port")
	}
	mc.SetValue(q)
	return nil
}

// PrimWrite implements the write primitive.
// Writes a machine-readable representation of an object to the current output port or to the specified port.
// R7RS §6.13.3: write uses datum labels to handle circular and shared structures.
func PrimWrite(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", port)
		}
	}
	// Use cycle-aware writer to handle circular structures
	_, err := writer.Write([]byte(values.WriteValueToString(obj)))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing to output port")
	}
	mc.SetValues()
	return nil
}

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	ch, ok := obj.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "expected a character but got %T", obj)
	}
	o := mc.EnvironmentFrame().GetLocalBinding(environment.NewLocalIndex(1, 0)).Value()
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", port)
		}
	}
	buf := make([]byte, 0, utf8.UTFMax)
	_, err := writer.Write(utf8.AppendRune(buf, ch.Value))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing character to output port")
	}
	mc.SetValues()
	return nil
}

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
// R7RS §6.13.3: display uses datum labels to handle circular and shared structures.
func PrimDisplay(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", port)
		}
	}
	// Use cycle-aware writer to handle circular structures
	_, err := writer.Write([]byte(values.DisplayValueToString(obj)))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing to output port")
	}
	mc.SetValues()
	return nil
}

// PrimNewline implements the newline primitive.
// Writes a newline character to the output port.
func PrimNewline(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", port)
		}
	}
	_, err := writer.Write([]byte("\n"))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing newline to output port")
	}
	mc.SetValues()
	return nil
}

// PrimWriteSimple implements the write-simple primitive (R7RS).
// Writes a machine-readable representation of an object without using datum labels
// for shared or circular structure. This is the same as write for non-circular data.
// (write-simple obj) or (write-simple obj port)
func PrimWriteSimple(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-simple: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-simple: expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "write-simple: expected an output port but got %T", port)
		}
	}
	_, err := writer.Write([]byte(obj.SchemeString()))
	if err != nil {
		return values.WrapForeignErrorf(err, "write-simple: error writing to output port")
	}
	mc.SetValues()
	return nil
}

// PrimWriteShared implements the write-shared primitive (R7RS).
// Writes a machine-readable representation of an object using datum labels
// (#n= and #n#) for shared and circular structure.
// R7RS §6.13.3: write-shared always uses datum labels for shared structure.
//
// (write-shared obj) or (write-shared obj port)
func PrimWriteShared(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-shared: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-shared: expected a list but got %s", tuple.SchemeString())
	}
	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "write-shared: expected an output port but got %T", port)
		}
	}
	// Use cycle-aware writer with datum labels for shared structure
	_, err := writer.Write([]byte(values.WriteValueToString(obj)))
	if err != nil {
		return values.WrapForeignErrorf(err, "write-shared: error writing to output port")
	}
	mc.SetValues()
	return nil
}

// PrimReadChar implements the read-char primitive.
// R7RS §6.13.2: (read-char [port])
// Reads and returns a single character from the input port.
func PrimReadChar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-char: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-char: expected a list but got %s", tuple.SchemeString())
	}

	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-char: expected an input port but got %T", port)
		}
	}

	r, _, err := runeReader.ReadRune()
	if err == io.EOF {
		mc.SetValue(values.EofObject)
		return nil
	}
	if err != nil {
		return values.WrapForeignErrorf(err, "read-char: error reading character")
	}
	mc.SetValue(values.NewCharacter(r))
	return nil
}

// PrimPeekChar implements the peek-char primitive.
// R7RS §6.13.2: (peek-char [port])
// Reads and returns a single character from the input port without consuming it.
func PrimPeekChar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "peek-char: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "peek-char: expected a list but got %s", tuple.SchemeString())
	}

	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "peek-char: expected an input port but got %T", port)
		}
	}

	r, _, err := runeReader.ReadRune()
	if err == io.EOF {
		mc.SetValue(values.EofObject)
		return nil
	}
	if err != nil {
		return values.WrapForeignErrorf(err, "peek-char: error reading character")
	}
	// Unread the character so it can be read again
	err = runeReader.UnreadRune()
	if err != nil {
		return values.WrapForeignErrorf(err, "peek-char: error unreading character")
	}
	mc.SetValue(values.NewCharacter(r))
	return nil
}

// PrimReadLine implements the read-line primitive.
// R7RS §6.13.2: (read-line [port])
// Reads a line of text from the input port, not including the line ending.
func PrimReadLine(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-line: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-line: expected a list but got %s", tuple.SchemeString())
	}

	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-line: expected an input port but got %T", port)
		}
	}

	var line []rune
	for {
		r, _, err := runeReader.ReadRune()
		if err == io.EOF {
			if len(line) == 0 {
				mc.SetValue(values.EofObject)
				return nil
			}
			break
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "read-line: error reading line")
		}
		if r == '\n' {
			break
		}
		// Handle \r\n by discarding \r if followed by \n
		if r == '\r' {
			nextR, _, err := runeReader.ReadRune()
			if err == nil && nextR != '\n' {
				runeReader.UnreadRune() //nolint:errcheck
			}
			break
		}
		line = append(line, r)
	}

	mc.SetValue(values.NewMutableString(string(line)))
	return nil
}

// PrimCharReadyQ implements the char-ready? primitive.
// R7RS §6.13.2: (char-ready? [port])
// Returns #t if a character is ready on the input port, #f otherwise.
func PrimCharReadyQ(_ context.Context, mc *machine.MachineContext) error {
	// For now, we assume a character is always ready for string input ports
	// and the character input port (stdin may block, but we can't easily check)
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimReadString implements the read-string primitive.
// R7RS §6.13.2: (read-string k [port])
// Reads up to k characters from the input port and returns them as a string.
func PrimReadString(_ context.Context, mc *machine.MachineContext) error {
	kVal := mc.Arg(0)
	rest := mc.Arg(1)

	k, ok := kVal.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "read-string: expected an integer for k but got %T", kVal)
	}
	if k.Value < 0 {
		return values.NewForeignError("read-string: k must be non-negative")
	}

	// Get the port from rest arguments
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-string: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-string: expected a list but got %s", tuple.SchemeString())
	}

	var runeReader runeReaderUnreader
	if tuple.IsEmptyList() {
		inpp := GetCurrentInputPort()
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterInputPort:
			runeReader = p.Value.(runeReaderUnreader)
		case *values.StringInputPort:
			runeReader = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-string: expected an input port but got %T", port)
		}
	}

	// Read up to k characters
	chars := make([]rune, 0, k.Value)
	for i := int64(0); i < k.Value; i++ {
		r, _, err := runeReader.ReadRune()
		if err == io.EOF {
			break
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "read-string: error reading string")
		}
		chars = append(chars, r)
	}

	if len(chars) == 0 {
		mc.SetValue(values.EofObject)
		return nil
	}

	mc.SetValue(values.NewMutableString(string(chars)))
	return nil
}

// PrimWriteString implements the write-string primitive.
// R7RS §6.13.3: (write-string string [port [start [end]]])
// Writes the characters of string (optionally between start and end) to port.
func PrimWriteString(_ context.Context, mc *machine.MachineContext) error {
	strVal := mc.Arg(0)
	rest := mc.Arg(1)

	str, ok := strVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "write-string: expected a string but got %T", strVal)
	}

	runes := str.Runes()
	length := len(runes)
	start := 0
	end := length

	// Parse optional arguments: [port [start [end]]]
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-string: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-string: expected a list but got %s", tuple.SchemeString())
	}

	var writer io.Writer
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort().Value
	} else {
		port := tuple.Car()
		switch p := port.(type) {
		case *values.CharacterOutputPort:
			writer = p.Value
		case *values.StringOutputPort:
			writer = p
		case *values.BytevectorOutputPort:
			writer = p
		default:
			return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "write-string: expected an output port but got %T", port)
		}

		// Check for start/end arguments
		if tuple.Cdr() != values.EmptyList {
			tuple2, ok := tuple.Cdr().(values.Tuple)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "write-string: improper argument list")
			}
			startVal, ok := tuple2.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "write-string: expected an integer for start but got %T", tuple2.Car())
			}
			start = int(startVal.Value)

			if tuple2.Cdr() != values.EmptyList {
				tuple3, ok := tuple2.Cdr().(values.Tuple)
				if !ok {
					return values.WrapForeignErrorf(values.ErrNotAList, "write-string: improper argument list")
				}
				endVal, ok := tuple3.Car().(*values.Integer)
				if !ok {
					return values.WrapForeignErrorf(values.ErrNotANumber, "write-string: expected an integer for end but got %T", tuple3.Car())
				}
				end = int(endVal.Value)
			}
		}
	}

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("write-string: invalid indices")
	}

	// Write the substring
	_, err := writer.Write([]byte(string(runes[start:end])))
	if err != nil {
		return values.WrapForeignErrorf(err, "write-string: error writing to output port")
	}
	mc.SetValues()
	return nil
}

// PrimWriteU8 implements the write-u8 primitive.
// R7RS §6.13.3: (write-u8 byte [port])
// Writes byte to the given binary output port and returns an unspecified value.
func PrimWriteU8(_ context.Context, mc *machine.MachineContext) error {
	byteVal := mc.Arg(0)
	rest := mc.Arg(1)

	// Validate byte argument (must be exact integer 0-255)
	var b byte
	switch v := byteVal.(type) {
	case *values.Integer:
		if v.Value < 0 || v.Value > 255 {
			return values.NewForeignError("write-u8: byte must be an exact integer in the range 0-255")
		}
		b = byte(v.Value)
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "write-u8: expected an exact integer but got %T", byteVal)
	}

	// Get the port from rest arguments
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-u8: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-u8: expected a list but got %s", tuple.SchemeString())
	}

	// Write to the appropriate port
	if tuple.IsEmptyList() {
		// Default to current output port - but write-u8 requires a binary port
		return values.NewForeignError("write-u8: no binary output port specified")
	}

	port := tuple.Car()
	switch p := port.(type) {
	case *values.BytevectorOutputPort:
		err := p.WriteByte(b)
		if err != nil {
			return values.WrapForeignErrorf(err, "write-u8: error writing byte")
		}
	case *values.BinaryOutputPort:
		_, err := p.Write([]byte{b})
		if err != nil {
			return values.WrapForeignErrorf(err, "write-u8: error writing byte")
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "write-u8: expected a binary output port but got %T", port)
	}

	mc.SetValues()
	return nil
}

// PrimReadU8 implements the read-u8 primitive.
// R7RS §6.13.3: (read-u8 [port])
// Reads the next byte from the given binary input port and returns it as an exact integer.
// Returns eof-object at end of file.
func PrimReadU8(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-u8: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-u8: expected a list but got %s", tuple.SchemeString())
	}

	if tuple.IsEmptyList() {
		return values.NewForeignError("read-u8: no binary input port specified")
	}

	port := tuple.Car()
	switch p := port.(type) {
	case *values.BytevectorInputPort:
		b, err := p.ReadByte()
		if err == io.EOF {
			mc.SetValue(values.EofObject)
			return nil
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "read-u8: error reading byte")
		}
		mc.SetValue(values.NewInteger(int64(b)))
	case *values.BinaryInputPort:
		buf := make([]byte, 1)
		n, err := p.Read(buf)
		if err == io.EOF || n == 0 {
			mc.SetValue(values.EofObject)
			return nil
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "read-u8: error reading byte")
		}
		mc.SetValue(values.NewInteger(int64(buf[0])))
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-u8: expected a binary input port but got %T", port)
	}

	return nil
}

// PrimPeekU8 implements the peek-u8 primitive.
// R7RS §6.13.3: (peek-u8 [port])
// Like read-u8, but does not consume the byte from the port.
func PrimPeekU8(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "peek-u8: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "peek-u8: expected a list but got %s", tuple.SchemeString())
	}

	if tuple.IsEmptyList() {
		return values.NewForeignError("peek-u8: no binary input port specified")
	}

	port := tuple.Car()
	switch p := port.(type) {
	case *values.BytevectorInputPort:
		b, err := p.ReadByte()
		if err == io.EOF {
			mc.SetValue(values.EofObject)
			return nil
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "peek-u8: error reading byte")
		}
		// Unread the byte so it can be read again
		err = p.UnreadByte()
		if err != nil {
			return values.WrapForeignErrorf(err, "peek-u8: error unreading byte")
		}
		mc.SetValue(values.NewInteger(int64(b)))
	case *values.BinaryInputPort:
		// BinaryInputPort wraps io.Reader which doesn't support UnreadByte
		// We would need a buffered reader for this - for now, return error
		return values.NewForeignError("peek-u8: peek not supported on this binary input port")
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "peek-u8: expected a binary input port but got %T", port)
	}

	return nil
}

// PrimU8ReadyQ implements the u8-ready? primitive.
// R7RS §6.13.3: (u8-ready? [port])
// Returns #t if a byte is available for reading from the binary input port.
func PrimU8ReadyQ(_ context.Context, mc *machine.MachineContext) error {
	// For now, assume a byte is always ready for bytevector input ports
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimReadBytevector implements the read-bytevector primitive.
// R7RS §6.13.3: (read-bytevector k [port])
// Reads the next k bytes from port into a newly allocated bytevector.
// Returns eof-object if no bytes are available before end of file.
func PrimReadBytevector(_ context.Context, mc *machine.MachineContext) error {
	kVal := mc.Arg(0)
	rest := mc.Arg(1)

	k, ok := kVal.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "read-bytevector: expected an integer for k but got %T", kVal)
	}
	if k.Value < 0 {
		return values.NewForeignError("read-bytevector: k must be non-negative")
	}

	// Get the port from rest arguments
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector: expected a list but got %s", tuple.SchemeString())
	}

	if tuple.IsEmptyList() {
		return values.NewForeignError("read-bytevector: no binary input port specified")
	}

	port := tuple.Car()

	// Read up to k bytes
	buf := make([]byte, k.Value)
	var n int
	var err error

	switch p := port.(type) {
	case *values.BytevectorInputPort:
		n, err = p.Read(buf)
	case *values.BinaryInputPort:
		n, err = p.Read(buf)
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-bytevector: expected a binary input port but got %T", port)
	}

	if err == io.EOF && n == 0 {
		mc.SetValue(values.EofObject)
		return nil
	}
	if err != nil && err != io.EOF {
		return values.WrapForeignErrorf(err, "read-bytevector: error reading from port")
	}

	// Create bytevector from read bytes
	bv := make(values.ByteVector, n)
	for i := 0; i < n; i++ {
		bv[i] = values.Byte{Value: buf[i]}
	}
	mc.SetValue(&bv)
	return nil
}

// PrimReadBytevectorBang implements the read-bytevector! primitive.
// R7RS §6.13.3: (read-bytevector! bytevector [port [start [end]]])
// Reads bytes from port into an existing bytevector.
// Returns the number of bytes read, or eof-object if no bytes available.
func PrimReadBytevectorBang(_ context.Context, mc *machine.MachineContext) error {
	bvVal := mc.Arg(0)
	rest := mc.Arg(1)

	bv, ok := bvVal.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "read-bytevector!: expected a bytevector but got %T", bvVal)
	}

	// Parse optional arguments: [port [start [end]]]
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector!: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector!: expected a list but got %s", tuple.SchemeString())
	}

	if tuple.IsEmptyList() {
		return values.NewForeignError("read-bytevector!: no binary input port specified")
	}

	port := tuple.Car()
	start := int64(0)
	end := int64(len(*bv))

	// Check for start/end arguments
	if tuple.Cdr() != values.EmptyList {
		tuple2, ok := tuple.Cdr().(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector!: improper argument list")
		}
		if !tuple2.IsEmptyList() {
			startVal, ok := tuple2.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "read-bytevector!: expected an integer for start but got %T", tuple2.Car())
			}
			start = startVal.Value

			if tuple2.Cdr() != values.EmptyList {
				tuple3, ok := tuple2.Cdr().(values.Tuple)
				if !ok {
					return values.WrapForeignErrorf(values.ErrNotAList, "read-bytevector!: improper argument list")
				}
				if !tuple3.IsEmptyList() {
					endVal, ok := tuple3.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotANumber, "read-bytevector!: expected an integer for end but got %T", tuple3.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	// Validate indices
	bvLen := int64(len(*bv))
	if start < 0 || start > bvLen {
		return values.NewForeignError("read-bytevector!: start index out of bounds")
	}
	if end < start || end > bvLen {
		return values.NewForeignError("read-bytevector!: end index out of bounds")
	}

	// Read into temporary buffer
	buf := make([]byte, end-start)
	var n int
	var err error

	switch p := port.(type) {
	case *values.BytevectorInputPort:
		n, err = p.Read(buf)
	case *values.BinaryInputPort:
		n, err = p.Read(buf)
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "read-bytevector!: expected a binary input port but got %T", port)
	}

	if err == io.EOF && n == 0 {
		mc.SetValue(values.EofObject)
		return nil
	}
	if err != nil && err != io.EOF {
		return values.WrapForeignErrorf(err, "read-bytevector!: error reading from port")
	}

	// Copy bytes into the bytevector
	for i := 0; i < n; i++ {
		(*bv)[start+int64(i)] = values.Byte{Value: buf[i]}
	}

	mc.SetValue(values.NewInteger(int64(n)))
	return nil
}

// PrimWriteBytevector implements the write-bytevector primitive.
// R7RS §6.13.3: (write-bytevector bytevector [port [start [end]]])
// Writes the bytes of bytevector to port.
func PrimWriteBytevector(_ context.Context, mc *machine.MachineContext) error {
	bvVal := mc.Arg(0)
	rest := mc.Arg(1)

	bv, ok := bvVal.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "write-bytevector: expected a bytevector but got %T", bvVal)
	}

	bvLen := int64(len(*bv))
	start := int64(0)
	end := bvLen

	// Parse optional arguments: [port [start [end]]]
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-bytevector: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-bytevector: expected a list but got %s", tuple.SchemeString())
	}

	if tuple.IsEmptyList() {
		return values.NewForeignError("write-bytevector: no binary output port specified")
	}

	port := tuple.Car()

	// Check for start/end arguments
	if tuple.Cdr() != values.EmptyList {
		tuple2, ok := tuple.Cdr().(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "write-bytevector: improper argument list")
		}
		if !tuple2.IsEmptyList() {
			startVal, ok := tuple2.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "write-bytevector: expected an integer for start but got %T", tuple2.Car())
			}
			start = startVal.Value

			if tuple2.Cdr() != values.EmptyList {
				tuple3, ok := tuple2.Cdr().(values.Tuple)
				if !ok {
					return values.WrapForeignErrorf(values.ErrNotAList, "write-bytevector: improper argument list")
				}
				if !tuple3.IsEmptyList() {
					endVal, ok := tuple3.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotANumber, "write-bytevector: expected an integer for end but got %T", tuple3.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	// Validate indices
	if start < 0 || start > bvLen {
		return values.NewForeignError("write-bytevector: start index out of bounds")
	}
	if end < start || end > bvLen {
		return values.NewForeignError("write-bytevector: end index out of bounds")
	}

	// Convert to []byte
	buf := make([]byte, end-start)
	for i := start; i < end; i++ {
		buf[i-start] = (*bv)[i].Value
	}

	// Write to the appropriate port
	switch p := port.(type) {
	case *values.BytevectorOutputPort:
		_, err := p.Write(buf)
		if err != nil {
			return values.WrapForeignErrorf(err, "write-bytevector: error writing to port")
		}
	case *values.BinaryOutputPort:
		_, err := p.Write(buf)
		if err != nil {
			return values.WrapForeignErrorf(err, "write-bytevector: error writing to port")
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "write-bytevector: expected a binary output port but got %T", port)
	}

	mc.SetValues()
	return nil
}

// PrimFlushOutputPort implements the flush-output-port primitive.
// R7RS §6.13.3: (flush-output-port [port])
// Flushes any buffered output to the underlying output device.
func PrimFlushOutputPort(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "flush-output-port: expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "flush-output-port: expected a list but got %s", tuple.SchemeString())
	}

	var port values.Value
	if tuple.IsEmptyList() {
		port = GetCurrentOutputPort()
	} else {
		port = tuple.Car()
	}

	// Check if the port supports flushing (implements Sync or Flush)
	switch p := port.(type) {
	case *values.CharacterOutputPort:
		if syncer, ok := p.Value.(interface{ Sync() error }); ok {
			if err := syncer.Sync(); err != nil {
				return values.WrapForeignErrorf(err, "flush-output-port: error flushing port")
			}
		} else if flusher, ok := p.Value.(interface{ Flush() error }); ok {
			if err := flusher.Flush(); err != nil {
				return values.WrapForeignErrorf(err, "flush-output-port: error flushing port")
			}
		}
		// If neither interface is available, we just silently succeed
	case *values.StringOutputPort:
		// String output ports don't need flushing
	case *values.BytevectorOutputPort:
		// Bytevector output ports don't need flushing
	case *values.BinaryOutputPort:
		if syncer, ok := p.Value.(interface{ Sync() error }); ok {
			if err := syncer.Sync(); err != nil {
				return values.WrapForeignErrorf(err, "flush-output-port: error flushing port")
			}
		} else if flusher, ok := p.Value.(interface{ Flush() error }); ok {
			if err := flusher.Flush(); err != nil {
				return values.WrapForeignErrorf(err, "flush-output-port: error flushing port")
			}
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "flush-output-port: expected an output port but got %T", port)
	}

	mc.SetValues()
	return nil
}
