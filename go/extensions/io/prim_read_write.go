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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if values.IsEmptyList(pr) {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := pr.Car()
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
	q := syn.UnwrapAll()
	mc.SetValue(q)
	return nil
}

// PrimReadToken implements the (read-token) primitive.
// Reads a single token from port.
func PrimReadToken(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if values.IsEmptyList(pr) {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := pr.Car()
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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}

	// Get the port to read from
	var portKey values.Value
	var runeReader runeReaderUnreader
	if values.IsEmptyList(pr) {
		inpp := GetCurrentInputPort()
		portKey = inpp
		runeReader = inpp.Value.(runeReaderUnreader)
	} else {
		port := pr.Car()
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
func PrimWrite(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
	_, err := writer.Write([]byte(obj.SchemeString()))
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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
func PrimDisplay(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
	_, err := writer.Write([]byte(StringValue(obj)))
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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "write-simple: expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-simple: expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
//
// NOTE: This is a basic implementation that behaves like write.
// Full shared structure support with datum labels would require:
// 1. First pass to detect shared nodes (nodes referenced more than once)
// 2. Second pass to output with labels for shared nodes
//
// (write-shared obj) or (write-shared obj port)
func PrimWriteShared(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	o := mc.Arg(1)
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "write-shared: expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "write-shared: expected a list but got %s", pr.SchemeString())
	}
	var writer io.Writer
	if values.IsEmptyList(pr) {
		writer = GetCurrentOutputPort().Value
	} else {
		port := pr.Car()
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
	// TODO: Implement proper shared structure detection and datum labels
	// For now, this behaves like write
	_, err := writer.Write([]byte(obj.SchemeString()))
	if err != nil {
		return values.WrapForeignErrorf(err, "write-shared: error writing to output port")
	}
	mc.SetValues()
	return nil
}
