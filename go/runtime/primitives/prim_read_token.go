// Copyright 2025 Aaron Alpar
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

package primitives

import (
	"context"
	"io"
	"weak"

	"wile/machine"
	"wile/tokenizer"
	"wile/values"
)

// PrimReadToken implements the (read-token) primitive.
// Reads a single token from port.
func PrimReadToken(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
