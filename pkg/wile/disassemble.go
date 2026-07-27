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

package wile

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/werr"
)

// FormLabel returns a human-readable type label for a value:
// "primitive" for foreign (Go-implemented) closures,
// "procedure" for compiled Scheme closures,
// "" for everything else, including parameters, continuations, typed nils,
// and non-callable values.
func (p *Engine) FormLabel(v Value) string {
	inner := unwrapValue(v)
	switch c := inner.(type) {
	case *machine.ForeignClosure:
		if c == nil {
			return ""
		}
		return "primitive"
	case *machine.MachineClosure:
		if c == nil {
			return ""
		}
		return "procedure"
	case *machine.CaseLambdaClosure:
		if c == nil {
			return ""
		}
		return "procedure"
	default:
		return ""
	}
}

// DisassembleValue returns the formatted disassembly of a callable value.
// For compiled closures, shows bytecode instructions. For case-lambda,
// shows each clause separately. For foreign closures, shows name, arity,
// and documentation. Returns an error for anything other than a compiled
// closure, a case-lambda, or a foreign closure, parameters and continuations
// included.
func (p *Engine) DisassembleValue(v Value) (string, error) {
	inner := unwrapValue(v)
	switch c := inner.(type) {
	case *machine.MachineClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil closure")
		}
		return machine.DisassembleString(c.Template()), nil

	case *machine.CaseLambdaClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil case-lambda closure")
		}
		var q strings.Builder
		for i, clause := range c.Clauses() {
			if i > 0 {
				q.WriteString("\n")
			}
			fmt.Fprintf(&q, "--- clause %d ---\n", i)
			q.WriteString(machine.DisassembleString(clause.Template()))
		}
		return q.String(), nil

	case *machine.ForeignClosure:
		if c == nil {
			return "", werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"DisassembleValue: nil foreign closure")
		}
		var q strings.Builder
		fmt.Fprintf(&q, "%s  (foreign, params: %d, variadic: %v)\n",
			c.Name(), c.ParameterCount(), c.IsVariadic())
		if c.Doc() != "" {
			fmt.Fprintf(&q, "doc: %s\n", c.Doc())
		}
		return q.String(), nil

	default:
		return "", werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"DisassembleValue: not a procedure (type: %T)", inner)
	}
}
